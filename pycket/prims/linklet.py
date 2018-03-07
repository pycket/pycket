""" Implementation of linklets

class W_LinkletInstance(W_Object)
class W_Linklet(object)
class W_LinkletBundle(W_Object)
class W_LinkletDirectory(W_Object)
 """

#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from pycket.expand import readfile_rpython, getkey
from pycket.interpreter import DefineValues, interpret_one, Context, return_value, return_value_direct, return_multi_vals, Quote, App, ModuleVar, make_lambda, LexicalVar, LinkletVar, CaseLambda, make_let, If, make_letrec, Begin, CellRef, SetBang, Begin0, VariableReference, WithContinuationMark, Var, Lambda
from pycket.assign_convert import assign_convert
from pycket.values import W_Object, W_Symbol, w_true, w_false, W_List, W_Cons, W_WrappedConsProper, w_null, Values, W_Number, w_void, W_Bool, w_default_continuation_prompt_tag, parameterization_key, W_ImmutableBytes, W_VariableReference
from pycket.values_string import W_String
from pycket.error import SchemeException
from pycket import pycket_json
from pycket.prims.expose import prim_env, expose, default
from pycket.prims.general import make_pred
from pycket.prims.correlated import W_Correlated
from pycket.prims.vector import vector
from pycket.AST import AST
from pycket.cont import Prompt, NilCont

class W_Uninitialized(W_Object):
    errorname = "uninitialized"
    _attrs_ = []
    def __init__(self):
        pass
    def tostring(self):
        return "#<uninitialized>"

w_uninitialized = W_Uninitialized()

class W_LinkletInstance(W_Object):

    def __init__(self, name, exports, defs, data=w_false):
        self.name = name # W_Symbol (for debugging)
        self.exports = exports # {external_id(W_Symbol):internal_id(W_Symbol)}
        self.defs = defs # {W_Symbol-W_Object}
        self.data = data
        
    def tostring(self):
        exports_str = ""
        for ext_name, int_name in self.exports.iteritems():
            exports_str += "[%s - %s]," % (ext_name.tostring(), int_name.tostring())

        defs_str = ", ".join([d.tostring() for d in self.defs])
        return "<#linklet-instance:%s|(e/i)[ %s ]|%s>\n" % (self.name, exports_str, defs_str)

    def get_name(self):
        return self.name

    def get_exports(self):
        return self.exports

    def get_data(self):
        return self.data

    def set_data(self, data):
        self.data = data

    def get_defs(self):
        return self.defs

    def is_defined(self, name):
        """ Checks if given id is defined by this instance. """
        return name in self.defs

    def is_exported(self, name):
        """ Checks if given id is exported by this instance. """
        return name in self.exports

    def is_var_uninitialized(self, internal_name):
        return self.is_defined(internal_name) and self.defs[internal_name] is w_uninitialized
    
    def internal_id_of(self, external_id):
        # may be the same with external_id if it's not renamed
        # or may be the internal_id for it
        return self.exports[external_id]

    def external_id_of(self, internal_id):
        for ext_id, int_id in self.exports.iteritems():
            if internal_id is int_id:
                return ext_id
        return None

    def provide_val_of(self, name):
        # to be used when a linklet imports from this instance
        if not self.is_exported(name):
            raise SchemeException("Id --> %s <-- is not exported by this instance : \n%s" % (name.tostring(), self.tostring()))

        return self.get_val_of(self.internal_id_of(name))

    def get_val_of(self, internal_name):
        """ Returns a defined value."""
        if not self.is_defined(internal_name):
            raise SchemeException("Reference to an undefined variable : %s" % internal_name.tostring())

        val = self.defs[internal_name]
        if val is w_uninitialized:
            raise SchemeException("Reference to an uninitialized variable : %s" % internal_name.tostring())

        return val

    def provide_all_exports_to_prim_env(self):
        for name, value in self.defs.iteritems():
            prim_env[name] = value

    # FIXME : get rid of LinkletVar entirely
    def lookup_linkl(self, id_sym, import_num):
        # lookup is gonna be called by a LinkletVar and
        # a LinkletVar will always get a value from the
        # instance they belong to, so no need to check renamings
        return self.get_val_of(id_sym)

    def set_defs(self, defs):
        self.defs = defs

    def set_bang_def(self, name, val):
        self.defs[name] = val

    def add_def(self, name, val, reason="definition", linkl=None):
        if name in self.defs:
            raise SchemeException("Duplicate %s : %s" % (reason, name.tostring()))

        self.defs[name] = val

    def add_export(self, ext_name, int_name):
        if ext_name in self.exports:
            raise SchemeException("Name is already exported through this instance : %s -- id : %s" % (self.name.tostring(), ext_name.tostring()))
        self.exports[ext_name] = int_name

    def initialize_or_add(self, name, val):
        # adds only if it doesn't exist OR is uninitialized
        # otherwise doesn't do anything, no errors
        if (not self.is_defined(name)) or (self.is_defined(name) and self.is_var_uninitialized(name)):
            self.defs[name] = val
            # Anything wrong with blindly exporting ids?
            self.exports[name] = name

    def initialize_var(self, name, val):
        # adds only if self has it but it's uninitialized
        # otherwise doesn't do anything (no errors)
        if self.is_defined(name) and self.is_var_uninitialized(name):
            self.defs[name] = val

        
    def append_defs(self, new_defs):

        # check if we already have any of the new defs
        for name, val in new_defs.iteritems():
            if self.is_defined(name):
                raise SchemeException("Duplicate definition : %s" % name)

        self.defs.update(new_defs)
        
class W_LinkletBundle(W_Object):
    # Information in a linklet bundle is keyed by either a symbol or a fixnum
    
    def __init__(self,bundle_mapping):
        self.bundle_mapping = bundle_mapping

@expose("hash->linklet-bundle", [W_Object])
def hash_to_linklet_bundle(content):
    return W_LinkletBundle(content)

@expose("linklet-bundle->hash", [W_LinkletBundle])
def linklet_bundle_to_hash(linkl_bundle):
    return linkl_bundle.bundle_mapping

class W_LinkletDirectory(W_Object):

    # When a Racket module has submodules, the linklet bundles for the module and the submodules are grouped together in a linklet directory. A linklet directory can have nested linklet directories. Information in a linklet directory is keyed by #f or a symbol, where #f must be mapped to a linklet bundle (if anything) and each symbol must be mapped to a linklet directory. A linklet directory can be equivalently viewed as a mapping from a lists of symbols to a linklet bundle.
    
    def __init__(self,dir_mapping):
        self.dir_mapping = dir_mapping

@expose("hash->linklet-directory", [W_Object])
def hash_to_linklet_directory(content):
    return W_LinkletDirectory(content)

@expose("linklet-directory->hash", [W_LinkletDirectory])
def linklet_directory_to_hash(linkl_directory):
    return linkl_directory.dir_mapping

class W_Linklet(W_Object):

    def __init__(self, name, importss, exports, all_forms):
        self.name = name # W_Symbol -- for debugging
        """ importss
        [...,{W_Symbol:W_Symbol},...]
        [...,{exported_by_the_instance:referenced_in_self_forms},...]

        if not renamed, then it has it's own name as the value (thanks RPython!)
        """
        self.importss = importss
        self.exports = exports
        # {internal_id(W_Symbol):external_id(W_Symbol)}
        # again, may be the same if it's not renamed

        self.forms = all_forms # [..., AST ,...]

    def tostring(self):
        forms_str = ", ".join([f.tostring() for f in self.forms])
        importss_str = ""
        for imp_dict in self.importss:
            imports_str = ""
            for ext_name, int_name in imp_dict.iteritems():
                imports_str += "[%s - %s]," % (ext_name.tostring(), int_name.tostring())
            importss_str += "[%s]," % imports_str

        exports_str = ""
        for int_name, ext_name in self.exports.iteritems():
            exports_str += "[%s - %s]," % (int_name.tostring(), ext_name.tostring())

        return "#<linklet:%s|(e/i)[ %s ]|(i/e)[ %s ]|%s>\n" % (self.name, importss_str, exports_str, forms_str)

    def instantiate(self, w_imported_instances, config, toplevel_eval=False, prompt=True, target=None, cont_params=None):
        """ Instantiates the linklet:
        --- takes the imported linklet instances (list W_LinkletInstances)
        --- extracts the specified set of variables
        --- returns a W_LinkletInstance (or a result if toplevel_eval)
        """
        
        l_importss = len(self.importss)
        l_given_instances = len(w_imported_instances)
        
        if l_importss != l_given_instances:
            raise SchemeException("Required %s instances but given %s" % (str(l_importss), str(l_given_instances)))


        """
        ||| create instance or use target to evaluate the linklet forms
        """
        used_instance = W_LinkletInstance(self.name, self.exports, {})

        if target:
           used_instance.set_data(target.get_data())
        
        """
        -- get the imported values from w_imported_instances -- name, val 
        -- put them into the used_instance
        """
        for instance_index, imports_dict in enumerate(self.importss):
            for ext_name, int_name in imports_dict.iteritems():

                if int_name in self.exports:
                    raise SchemeException("export duplicates import : %s" % int_name.tostring())

                imported_val = w_imported_instances[instance_index].provide_val_of(ext_name)
                if imported_val is w_uninitialized:
                    raise SchemeException("Reference to a variable that is uninitialized : %s" % ext_name.tostring())

                used_instance.add_def(int_name, imported_val, reason="import", linkl=self)

        """
        collect the ids defined in the given linklet's forms
        """
        linklet_defined_ids = []
        for b in self.forms:
            if isinstance(b, DefineValues):
                linklet_defined_ids += b.names

        """
        uninitialize the undefined exports -- name, undef
        """
        for internal_id, external_id in self.exports.iteritems():
            if internal_id not in linklet_defined_ids:
                used_instance.add_def(internal_id, w_uninitialized, reason="un-initialization")
        
        """
        initialize the uninitialized vars using target's defs
        """
        if target:
            for name, val in target.get_defs().iteritems():
                used_instance.initialize_var(name, val)

        """
        prep the environment and the continuation
        put the instance into the environment
        """
        from pycket.env import ToplevelEnv
        env = ToplevelEnv(config, used_instance)
        
        cont = NilCont()
        if prompt:
            cont = Prompt(w_default_continuation_prompt_tag, None, env, cont)

        """
        evaluate forms (LinkletVar always -1), add newly defined vals into the instance

        so far used_instance has : imported values, initialized values (by target)
        """
        return_values = w_void
        for form in self.forms:
            if isinstance(form, DefineValues):
                expression = form.rhs
                values = interpret_one(expression, env, cont).get_all_values()
                len_values = len(values)
                if len(form.names) != len_values:
                    raise SchemeException("wrong number of values for define-values")

                for index in range(len_values):
                    name = form.names[index]
                    value = values[index]
                    
                    used_instance.add_def(name, value)

                    if target:
                        target.initialize_or_add(name, value)
                
                return_values = w_void
            else:
                # form is not a DefineValues, evaluate it for the side effects
                if isinstance(form, SetBang):
                    set_bang_target = form.get_var().get_sym()
                    for instance_index, imports_dict in enumerate(self.importss):
                        if set_bang_target in imports_dict:
                            raise SchemeException("Cannot mutate imported variable")
                return_values = interpret_one(form, env, cont)
                    
                    
        """
        return instance or return value
        """
        if target:
            assert return_values is not None
            return return_values

        return used_instance


    @staticmethod # json_file_name -> W_Linklet
    def load_linklet(json_file_name, loader):
        """ Expands and loads a linklet from a JSON file"""
        data = readfile_rpython(json_file_name)
        json = pycket_json.loads(data)
        assert json.is_object
        json_python_dict = json.value_object()
        assert "linklet" in json_python_dict
        linklet_dict = getkey(json_python_dict, "linklet", type='o')
        assert "exports" in linklet_dict and "body" in linklet_dict # and "importss" in linklet_dict

        # list of JsonObject
        exports_list = getkey(linklet_dict, "exports", type='a')

        exports = {}
        for exp in exports_list:
            if exp.is_array:
                arr = exp.value_array()
                defined_name = W_Symbol.make(arr[0].value_object()['quote'].value_object()['toplevel'].value_string())
                exported_name = W_Symbol.make(arr[1].value_object()['quote'].value_object()['toplevel'].value_string())

                exports[defined_name] = exported_name
            else:
                exp_str = exp.value_object()['quote'].value_object()['toplevel'].value_string()
                exp_sym = W_Symbol.make(exp_str)

                exports[exp_sym] = exp_sym

        imports_list = getkey(linklet_dict, "importss", type='a')

        importss = [] # list of dict
        if "importss" in linklet_dict:
            for imports in imports_list:
                arr = imports.value_array()
                # we don't care about renamed imports because we will only load bootstrap
                # linklets from json (and they have no imports at all)
                # this is effectively only for debugging purposes
                instance_imports = {}
                for id_str in arr:
                    sym = W_Symbol.make(id_str.value_object()['quote'].value_object()['toplevel'].value_string())
                    instance_imports[sym] = sym
                importss.append(instance_imports)
                
        all_forms = []
        for body_form in getkey(linklet_dict, "body", type='a'):
            form = loader.to_ast(body_form)
            form = Context.normalize_term(form)
            form = assign_convert(form)
            form.clean_caches()
            all_forms.append(form)

        config = {}
        config_obj = getkey(linklet_dict, "config", type='o')
        if config_obj is not None:
            for k, v in config_obj.iteritems():
                config[k] = v.value_string()
            
        return W_Linklet(W_Symbol.make(json_file_name), importss, exports, all_forms), config


"""
 (define-values (1/read-compiled-linklet) (hash-ref linklet-primitive-table 'read-compiled-linklet #f))
 (define-values (1/instance-unset-variable!) (hash-ref linklet-primitive-table 'instance-unset-variable! #f))
 (define-values (1/variable-reference?) (hash-ref linklet-primitive-table 'variable-reference? #f))
 (define-values (1/variable-reference-constant?) (hash-ref linklet-primitive-table 'variable-reference-constant? #f))
"""

make_pred("linklet?", W_Linklet)

make_pred("instance?", W_LinkletInstance)


def to_rpython_list(r_list):
    length = 0
    acc = r_list
    while(acc is not w_null):
        length += 1
        acc = acc.cdr()
    acc = r_list
    py_ls = []
    for n in range(length):
        a = acc.car()
        py_ls.append(a)
        acc = acc.cdr()

    return py_ls

def def_vals_to_ast(def_vals_sexp, exports, linkl_toplevels, linkl_imports):
    if not len(to_rpython_list(def_vals_sexp)) == 3:
        raise SchemeException("defs_vals_to_ast : unhandled define-values form : %s" % def_vals_sexp.tostring())

    names = def_vals_sexp.cdr().car() # renames?
    names_ls = to_rpython_list(names)

    body = sexp_to_ast(def_vals_sexp.cdr().cdr().car(), [], exports, linkl_toplevels, linkl_imports, disable_conversions=False, cell_ref=False, name=names_ls[0].variable_name())

    return DefineValues(names_ls, body, names_ls)

def lam_to_ast(lam_sexp, lex_env, exports, linkl_toplevels, linkl_imports, disable_conversions, name=""):
    from pycket.expand import SourceInfo

    lam_sexp_elements = to_rpython_list(lam_sexp)
    l = len(lam_sexp_elements)
    if not (l == 3 or l == 2):
        raise SchemeException("lam_to_ast : unhandled lambda form : %s" % lam_sexp.tostring())

    if lam_sexp.car() is W_Symbol.make("lambda"):
        lam_sexp = lam_sexp.cdr()

    formals_ = lam_sexp.car()
    rest = None
    formals = w_null
    # check for a "rest"
    while (formals_ is not w_null):
        if isinstance(formals_, W_Symbol):
            rest = formals_
            lex_env.append(rest)
            break
        formals = W_Cons.make(formals_.car(), formals)
        formals_ = formals_.cdr()

    formals_ls = to_rpython_list(formals)
    formals_ls.reverse() # FIXME : refactor the double reverse

    body = sexp_to_ast(lam_sexp.cdr().car(), formals_ls + lex_env, exports, linkl_toplevels, linkl_imports, disable_conversions, cell_ref=False, name=name)
    dummy = 1
    return make_lambda(formals_ls, rest, [body], SourceInfo(dummy, dummy, dummy, dummy, name))

def is_imported(id_sym, linkl_importss):
    for imports_dict in linkl_importss:
        for ext_id, int_id in imports_dict.iteritems():
            if id_sym is int_id:
                return True
    return False

def let_like_to_ast(let_sexp, lex_env, exports, linkl_toplevels, linkl_imports, disable_conversions, is_letrec, cell_ref):
    if not len(to_rpython_list(let_sexp)) == 3:
        raise SchemeException("let_to_ast : unhandled let form : %s" % let_sexp.tostring())

    varss_rhss = to_rpython_list(let_sexp.cdr().car()) # a little inefficient but still..
    varss_list = [None] * len(varss_rhss)
    rhss_list = [None] * len(varss_rhss)
    num_ids = 0
    i = 0
    for w_vars_rhss in varss_rhss:
        varr = [v.get_obj() if isinstance(v, W_Correlated) else v for v in to_rpython_list(w_vars_rhss.car())]
        varss_list[i] = varr
        if is_letrec:
            for varss in varss_list:
                if varss is not None:
                    lex_env += varss

        rhsr = sexp_to_ast(w_vars_rhss.cdr().car(), lex_env, exports, linkl_toplevels, linkl_imports, disable_conversions, cell_ref=is_letrec)
        rhss_list[i] = rhsr
        i += 1
        num_ids += len(varr)

    ids = [None] * num_ids
    index = 0
    for vars_ in varss_list:
        for var_ in vars_:
            ids[index] = var_ # W_Symbol
            index += 1

    body = sexp_to_ast(let_sexp.cdr().cdr().car(), ids + lex_env, exports, linkl_toplevels, linkl_imports, disable_conversions, cell_ref=False)

    if len(ids) == 0:
        return Begin.make([body])

    if is_letrec:
        return make_letrec(list(varss_list), list(rhss_list), [body])
    else:
        return make_let(varss_list, rhss_list, [body])

def is_val_type(form):
    val_types = [W_Number, W_Bool, W_String, W_ImmutableBytes]
    for t in val_types:
        if isinstance(form, t):
            return True
    return False

def sexp_to_ast(form, lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions=False, cell_ref=False, name=""):

    if isinstance(form, W_Correlated):
        return sexp_to_ast(form.get_obj(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
    elif is_val_type(form):
        form = Quote(form)
    elif isinstance(form, W_Symbol):
        # lexical?
        if form in lex_env:
            if cell_ref:
                form = CellRef(form)
            else:
                form = LexicalVar(form)
        # toplevel linklet var
        elif (form in linkl_toplevels) or (form in exports):
            form = LinkletVar(form, -1)
        elif is_imported(form, linkl_importss):
            # imported linklet var
            form = LinkletVar(form, -1) ############# FIXME -- no need for instance indices anymore
        else:
            # kernel primitive ModuleVar
            form = ModuleVar(form, "#%kernel", form, None)
    elif isinstance(form, W_List):
        if form.car() is W_Symbol.make("begin"):
            form = Begin.make([sexp_to_ast(f, lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name) for f in to_rpython_list(form.cdr())])
        elif form.car() is W_Symbol.make("begin0"):
            fst = sexp_to_ast(form.cdr().car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
            rst = [sexp_to_ast(f, lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name) for f in to_rpython_list(form.cdr().cdr())]
            if len(rst) == 0:
                form = fst
            else:
                form = Begin0.make(fst, rst)
        elif form.car() is W_Symbol.make("define-values"):
            form = def_vals_to_ast(form, exports, linkl_toplevels, linkl_importss)
        elif form.car() is W_Symbol.make("with-continuation-mark"):
            if len(to_rpython_list(form)) != 4:
                raise SchemeException("Unrecognized with-continuation-mark form : %s" % form.tostring())
            key = sexp_to_ast(form.cdr().car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
            val = sexp_to_ast(form.cdr().cdr().car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
            body = sexp_to_ast(form.cdr().cdr().cdr().car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
            form = WithContinuationMark(key, val, body)
        elif form.car() is W_Symbol.make("#%variable-reference"):
            if form.cdr() is w_null or isinstance(form.cdr().car(), W_Bool):
                form = VariableReference(None, None)
            else:
                if not isinstance(form.cdr().car(), W_Symbol):
                    raise SchemeException("NIY")
                else:
                    var = sexp_to_ast(form.cdr().car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
                    form = VariableReference(var, "dummy.rkt")
        elif form.car() is W_Symbol.make("case-lambda"):
            lams = [lam_to_ast(f, lex_env, exports, linkl_toplevels, linkl_importss, True, name) for f in to_rpython_list(form.cdr())]
            form = CaseLambda(lams)
        elif form.car() is W_Symbol.make("lambda"):
            form = CaseLambda([lam_to_ast(form, lex_env, exports, linkl_toplevels, linkl_importss, True, name)])
        elif form.car() is W_Symbol.make("let-values"):
            form = let_like_to_ast(form, lex_env, exports, linkl_toplevels, linkl_importss, True, False, cell_ref)
        elif form.car() is W_Symbol.make("letrec-values"):
            form = let_like_to_ast(form, lex_env, exports, linkl_toplevels, linkl_importss, True, True, cell_ref)
        elif form.car() is W_Symbol.make("set!"):
            var = sexp_to_ast(form.cdr().car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, True, name)
            rhs = sexp_to_ast(form.cdr().cdr().car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
            assert isinstance(var, Var)
            form = SetBang(var, rhs)
        elif form.car() is W_Symbol.make("quote"):
            if form.cdr().cdr() is not w_null:
                raise SchemeException("malformed quote form : %s" % form.tostring())
            form = Quote(form.cdr().car())
        elif form.car() is W_Symbol.make("if"):
            tst_w = form.cdr().car()
            thn_w = form.cdr().cdr().car()
            els_w = form.cdr().cdr().cdr().car()
            tst = sexp_to_ast(tst_w, lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
            thn = sexp_to_ast(thn_w, lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
            els = sexp_to_ast(els_w, lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
            form = If.make(tst, thn, els)
        else:
            form_inner = sexp_to_ast(form.car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions)

            rands_ls = to_rpython_list(form.cdr())
            rands = [sexp_to_ast(r, lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name) for r in rands_ls]
                    
            form = App.make(form_inner, rands)
    else:
        raise SchemeException("Don't know what to do with this form yet : %s", form)

    if not disable_conversions:
        form = Context.normalize_term(form)
        form = assign_convert(form)
        form.clean_caches()

    return form

# collect the ids in define-values forms
def extract_ids(forms_ls):
    linkl_toplevels = [] # [W_Symbol ...]
    for form in forms_ls:
        if isinstance(form, W_Correlated):
            form = form.get_obj()
        if isinstance(form, W_List) and form.car() is W_Symbol.make("define-values"):
            ids = form.cdr().car()
            ids_ls = to_rpython_list(ids)
            linkl_toplevels += ids_ls # don't worry about the duplicates now
    return linkl_toplevels


@expose("compile-linklet", [W_Object, default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false)], simple=False)
def compile_linklet(form, name, import_keys, get_import, serializable_huh, env, cont):
    return do_compile_linklet(form, name, import_keys, get_import, serializable_huh, env, cont)

def do_compile_linklet(form, name, import_keys, get_import, serializable_huh, env, cont):

    if isinstance(form, W_WrappedConsProper): # s-expr
        # read it and create an AST, put it in a W_Linklet and return
        if not isinstance(form.car(), W_Symbol) or "linklet" != form.car().tostring():
            raise SchemeException("Malformed s-expr. Expected a linklet, got %s" % form.tostring())
        else:
            # Process the imports
            w_importss = form.cdr().car()

            importss_list = []

            importss_acc = w_importss
            while (importss_acc is not w_null):
                importss_current = importss_acc.car()
                inner_acc = {}
                while (importss_current is not w_null):
                    c = importss_current.car()
                    if isinstance(c, W_Symbol):
                        inner_acc[c] = c
                    elif isinstance(c, W_List):
                        if c.cdr().cdr() is not w_null:
                            raise SchemeException("Unhandled renamed import form : %s" % c.tostring())
                        external_id = c.car()
                        internal_id = c.cdr().car()

                        assert isinstance(external_id, W_Symbol) and isinstance(internal_id, W_Symbol)
                        inner_acc[external_id] = internal_id

                    importss_current = importss_current.cdr()

                importss_list.append(inner_acc)
                importss_acc = importss_acc.cdr()

            # Process the exports
            w_exports = form.cdr().cdr().car()

            exports = {}
            r_exports = to_rpython_list(w_exports)
            for exp in r_exports:
                if isinstance(exp, W_WrappedConsProper):
                    internal_name = exp.car() # W_Symbol
                    external_name = exp.cdr().car() # W_Symbol
                    exports[internal_name] = external_name
                else:
                    exports[exp] = exp

            # Process the body
            w_body = form.cdr().cdr().cdr()
            body_forms_ls = to_rpython_list(w_body)

            linkl_toplevel_defined_ids = extract_ids(body_forms_ls)

            body_forms = [sexp_to_ast(b, [], exports, linkl_toplevel_defined_ids, importss_list) for b in body_forms_ls]

            if name is w_false:
                w_name = W_Symbol.make("ad-hoc")
            else:
                w_name = name

            linkl = W_Linklet(w_name, importss_list, exports, body_forms)
            if import_keys is w_false:
                return return_value_direct(linkl, env, cont)
            else:
                return return_multi_vals(Values.make([linkl, import_keys]), env, cont)
            
    else: # correlated
        # take the AST from the correlated and put it in a W_Linklet and return
        raise SchemeException("NYI")

    ##################################
    ##### The optional import-keys and get-import arguments support cross-linklet optimization.
    ##################################
    # (Pdb) import_keys
    # <pycket.vector.W_Vector object at 0x000000001baba608>
    # (Pdb) import_keys.tostring()
    # '#(#f #f #f)'

    # (Pdb) get_import
    # <W_Closure1AsEnvSize3 ['#<procedure:get-module-linklet-info_0:321>', '#<namespace>', '#<procedure:intern-module-use_0:321>'] <pycket.env.ToplevelEnv object at 0x0000000002b8c1e0>>
    ##################################

    ##### As long as serializable? is true, the resulting linklet can be marshaled to and from a byte stream when it is part of a linklet bundle.
        
    # (Pdb) serializable_huh
    # <pycket.values.W_Bool object at 0x00000000024e8640>
    ##################################        

@expose("instance-name", [W_LinkletInstance])
def instance_name(l_inst):
    return l_inst.get_name()
    
@expose("instantiate-linklet", [W_Linklet, W_List, default(W_Object, w_false), default(W_Object, w_true)], simple=False)
def instantiate_linklet(linkl, import_instances, target_instance, use_prompt, env, cont):

    prompt = False
    if use_prompt is w_true: # use-prompt? : any/c = #t - what happens when it is 3 ?
        prompt = True
    
    im_list = to_rpython_list(import_instances)
    expected = len(linkl.importss)
    given = len(im_list)

    if expected != given:
        raise SchemeException("The number of instances in import-instances must match the number of import sets in linklet. Expected %s but got %s" % (expected, given))

    cont_params = None
    from pycket.cont import Link
    if isinstance(cont.marks, Link):
        cont_params = cont.marks.val

    if target_instance is None or target_instance is w_false:
        return return_value(linkl.instantiate(im_list, env.toplevel_env()._pycketconfig, prompt=prompt, target=None, cont_params=cont_params), env, cont)
    
    elif isinstance(target_instance, W_LinkletInstance):
        return return_value(linkl.instantiate(im_list, env.toplevel_env()._pycketconfig, toplevel_eval=True, prompt=prompt, target=target_instance, cont_params=cont_params), env, cont)

    else:
        raise SchemeException("Expected #f or instance? as target-instance, got : %s" % target_instance)

@expose("linklet-import-variables", [W_Linklet])
def linklet_import_variables(linkl):
    importss_py_lst = linkl.importss
    importss = w_null
    for imp_dict in importss_py_lst:
        imp_inner = w_null
        for ext_name, int_name in imp_dict.iteritems():
            imp_inner = W_Cons.make(ext_name, imp_inner)
        importss = W_Cons.make(imp_inner, importss)
    return importss

@expose("linklet-export-variables", [W_Linklet])
def linklet_export_variables(linkl):
    exports = w_null
    for ext_name in linkl.exports.values():
        exports = W_Cons.make(ext_name, exports)
    return exports

@expose("instance-variable-names", [W_LinkletInstance])
def instance_variable_names(inst):
    names = w_null
    for name in inst.defs.keys():
        names = W_Cons.make(name, names)
        
    return names

make_pred("linklet-directory?", W_LinkletDirectory)

make_pred("linklet-bundle?", W_LinkletBundle)

@expose("make-instance") #FIXME: [W_Object, W_Object, [W_Symbol, W_Object] ....]
def make_instance(args): # name, data, *vars_vals

    name = args[0] # W_Symbol
    data = w_false
    mode = w_false
    if len(args) <= 2:
        data = args[1]
        vars_vals = []
    else:
        data = args[1]
        mode = args[2]
        vars_vals = args[3:]

    # check if the vars and vals match
    if ((len(vars_vals) % 2) != 0):
        raise SchemeException("Variable names and values do not match : %s" % vars_vals)

    vars_vals_dict = {}
    exports = {}
    for i in range(0, len(vars_vals), 2):
        n = vars_vals[i]
        v = vars_vals[i+1]
        vars_vals_dict[n] = v
        exports[n] = n

    return W_LinkletInstance(name, exports, vars_vals_dict, data)

@expose("recompile-linklet", [W_Linklet, default(W_Object, None), default(W_Object, w_false), default(W_Object, None)], simple=False)
def recompile_linklet(linkl, name, import_keys, get_import, env, cont):
    if import_keys is not None:
        return return_multi_vals(Values.make([linkl, import_keys]), env, cont)
    else:
        return return_value(linkl, env, cont)

@expose("instance-variable-value", [W_LinkletInstance, W_Symbol, default(W_Object, None)], simple=False)
def instance_variable_value(instance, name, fail_k, env, cont):
    from pycket.interpreter import return_value
    if not instance.is_exported(name):
        if fail_k is not None and fail_k.iscallable():
            return fail_k.call([], env, cont)
        else:
            raise SchemeException("key %s not found in exports of the instance %s" % (name.tostring(), instance.name.tostring()))

    return return_value(instance.provide_val_of(name), env, cont)

@expose("instance-data", [W_LinkletInstance])
def instance_data(inst):
    return inst.get_data()

@expose("eval-linklet", [W_Linklet])
def eval_linklet(l):
    return l

@expose("instance-set-variable-value!", [W_LinkletInstance, W_Symbol, W_Object, default(W_Object, w_false)])
def instance_set_variable_value(instance, name, val, mode):
    instance.set_bang_def(name, val)
    instance.add_export(name, name)
    return w_void

@expose("primitive->compiled-position", [W_Object])
def prim_to_compiled_pos(prim):
    return w_false

@expose("compiled-position->primitive", [W_Object])
def compiled_pos_to_prim(pos):
    return w_false

@expose("primitive-in-category?", [W_Object, W_Object])
def prim_in_category(sym, cat):
    return w_false

@expose("variable-reference->instance", [W_VariableReference, default(W_Bool, w_false)])
def var_ref_to_instance(varref, ref_site):

    if ref_site is w_true:
        return varref.get_instance()
    v = varref.varref.var
    if not v:
        return w_false # anonymous
    else:
        return varref.get_instance()

@expose("variable-reference-from-unsafe?", [W_VariableReference])
def var_ref_from_unsafe_huh(varref):
    """
    Returns #t if the module of the variable reference itself
    (not necessarily a referenced variable) is compiled in unsafe mode,
    #f otherwise.
    """
    return w_false
