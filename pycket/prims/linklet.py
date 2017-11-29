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
from pycket.interpreter import DefineValues, interpret_one, Context, return_value, return_multi_vals, Quote, App, ModuleVar, make_lambda, LexicalVar, LinkletVar, CaseLambda, make_let, If, make_letrec, Begin, CellRef, SetBang, Begin0, VariableReference, WithContinuationMark, Var, Lambda
from pycket.assign_convert import assign_convert
from pycket.values import W_Object, W_Symbol, w_true, w_false, W_List, W_Cons, W_WrappedConsProper, w_null, Values, W_Number, w_void, W_Bool, w_default_continuation_prompt_tag
from pycket.values_string import W_String
from pycket.error import SchemeException
from pycket import pycket_json
from pycket.prims.expose import prim_env, expose, default
from pycket.prims.general import make_pred
from pycket.prims.correlated import W_Correlated
from pycket.prims.vector import vector
from pycket.AST import AST
from pycket.cont import Prompt, NilCont

class W_LinkletInstance(W_Object):
    """
    def tostring(self):

    def get_val(self, id_str):
    def is_defined(self, id_str):
    def is_exporting(self, id_str):

    def provide_all_exports_to_prim_env(self):
    def lookup(self, id_str, import_num):

    def set_defs(self, defs):
    def set_bang_def(self, name, val):
    def add_def(self, name, val):
    def append_defs(self, new_defs):

    """

    def __init__(self, name, imported_instances, export_ids, renamings, defs, data=w_false):
        self.name = name # W_Symbol (for debugging)
        self.imported_instances = imported_instances # [[...],[..., W_LinkletInstance ,...],...]
        self.export_ids = export_ids # [..., str ,...]
        self.renamings = renamings # {W_Symbol:W_Symbol}
        self.defs = defs # {W_Symbol-W_Object}
        self.data = data
        
    def tostring(self):
        return "W_Linklet Instance : %s - Importing : %s" % (self.name, self.imported_instances)

    def get_val(self, id_sym):
        """ Returns a defined value."""
        if id_sym in self.renamings:
            id_sym = self.renamings[id_sym]

        if id_sym not in self.defs.keys():
            raise SchemeException("%s is not defined/exported in (or through) this instance" % id_sym)

        return self.defs[id_sym]

    def is_defined(self, id_str):
        """ Checks if given id is defined by this instance. """
        return id_str in self.defs.keys()

    def is_exporting(self, id_str):
        """ Checks if given id is exported by this instance. """
        return id_str.variable_name() in self.export_ids

    def provide_all_exports_to_prim_env(self):
        """ Puts all exported values to prim_env. """
        for name, value in self.defs.iteritems():
            prim_env[name] = value

    def lookup_linkl(self, id_str, import_num):
        """ Gets the requested value from the appropriate instance. """
        if import_num < 0:
            rv = self.get_val(id_str)
            return rv
        else:
            if id_str in self.renamings:
                id_str = self.renamings[id_str]
            inst = self.imported_instances[import_num]
            return inst.lookup_linkl(id_str, -1)

    def set_defs(self, defs):
        self.defs = defs

    def set_bang_def(self, name, val):
        self.defs[name] = val
        
    def add_def(self, name, val):
        if name in self.defs:
            raise SchemeException("Duplicate definition : %s" % name)

        self.defs[name] = val
        
    def append_defs(self, new_defs):

        # check if we already have any of the new defs
        for name, val in new_defs.iteritems():
            if name in self.defs.keys():
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
    """
    def instantiate(self, env, imported_instances):

    @staticmethod
    def load_linklet(json_file_name, loader):
    """

    def __init__(self, name, importss, exports, renamings, all_forms):
        self.name = name # W_Symbol -- for debugging
        self.importss = importss # [...,[..., str ,...],...]
        self.exports = exports # [..., str ,...]
        self.renamings = renamings # {str:W_Symbol}
        self.forms = all_forms # [..., AST ,...]

    def instantiate(self, w_imported_instances, config, toplevel_eval=False, prompt=True, target=None):
        """ Instantiates the linklet:
        --- takes the imported linklet instances (list W_LinkletInstances)
        --- extracts the specified set of variables
        --- returns a W_LinkletInstance (or a result if toplevel_eval)
        """

        return_val = None
        l_importss = len(self.importss)

        l_given_instances = len(w_imported_instances)
        
        # FIXME: coming from "instantiate-linklet" when toplevel_eval==True, there's 3 additional instances when self.importss is []
        # (when target_instance is a W_LinkletInstance)
        if not toplevel_eval and l_importss != l_given_instances:
            raise SchemeException("Required %s instances but given %s" % (l_importss, l_given_instances))

        # FIXME:
        # Check if the imports are really exported by the given instances
        #import pdb;pdb.set_trace()
        # for index in range(l_importss):
        #     imported_ids = self.importss[index]
        #     for id_str in imported_ids:
        #         assert index >= 0 and imported_instances is not None and len(imported_instances) != 0
        #         assert isinstance(id_str, str)
        #         assert imported_instances[index].is_exporting(id_str)

        inst = W_LinkletInstance(self.name, w_imported_instances, self.exports, self.renamings, {})

        from pycket.env import ToplevelEnv
        env = ToplevelEnv(config, inst)

        cont = NilCont()
        if prompt:
            cont = Prompt(w_default_continuation_prompt_tag, None, env, cont)

        # FIXME : check for existing exports and imports in the target
        if target is not None:
            target.export_ids = self.exports
            target.imported_instances = w_imported_instances
            target.renamings = self.renamings

        for form in self.forms:
            if isinstance(form, DefineValues):
                expression = form.rhs
                values = interpret_one(expression, env, cont).get_all_values()
                len_values = len(values)
                if len(form.names) == len_values:
                    for index in range(len_values): 
                        name = form.names[index]
                        value = values[index]

                        if target is not None and not target.is_defined(name):
                            target.add_def(name, value)

                        inst.add_def(name, value)
                else:
                    raise SchemeException("wrong number of values for define-values")

            else: # any expression
                values = interpret_one(form, env, cont)
                # FIXME: chech multiple values??
                if toplevel_eval:
                    return_val = values

        if toplevel_eval:
            return return_val
        
        return inst

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

        exports = []
        renamings = {}
        for exp in exports_list:
            if exp.is_array:
                arr = exp.value_array()
                defined_name = arr[0].value_object()['quote'].value_object()['toplevel'].value_string()
                exported_name = arr[1].value_object()['quote'].value_object()['toplevel'].value_string()

                renamings[W_Symbol.make(exported_name)] = W_Symbol.make(defined_name)
                exports.append(exported_name)
            else:
                exports.append(exp.value_object()['quote'].value_object()['toplevel'].value_string())
        
        imports_list = getkey(linklet_dict, "importss", type='a')

        importss = []
        if "importss" in linklet_dict:
            for imports in imports_list:
                arr = imports.value_array()
                importss.append([sym.value_object()['quote'].value_object()['toplevel'].value_string() for sym in arr])
                
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
            
        return W_Linklet(W_Symbol.make(json_file_name), importss, exports, renamings, all_forms), config


"""
 (define-values (1/primitive->compiled-position) (hash-ref linklet-primitive-table 'primitive->compiled-position #f))
 (define-values (1/compiled-position->primitive) (hash-ref linklet-primitive-table 'compiled-position->primitive #f))
 (define-values (1/read-compiled-linklet) (hash-ref linklet-primitive-table 'read-compiled-linklet #f))
 (define-values (1/instance-unset-variable!) (hash-ref linklet-primitive-table 'instance-unset-variable! #f))
 (define-values (1/variable-reference?) (hash-ref linklet-primitive-table 'variable-reference? #f))
 (define-values (1/variable-reference->instance) (hash-ref linklet-primitive-table 'variable-reference->instance #f))
 (define-values (1/variable-reference-constant?) (hash-ref linklet-primitive-table 'variable-reference-constant? #f))

 STUB (define-values (1/compile-linklet) (hash-ref linklet-primitive-table 'compile-linklet #f))
 STUB (define-values (1/instantiate-linklet) (hash-ref linklet-primitive-table 'instantiate-linklet #f))

 OK (define-values (1/recompile-linklet) (hash-ref linklet-primitive-table 'recompile-linklet #f))
 OK (define-values (1/eval-linklet) (hash-ref linklet-primitive-table 'eval-linklet #f))
 OK (define-values (1/instance-data) (hash-ref linklet-primitive-table 'instance-data #f))
 OK (define-values (1/instance-variable-value) (hash-ref linklet-primitive-table 'instance-variable-value #f))
 OK (define-values (1/primitive-table) (hash-ref linklet-primitive-table 'primitive-table #f))
 OK (define-values (1/linklet?) (hash-ref linklet-primitive-table 'linklet? #f))
 OK (define-values (1/linklet-import-variables) (hash-ref linklet-primitive-table 'linklet-import-variables #f))
 OK (define-values (1/linklet-export-variables) (hash-ref linklet-primitive-table 'linklet-export-variables #f))
 OK (define-values (1/instance?) (hash-ref linklet-primitive-table 'instance? #f))
 OK (define-values (1/make-instance) (hash-ref linklet-primitive-table 'make-instance #f))
 OK (define-values (1/instance-name) (hash-ref linklet-primitive-table 'instance-name #f))
 OK (define-values (1/instance-variable-names) (hash-ref linklet-primitive-table 'instance-variable-names #f))
 OK (define-values (1/instance-set-variable-value!) (hash-ref linklet-primitive-table 'instance-set-variable-value! #f))
 OK (define-values (1/linklet-directory?) (hash-ref linklet-primitive-table 'linklet-directory? #f))
 OK (define-values (1/hash->linklet-directory) (hash-ref linklet-primitive-table 'hash->linklet-directory #f))
 OK (define-values (1/linklet-directory->hash) (hash-ref linklet-primitive-table 'linklet-directory->hash #f))
 OK (define-values (1/linklet-bundle?) (hash-ref linklet-primitive-table 'linklet-bundle? #f))
 OK (define-values (1/hash->linklet-bundle) (hash-ref linklet-primitive-table 'hash->linklet-bundle #f))
 OK (define-values (1/linklet-bundle->hash) (hash-ref linklet-primitive-table 'linklet-bundle->hash #f))
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

def def_vals_to_ast(def_vals_sexp, linkl_toplevels, linkl_imports):
    if not len(to_rpython_list(def_vals_sexp)) == 3:
        raise Exception("defs_vals_to_ast : unhandled define-values form : %s" % def_vals_sexp.tostring())

    names = def_vals_sexp.cdr().car() # renames?
    names_ls = to_rpython_list(names)

    body = sexp_to_ast(def_vals_sexp.cdr().cdr().car(), [], linkl_toplevels, linkl_imports, disable_conversions=False, cell_ref=False, name=names_ls[0].variable_name())

    return DefineValues(names_ls, body, names_ls)

def lam_to_ast(lam_sexp, lex_env, linkl_toplevels, linkl_imports, disable_conversions, name=""):
    from pycket.expand import SourceInfo

    if not len(to_rpython_list(lam_sexp)) == 3:
        raise Exception("lam_to_ast : unhandled lambda form : %s" % lam_sexp.tostring())

    formals = lam_sexp.cdr().car() # rest?
    formals_ls = to_rpython_list(formals)

    body = sexp_to_ast(lam_sexp.cdr().cdr().car(), formals_ls + lex_env, linkl_toplevels, linkl_imports, disable_conversions, cell_ref=False, name=name)
    dummy = 1
    return make_lambda(formals_ls, None, [body], SourceInfo(dummy, dummy, dummy, dummy, name))

def is_imported(id_str, linkl_importss):
    # linkl_importss : [...[..str..]..]
    for imports in linkl_importss:
        for id in imports:
            if id_str == id:
                return True
    return False

def import_instance_num(id_str, linkl_importss):
    # assumes id_str is in the linkl_importss
    inst_num = -1
    for imports in linkl_importss:
        inst_num += 1
        for id in imports:
            if id_str == id:
                return inst_num
    return inst_num

def let_like_to_ast(let_sexp, lex_env, linkl_toplevels, linkl_imports, disable_conversions, is_letrec):
    if not len(to_rpython_list(let_sexp)) == 3:
        raise Exception("let_to_ast : unhandled let form : %s" % let_sexp.tostring())

    varss_rhss = to_rpython_list(let_sexp.cdr().car()) # a little inefficient but still..
    varss_list = [None] * len(varss_rhss)
    rhss_list = [None] * len(varss_rhss)
    num_ids = 0
    i = 0
    for w_vars_rhss in varss_rhss:
        varr = [v.get_obj() if isinstance(v, W_Correlated) else v for v in to_rpython_list(w_vars_rhss.car())]
        rhs_env = varr + lex_env if is_letrec else lex_env
        rhsr = sexp_to_ast(w_vars_rhss.cdr().car(), rhs_env, linkl_toplevels, linkl_imports, disable_conversions)
        varss_list[i] = varr
        rhss_list[i] = rhsr
        i += 1
        num_ids += len(varr)

    ids = [None] * num_ids
    index = 0
    for vars_ in varss_list:
        for var_ in vars_:
            ids[index] = var_ # W_Symbol
            index += 1

    body = sexp_to_ast(let_sexp.cdr().cdr().car(), ids + lex_env, linkl_toplevels, linkl_imports, disable_conversions)

    if len(ids) == 0:
        return Begin.make([body])

    if is_letrec:
        return make_letrec(list(varss_list), list(rhss_list), [body])
    else:
        return make_let(varss_list, rhss_list, [body])

def is_val_type(form):
    val_types = [W_Number, W_Bool, W_String]
    for t in val_types:
        if isinstance(form, t):
            return True
    return False

def sexp_to_ast(form, lex_env, linkl_toplevels, linkl_importss, disable_conversions=False, cell_ref=False, name=""):

    if isinstance(form, W_Correlated):
        return sexp_to_ast(form.get_obj(), lex_env, linkl_toplevels, linkl_importss, disable_conversions)
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
        elif form in linkl_toplevels:
            form = LinkletVar(form, -1)
        elif is_imported(form.variable_name(), linkl_importss):
            # imported linklet var
            inst_num = import_instance_num(form.variable_name(), linkl_importss)
            form = LinkletVar(form, inst_num)
        else:
            # kernel primitive ModuleVar
            form = ModuleVar(form, "#%kernel", form, None)
    elif isinstance(form, W_List):
        if form.car() is W_Symbol.make("begin"):
            form = Begin.make([sexp_to_ast(f, lex_env, linkl_toplevels, linkl_importss, disable_conversions) for f in to_rpython_list(form.cdr())])
        elif form.car() is W_Symbol.make("begin0"):
            fst = sexp_to_ast(form.cdr().car(), lex_env, linkl_toplevels, linkl_importss, disable_conversions)
            rst = [sexp_to_ast(f, lex_env, linkl_toplevels, linkl_importss, disable_conversions) for f in to_rpython_list(form.cdr().cdr())]
            if len(rst) == 0:
                form = fst
            else:
                form = Begin0.make(fst, rst)
        elif form.car() is W_Symbol.make("define-values"):
            form = def_vals_to_ast(form, linkl_toplevels, linkl_importss)
        elif form.car() is W_Symbol.make("with-continuation-mark"):
            if len(to_rpython_list(form)) != 4:
                raise Exception("Unrecognized with-continuation-mark form : %s" % form.tostring())
            key = sexp_to_ast(form.cdr().car(), lex_env, linkl_toplevels, linkl_importss, disable_conversions)
            val = sexp_to_ast(form.cdr().cdr().car(), lex_env, linkl_toplevels, linkl_importss, disable_conversions)
            body = sexp_to_ast(form.cdr().cdr().cdr().car(), lex_env, linkl_toplevels, linkl_importss, disable_conversions)
            form = WithContinuationMark(key, val, body)
        elif form.car() is W_Symbol.make("#%variable-reference"):
            raise Exception("variable-reference not yet implemented")
        elif form.car() is W_Symbol.make("case-lambda"):
            lams = [lam_to_ast(f, lex_env, linkl_toplevels, linkl_importss, True, name) for f in to_rpython_list(form.cdr())]
            form = CaseLambda(lams)
        elif form.car() is W_Symbol.make("lambda"):
            form = CaseLambda([lam_to_ast(form, lex_env, linkl_toplevels, linkl_importss, True, name)])
        elif form.car() is W_Symbol.make("let-values"):
            form = let_like_to_ast(form, lex_env, linkl_toplevels, linkl_importss, True, False)
        elif form.car() is W_Symbol.make("letrec-values"):
            form = let_like_to_ast(form, lex_env, linkl_toplevels, linkl_importss, True, True)
        elif form.car() is W_Symbol.make("set!"):
            var = sexp_to_ast(form.cdr().car(), lex_env, linkl_toplevels, linkl_importss, disable_conversions, cell_ref=True)
            rhs = sexp_to_ast(form.cdr().cdr().car(), lex_env, linkl_toplevels, linkl_importss, disable_conversions)
            assert isinstance(var, Var)
            form = SetBang(var, rhs)
        elif form.car() is W_Symbol.make("quote"):
            if form.cdr().cdr() is not w_null:
                raise Exception("malformed quote form : %s" % form.tostring())
            form = Quote(form.cdr().car())
        elif form.car() is W_Symbol.make("if"):
            tst_w = form.cdr().car()
            thn_w = form.cdr().cdr().car()
            els_w = form.cdr().cdr().cdr().car()
            tst = sexp_to_ast(tst_w, lex_env, linkl_toplevels, linkl_importss, disable_conversions)
            thn = sexp_to_ast(thn_w, lex_env, linkl_toplevels, linkl_importss, disable_conversions)
            els = sexp_to_ast(els_w, lex_env, linkl_toplevels, linkl_importss, disable_conversions)
            form = If.make(tst, thn, els)
        else:
            form_inner = sexp_to_ast(form.car(), lex_env, linkl_toplevels, linkl_importss, disable_conversions)

            rands_ls = to_rpython_list(form.cdr())
            rands = [sexp_to_ast(r, lex_env, linkl_toplevels, linkl_importss, disable_conversions) for r in rands_ls]
                    
            form = App.make(form_inner, rands)
    else:
        raise Exception("Don't know what to do with this form yet : %s", form)

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

#[`(define-values (,id) ,rhs)
#[`(define-values c(,struct:s ,make-s ,s? ,acc/muts ...) ; pattern from `struct` or `define-struct`
#[`(define-values (,struct:s ,make-s ,s? ,s-ref ,s-set!) ,rhs) ; direct use of `make-struct-type`
#[`(define-values (,prop:s ,s? ,s-ref)
#       (make-struct-type-property ,_ . ,rest))
#[`,_ (values knowns #f)]


@expose("compile-linklet", [W_Object, default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false)], simple=False)
def compile_linklet(form, name, import_keys, get_import, serializable_huh, env, cont):

    if isinstance(form, W_WrappedConsProper): # s-expr
        # read it and create an AST, put it in a W_Linklet and return
        if not isinstance(form.car(), W_Symbol) or "linklet" != form.car().tostring():
            raise SchemeException("Malformed s-expr. Expected a linklet, got %s" % form.tostring())
        else:
            # Process the imports
            w_importss = form.cdr().car()
            renamings = {}
            importss_list = []
            importss_count = 0
            importss_acc = w_importss
            while (importss_acc is not w_null):
                importss_current = importss_acc.car()
                inner_acc = []
                while (importss_current is not w_null):
                    c = importss_current.car()
                    if isinstance(c, W_Symbol):
                        inner_acc.append(c.variable_name())
                    elif isinstance(c, W_List):
                        if c.cdr().cdr() is not w_null:
                            Exception("Unhandled renamed import form : %s" % c.tostring())
                        external_id = c.car()
                        internal_id = c.cdr().car()

                        assert isinstance(external_id, W_Symbol) and isinstance(internal_id, W_Symbol)
                        inner_acc.append(internal_id.variable_name())
                        renamings[internal_id] = external_id

                    importss_current = importss_current.cdr()

                importss_list.append(inner_acc)
                importss_count += 1
                importss_acc = importss_acc.cdr()

            # Process the exports
            w_exports = form.cdr().cdr().car()

            exports = []
            r_exports = to_rpython_list(w_exports)
            for exp in r_exports:
                if isinstance(exp, W_WrappedConsProper):
                    defined_name = exp.car() # W_Symbol
                    exported_name = exp.cdr().car() # W_Symbol
                    renamings[exported_name] = defined_name

                    exports.append(exported_name.variable_name())
                else:
                    exports.append(exp.variable_name())

            # Process the body
            w_body = form.cdr().cdr().cdr()
            body_forms_ls = to_rpython_list(w_body)

            linkl_toplevel_defined_ids = extract_ids(body_forms_ls)

            body_forms = [sexp_to_ast(b, [], linkl_toplevel_defined_ids, importss_list) for b in body_forms_ls]

            if name is w_false:
                w_name = W_Symbol.make("ad-hoc")
            else:
                w_name = name

            linkl = W_Linklet(w_name, importss_list, exports, renamings, body_forms)
            if import_keys is w_false:
                return return_value(linkl, env, cont)
            else:
                return return_multi_vals(Values.make([linkl, import_keys]), env, cont)
            
    else: # correlated
        # take the AST from the correlated and put it in a W_Linklet and return
        #import pdb;pdb.set_trace()
        raise Exception("NYI")
    # (Pdb) name
    # module
    # (Pdb) type(name)
    # <class 'pycket.values.W_Symbol'>

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
    return l_inst.name
    
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

    if target_instance is None or target_instance is w_false:

        return return_value(linkl.instantiate(im_list, env.toplevel_env()._pycketconfig, prompt=prompt), env, cont)
    
    elif isinstance(target_instance, W_LinkletInstance):
        #Providing a target instance to `instantiate-linklet` means that we get the body's results instead of the instance as a result
        # use and modify target_instance for the linklet definitions and expressions"
        # When a target instance is provided to instantiate-linklet, any existing variable with the same name will be left as-is, instead of set to undefined. This treatment of uninitialized variables provides core support for top-level evaluation where variables may be referenced and then defined in a separate element of compilation.

        # The linklet’s exported variables are accessible in the result instance or in target-instance using the linklet’s external name for each export. If target-instance is provided as non-#f, its existing variables remain intact if they are not modified by a linklet definition.l

        return return_value(linkl.instantiate(im_list, env.toplevel_env()._pycketconfig, toplevel_eval=True, prompt=prompt, target=target_instance), env, cont)

    else:
        raise SchemeException("Expected #f or instance? as target-instance, got : %s" % target_instance)

@expose("linklet-import-variables", [W_Linklet])
def linklet_import_variables(linkl):
    importss_py_lst = linkl.importss
    importss = w_null
    for imp in importss_py_lst:
        imp_inner = w_null
        for id_str in imp:
            imp_inner = W_Cons.make(W_Symbol.make(id_str), imp_inner)
        importss = W_Cons.make(imp_inner, importss)
    return importss

@expose("linklet-export-variables", [W_Linklet])
def linklet_export_variables(linkl):
    exports = w_null
    for e in linkl.exports:
        exports = W_Cons.make(W_Symbol.make(e), exports)
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
    data = args[1] # W_Object, w_false
    vars_vals = args[2:]
    # check if the vars and vals match
    if ((len(vars_vals) % 2) != 0):
        raise SchemeException("Variable names and values do not match : %s" % vars_vals)

    vars_vals_dict = {}
    for i in range(0, len(vars_vals), 2):
        n = vars_vals[i]
        v = vars_vals[i+1]
        vars_vals_dict[n] = v

    return W_LinkletInstance(name, [], [], {}, vars_vals_dict, data)

@expose("recompile-linklet", [W_Linklet, default(W_Object, None), default(W_Object, w_false), default(W_Object, None)], simple=False)
def recompile_linklet(linkl, name, import_keys, get_import, env, cont):
    if import_keys is not None:
        return return_multi_vals(Values.make([linkl, import_keys]), env, cont)
    else:
        return return_value(linkl, env, cont)

@expose("instance-variable-value", [W_LinkletInstance, W_Symbol, default(W_Object, None)], simple=False)
def instance_variable_value(instance, name, fail_k, env, cont):
    from pycket.interpreter import return_value
    if not instance.is_defined(name): #instance.is_exporting(name):
        if fail_k is not None and fail_k.iscallable():
            return fail_k.call([], env, cont)
        else:
            raise SchemeException("key %s not found in exports of the instance %s" % (name.tostring(), instance.name.tostring()))

    return return_value(instance.get_val(name), env, cont)

@expose("instance-data", [W_LinkletInstance])
def instance_data(inst):
    return inst.data

@expose("eval-linklet", [W_Linklet])
def eval_linklet(l):
    return l

@expose("instance-set-variable-value!", [W_LinkletInstance, W_Symbol, W_Object, default(W_Object, w_false)])
def instance_set_variable_value(instance, name, val, mode):

    instance.set_bang_def(name, val)
    return w_void
