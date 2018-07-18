""" Implementation of linklets

class W_LinkletInstance(W_Object)
class W_Linklet(object)
class W_LinkletBundle(W_Object)
class W_LinkletDirectory(W_Object)
 """

#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from pycket.interpreter import *
from pycket.assign_convert import assign_convert
from pycket.values import *
from pycket.values_string import W_String
from pycket.values_parameter import top_level_config
from pycket.error import SchemeException
from pycket import pycket_json
from pycket.prims.expose import prim_env, expose, default
from pycket.prims.general import make_pred
from pycket.prims.correlated import W_Correlated
from pycket.prims.vector import vector
from pycket.AST import AST
from pycket.cont import Prompt, NilCont, continuation
from pycket.prims.control import default_error_escape_handler, default_uncaught_exception_handler
from pycket.hash.simple import W_SimpleMutableHashTable, W_EqvImmutableHashTable, W_EqImmutableHashTable, make_simple_immutable_table
from pycket.hash.equal import W_EqualHashTable

class W_Uninitialized(W_Object):
    errorname = "uninitialized"
    _attrs_ = []
    def __init__(self):
        pass
    def tostring(self):
        return "#<uninitialized>"

w_uninitialized = W_Uninitialized()

class W_LinkletInstance(W_Object):

    _attrs_ = ["name", "vars", "exports", "data"]
    _immutable_fields_ = ["name", "data"]

    def __init__(self, name, vars, exports, data=w_false):
        self.name = name # W_Symbol (for debugging)
        self.vars = vars # {W_Symbol:LinkletVar}
        self.exports = exports
        self.data = data #

    def tostring(self):

        vars_str = " ".join(["(%s : %s)" % (name.tostring(), var.tostring()) for name, var in self.vars.iteritems()])
        exports_str = "".join(["(%s . %s)" % (int_name.tostring(), ext_name.tostring()) for int_name, ext_name in self.exports.iteritems()])
        data_str = self.data.tostring()
        return "(linklet-instance %s (%s) (%s) %s)" % (self.name, data_str, exports_str, vars_str)

    def get_name(self):
        return self.name

    def get_data(self):
        return self.data

    def get_vars(self):
        return self.vars

    def get_var_names(self):
        return self.vars.keys()

    def set_exports(self, exports):
        self.exports = exports

    def has_var(self, var_name):
        return var_name in self.vars

    # for tests
    def is_var_uninitialized(self, name):
        var = self.get_var(name)
        return var.is_uninitialized()

    def check_var_exists(self, name):
        if not self.has_var(name):
            name_rep = name
            if name in self.exports:
                name_rep = self.exports[name]
            raise SchemeException("Reference to an undefined variable : %s" % name_rep.tostring())

    def get_var(self, name):
        self.check_var_exists(name)
        return self.vars[name]

    def add_var(self, name, var):
        if name in self.vars and not self.is_var_uninitialized(name):
            raise SchemeException("Already : %s" % name.tostring())
        self.vars[name] = var

    def overwrite_var(self, name, value):
        self.check_var_exists(name)
        self.vars[name].set_bang(value)

    def set_var(self, name, val, mode):
        self.check_var_exists(name)
        self.vars[name].set(val, None, mode)

    def provide_all_exports_to_prim_env(self, excludes=[]):
        for name, var in self.vars.iteritems():
            if name not in excludes:
                prim_env[name] = var.get_value_direct()

    def lookup_var_value(self, name):
        var = self.get_var(name)
        return var.get_value_unstripped()

class W_LinkletBundle(W_Object):
    # Information in a linklet bundle is keyed by either a symbol or a fixnum

    _attrs_ = _immutable_fields_ = ["bundle_mapping"]

    def __init__(self,bundle_mapping):
        self.bundle_mapping = bundle_mapping

    def get_mapping(self):
        return self.bundle_mapping

@expose("hash->linklet-bundle", [W_Object])
def hash_to_linklet_bundle(content):
    return W_LinkletBundle(content)

@expose("linklet-bundle->hash", [W_LinkletBundle])
def linklet_bundle_to_hash(linkl_bundle):
    return linkl_bundle.bundle_mapping

class W_LinkletDirectory(W_Object):

    # When a Racket module has submodules, the linklet bundles for the
    # module and the submodules are grouped together in a linklet
    # directory. A linklet directory can have nested linklet
    # directories. A linklet directory can be equivalently viewed as a
    # mapping from a lists of symbols to a linklet bundle.

    _attrs_ = _immutable_fields_ = ["dir_mapping"]

    def __init__(self,dir_mapping):
        self.dir_mapping = dir_mapping

    def get_mapping(self):
        return self.dir_mapping

@expose("hash->linklet-directory", [W_Object])
def hash_to_linklet_directory(content):
    return W_LinkletDirectory(content)

@expose("linklet-directory->hash", [W_LinkletDirectory])
def linklet_directory_to_hash(linkl_directory):
    return linkl_directory.dir_mapping

@continuation
def instantiate_val_cont(forms, index, gensym_count, return_val, target, exports, env, cont, _vals):
    if index >= len(forms):
        if return_val:
            return return_value(_vals, env, cont)
        else:
            return return_value(target, env, cont)

    # there's more
    return instantiate_loop(forms, index, gensym_count, return_val, target, exports, env, cont)

@continuation
def instantiate_def_cont(form, forms, index, gensym_count, return_val, target, exports, env, cont, _vals):

    values = _vals.get_all_values()
    len_values = len(values)
    if len(form.names) != len_values:
        raise SchemeException("%s -- expected %s values but got %s" % (form.tostring(), str(len(form.names)), str(len_values)))

    for i in range(len_values):
        name = form.names[i]
        value = values[i]

        # modify target
        ext_name = exports[name] if name in exports else name

        c = W_Cell(value)
        env.toplevel_env().toplevel_set(name, c, already_celled=True)

        var = LinkletVar(ext_name, c)
        if name in exports or not target.has_var(ext_name): # variable is definitely going into target
            if target.has_var(ext_name) and not target.is_var_uninitialized(ext_name):
                target.overwrite_var(ext_name, value)
            else:
                target.add_var(ext_name, var)
        elif external_of_an_export(name, exports):
            gensym_count += 1
            ex_name = W_Symbol(name.tostring() + "." + str(gensym_count))
            target.add_var(ex_name, var)

    return return_value(w_void, env, instantiate_val_cont(forms, index + 1, gensym_count, return_val, target, exports, env, cont))

def instantiate_loop(forms, index, gensym_count, return_val, target, exports, env, cont):
    form = forms[index]
    if isinstance(form, DefineValues):
        return form.rhs, env, instantiate_def_cont(form, forms, index, gensym_count, return_val, target, exports, env, cont)
    else:
        return form, env, instantiate_val_cont(forms, index + 1, gensym_count, return_val, target, exports, env, cont)

class W_Linklet(W_Object):

    _immutable_fields_ = ["name", "importss[*]", "exports", "all_forms"]

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

    def get_name(self):
        return self.name

    def get_importss(self):
        return self.importss

    def get_exports(self):
        return self.exports

    def get_forms(self):
        return self.forms

    def tostring(self):
        forms_str = " ".join([f.tostring() for f in self.forms])
        importss_ls = [None]*len(self.importss)

        for index, imp_dict in enumerate(self.importss):
            importss_ls[index] = "".join(["(%s %s)" % (ext_name.tostring(), int_name.tostring()) for ext_name, int_name in imp_dict.iteritems()])

        importss_str = "".join(importss_ls)

        exports_str = "".join(["(%s %s)" % (int_name.tostring(), ext_name.tostring()) for int_name, ext_name in self.exports.iteritems()])

        return "(linklet %s (%s) (%s) %s)" % (self.name, importss_str, exports_str, forms_str)

    def instantiate(self, w_imported_instances, config, prompt=False, target=None, env=None, cont=None):

        l_importss = len(self.importss)
        l_given_instances = len(w_imported_instances)

        if l_importss != l_given_instances:
            raise SchemeException("Required %s instances but given %s" % (str(l_importss), str(l_given_instances)))

        return_val = True
        if not target:
            target = W_LinkletInstance(self.name, {}, self.exports)
            return_val = False
        else:
            target.set_exports(self.exports)

        """Instantiates the linklet:

        --- Prep the environment and the continuation for the
            evaluation of linklet forms
        --- Process the imports, get them into the toplevel
            environment
        --- Collect the ids defined in the self linklet's forms
        --- Uninitialize the undefined exports in the linklet into the
            target (if it doesn't already have it)
        --- Evaluate linklet forms
        --- Return target instance and return value (None if a target
            is given to instantiate)

        """

        """
        Prep the environment and the continuation
        Put the target into the environment
        """
        # Discard the current environment and create a new one at
        # every instantiation This is crucial because linklet
        # variables are put into the toplevel environment, and
        # replacing the W_Cell (not overwriting its value) with
        # another one makes the corresponding target to lose the link
        # to that variable (whenever the target's variable is
        # modified, the one in the environment has to be updated too)
        from pycket.env import ToplevelEnv
        env = ToplevelEnv(config)

        env.set_current_linklet_instance(target)

        if not cont:
            cont = Prompt(w_default_continuation_prompt_tag, None, env, NilCont())

        if prompt:
            Prompt(w_default_continuation_prompt_tag, None, env, cont)

        cont.update_cm(parameterization_key, top_level_config)
        cont.update_cm(exn_handler_key, default_uncaught_exception_handler)


        """
        Process the imports, get them into the toplevel environment
        """
        for instance_index, imports_dict in enumerate(self.importss):
            for ext_name, int_name in imports_dict.iteritems():
                # (linklet (((x x1))) () (define-values (x) 14) (+ x1 x))
                if int_name in self.exports:
                    raise SchemeException("export duplicates import : %s" % int_name.tostring())

                imported_var = w_imported_instances[instance_index].get_var(ext_name)
                if imported_var.is_uninitialized():
                    raise SchemeException("Trying to import a variable that is uninitialized : %s" % ext_name.tostring())

                # imports never get into the target
                # put the into the toplevel env
                env.toplevel_env().toplevel_set(int_name, imported_var.get_value_direct())

        """
        Collect the ids defined in the given linklet's forms
        """
        linklet_defined_names = []
        for b in self.forms:
            if isinstance(b, DefineValues):
                linklet_defined_names += b.names

        """
        Uninitialize the undefined exports -- name, undef
        """
        for internal_name, external_name in self.exports.iteritems():
            # Defined name ids are changed in compilation based on renames
            if external_name not in linklet_defined_names:
                if not target.has_var(external_name):
                    target.add_var(external_name, LinkletVar(external_name))

        if len(self.forms) == 0:
            # no need for any evaluation, just return the instance or the value
            if return_val:
                return return_value(w_void, env, cont)
            else:
                return return_value(target, env, cont)

        return instantiate_loop(self.forms, 0, 0, return_val, target, self.exports, env, cont)

    @staticmethod # json_file_name -> W_Linklet
    def load_linklet(json_file_name, loader):
        from pycket.expand import readfile_rpython, getkey
        from pycket.util import console_log
        """ Expands and loads a linklet from a JSON file"""
        data = readfile_rpython(json_file_name)
        json = pycket_json.loads(data)
        console_log("Finished reading JSON from %s" % json_file_name, 2)
        assert json.is_object
        json_python_dict = json.value_object()
        assert "linklet" in json_python_dict
        linklet_dict = getkey(json_python_dict, "linklet", type='o')
        assert "exports" in linklet_dict and "body" in linklet_dict

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

        imports_list = getkey(linklet_dict, "importss", type='a', default=[])

        importss = [None]*len(imports_list) # list of dict

        if "importss" in linklet_dict:
            for index, imports in enumerate(imports_list):
                arr = imports.value_array()
                # bootstrap linklets have no imports at all
                # this is only for debugging purposes
                instance_imports = {}
                for id_str in arr:
                    sym = W_Symbol.make(id_str.value_object()['quote'].value_object()['toplevel'].value_string())
                    instance_imports[sym] = sym
                importss[index] = instance_imports

        console_log("Converting linklet forms to AST ...", 2)
                
        all_forms = []
        for body_form in getkey(linklet_dict, "body", type='a'):
            form = loader.to_ast(body_form)
            form = Context.normalize_term(form)
            form = assign_convert(form)
            form.clean_caches()
            all_forms.append(form)

        console_log("Finished converting linklet forms to AST ...", 2)

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

    names = def_vals_sexp.cdr().car()
    names_ls = to_rpython_list(names)

    body = sexp_to_ast(def_vals_sexp.cdr().cdr().car(), [], exports, linkl_toplevels, linkl_imports, disable_conversions=False, cell_ref=[], name=names_ls[0].variable_name())

    return DefineValues(names_ls, body, names_ls)

def lam_to_ast(lam_sexp, lex_env, exports, linkl_toplevels, linkl_imports, disable_conversions, cell_ref, name=""):
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

    for f in formals_ls:
        if f in cell_ref:
            cell_ref.remove(f)

    body = sexp_to_ast(lam_sexp.cdr().car(), formals_ls + lex_env, exports, linkl_toplevels, linkl_imports, disable_conversions, cell_ref=[], name=name)
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
    cells_for_the_body = list(cell_ref) if is_letrec else cell_ref
    cells_for_the_rhss = list(cell_ref) if is_letrec else cell_ref

    if is_letrec:
        # populate lex_env // cell_refs for rhss ahead of time
        for rhs in varss_rhss: # rhs : ((id ...) rhs-expr)
            ids = to_rpython_list(rhs.car()) # (id ...)
            cells_for_the_rhss += ids
            lex_env += [i.get_obj() if isinstance(i, W_Correlated) else i for i in ids]

    num_ids = 0
    i = 0
    for w_vars_rhss in varss_rhss:
        varr = [v.get_obj() if isinstance(v, W_Correlated) else v for v in to_rpython_list(w_vars_rhss.car())]
        varss_list[i] = varr

        rhsr = sexp_to_ast(w_vars_rhss.cdr().car(), lex_env, exports, linkl_toplevels, linkl_imports, disable_conversions, cell_ref=[])
        rhss_list[i] = rhsr
        i += 1
        num_ids += len(varr)

    ids = [None] * num_ids
    index = 0
    for vars_ in varss_list:
        for var_ in vars_:
            ids[index] = var_ # W_Symbol
            index += 1

    body = sexp_to_ast(let_sexp.cdr().cdr().car(), ids + lex_env, exports, linkl_toplevels, linkl_imports, disable_conversions, cell_ref=[])

    if len(varss_rhss) == 0:
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

def sexp_to_ast(form, lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions=False, cell_ref=[], name=""):
    if isinstance(form, W_Correlated):
        return sexp_to_ast(form.get_obj(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name)
    elif is_val_type(form):
        form = Quote(form)
    elif isinstance(form, W_Symbol):
        if form in cell_ref:
            form = CellRef(form)
        elif form in lex_env:
            form = LexicalVar(form)
        # toplevel linklet var
        elif is_imported(form, linkl_importss):
            form = LinkletVar(form, None, W_Symbol.make("constant"), is_transparent=True)
        elif (form in linkl_toplevels) or (form in exports):
            if form in linkl_toplevels:
                form = linkl_toplevels[form]
            else:
                form = LinkletVar(form, is_transparent=True)
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
            lams = [lam_to_ast(f, lex_env, exports, linkl_toplevels, linkl_importss, True, cell_ref, name) for f in to_rpython_list(form.cdr())]
            form = CaseLambda(lams)
        elif form.car() is W_Symbol.make("lambda"):
            form = CaseLambda([lam_to_ast(form, lex_env, exports, linkl_toplevels, linkl_importss, True, cell_ref, name)])
        elif form.car() is W_Symbol.make("let-values"):
            form = let_like_to_ast(form, lex_env, exports, linkl_toplevels, linkl_importss, True, False, cell_ref)
        elif form.car() is W_Symbol.make("letrec-values"):
            form = let_like_to_ast(form, lex_env, exports, linkl_toplevels, linkl_importss, True, True, cell_ref)
        elif form.car() is W_Symbol.make("set!"):
            if is_imported(form.cdr().car(), linkl_importss):
                raise SchemeException("cannot mutate imported variable : %s" % form.tostring())
            cr = cell_ref
            target = form.cdr().car()
            if target in lex_env:
                cr = [target] if not cr else [target] + cr
            var = sexp_to_ast(form.cdr().car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref=cr, name=name)
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
            form_rator = sexp_to_ast(form.car(), lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref)

            rands_ls = to_rpython_list(form.cdr())
            rands = [sexp_to_ast(r, lex_env, exports, linkl_toplevels, linkl_importss, disable_conversions, cell_ref, name) for r in rands_ls]

            form = App.make(form_rator, rands)
    else:
        raise SchemeException("Don't know what to do with this form yet : %s" % form.tostring())

    return form

# collect the ids in define-values forms
def create_toplevel_linklet_vars(forms_ls):
    linkl_toplevels = {} # {W_Symbol:LinkletVar}
    for form in forms_ls:
        if isinstance(form, W_Correlated):
            form = form.get_obj()
        if isinstance(form, W_List) and form.car() is W_Symbol.make("define-values"):
            ids = form.cdr().car()
            ids_ls = to_rpython_list(ids)
            # create LinkletVar for each id
            for id in ids_ls:
                if id in linkl_toplevels:
                    raise SchemeException("duplicate binding name : %s" % id.tostring())
                linkl_toplevels[id] = LinkletVar(id, is_transparent=True)

    return linkl_toplevels

def external_of_an_export(sym, exports):
    # checks if the given sym is used as an external name
    # for an internally defined variable
    for int_name, ext_name in exports.iteritems():
        if sym is ext_name and int_name is not ext_name:
            return True
    return False

@expose("compile-linklet", [W_Object, default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false)], simple=False)
def compile_linklet(form, name, import_keys, get_import, options, env, cont):
    from pycket.util import console_log
    console_log("compiling linklet : %s" % form.tostring(), 3)
    return do_compile_linklet(form, name, import_keys, get_import, options, env, cont)

def do_compile_linklet(form, name, import_keys, get_import, options, env, cont):

    if isinstance(form, W_WrappedConsProper): # s-expr
        # read it and create an AST, put it in a W_Linklet and return
        if not isinstance(form.car(), W_Symbol) or "linklet" != form.car().tostring():
            raise SchemeException("Malformed s-expr. Expected a linklet, got %s" % form.tostring())
        else:
            # Process the imports
            w_importss = form.cdr().car()

            importss_acc = to_rpython_list(w_importss)
            importss_list = [None]*len(importss_acc)
            for index, importss_current in enumerate(importss_acc):
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

                importss_list[index] = inner_acc

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

            toplevel_defined_linklet_vars = create_toplevel_linklet_vars(body_forms_ls)

            _body_forms = [sexp_to_ast(b, [], exports, toplevel_defined_linklet_vars, importss_list) for b in body_forms_ls]
            # FIXME : remove "disable_conversions" argument entirely
            body_forms = [None]*len(_body_forms)
            for i, bf in enumerate(_body_forms):
                b_form = Context.normalize_term(bf)
                b_form = assign_convert(b_form)
                b_form.clean_caches()
                body_forms[i] = b_form

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

    if target_instance is None or target_instance is w_false:
        target = None
    elif isinstance(target_instance, W_LinkletInstance):
        target = target_instance
    else:
        raise SchemeException("Expected #f or instance? as target-instance, got : %s" % target_instance)

    return linkl.instantiate(im_list, env.toplevel_env()._pycketconfig, prompt, target, env, cont)

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
    return get_instance_variable_names(inst)
# to be able to call it without prim_env indirection
def get_instance_variable_names(inst):
    names = w_null
    for name in inst.get_var_names():
        names = W_Cons.make(name, names)

    return names

make_pred("linklet-directory?", W_LinkletDirectory)

make_pred("linklet-bundle?", W_LinkletBundle)

# for internal use
def is_directory(v):
    return isinstance(v, W_LinkletDirectory)
def is_bundle(v):
    return isinstance(v, W_LinkletBundle)

@expose("make-instance") #FIXME: [W_Object, W_Object, [W_Symbol, W_Object] ....]
def make_instance(args): # name, data, *vars_vals

    name = args[0] # W_Symbol
    data = w_false
    mode = w_false

    from pycket.util import console_log
    if "'" in name.tostring():
        console_log("making instance : %s" % name.tostring(), 1)
    else:
        console_log("making instance : %s" % name.tostring(), 2)

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
    for i in range(0, len(vars_vals), 2):
        n = vars_vals[i]
        v = vars_vals[i+1]
        vars_vals_dict[n] = LinkletVar(n, v, mode)

    return W_LinkletInstance(name, vars_vals_dict, {}, data)

@expose("recompile-linklet", [W_Linklet, default(W_Object, None), default(W_Object, w_false), default(W_Object, None)], simple=False)
def recompile_linklet(linkl, name, import_keys, get_import, env, cont):
    if import_keys is not None:
        return return_multi_vals(Values.make([linkl, import_keys]), env, cont)
    else:
        return return_value(linkl, env, cont)

@expose("instance-variable-value", [W_LinkletInstance, W_Symbol, default(W_Object, None)], simple=False)
def instance_variable_value(instance, name, fail_k, env, cont):
    if not instance.has_var(name) or instance.get_var(name).is_uninitialized():
        if fail_k is not None and fail_k.iscallable():
            return fail_k.call([], env, cont)
        else:
            raise SchemeException("key %s not found in the instance %s" % (name.tostring(), instance.name.tostring()))

    var = instance.get_var(name)
    return return_value(var.get_value_direct(), env, cont)

@expose("instance-data", [W_LinkletInstance])
def instance_data(inst):
    return inst.get_data()

@expose("eval-linklet", [W_Linklet])
def eval_linklet(l):
    return l

@expose("instance-set-variable-value!", [W_LinkletInstance, W_Symbol, W_Object, default(W_Object, w_false)])
def instance_set_variable_value(instance, name, val, mode):
    if instance.has_var(name) and not instance.is_var_uninitialized(name):
        v = instance.get_var(name)
        if v.is_constant():
            raise SchemeException("Cannot mutate a constant : %s" % name.tostring())
        # FIXME : change to be constant?
        instance.overwrite_var(name, val)
    else:
        instance.add_var(name, LinkletVar(name, val, mode))

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
    """Returns #t if the module of the variable reference itself (not
    necessarily a referenced variable) is compiled in unsafe mode, #f
    otherwise.
    """
    return w_false

dir_sym = values.W_Symbol.make(":D:")
bundle_sym = values.W_Symbol.make(":B:")
linklet_sym = values.W_Symbol.make("linklet")

@expose("read-compiled-linklet", [values.W_InputPort], simple=False)
def read_compiled_linklet(in_port, env, cont):
    # port position comes at 2 (i.e. #~ is already read)
    from pycket.interpreter import return_value
    from pycket.env import w_version
    current_version = w_version.get_version() # str

    read_sym = values.W_Symbol.make("read")
    read = prim_env[read_sym]

    # Racket's read
    pycketconfig = env.toplevel_env()._pycketconfig
    sexp = read.call_interpret([in_port], pycketconfig)

    written_version = sexp.car().car() # W_Symbol

    if written_version.tostring() != current_version:
        raise SchemeException("versions don't match")

    data = sexp.cdr().car()

    D_or_B = data.car()

    if not D_or_B is dir_sym and not D_or_B is bundle_sym:
        raise SchemeException("malformed compiled code : Expected %s or %s to start with" % (dir_sym.tostring(), bundle_sym.tostring()))

    return return_value(read_loop(data), env, cont)

def read_loop(sexp):
    # Work in progress

    if isinstance(sexp, W_Cons):
        c = sexp.car()
        #import pdb;pdb.set_trace()
        if c is dir_sym:
            dir_map = sexp.cdr()
            return W_LinkletDirectory(read_loop(dir_map))
        elif c is bundle_sym:
            bundle_map = sexp.cdr()
            return W_LinkletBundle(read_loop(bundle_map))
        elif c is linklet_sym:
            # Unify this with compile_linklet
            w_name = sexp.cdr().cdr().car()
            w_importss = sexp.cdr().cdr().car()

            importss_acc = to_rpython_list(w_importss)
            importss_list = [None]*len(importss_acc)
            for index, importss_current in enumerate(importss_acc):
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

                importss_list[index] = inner_acc

            # Process the exports
            w_exports = sexp.cdr().cdr().cdr().car()

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
            w_body = sexp.cdr().cdr().cdr().cdr()
            body_forms_ls = to_rpython_list(w_body)

            toplevel_defined_linklet_vars = create_toplevel_linklet_vars(body_forms_ls)

            _body_forms = [sexp_to_ast(b, [], exports, toplevel_defined_linklet_vars, importss_list) for b in body_forms_ls]
            # FIXME : remove "disable_conversions" argument entirely
            body_forms = [None]*len(_body_forms)
            for i, bf in enumerate(_body_forms):
                b_form = Context.normalize_term(bf)
                b_form = assign_convert(b_form)
                b_form.clean_caches()
                body_forms[i] = b_form

            return W_Linklet(w_name, importss_list, exports, body_forms)
        else:
            new = w_null
            while sexp is not w_null:
                new = W_Cons.make(read_loop(sexp.car()), new)
                sexp = sexp.cdr()
            return new
    elif isinstance(sexp, W_EqImmutableHashTable):
        l = sexp.length()
        keys = [None]*l
        vals = [None]*l
        i = 0
        #import pdb;pdb.set_trace()
        for k, v in sexp.iteritems():
            keys[i] = k
            vals[i] = read_loop(v)
            i += 1

        return make_simple_immutable_table(W_EqImmutableHashTable, keys, vals)
    elif isinstance(sexp, W_EqualHashTable):
        l = sexp.length()
        keys = [None]*l
        vals = [None]*l
        i = 0
        #import pdb;pdb.set_trace()
        for k, v in sexp.hash_items():
            keys[i] = k
            vals[i] = read_loop(v)
            i += 1

        return W_EqualHashTable(keys, vals, immutable=True)
    else:
        #import pdb;pdb.set_trace()
        return sexp
