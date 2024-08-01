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
from pycket.vector import W_Vector
from pycket.values_string import W_String
from pycket.values_parameter import top_level_config
from pycket.error import SchemeException
from pycket import pycket_json
from pycket.prims.expose import prim_env, expose, default, expose_val
from pycket.prims.general import make_pred
from pycket.prims.correlated import W_Correlated
from pycket.prims.vector import vector
from pycket.AST import AST
from pycket.cont import Prompt, NilCont, continuation, loop_label
from pycket.prims.control import default_error_escape_handler, default_uncaught_exception_handler
from pycket.hash.base import W_HashTable
from pycket.hash.simple import W_EqImmutableHashTable, make_simple_immutable_table
from pycket.util import PerfRegion, PerfRegionCPS

from pycket.ast_vs_sexp import *

class W_Uninitialized(W_Object):
    errorname = "uninitialized"
    _attrs_ = []
    def __init__(self):
        pass
    def tostring(self):
        return "#<uninitialized>"

w_uninitialized = W_Uninitialized()

class W_LinkletVar(W_Object):
    errorname = "linklet-var"
    _attrs_ = ["val", "name", "constance"]
    _immutabe_fields_ = ["name"]

    def __init__(self, val, name, source_name, constance=w_false):
        self.val = val
        self.name = name
        self.constance = constance

class W_LinkletInstance(W_Object):
    errorname = "linklet-instance"
    _attrs_ = ["name", "vars", "exports", "data"]
    _immutable_fields_ = ["name", "data"]

    def __init__(self, name, vars, data=w_false):
        self.name = name # W_Symbol (for debugging)
        self.vars = vars # {W_Symbol:W_LinkletVar}
        self.data = data #

    def get_var(self, name):
        return self.vars[name]

    def expose_vars_to_prim_env(self, excludes=[]):
        for name, w_var in self.vars.iteritems():
            if name not in excludes:
                prim_env[name] = w_var.val

    def tostring(self):
        vars_str = " ".join(["(%s : %s)" % (name.tostring(), var.tostring()) for name, var in self.vars.iteritems()])
        data_str = self.data.tostring()
        return "(linklet-instance %s (%s) %s)" % (self.name, data_str, vars_str)

class W_LinkletBundle(W_Object):
    # Information in a linklet bundle is keyed by either a symbol or a fixnum
    errorname = "linklet-bundle"
    _attrs_ = _immutable_fields_ = ["bundle_mapping"]

    def __init__(self,bundle_mapping):
        self.bundle_mapping = bundle_mapping

    def get_mapping(self):
        return self.bundle_mapping

    def tostring(self):
        mapping = self.bundle_mapping
        return "BUNDLE : %s" % mapping.tostring()

our_vm_bytes = values.W_Bytes.from_string("pycket")

@expose("linklet-virtual-machine-bytes", [])
def vm_bytes():
    return our_vm_bytes

w_pycket_sym = values.W_Symbol.make("pycket")

# FIXME: control initialization of this from command line using -W
expose_val("current-compile-target-machine", values_parameter.W_Parameter(w_pycket_sym))


@expose("compile-target-machine?", [values.W_Symbol])
def compile_machine_target_p(v):
    return values.W_Bool.make(v is w_pycket_sym)

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
    errorname = "linklet-directory"
    _attrs_ = _immutable_fields_ = ["dir_mapping"]

    def __init__(self,dir_mapping):
        self.dir_mapping = dir_mapping

    def get_mapping(self):
        return self.dir_mapping

    def tostring(self):
        mapping = self.dir_mapping
        return "DIRECTORY : %s" % mapping.tostring()

@expose("hash->linklet-directory", [W_Object])
def hash_to_linklet_directory(content):
    return W_LinkletDirectory(content)

@expose("linklet-directory->hash", [W_LinkletDirectory])
def linklet_directory_to_hash(linkl_directory):
    return linkl_directory.dir_mapping

@continuation
def finish_perf_region_cont(label, env, cont, _vals):
    from pycket.util import finish_perf_region
    finish_perf_region(label)
    return return_value(_vals, env, cont)

@continuation
def instantiate_val_cont(forms, index, return_val, target, env, cont, _vals):
    if index >= len(forms):
        if return_val:
            return return_value(_vals, env, cont)
        else:
            return return_value(target, env, cont)

    # there's more
    return instantiate_loop(forms, index, return_val, target, env, cont)

@continuation
def instantiate_def_cont(forms, form, index, return_val, target, env, cont, _vals):

    values = _vals.get_all_values()
    len_values = len(values)
    if len(form.names) != len_values:
        raise SchemeException("%s -- expected %s values but got %s" % (form.tostring(), str(len(form.names)), str(len_values)))

    for i in range(len_values):
        name = form.names[i]
        value = values[i]

        env.toplevel_env().toplevel_set(name, value)

    return return_value(w_void, env, instantiate_val_cont(forms, index + 1, return_val, target, env, cont))

@loop_label
def instantiate_loop(forms, index, return_val, target, env, cont):
    form = forms[index]

    if isinstance(form, DefineValues):
        return form.rhs, env, instantiate_def_cont(forms, form, index, return_val, target, env, cont)
    else:
        return form, env, instantiate_val_cont(forms, index + 1, return_val, target, env, cont)

class W_Linklet(W_Object):
    errorname = "linklet"
    _attrs_ = _immutable_fields_ = ["name", "importss", "exports", "forms"]

    def __init__(self, name, importss, exports, all_forms):
        self.name = name # W_Symbol -- for debugging
        self.importss = importss # [[Import ...] ...]
        self.exports = exports # {int_id:Export ...}
        self.forms = all_forms # [..., AST ,...]

    def tostring(self):
        forms_str = " ".join([f.tostring() for f in self.forms])
        importss_ls = [None]*len(self.importss)

        for index, imp_group in enumerate(self.importss):
            importss_ls[index] = "(" + "".join(["(%s . %s)" % (imp_obj.ext_id.tostring(), imp_obj.id.tostring()) for imp_obj in imp_group]) + ")"

        importss_str = "".join(importss_ls)

        exports_str = "".join(["(%s %s)" % (exp_obj.int_id.tostring(), exp_obj.ext_id.tostring()) for exp_obj in self.exports.values()])

        return "(linklet %s (%s) (%s) %s)" % (self.name.tostring(), importss_str, exports_str, forms_str)

    def instantiate(self, import_instances_ls, config, prompt=False, target=None, env=None, cont=None):
        from pycket.env import ToplevelEnv
        env = ToplevelEnv(config)

        if not cont:
            cont = NilCont()
            cont.update_cm(parameterization_key, top_level_config)
            cont.update_cm(exn_handler_key, default_uncaught_exception_handler)

        if prompt:
            Prompt(w_default_continuation_prompt_tag, None, env, cont)

        for group_index, import_group in enumerate(self.importss):
            for imp in import_group:
                w_imp_var = import_instances_ls[group_index].get_var(imp.ext_id)
                env.toplevel_env().toplevel_set(imp.id, w_imp_var)

        return_val = True
        if not target:
            target = W_LinkletInstance(self.name, {})
            return_val = False

        # FIXME: gensym this ref name and put it in the linklet at compile
        env.toplevel_env().toplevel_set(mksym("instance-variable-reference"), target)

        for exp_sym, exp_obj in self.exports.iteritems():
            if target and exp_obj.ext_id in target.vars:
                var = target.vars[exp_obj.ext_id]
            else:
                var = W_LinkletVar(w_uninitialized, exp_obj.ext_id, w_false)
                target.vars[exp_obj.ext_id] = var

            env.toplevel_env().toplevel_set(exp_obj.int_id, var)

        if len(self.forms) == 0:
            # no need for any evaluation, just return the instance or the value
            if return_val:
                return return_value(w_void, env, cont)
            else:
                return return_value(target, env, cont)

        return instantiate_loop(self.forms, 0, return_val, target, env, cont)

    @staticmethod # json_file_name -> W_Linklet
    def load_linklet(json_file_name, set_version=False):
        from pycket.expand import readfile_rpython, getkey, JsonLoader
        from pycket.util import console_log
        from pycket.env import w_version

        """ Expands and loads a linklet from a JSON file"""
        with PerfRegion("json-load"):
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

        if set_version:
            from pycket.util import console_log
            conf = getkey(linklet_dict, "config", type='o')
            ver = conf['version'].value_string()
            console_log("Setting the version to %s" % ver)
            w_version.set_version(ver)

        exports = {}
        for exp in exports_list:
            if exp.is_array:
                arr = exp.value_array()
                internal_str = arr[0].value_object()['quote'].value_object()['toplevel'].value_string()
                internal_name = W_Symbol.make(internal_str)
                external_name = W_Symbol.make(arr[1].value_object()['quote'].value_object()['toplevel'].value_string())
                w_internal_name = Gensym.gensym(internal_str)
                exports[internal_name] = Export(w_internal_name, external_name)
            else:
                exp_str = exp.value_object()['quote'].value_object()['toplevel'].value_string()
                exp_sym = W_Symbol.make(exp_str)
                w_exp_sym = Gensym.gensym(exp_str)
                exports[exp_sym] = Export(w_exp_sym, exp_sym)

        imports_list = getkey(linklet_dict, "importss", type='a', default=[])

        importss = [None]*len(imports_list) # list of dict

        if "importss" in linklet_dict:
            for index, imports in enumerate(imports_list):
                arr = imports.value_array()
                # bootstrap linklets have no imports at all
                # this is only for debugging purposes
                instance_imports = []
                for id_str in arr:
                    imp_str = id_str.value_object()['quote'].value_object()['toplevel'].value_string()
                    imp_sym = W_Symbol.make(imp_str)
                    w_imp_sym = Gensym.gensym(imp_str)
                    instance_imports.append(Import(W_Fixnum(index), imp_sym, w_imp_sym, w_imp_sym))
                importss[index] = instance_imports

        console_log("Converting linklet forms to AST ...", 2)

        loader = JsonLoader()
        with PerfRegion("json-to-ast"):
            all_forms = []
            for body_form in getkey(linklet_dict, "body", type='a'):
                form_2 = loader.to_ast(body_form)
                form_1 = Context.normalize_term(form_2)
                form = assign_convert(form_1)
                all_forms.append(form)
            # for each exported defined id, we need to add a
            # variable-set! for the exported var with the defined id
            for exp_sym, exp_obj in exports.iteritems():
                rator = ModuleVar(var_set_sym, "#%kernel", var_set_sym, None)
                exp_var = LinkletVar(exp_obj.int_id)
                top_var = ToplevelVar(exp_sym)
                mode = Quote(values.w_false) # FIXME: possible optimization
                rands = [exp_var, top_var, mode]
                all_forms.append(App.make(rator,rands))

        linkl = W_Linklet(W_Symbol.make(json_file_name), importss, exports, all_forms)
        console_log("Finished converting linklet forms to AST ...", 2)

        config = {}
        config_obj = getkey(linklet_dict, "config", type='o')
        if config_obj is not None:
            for k, v in config_obj.iteritems():
                config[k] = v.value_string()

        return linkl, config


"""
 (define-values (1/read-compiled-linklet) (hash-ref linklet-primitive-table 'read-compiled-linklet #f))
 (define-values (1/instance-unset-variable!) (hash-ref linklet-primitive-table 'instance-unset-variable! #f))
 (define-values (1/variable-reference?) (hash-ref linklet-primitive-table 'variable-reference? #f))
 (define-values (1/variable-reference-constant?) (hash-ref linklet-primitive-table 'variable-reference-constant? #f))
"""

make_pred("linklet?", W_Linklet)

make_pred("instance?", W_LinkletInstance)

# compile-linklet prepares a linklet for instantiation. Takes an s-expr (or a
# W_Linklet) and returns a W_Linklet to be passed to instantiate-linklet.
@expose("compile-linklet", [W_Object, default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false)], simple=False)
def compile_linklet(form, name, import_keys, get_import, options, env, cont):
    from pycket.util import console_log
    console_log("compiling linklet : %s %s\n import_keys : %s -- get_import : %s" % (name.tostring(), form.tostring(), import_keys.tostring(), get_import.tostring()), 5)
    with PerfRegionCPS("compile-linklet"):
        cont_ = finish_perf_region_cont("compile-linklet", env, cont)
        return do_compile_linklet(form, name, import_keys, get_import, options, env, cont_)

# do-compile-linklet prepares a linklet for instantiation. Takes an s-expr (or a
# W_Linklet) and returns a W_Linklet to be passed to instantiate-linklet.
def do_compile_linklet(form, name, import_keys, get_import, options, env, cont):
    from pycket.util import console_log
    if isinstance(form, W_WrappedConsProper): # s-expr
        # read it and create an AST, put it in a W_Linklet and return
        if not isinstance(form.car(), W_Symbol) or "linklet" != form.car().tostring():
            raise SchemeException("Malformed s-expr. Expected a linklet, got %s" % form.tostring())
        else:
            w_name = W_Symbol.make("ad-hoc") if name is w_false else name

            # Process the imports
            w_importss = form.cdr().car()
            importss = get_imports_from_w_importss_sexp(w_importss)
            # list of directories (one for each import group)
            # importss_list ==> [[Import ...] ...]

            # Process the exports
            w_exports = form.cdr().cdr().car()
            exports = get_exports_from_w_exports_sexp(w_exports)
            # exports ==> {int_id:Export ...}

            # Process the body
            w_body = form.cdr().cdr().cdr()
            with PerfRegion("compile-sexp-to-ast"):
                body_forms = process_w_body_sexp(w_body, importss, exports)

            linkl = W_Linklet(w_name, importss, exports, body_forms)

            console_log("compiled linklet : %s" % (linkl.tostring()), 6)

            if import_keys is w_false:
                return return_value_direct(linkl, env, cont)
            else:
                return return_multi_vals(Values.make([linkl, import_keys]), env, cont)

    elif isinstance(form, W_Linklet):
        if import_keys is w_false:
            return return_value_direct(form, env, cont)
        else:
            return return_multi_vals(Values.make([form, import_keys]), env, cont)

    else: # correlated
        # take the AST from the correlated and put it in a W_Linklet and return
        raise SchemeException("NYI")

@expose("instance-name", [W_LinkletInstance])
def instance_name(l_inst):
    return l_inst.name

@expose("instantiate-linklet", [W_Linklet, W_List, default(W_Object, w_false), default(W_Object, w_true)], simple=False)
def instantiate_linklet(linkl, import_instances, target_instance, use_prompt, env, cont):
    from pycket.util import console_log
    console_log("instantiating linklet : %s" % linkl.name.tostring(), 4)

    prompt = False
    if use_prompt is not w_false: # use-prompt? : any/c = #t - what happens when it is 3 ?
        prompt = True

    im_list, im_length = to_rpython_list(import_instances)
    expected = len(linkl.importss)

    if expected != im_length:
        raise SchemeException("The number of instances in import-instances must match the number of import sets in linklet. Expected %s but got %s" % (expected, im_length))

    if target_instance is w_false:
        target = None
    elif isinstance(target_instance, W_LinkletInstance):
        target = target_instance
    else:
        raise SchemeException("Expected #f or instance? as target-instance, got : %s" % target_instance)
    with PerfRegionCPS("instantiate-linklet"):
        cont_ = finish_perf_region_cont("instantiate-linklet", env, cont)
        return linkl.instantiate(im_list, env.toplevel_env()._pycketconfig, prompt, target, env, cont_)

@expose("linklet-import-variables", [W_Linklet])
def linklet_import_variables(linkl):
    importss_py_lst = linkl.importss
    importss = w_null
    for imp_group in importss_py_lst:
        imp_inner = w_null
        for imp_obj in imp_group:
            imp_inner = W_Cons.make(imp_obj.ext_id, imp_inner)
        importss = W_Cons.make(imp_inner, importss)
    return importss

@expose("linklet-export-variables", [W_Linklet])
def linklet_export_variables(linkl):
    exports = w_null
    for ext_sym in linkl.exports.keys():
        exports = W_Cons.make(ext_sym, exports)
    return exports

@expose("instance-variable-names", [W_LinkletInstance])
def instance_variable_names(inst):
    return get_instance_variable_names(inst)
# to be able to call it without prim_env indirection
def get_instance_variable_names(inst):
    names = w_null
    for name in inst.vars.keys():
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
    with PerfRegion("make-instance"):
        name = args[0] # W_Symbol
        data = w_false
        mode = w_false

        from pycket.util import console_log
        if "'" in name.tostring():
            console_log("making instance : %s" % name.tostring(), 3)
        else:
            console_log("making instance : %s" % name.tostring(), 4)

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
            vars_vals_dict[n] = W_LinkletVar(v, n, mode)

        return W_LinkletInstance(name, vars_vals_dict, data)

@expose("recompile-linklet", [W_Linklet, default(W_Object, None), default(W_Object, w_false), default(W_Object, None)], simple=False)
def recompile_linklet(linkl, name, import_keys, get_import, env, cont):
    if import_keys is not None:
        return return_multi_vals(Values.make([linkl, import_keys]), env, cont)
    else:
        return return_value(linkl, env, cont)

@expose("instance-variable-value", [W_LinkletInstance, W_Symbol, default(W_Object, None)], simple=False)
def instance_variable_value(instance, name, fail_k, env, cont):
    if name not in instance.vars or instance.vars[name].val is w_uninitialized:
        if fail_k is not None and fail_k.iscallable():
            return fail_k.call([], env, cont)
        else:
            raise SchemeException("key %s not found in the instance %s" % (name.tostring(), instance.name.tostring()))
    return return_value(instance.vars[name].val, env, cont)

@expose("instance-describe-variable!", [W_LinkletInstance, W_Symbol, W_Object])
def instance_describe_variable(inst, name, desc_v):
    return w_void

@expose("instance-data", [W_LinkletInstance])
def instance_data(inst):
    return inst.data

@expose("eval-linklet", [W_Linklet])
def eval_linklet(l):
    return l

@expose("instance-set-variable-value!", [W_LinkletInstance, W_Symbol, W_Object, default(W_Object, w_false)])
def instance_set_variable_value(instance, name, w_val, mode):
    var = instance.vars.get(name, None)
    if var:
        if var.constance is not w_false:
            raise SchemeException("Cannot mutate a constant : %s" % name.tostring())
    else:
        var = W_LinkletVar(w_val, name, mode)
        instance.vars[name] = var

    var.val = w_val
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
    return varref.is_unsafe()

# @continuation
# def read_linklet_cont(env, cont, _vals):
#     from pycket.util import console_log
#     from pycket.util import finish_perf_region
#     bundle_map = check_one_val(_vals)
#     finish_perf_region("fasl->s-exp")
#     if not isinstance(bundle_map, W_HashTable):
#         raise SchemeException("got something that is not a table: %s"%bundle_map.tostring())
#     console_log("BUNDLE SEXP FASL-READ from ZO: %s" % deserialize_loop(bundle_map).tostring(), 7)
#     with PerfRegion("s-exp->ast"):
#         return return_value(deserialize_loop(bundle_map), env, cont)

# Keeping the use of racket/fasl for future comparisons

@expose("read-linklet-bundle-hash", [values.W_InputPort], simple=False)
def read_linklet_bundle_hash(in_port, env, cont):
    from pycket.racket_entry import get_primitive
    from pycket.fasl import Fasl
    from pycket.util import console_log

    current_load_relative_dir_path = get_primitive("current-load-relative-directory").get_cell_value(cont)

    fasl_to_s_exp = get_primitive("fasl->s-exp")
    with PerfRegion("fasl->s-exp"):
        bundle_map = Fasl(current_load_relative_dir_path).to_sexp_from_w_port(in_port)
        #return fasl_to_s_exp.call([in_port, values.w_true], env, read_linklet_cont(env, cont))
    if not isinstance(bundle_map, W_HashTable):
        raise SchemeException("got something that is not a table: %s" % bundle_map.tostring())
    console_log("BUNDLE SEXP FASL-READ from ZO: %s" % deserialize_loop(bundle_map).tostring(), 7)
    with PerfRegion("s-exp->ast"):
        return return_value(deserialize_loop(bundle_map), env, cont)

@expose("write-linklet-bundle-hash", [W_EqImmutableHashTable, values.W_OutputPort], simple=False)
def write_linklet_bundle_hash(ht, out_port, env, cont):
    from pycket.util import console_log
    from pycket.racket_entry import get_primitive
    console_log("BUNDLE AST TO BE SERIALIZED: %s" % ht.tostring(), 7)

    with PerfRegion("ast->sexp"):
        l = ht.length()
        keys = [None]*l
        vals = [None]*l

        i = 0
        for k, v in ht.iteritems():
            keys[i] = k
            vals[i] = ast_to_sexp(v)
            i += 1

        bundle_s_exp = make_simple_immutable_table(W_EqImmutableHashTable, keys, vals)

    console_log("WRITING BUNDLE SEXP : %s" % bundle_s_exp.tostring(), 7)

    s_exp_to_fasl = get_primitive("s-exp->fasl")
    with PerfRegionCPS("s-exp->fasl"):
        return s_exp_to_fasl.call([bundle_s_exp, out_port, values.w_false], env,
                                  finish_perf_region_cont("s-exp->fasl", env, cont))

@expose("variable-ref", [W_LinkletVar])
def variable_ref(w_var):
    v = w_var.val
    if v is w_uninitialized:
        raise SchemeException("Reference to an uninitialized variable : %s" % w_var.name.tostring())
    return v

@expose("variable-ref/no-check", [W_LinkletVar])
def variable_ref(w_var):
    return w_var.val

def do_var_set_bang(w_var, w_val, constance):
    if w_var.constance is not w_false:
        raise SchemeException("Cannot modify a constant %s" % w_var.name.tostring())
    w_var.val = w_val
    if constance is not w_false:
        w_var.constance = constance

@expose("variable-set!", [W_LinkletVar, W_Object, W_Object])
def variable_set_bang(w_var, w_val, constance):
    do_var_set_bang(w_var, w_val, constance)
    return w_void

@expose("variable-set!/check-undefined", [W_LinkletVar, W_Object, W_Object])
def variable_set_bang(w_var, w_val, constance):
    if w_var.val is w_uninitialized:
        raise SchemeException("Reference to an uninitialized variable : %s" % w_var.name.tostring())
    do_var_set_bang(w_var, w_val, constance)
    return w_void

@expose("linklet-add-target-machine-info", [W_Linklet, W_Linklet])
def linklet_add_target_machine_info(linkl, from_linklet):
    """linklet-add-target-machine-info injects target-specific information from the from_linklet into the linkl to be used in cross-compilation.

    A linklet implementation may support information for multiple target machines within a linklet (which we don't in Pycket, hence the no-op).
    """
    return linkl
