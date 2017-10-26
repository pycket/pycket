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
from pycket.interpreter import DefineValues, interpret_one, Context, return_value, return_multi_vals, Quote, App, ModuleVar
from pycket.assign_convert import assign_convert
from pycket.values import W_Object, W_Symbol, w_true, w_false, W_List, W_Cons, W_WrappedConsProper, w_null, Values, W_Number, w_void
from pycket.error import SchemeException
from pycket import pycket_json
from pycket.prims.expose import prim_env, expose, default
from pycket.prims.general import make_pred
from pycket.prims.correlated import W_Correlated
from pycket.prims.vector import vector
from pycket.AST import AST

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

    def __init__(self, name, imported_instances, export_ids, renamings, defs):
        self.name = name # W_Symbol (for debugging)
        self.imported_instances = imported_instances # [[...],[..., W_LinkletInstance ,...],...]
        self.export_ids = export_ids # [..., str ,...]
        self.renamings = renamings # {str:W_Symbol}
        self.defs = defs # {W_Symbol-W_Object}

    def tostring(self):
        return "W_Linklet Instance : %s - Importing : %s" % (self.name, self.imported_instances)

    def get_val(self, id_str):
        """ Returns a defined value."""
        if id_str not in self.defs.keys():
             raise SchemeException("%s is not defined/exported in (or through) this instance" % id_str)

        if id_str.variable_name() in self.renamings:
            id_str = self.renamings[id_str.variable_name()]
        return self.defs[id_str]

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

    def instantiate(self, w_imported_instances, config, toplevel_eval=False):
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

        for form in self.forms:
            if isinstance(form, DefineValues):
                expression = form.rhs
                values = interpret_one(expression, env).get_all_values()
                len_values = len(values)
                if len(form.names) == len_values:
                    for index in range(len_values): 
                        name = form.names[index]
                        value = values[index]

                        inst.add_def(name, value)
                else:
                    raise SchemeException("wrong number of values for define-values")

            else: # any expression
                values = interpret_one(form, env)
                # FIXME: chech multiple values??
                if toplevel_eval:
                    return_val = values

        if toplevel_eval:
            return return_val
        
        return inst

    @staticmethod # json_file_name -> W_Linklet
    def load_linklet(json_file_name, loader):
        """ Expands and loads a linklet from a JSON file"""
        data = readfile_rpython(json_file_name+".linklet")
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

                renamings[exported_name] = W_Symbol.make(defined_name)
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
 (define-values (1/recompile-linklet) (hash-ref linklet-primitive-table 'recompile-linklet #f))
 (define-values (1/eval-linklet) (hash-ref linklet-primitive-table 'eval-linklet #f))
 (define-values (1/read-compiled-linklet) (hash-ref linklet-primitive-table 'read-compiled-linklet #f))
 (define-values (1/instance-data) (hash-ref linklet-primitive-table 'instance-data #f))
 (define-values (1/instance-variable-value) (hash-ref linklet-primitive-table 'instance-variable-value #f))
 (define-values (1/instance-unset-variable!) (hash-ref linklet-primitive-table 'instance-unset-variable! #f))
 (define-values (1/variable-reference?) (hash-ref linklet-primitive-table 'variable-reference? #f))
 (define-values (1/variable-reference->instance) (hash-ref linklet-primitive-table 'variable-reference->instance #f))
 (define-values (1/variable-reference-constant?) (hash-ref linklet-primitive-table 'variable-reference-constant? #f))

 STUB (define-values (1/compile-linklet) (hash-ref linklet-primitive-table 'compile-linklet #f))
 STUB (define-values (1/instantiate-linklet) (hash-ref linklet-primitive-table 'instantiate-linklet #f))

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

def sexp_to_ast(form):
    if isinstance(form, W_Number):
        form = Quote(form)
    elif isinstance(form, W_List):
        form_inner = sexp_to_ast(form.car())

        rands_ls = to_rpython_list(form.cdr())
        rands = [sexp_to_ast(r) for r in rands_ls]
                    
        form = App.make(form_inner, rands)
    elif isinstance(form, W_Symbol):
        # form_inner.tostring() == "'let"
        form = ModuleVar(form, "#%kernel", form, None)
    else:
        raise Exception("Don't know what to do with this form yet : %s", form)

    form = Context.normalize_term(form)
    form = assign_convert(form)
    form.clean_caches()
    return form

@expose("compile-linklet", [W_Object, default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false), default(W_Object, w_false)], simple=False)
def compile_linklet(form, name, import_keys, get_import, serializable_huh, env, cont):

    #     pycket.values.W_WrappedConsProper object at 0x000000001baba5d0>
    # (Pdb) form.tostring()
    # "('linklet (('.'top-level-bind! '.'top-level-require!) ('.'mpi-vector '.'syntax-literals) ('.'namespace '.'phase '.'self '.'inspector '.'bulk-binding-registry '.'set-transformer!)) () 1)"

    if isinstance(form, W_WrappedConsProper): # s-expr
        # read it and create an AST, put it in a W_Linklet and return
        if not isinstance(form.car(), W_Symbol) or "'linklet" != form.car().tostring():
            raise SchemeException("Malformed s-expr. Expected a linklet")
        else:
            w_importss = form.cdr().car()
            w_exports = form.cdr().cdr().car()
            w_body = form.cdr().cdr().cdr()

            exports = []
            renamings = {}
            r_exports = to_rpython_list(w_exports)
            for exp in r_exports:
                if isinstance(exp, W_WrappedConsProper):
                    defined_name = exp.car() # W_Symbol
                    exported_name = exp.cdr().car().variable_name() # str
                    renamings[exported_name] = defined_name

                    exports.append(exported_name)
                else:
                    exports.append(exp.variable_name())
            
            body_forms_ls = to_rpython_list(w_body)
            body_forms = [sexp_to_ast(b) for b in body_forms_ls]

            importss_list = []
            importss_count = 0
            importss_acc = w_importss
            while (importss_acc is not w_null):
                importss_current = importss_acc.car()
                inner_acc = []
                while (importss_current is not w_null):
                    c = importss_current.car()
                    inner_acc.append(c.variable_name())
                    importss_current = importss_current.cdr()
                importss_list.append(inner_acc)
                importss_count += 1
                importss_acc = importss_acc.cdr()

            if name is w_false:
                w_name = W_Symbol.make("ad-hoc")
            else:
                w_name = name
            linkl = W_Linklet(w_name, importss_list, exports, renamings, body_forms)
            return return_multi_vals(Values.make([linkl, vector([])]), env, cont)
            
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

    if use_prompt is w_true:
        raise Exception("instantiate-linklet : not implemented yet : wrap each definition and expression in the linklet in a prompt")
    
    im_list = to_rpython_list(import_instances)

    if target_instance is None or target_instance is w_false:

        return return_value(linkl.instantiate(im_list, env.toplevel_env()._pycketconfig), env, cont)
    
    elif isinstance(target_instance, W_LinkletInstance):
        #Providing a target instance to `instantiate-linklet` means that we get the body's results instead of the instance as a result
        # use and modify target_instance for the linklet definitions and expressions"
        # When a target instance is provided to instantiate-linklet, any existing variable with the same name will be left as-is, instead of set to undefined. This treatment of uninitialized variables provides core support for top-level evaluation where variables may be referenced and then defined in a separate element of compilation.

        # The linklet’s exported variables are accessible in the result instance or in target-instance using the linklet’s external name for each export. If target-instance is provided as non-#f, its existing variables remain intact if they are not modified by a linklet definition.

        return return_value(linkl.instantiate(im_list, env.toplevel_env()._pycketconfig, toplevel_eval=True), env, cont)

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

    return W_LinkletInstance(name, [], [], {}, vars_vals_dict)

@expose("instance-set-variable-value!", [W_LinkletInstance, W_Symbol, W_Object, default(W_Object, w_false)])
def instance_set_variable_value(instance, name, val, mode):

    instance.set_bang_def(name, val)
    return w_void
