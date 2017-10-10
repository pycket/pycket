""" Implementation of linklets

class LinkletInstance(W_Object)
class Linklet(object)
class LinkletBundle(W_Object)
class LinkletDirectory(W_Object)
 """

#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from pycket.expand import readfile_rpython, getkey, mksym
from pycket.interpreter import DefineValues, interpret_one, Context
from pycket.assign_convert import assign_convert
from pycket.values import W_LinkletPrim, W_Procedure, W_Object, W_Bool, W_Symbol, to_list, w_true, w_false, W_List
from pycket.error import SchemeException
from pycket import pycket_json
from pycket.prims.expose import prim_env, expose, expose_val, default
from pycket.prims.general import make_pred

class LinkletInstance(W_Object):
    """
    def tostring(self):

    def export_val(self, id_str):
    def is_defined(self, id_str):
    def is_exported(self, id_str):

    def provide_all_exports_to_prim_env(self):
    def lookup(self, id_str, import_num):

    def set_defs(self, defs):
    def set_bang_def(self, name, val):
    def add_def(self, name, val):
    def append_defs(self, new_defs):

    """

    def __init__(self, name, imported_instances, export_ids, defs):
        self.name = name # for debugging
        self.imported_instances = imported_instances
        # [[...],[...,LinkletInstance,...],...]
        self.export_ids = export_ids # python str's
        self.defs = defs # name-value

    def tostring(self):
        return "Linklet Instance : %s - Importing : %s" % (self.name, self.imported_instances)

    def export_val(self, id_str):
        """ Exports a defined value."""
        return self.is_defined(id_str) and self.defs[id_str]

    def is_defined(self, id_str):
        """ Checks if given id is defined by this instance. """
        if id_str not in self.defs.keys():
            raise SchemeException("%s is not defined in (or through) this instance" % id_str)
        return True

    def is_exported(self, id_str):
        """ Checks if given id is exported by this instance. """
        if id_str not in self.export_ids:
            raise SchemeException("%s is not in the exports of this linklet instance" % id_str)
        return True

    def provide_all_exports_to_prim_env(self):
        """ Puts all exported values to prim_env. """
        for name, value in self.defs.iteritems():
            assert isinstance(value, W_Object)
            if isinstance(value, W_Procedure):
                prim_env[name] = W_LinkletPrim(value)
            else:
                prim_env[name] = value

    def lookup(self, id_str, import_num):
        """ Gets the requested value from the appropriate instance. """
        if import_num < 0:
            rv = self.export_val(id_str)
            assert isinstance(rv, W_Object) # it's a racket value
            return rv
        else:
            assert import_num >= 0 and self.imported_instances is not None and len(self.imported_instances) != 0
            inst = self.imported_instances[import_num]
            return inst.lookup(id_str, -1)

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
        for name, val in new_defs.iteritems:
            if name in self.defs:
                raise SchemeException("Duplicate definition : %s" % name)

        self.defs.update(new_defs)

class LinkletBundle(W_Object):

    def __init__():
        self.linklets = []

class LinkletDirectory(W_Object):

    def __init__():
        self.bundles = []
        
class Linklet(W_Object):
    """
    def instantiate(self, env, imported_instances):

    @staticmethod
    def load_linklet(json_file_name, loader):
    """

    def __init__(self, name, importss, exports, all_forms):
        self.name = name # for debugging
        self.importss = importss # list of list of W_Symbol
        self.exports = exports # list of W_Symbol
        self.forms = all_forms # list of pycket asts

    def instantiate(self, env, imported_instances):
        """ Instantiates the linklet:
        --- takes the imported linklet instances
        --- extracts the specified set of variables
        --- returns a LinkletInstance
        """
        l_importss = len(self.importss)
        l_given_instances = len(imported_instances)
        if l_importss != l_given_instances:
            raise SchemeException("Required %s instances but given %s" % (l_importss, l_given_instances))
        # Check if the imports are really exported by the given instances
        for index in range(l_importss):
            imported_ids = self.importss[index]
            for id_ in imported_ids:
                assert index >= 0 and imported_instances is not None and len(imported_instances) != 0
                inst = imported_instances[index]


        inst = LinkletInstance(self.name, imported_instances, self.exports, {})

        env.current_linklet_instance = inst

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
                continue

        return inst

    @staticmethod # json_file_name -> Linklet
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
        # list of python string
        exports = [mksym(sym.value_object()['quote']) for sym in exports_list]
        # FIXME: we're currently not getting the renamings of the exports
        # fix the linkl_expander for that

        imports_list = getkey(linklet_dict, "importss", type='a')

        importss = []
        if "importss" in linklet_dict:
            for imports in imports_list:
                arr = imports.value_array()
                importss.append([mksym(sym.value_object()['quote']) for sym in arr])

        all_forms = []
        for body_form in getkey(linklet_dict, "body", type='a'):
            form = loader.to_ast(body_form)
            form = Context.normalize_term(form)
            form = assign_convert(form)
            form.clean_caches()
            all_forms.append(form)

        return Linklet(json_file_name, importss, exports, all_forms)


    
"""
 OK (define-values (1/primitive-table) (hash-ref linklet-primitive-table 'primitive-table #f))
 (define-values (1/primitive->compiled-position) (hash-ref linklet-primitive-table 'primitive->compiled-position #f))
 (define-values (1/compiled-position->primitive) (hash-ref linklet-primitive-table 'compiled-position->primitive #f))
 OK (define-values (1/linklet?) (hash-ref linklet-primitive-table 'linklet? #f))
 (define-values (1/compile-linklet) (hash-ref linklet-primitive-table 'compile-linklet #f))
 (define-values (1/recompile-linklet) (hash-ref linklet-primitive-table 'recompile-linklet #f))
 (define-values (1/eval-linklet) (hash-ref linklet-primitive-table 'eval-linklet #f))
 (define-values (1/read-compiled-linklet) (hash-ref linklet-primitive-table 'read-compiled-linklet #f))
 STUB (define-values (1/instantiate-linklet) (hash-ref linklet-primitive-table 'instantiate-linklet #f))
 OK (define-values (1/linklet-import-variables) (hash-ref linklet-primitive-table 'linklet-import-variables #f))
 OK (define-values (1/linklet-export-variables) (hash-ref linklet-primitive-table 'linklet-export-variables #f))
 OK (define-values (1/instance?) (hash-ref linklet-primitive-table 'instance? #f))
 OK (define-values (1/make-instance) (hash-ref linklet-primitive-table 'make-instance #f))
 OK (define-values (1/instance-name) (hash-ref linklet-primitive-table 'instance-name #f))
 (define-values (1/instance-data) (hash-ref linklet-primitive-table 'instance-data #f))
 OK (define-values (1/instance-variable-names) (hash-ref linklet-primitive-table 'instance-variable-names #f))
 (define-values (1/instance-variable-value) (hash-ref linklet-primitive-table 'instance-variable-value #f))
 OK (define-values (1/instance-set-variable-value!) (hash-ref linklet-primitive-table 'instance-set-variable-value! #f))
 (define-values (1/instance-unset-variable!) (hash-ref linklet-primitive-table 'instance-unset-variable! #f))
 (define-values (1/linklet-directory?) (hash-ref linklet-primitive-table 'linklet-directory? #f))
 (define-values (1/hash->linklet-directory) (hash-ref linklet-primitive-table 'hash->linklet-directory #f))
 (define-values (1/linklet-directory->hash) (hash-ref linklet-primitive-table 'linklet-directory->hash #f))
 (define-values (1/linklet-bundle?) (hash-ref linklet-primitive-table 'linklet-bundle? #f))
 (define-values (1/hash->linklet-bundle) (hash-ref linklet-primitive-table 'hash->linklet-bundle #f))
 (define-values (1/linklet-bundle->hash) (hash-ref linklet-primitive-table 'linklet-bundle->hash #f))
 (define-values (1/variable-reference?) (hash-ref linklet-primitive-table 'variable-reference? #f))
 (define-values (1/variable-reference->instance) (hash-ref linklet-primitive-table 'variable-reference->instance #f))
 (define-values (1/variable-reference-constant?) (hash-ref linklet-primitive-table 'variable-reference-constant? #f))
"""

make_pred("linklet?", Linklet)

make_pred("instance?", LinkletInstance)

@expose("instance-name", [LinkletInstance])
def instance_name(l_inst):
    return W_Symbol.make(l_inst.name)
    
# @expose("instantiate-linklet", [Linklet, W_List, default(W_Object, w_false), default(W_Object, w_true)], simple=False)
# def instantiate_linklet(linkl, import_instances, target_instance, use_prompt, env, cont):
#     if use_prompt is w_true:
#         raise Exception("instantiate-linklet : not implemented yet : wrap each definition and expression in the linklet in a prompt")
#     if target_instance is None or target_instance is w_false:
#         return linkl.instantiate(env, import_instances) <<<< FIXME: import_instances : W_List to 'list'
#     elif isinstance(target_instance, LinkletInstance):
#         raise Exception("instantiate-linklet : not implemented yet : use and modify target_instance for the linklet definitions and expressions")
#     # FIXME
#     # The linklet’s exported variables are accessible in the result instance or in target-instance using the linklet’s external name for each export. If target-instance is provided as non-#f, its existing variables remain intact if they are not modified by a linklet definition.
#     else:
#         raise SchemeException("Expected #f or instance? as target-instance, got : %s" % target_instance)

@expose("linklet-import-variables", [Linklet])
def linklet_import_variables(linkl):
    importss_py_lst = linkl.importss
    importss = []
    for imp in importss_py_lst:
        imp_py_lst = []
        for sym in imp:
            imp_py_lst.append(sym)
        importss.append(to_list(imp_py_lst))
    return to_list(importss)

@expose("linklet-export-variables", [Linklet])
def linklet_export_variables(linkl):
    return to_list(linkl.exports)
    
    
@expose("instance-variable-names", [LinkletInstance])
def instance_variable_names(inst):
    # inst = args[0]
    # if not isinstance(inst, LinkletInstance):
    #     raise SchemeException("Not a linklet instance! %s" % inst)

    names = []
    for name in inst.defs.keys():
        
        if isinstance(name, str):
            n = W_Symbol.make(name)
        elif isinstance(name, W_Symbol):
            n = name
        else:
            raise SchemeException("name not a symbol : %s" % name)

        names.append(n)

    return to_list(names)

make_pred("linklet-directory?", LinkletDirectory)

make_pred("linklet-bundle?", LinkletBundle)

@expose("make-instance") #FIXME: [W_Object, W_Object, [W_Symbol, W_Object] ....]
def make_instance(args): # name, data, *vars_vals
    name = args[0]
    data = args[1]
    vars_vals = args[2:]
    # check if the vars and vals match
    if ((len(vars_vals) % 2) != 0):
        raise SchemeException("Variable names and values do not match : %s" % vars_vals)

    if isinstance(name, W_Symbol):
        name_str = name.utf8value # which is a str
    else:
        name_str = name.tostring()
    
    vars_vals_dict = {}
    for i in range(0, len(vars_vals), 2):
        n = vars_vals[i]
        v = vars_vals[i+1]
        vars_vals_dict[n] = v
    
    return LinkletInstance(name_str, [], [], vars_vals_dict)

@expose("instance-set-variable-value!", [LinkletInstance, W_Symbol, W_Object, default(W_Object, w_false)])
def instance_set_variable_value(instance, name, val, mode):
    # if len(args) != 4:
    #     raise SchemeException("Expected 4 arguments, given %s - %s" % (len(args), args))

    # instance = args[0]
    # name = args[1]
    # val = args[2]
    # mode = args[3]

    instance.set_bang_def(name, val)
