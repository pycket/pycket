#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket              import impersonators as imp
from pycket              import values
from pycket              import values_parameter, values_struct
from pycket.arity        import Arity
from pycket.error        import SchemeException
from pycket.prims.expose import unsafe, default, expose, expose_val

expose_val("current-inspector", values_struct.current_inspector_param)

expose_val("current-code-inspector", values_struct.current_inspector_param)

@expose("make-inspector", [default(values_struct.W_StructInspector, None)], simple=False)
def do_make_instpector(w_inspector, env, cont):
    from pycket.interpreter import return_value
    if w_inspector is None:
        w_inspector = values_struct.current_inspector_param.get(cont)
    new_inspector = values_struct.W_StructInspector.make(w_inspector)
    return return_value(new_inspector, env, cont)

@expose("make-sibling-inspector", [default(values_struct.W_StructInspector, None)], simple=False)
def do_make_sibling_instpector(w_inspector, env, cont):
    from pycket.interpreter import return_value
    if w_inspector is None:
        w_inspector = values_struct.current_inspector_param.get(cont)
    new_inspector = values_struct.W_StructInspector.make(w_inspector, issibling=True)
    return return_value(new_inspector, env, cont)

@expose("inspector-superior?", [values_struct.W_StructInspector, values_struct.W_StructInspector])
def inspector_superior_huh(w_inspector, maybe_subinspector):
    if w_inspector is maybe_subinspector:
        return values.w_false

    s = maybe_subinspector.w_super
    while(s is not None):
        if w_inspector is s:
            return values.w_true
        s = s.w_super

    return values.w_false

@expose("struct?", [values.W_Object], simple=False)
def do_is_struct(v, env, cont):
    from pycket.interpreter import return_value
    current_inspector = values_struct.current_inspector_param.get(cont)
    if isinstance(v, values_struct.W_RootStruct):
        if current_inspector.has_control(v.struct_type()):
            return return_value(values.w_true, env, cont)
    return return_value(values.w_false, env, cont)

@expose("struct-info", [values.W_Object], simple=False)
def do_struct_info(v, env, cont):
    from pycket.interpreter import return_multi_vals
    current_inspector = values_struct.current_inspector_param.get(cont)
    if (isinstance(v, values_struct.W_RootStruct) and
        current_inspector.has_control(v.struct_type())):
        return v.get_struct_info(env, cont)
    return return_multi_vals(
            values.Values.make([values.w_false, values.w_true]), env, cont)

struct_info = do_struct_info.w_prim

@expose("struct-type-info", [values_struct.W_StructType], simple=False)
def do_struct_type_info(struct_type, env, cont):
    from pycket.interpreter import return_value
    return return_value(values.Values.make(struct_type.struct_type_info(cont)), env, cont)

@expose("struct-type-make-constructor", [values_struct.W_StructType], simple=False)
def do_struct_type_make_constructor(struct_type, env, cont):
    from pycket.interpreter import return_value
    current_inspector = values_struct.current_inspector_param.get(cont)
    if not current_inspector.has_control(struct_type):
        # TODO: we should raise exn:fail:contract
        raise SchemeException("fail_contract")
    return return_value(struct_type.constructor, env, cont)

@expose("struct-type-make-predicate", [values_struct.W_StructType], simple=False)
def do_struct_type_make_predicate(struct_type, env, cont):
    from pycket.interpreter import return_value
    current_inspector = values_struct.current_inspector_param.get(cont)
    if not current_inspector.has_control(struct_type):
        # TODO: we should raise exn:fail:contract
        raise SchemeException("fail_contract")
    return return_value(struct_type.predicate, env, cont)

@expose("make-struct-type",
        [values.W_Symbol, values.W_Object, values.W_Fixnum, values.W_Fixnum,
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_null),
         default(values.W_Object, None),
         default(values.W_Object, values.w_false),
         default(values.W_List, values.w_null),
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_false)], simple=False)
def do_make_struct_type(w_name, w_super_type, w_init_field_count,
                        w_auto_field_count, w_auto_value, w_properties, w_inspector,
                        w_proc_spec, w_immutables, w_guard, w_constructor_name,
                        env, cont):
    if w_inspector is None:
        w_inspector = values_struct.current_inspector_param.get(cont)

    if (w_constructor_name is not values.w_false and 
        not isinstance(w_constructor_name, values.W_Symbol)):
        raise SchemeException("make-struct-type: constructor name mustbe be symbol? or #f")

    if not (isinstance(w_super_type, values_struct.W_StructType) or
            w_super_type is values.w_false):
        raise SchemeException("make-struct-type: expected a struct-type? or #f for the super type , but got %s : %s" % (w_super_type, w_super_type.tostring()))

    if (isinstance(w_super_type, values_struct.W_StructType) and
       w_super_type.prop_sealed):
      raise SchemeException("make-struct-type: cannot make a subtype of a sealed type")

    init_field_count = w_init_field_count.value
    auto_field_count = w_auto_field_count.value

    immutables = []
    for i in values.from_list_iter(w_immutables):
        if not isinstance(i, values.W_Fixnum) or i.value < 0:
            raise SchemeException("make-struct-type: expected list of positive integers for immutable fields")
        immutables.append(i.value)

    return values_struct.W_StructType.make(w_name=w_name,
        w_super_type=w_super_type, init_field_count=init_field_count,
        auto_field_count=auto_field_count, w_auto_value=w_auto_value,
        w_properties=w_properties, w_inspector=w_inspector,
        w_proc_spec=w_proc_spec, immutables=immutables,
        w_guard=w_guard, w_constructor_name=w_constructor_name,
        env=env, cont=cont)

@expose("struct-accessor-procedure?", [values.W_Object])
def do_is_struct_accessor_procedure(v):
    return values.W_Bool.make(isinstance(v, values_struct.W_StructAccessor) or
                              isinstance(v, values_struct.W_StructFieldAccessor))

@expose("make-struct-field-accessor", [values_struct.W_StructAccessor,
    values.W_Fixnum, default(values.W_Object, values.w_false)])
def do_make_struct_field_accessor(accessor, field, field_name):
    if field_name is values.w_false:
        return values_struct.W_StructFieldAccessor(accessor, field.value, None)
    if not isinstance(field_name, values.W_Symbol):
        raise SchemeException("make-struct-field-accessor: expected symbol or #f as argument 2")
    return values_struct.W_StructFieldAccessor(accessor, field.value, field_name)

@expose("struct-mutator-procedure?", [values.W_Object])
def do_is_struct_mutator_procedure(v):
    return values.W_Bool.make(isinstance(v, values_struct.W_StructMutator) or
                              isinstance(v, values_struct.W_StructFieldMutator))

@expose("make-struct-field-mutator", [values_struct.W_StructMutator,
    values.W_Fixnum, default(values.W_Object, values.w_false)])
def do_make_struct_field_mutator(mutator, field, field_name):
    if field_name is values.w_false:
        return values_struct.W_StructFieldMutator(mutator, field.value, None)
    if not isinstance(field_name, values.W_Symbol):
        raise SchemeException("make-struct-field-mutator: expected symbol or #f as argument 2")
    return values_struct.W_StructFieldMutator(mutator, field.value, field_name)

@expose("struct->vector", [values_struct.W_RootStruct])
def expose_struct2vector(struct):
    return values_struct.struct2vector(struct)

@expose("prefab-struct-key", [values.W_Object])
def do_prefab_struct_key(v):
    if not (isinstance(v, values_struct.W_Struct) and v.struct_type().isprefab):
        return values.w_false
    prefab_key = values_struct.W_PrefabKey.from_struct_type(v.struct_type())
    return prefab_key.short_key()

@expose("make-prefab-struct", arity=Arity.geq(1))
def do_make_prefab_struct(args):
    key  = args[0]
    vals = args[1:]
    return values_struct.W_Struct.make_prefab(key, vals)

@expose("prefab-key->struct-type", [values.W_Object, values.W_Fixnum])
def expose_prefab_key2struct_type(w_key, field_count):
    return values_struct.W_StructType.make_prefab(
      values_struct.W_PrefabKey.from_raw_key(w_key, field_count.value))

@expose("prefab-key?", [values.W_Object])
def do_prefab_key(v):
    return values_struct.W_PrefabKey.is_prefab_key(v)

w_can_impersonate = values.W_Symbol.make("can-impersonate")

@expose("make-struct-type-property", [values.W_Symbol,
                                      default(values.W_Object, values.w_false),
                                      default(values.W_List, values.w_null),
                                      default(values.W_Object, values.w_false)])
def mk_stp(sym, guard, supers, _can_imp):
    can_imp = False
    if guard is w_can_impersonate:
        guard = values.w_false
        can_imp = True
    if _can_imp is not values.w_false:
        can_imp = True
    prop = values_struct.W_StructProperty(sym, guard, supers, can_imp)
    return values.Values.make([prop,
                               values_struct.W_StructPropertyPredicate(prop),
                               values_struct.W_StructPropertyAccessor(prop)])

# Unsafe struct ops
@expose("unsafe-struct-ref", [values.W_Object, unsafe(values.W_Fixnum)])
def unsafe_struct_ref(v, k):
    v = imp.get_base_object(v)
    assert isinstance(v, values_struct.W_Struct)
    assert 0 <= k.value <= v.struct_type().total_field_count
    return v._ref(k.value)

@expose("unsafe-struct-set!", [values.W_Object, unsafe(values.W_Fixnum),
    values.W_Object])
def unsafe_struct_set(v, k, val):
    v = imp.get_base_object(v)
    assert isinstance(v, values_struct.W_Struct)
    assert 0 <= k.value < v.struct_type().total_field_count
    return v._set(k.value, val)

@expose("unsafe-struct*-ref", [values_struct.W_Struct, unsafe(values.W_Fixnum)])
def unsafe_struct_star_ref(v, k):
    assert 0 <= k.value < v.struct_type().total_field_count
    return v._ref(k.value)

@expose("unsafe-struct*-set!", [values_struct.W_Struct, unsafe(values.W_Fixnum),
                                values.W_Object])
def unsafe_struct_star_set(v, k, val):
    assert 0 <= k.value <= v.struct_type().total_field_count
    return v._set(k.value, val)

@expose("unsafe-struct*-cas!", [values_struct.W_Struct, unsafe(values.W_Fixnum),
                                values.W_Object, values.W_Object])
def unsafe_struct_star_cas(v, k, old_val, new_val):
    assert 0 <= k.value <= v.struct_type().total_field_count
    if v._ref(k.value) is old_val:
        v._set(k.value, new_val)
        return values.w_true
    return values.w_false
