#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket import impersonators as imp
from pycket import values
from pycket import values_struct
from pycket.error import SchemeException
from pycket.prims.expose import unsafe, default, expose

@expose("make-inspector", [default(values_struct.W_StructInspector,
    values_struct.current_inspector)])
def do_make_instpector(inspector):
    return values_struct.W_StructInspector.make(inspector)

@expose("make-sibling-inspector", [default(values_struct.W_StructInspector,
    values_struct.current_inspector)])
def do_make_sibling_instpector(inspector):
    return values_struct.W_StructInspector.make(inspector, True)

@expose("current-inspector")
def do_current_instpector(args):
    return values_struct.current_inspector

@expose("struct?", [values.W_Object])
def do_is_struct(v):
    return values.W_Bool.make(isinstance(v, values_struct.W_RootStruct) and
        values_struct.current_inspector.has_control(v.struct_type()))

@expose("struct-info", [values.W_Object], simple=False)
def do_struct_info(v, env, cont):
    from pycket.interpreter import return_multi_vals
    if (isinstance(v, values_struct.W_RootStruct) and
        values_struct.current_inspector.has_control(v.struct_type())):
        return v.get_struct_info(env, cont)
    return return_multi_vals(
            values.Values.make([values.w_false, values.w_true]), env, cont)

struct_info = do_struct_info.w_prim

@expose("struct-type-info", [values_struct.W_StructType])
def do_struct_type_info(struct_type):
    return values.Values.make(struct_type.struct_type_info())

@expose("struct-type-make-constructor", [values_struct.W_StructType])
def do_struct_type_make_constructor(struct_type):
    if struct_type.w_inspector is not values_struct.current_inspector:
        # TODO: we should raise exn:fail:contract
        raise SchemeException("fail_contract")
    return struct_type.constructor

@expose("struct-type-make-predicate", [values_struct.W_StructType])
def do_struct_type_make_predicate(struct_type):
    if struct_type.w_inspector is not values_struct.current_inspector:
        # TODO: we should raise exn:fail:contract
        raise SchemeException("fail_contract")
    return struct_type.predicate

@expose("make-struct-type",
        [values.W_Symbol, values.W_Object, values.W_Fixnum, values.W_Fixnum,
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_null),
         default(values.W_Object, values_struct.current_inspector),
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_null),
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_false)], simple=False)
def do_make_struct_type(w_name, w_super_type, w_init_field_count,
                        w_auto_field_count, w_auto_value, w_properties, w_inspector,
                        w_proc_spec, w_immutables, w_guard, w_constructor_name,
                        env, cont):
    if not (isinstance(w_super_type, values_struct.W_StructType) or
            w_super_type is values.w_false):
        raise SchemeException("make-struct-type: expected a struct-type? or #f")
    return values_struct.W_StructType.make(w_name=w_name,
        w_super_type=w_super_type, w_init_field_count=w_init_field_count,
        w_auto_field_count=w_auto_field_count, w_auto_value=w_auto_value,
        w_properties=w_properties, w_inspector=w_inspector,
        w_proc_spec=w_proc_spec, w_immutables=w_immutables,
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
        return values_struct.W_StructFieldAccessor(accessor, field, None)
    if not isinstance(field_name, values.W_Symbol):
        raise SchemeException("make-struct-field-accessor: expected symbol or #f as argument 2")
    return values_struct.W_StructFieldAccessor(accessor, field, field_name)

@expose("struct-mutator-procedure?", [values.W_Object])
def do_is_struct_mutator_procedure(v):
    return values.W_Bool.make(isinstance(v, values_struct.W_StructMutator) or
        isinstance(v, values_struct.W_StructFieldMutator))

@expose("make-struct-field-mutator", [values_struct.W_StructMutator,
    values.W_Fixnum, default(values.W_Object, values.w_false)])
def do_make_struct_field_mutator(mutator, field, field_name):
    if field_name is values.w_false:
        return values_struct.W_StructFieldMutator(mutator, field, None)
    if not isinstance(field_name, values.W_Symbol):
        raise SchemeException("make-struct-field-mutator: expected symbol or #f as argument 2")
    return values_struct.W_StructFieldMutator(mutator, field, field_name)

@expose("struct->vector", [values_struct.W_RootStruct])
def expose_struct2vector(struct):
    return values_struct.struct2vector(struct)

@expose("prefab-struct-key", [values.W_Object])
def do_prefab_struct_key(v):
    if not (isinstance(v, values_struct.W_Struct) and v.struct_type().isprefab):
        return values.w_false
    prefab_key = values_struct.W_PrefabKey.from_struct_type(v.struct_type())
    return prefab_key.short_key()

@expose("make-prefab-struct")
def do_make_prefab_struct(args):
    assert len(args) > 1
    key = args[0]
    vals = args[1:]
    return values_struct.W_Struct.make_prefab(key, vals)

@expose("prefab-key->struct-type", [values.W_Object, values.W_Fixnum])
def expose_prefab_key2struct_type(w_key, field_count):
    return values_struct.W_StructType.make_prefab(
      values_struct.W_PrefabKey.from_raw_key(w_key, field_count.value))

@expose("prefab-key?", [values.W_Object])
def do_prefab_key(v):
    return values_struct.W_PrefabKey.is_prefab_key(v)

@expose("make-struct-type-property", [values.W_Symbol,
                                      default(values.W_Object, values.w_false),
                                      default(values.W_List, values.w_null),
                                      default(values.W_Object, values.w_false)])
def mk_stp(sym, guard, supers, _can_imp):
    can_imp = False
    if guard is values.W_Symbol.make("can-impersonate"):
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
    while isinstance(v, imp.W_ChpStruct) or isinstance(v, imp.W_ImpStruct):
        v = v.inner
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
