#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket import impersonators as imp
from pycket import values
from pycket import values_struct
from pycket.error import SchemeException
from pycket.prims.expose import unsafe, default, expose, procedure
from rpython.rlib        import jit

@expose("make-inspector", [default(values_struct.W_StructInspector, None)])
def do_make_instpector(inspector):
    return values_struct.W_StructInspector.make(inspector)

@expose("make-sibling-inspector", [default(values_struct.W_StructInspector, None)])
def do_make_sibling_instpector(inspector):
    return values_struct.W_StructInspector.make(inspector, True)

@expose("current-inspector")
def do_current_instpector(args):
    return values_struct.current_inspector

@expose("struct?", [values.W_Object])
def do_is_struct(v):
    return values.W_Bool.make(isinstance(v, values_struct.W_RootStruct) and
                              not v.struct_type().isopaque)

@expose("struct-info", [values.W_Object])
def do_struct_info(v):
    if (isinstance(v, values_struct.W_RootStruct) and
        v.struct_type().inspector == values_struct.current_inspector):
        struct_type = v.struct_type()
        skipped = values.w_false
    else:
        struct_type = values.w_false
        skipped = values.w_true
    return values.Values.make([struct_type, skipped])

@expose("struct-type-info", [values_struct.W_StructType])
def do_struct_type_info(struct_type):
    return values.Values.make(struct_type.struct_type_info())

@expose("struct-type-make-constructor", [values_struct.W_StructType])
def do_struct_type_make_constructor(struct_type):
    if struct_type.inspector != values_struct.current_inspector:
        # TODO: we should raise exn:fail:contract
        raise SchemeException("fail_contract")
    return struct_type.constr

@expose("struct-type-make-predicate", [values_struct.W_StructType])
def do_struct_type_make_predicate(struct_type):
    if struct_type.inspector != values_struct.current_inspector:
        # TODO: we should raise exn:fail:contract
        raise SchemeException("fail_contract")
    return struct_type.pred

@expose("make-struct-type",
        [values.W_Symbol, values.W_Object, values.W_Fixnum, values.W_Fixnum,
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_null),
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_null),
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_false)], simple=False)
def do_make_struct_type(name, super_type, init_field_cnt, auto_field_cnt,
        auto_v, props, inspector, proc_spec, immutables, guard, constr_name, env, cont):
    if not (isinstance(super_type, values_struct.W_StructType) or super_type is values.w_false):
        raise SchemeException("make-struct-type: expected a struct-type? or #f")
    return values_struct.W_StructType.make(name, super_type, init_field_cnt,
        auto_field_cnt, auto_v, props, inspector, proc_spec, immutables,
        guard, constr_name, env, cont)

@expose("struct-accessor-procedure?", [values.W_Object])
def do_is_struct_accessor_procedure(v):
    return values.W_Bool.make(isinstance(v, values_struct.W_StructAccessor) or
        isinstance(v, values_struct.W_StructFieldAccessor))

@expose("make-struct-field-accessor", [values_struct.W_StructAccessor,
    values.W_Fixnum, default(values.W_Symbol, None)])
def do_make_struct_field_accessor(accessor, field, field_name):
    return values_struct.W_StructFieldAccessor(accessor, field, field_name)

@expose("struct-mutator-procedure?", [values.W_Object])
def do_is_struct_mutator_procedure(v):
    return values.W_Bool.make(isinstance(v, values_struct.W_StructMutator) or
        isinstance(v, values_struct.W_StructFieldMutator))

@expose("make-struct-field-mutator", [values_struct.W_StructMutator,
    values.W_Fixnum, default(values.W_Symbol, None)])
def do_make_struct_field_mutator(mutator, field, field_name):
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
    assert 0 <= k.value <= v.struct_type().total_field_cnt
    return v._ref(k.value)

@expose("unsafe-struct-set!", [values.W_Object, unsafe(values.W_Fixnum),
    values.W_Object])
def unsafe_struct_set(v, k, val):
    while isinstance(v, imp.W_ChpStruct) or isinstance(v, imp.W_ImpStruct):
        v = v.inner
    assert isinstance(v, values_struct.W_Struct)
    assert 0 <= k.value < v.struct_type().total_field_cnt
    return v._set(k.value, val)

@expose("unsafe-struct*-ref", [values_struct.W_Struct, unsafe(values.W_Fixnum)])
def unsafe_struct_star_ref(v, k):
    assert 0 <= k.value < v.struct_type().total_field_cnt
    return v._ref(k.value)

@expose("unsafe-struct*-set!", [values_struct.W_Struct, unsafe(values.W_Fixnum),
    values.W_Object])
def unsafe_struct_star_set(v, k, val):
    assert 0 <= k.value <= v.struct_type().total_field_cnt
    return v._set(k.value, val)
