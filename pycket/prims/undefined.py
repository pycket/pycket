#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket.prims.expose import expose, expose_val, make_call_method, make_procedure
from pycket.error import SchemeException
from pycket import impersonators as imp
from pycket import values
from pycket import values_struct

expose_val("unsafe-undefined", values.w_unsafe_undefined)

@expose("check-not-unsafe-undefined", [values.W_Object, values.W_Symbol])
def cnuu(obj, sym):
    if obj is values.w_unsafe_undefined:
        raise SchemeException("%s: undefined; use before initialization" % sym.value)
    return obj

@expose("check-not-unsafe-undefined/assign", [values.W_Object, values.W_Symbol])
def cnuua(obj, sym):
    if obj is values.w_unsafe_undefined:
        raise SchemeException("%s: undefined; assignment before initialization" % sym.value)
    return obj

def make_accessor(acc, i):
    idx = values.W_Fixnum(i)
    sym = values.W_Symbol.make("<internal-accessor>")
    return values_struct.W_StructFieldAccessor(acc, idx, sym)

def make_mutator(mut, i):
    idx = values.W_Fixnum(i)
    sym = values.W_Symbol.make("<internal-mutator>")
    return values_struct.W_StructFieldMutator(mut, idx, sym)

# Accessor handler for chaperone-struct-unsafe-undefined
@make_procedure("<accessor-handler>", [values.W_Object, values.W_Object])
def accessor_handler(struct, val):
    if val is values.w_unsafe_undefined:
        raise SchemeException("chaperone-struct-unsafe-undefined: produced undefined")
    return val

# Mutator handler for chaperone-struct-unsafe-undefined
@make_procedure("<mutator-handler>", [values.W_Object, values.W_Object])
def mutator_handler(struct, val):
    if val is values.w_unsafe_undefined:
        raise SchemeException("chaperone-struct-unsafe-undefined: produced undefined")
    return val

@expose("chaperone-struct-unsafe-undefined", [values.W_Object])
def csuu(obj):
    if not isinstance(obj, values_struct.W_RootStruct):
        return obj
    typ    = obj.struct_type()
    fields = typ.total_field_cnt
    acc    = [make_accessor(typ.acc, i) for i in range(fields)]
    mut    = [make_mutator(typ.mut, i) for i in range(fields)]
    h1     = [accessor_handler] * fields
    h2     = [mutator_handler] * fields
    return imp.W_ChpStruct(obj, acc + mut, h1 + h2, [], [])

