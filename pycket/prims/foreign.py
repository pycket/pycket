#! /usr/bin/env python
# -*- coding: utf-8 -*-

# XXX: This whole module is wrong

from pycket              import values
from pycket.prims.expose import default, expose, expose_val, procedure

expose_val("_int8"   , values.W_Fixnum.make(0))
expose_val("_int16"  , values.W_Fixnum.make(1))
expose_val("_int32"  , values.W_Fixnum.make(2))
expose_val("_int64"  , values.W_Fixnum.make(3))
expose_val("_uint8"  , values.W_Fixnum.make(4))
expose_val("_uint16" , values.W_Fixnum.make(5))
expose_val("_uint32" , values.W_Fixnum.make(6))
expose_val("_uint64" , values.W_Fixnum.make(7))
expose_val("_bytes"  , values.W_Fixnum.make(8))
expose_val("_path"   , values.W_Fixnum.make(9))
expose_val("_void"   , values.W_Fixnum.make(10))
expose_val("_pointer"  , values.W_Fixnum.make(11))
expose_val("_fpointer" , values.W_Fixnum.make(12))

ctype = values.W_Fixnum

@expose("make-ctype", [ctype, default(values.W_Object, values.w_false), default(values.W_Object, values.w_false)])
def make_c_type(typ, rtc, ctr):
    return values.W_Fixnum(0)

@expose("compiler-sizeof", [values.W_Object])
def compiler_sizeof(sym):
    return values.W_Fixnum(8)

@expose("make-stubborn-will-executor", [])
def make_stub_will_executor():
    return values.w_false

