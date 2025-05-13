#! /usr/bin/env python
# -*- coding: utf-8 -*-

import struct
from pycket       import values
from rpython.rlib import jit

class W_CType(values.W_Object):

    errorname = "ctype"
    _attrs_ = []

    def __init__(self):
        raise NotImplementedError("abstract base class")

    def basetype(self):
        raise NotImplementedError("abstract base class")

    def scheme_to_c(self):
        return values.w_false

    def c_to_scheme(self):
        return values.w_false

    def sizeof(self):
        raise NotImplementedError("abstract base class")

class W_PrimitiveCType(W_CType):

    _immutable_fields_ = ["name", "size", "alignment"]

    def __init__(self, name, size, alignment):
        assert isinstance(name, values.W_Symbol)
        self.name      = name
        self.size      = size
        self.alignment = alignment

    def sizeof(self):
        return self.size

    def alignof(self):
        return self.alignment

    def basetype(self):
        return self.name

    def tostring(self):
        return "#<ctype:%s>" % self.name.utf8value

class W_DerivedCType(W_CType):

    _immutable_fields_ = ["ctype", "racket_to_c", "c_to_racket"]

    def __init__(self, ctype, racket_to_c, c_to_racket):
        assert isinstance(ctype, W_CType)
        self.ctype       = ctype
        self.racket_to_c = racket_to_c
        self.c_to_racket = c_to_racket

    def sizeof(self):
        return self.ctype.sizeof()

    def alignof(self):
        return self.ctype.alignof()

    def has_conversions(self):
        return (self.racket_to_c is not values.w_false or
                self.c_to_racket is not values.w_false)

    def basetype(self):
        if self.has_conversions():
            return self.ctype
        return self.ctype.basetype()

    def scheme_to_c(self):
        return self.racket_to_c

    def c_to_scheme(self):
        return self.c_to_racket

    def tostring(self):
        if self.has_conversions():
            return "#<ctype>"
        return "#<ctype:%s>" % self.ctype.tostring()

class W_CStructType(W_CType):

    _immutable_fields_ = ["types[*]", "abi", "alignment"]

    def __init__(self, types, abi, alignment):
        self.types     = types
        self.abi       = abi
        self.alignment = alignment

    def sizeof(self):
        size = 0
        for type in self.types:
            size += type.sizeof()
        return size

    def alignof(self):
        alignment = 0
        for type in self.types:
            alignment = max(type.alignof(), alignment)
        return alignment

class W_CPointer(values.W_Object):
    _attrs_ = []

    errorname = "cpointer"

    def __init__(self):
        pass

def make_w_pointer_class(str_name):

    class _W_Custom(W_CPointer):
        _immutable_fields_ = _attrs_ = ["ptr"]

        def __init__(self, ptr):
            self.ptr = ptr

        def to_rffi(self):
            return self.ptr

    _W_Custom.__name__ = "W_%s" % str_name
    return _W_Custom

class W_FFILib(values.W_Object):
    errorname = "ffi-lib"
    def __init__(self):
        pass
