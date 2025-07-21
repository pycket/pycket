#! /usr/bin/env python
# -*- coding: utf-8 -*-

from rpython.rtyper.lltypesystem    import rffi, lltype
from rpython.rtyper                 import rclass
from pycket                         import values
from rpython.rlib                   import jit
from pycket.error                   import SchemeException

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

    errorname = "<cpointer>"

    def __init__(self):
        pass

def make_w_pointer_class(str_name):

    class _W_Custom(W_CPointer):
        _immutable_fields_ = _attrs_ = ["ptr"]

        errorname = "<cpointer %s>" % str_name

        def __init__(self, ptr):
            self.ptr = ptr

        def to_rffi(self):
            return self.ptr

        def as_voidp(self):
            return rffi.cast(rffi.VOIDP, self.to_rffi())

        # TODO: we don't need this method:
        # change the input type of rktio_identity_to_vector
        # from W_CPointer to W_R_PTR (void*) and then cast it
        # in there to the RKTIO_IDENTITY_PTR
        def as_rktio_identity_ptr(self):
            from pycket.rktio import bootstrap_structs
            return rffi.cast(bootstrap_structs.RKTIO_IDENTITY_PTR, self.to_rffi())

    _W_Custom.__name__ = "W_%s" % str_name
    return _W_Custom

class W_FFILib(values.W_Object):
    errorname = "ffi-lib"
    def __init__(self):
        pass

# Immobile Cells

# OBJECTPTR is like pointer to any W_Object
OBJ_PTR  = rclass.OBJECTPTR
OBJ_PTR_PTR = lltype.Ptr(lltype.Array(OBJ_PTR, hints={'nolength': True,
                                                      'render_as_void': True}))
CELL = lltype.GcStruct('immobile_cell', ('val', OBJ_PTR))
CELL_PTR = lltype.Ptr(CELL)

W_ImmobileCellPointer = make_w_pointer_class("immobile-cell")

@jit.dont_look_inside
def cell_to_voidp(cell_ptr):
    # GC -> raw
    return rffi.cast(rffi.VOIDP, cell_ptr)

@jit.dont_look_inside
def voidp_to_int(voidp):
    # raw -> int
    return rffi.cast(lltype.Unsigned, voidp)

@jit.dont_look_inside
def int_to_voidp(addr_int):
    # int -> raw
    return rffi.cast(rffi.VOIDP, addr_int)

NULL_VOIDP = rffi.cast(rffi.VOIDP, 0)

def voidp_from_any(ptr_obj, who):
    if ptr_obj is values.w_false:
        return NULL_VOIDP
    if isinstance(ptr_obj, W_ImmobileCellPointer):
        return ptr_obj.as_voidp()
    raise SchemeException("%s: expected cpointer immobile-cell, got %s"
                          % (who, ptr_obj.tostring()))
