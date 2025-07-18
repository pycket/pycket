#! /usr/bin/env python
# -*- coding: utf-8 -*-

from operator import add
import sys

from rpython.rtyper.lltypesystem.lloperation import void

from pycket                         import values
from pycket.error                   import SchemeException, FSException, ContractException, ArityException
from pycket.foreign                 import *
from pycket.prims.expose            import default, expose, expose_val, procedure
from pycket.rktio                   import types

from rpython.rlib                   import jit, unroll, rgc
from rpython.rtyper.lltypesystem    import rffi
from rpython.rtyper.annlowlevel     import cast_instance_to_base_ptr, cast_base_ptr_to_instance

POINTER_SIZE = rffi.sizeof(rffi.VOIDP)

PRIMITIVE_CTYPES = [
    ("bool"          , 4           , 4 ) ,
    ("stdbool"       , 1           , 1 ) ,
    ("int8"          , 1           , 1 ) ,
    ("int16"         , 2           , 2 ) ,
    ("int32"         , 4           , 4 ) ,
    ("int64"         , 8           , 8 ) ,
    ("uint8"         , 1           , 1 ) ,
    ("uint16"        , 2           , 2 ) ,
    ("uint32"        , 4           , 4 ) ,
    ("uint64"        , 8           , 8 ) ,
    ("float"         , 4           , 4 ) ,
    ("double"        , 8           , 8 ) ,
    ("double*"       , 8           , 8 ) ,
    ("longdouble"    , 8           , 8 ) , # FIXME ?
    ("void"          , 0           , 1 ) ,
    ("fixint"        , POINTER_SIZE, POINTER_SIZE),
    ("ufixint"       , POINTER_SIZE, POINTER_SIZE),
    ("fixnum"        , POINTER_SIZE, POINTER_SIZE),
    ("ufixnum"       , POINTER_SIZE, POINTER_SIZE),
    ("bytes"         , POINTER_SIZE, POINTER_SIZE) ,
    ("path"          , POINTER_SIZE, POINTER_SIZE) ,
    ("pointer"       , POINTER_SIZE, POINTER_SIZE) ,
    ("fpointer"      , POINTER_SIZE, POINTER_SIZE) ,
    ("string/utf-16" , POINTER_SIZE, POINTER_SIZE) ,
    ("string/ucs-4"  , POINTER_SIZE, POINTER_SIZE) ,
    ("gcpointer"     , POINTER_SIZE, POINTER_SIZE) ,
    ("or-null"       , POINTER_SIZE, POINTER_SIZE) ,
    ("gcable"        , POINTER_SIZE, POINTER_SIZE) ,
    ("symbol"        , POINTER_SIZE, POINTER_SIZE) ,
    ("scheme"        , POINTER_SIZE, POINTER_SIZE  , "racket") ,
    ]

sym = values.W_Symbol.make

COMPILER_SIZEOF = unroll.unrolling_iterable([
    (sym("int"    ) , rffi.sizeof(rffi.INT)    ),
    (sym("char"   ) , rffi.sizeof(rffi.CHAR)   ),
    (sym("short"  ) , rffi.sizeof(rffi.SHORT)  ),
    (sym("long"   ) , rffi.sizeof(rffi.LONG)   ),
    (sym("*"      ) , rffi.sizeof(rffi.VOIDP)  ),
    (sym("void"   ) , 0                        ),
    (sym("float"  ) , rffi.sizeof(rffi.FLOAT)  ),
    (sym("double" ) , rffi.sizeof(rffi.DOUBLE) ),
    (sym("wchar"  ) , rffi.sizeof(rffi.WCHAR_T))
    ])

CTYPES = {}

def expose_ctype(name, size, alignment, *extra_names):
    basetype = sym(name)
    ctype    = W_PrimitiveCType(basetype, size, alignment)

    exposed_name_str = "_" + name
    CTYPES[exposed_name_str] = ctype

    expose_val(exposed_name_str, ctype, check_alr_defined=False)
    for name in extra_names:
        expose_val(exposed_name_str, ctype, check_alr_defined=False)

for spec in PRIMITIVE_CTYPES:
    expose_ctype(*spec)

del sym

@expose("make-ctype", [W_CType, values.W_Object, values.W_Object])
def make_c_type(ctype, rtc, ctr):
    if rtc is not values.w_false and not rtc.iscallable():
        raise SchemeException("make-ctype: expected (or/c #f procedure) in argument 1 got %s" %
                              rtc.tostring())
    if ctr is not values.w_false and not ctr.iscallable():
        raise SchemeException("make-ctype: expected (or/c #f procedure) in argument 2 got %s" %
                              ctr.tostring())
    return W_DerivedCType(ctype, rtc, ctr)

def validate_alignment(ctx, arg, align):
    if align is values.w_false:
        return -1
    if isinstance(align, values.W_Fixnum) and align.value in (1, 2, 4, 8, 16):
        return align.value
    msg = ("%s: expected (or/c #f 1 2 4 8 16) in argument %d got %s" %
           (ctx, arg, align.tostring()))
    raise SchemeException(msg)

@expose("make-cstruct-type",
        [values.W_List,
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_false)])
def make_cstruct_type(types, abi, _alignment):
    alignment = validate_alignment("make-cstruct-type", 2, _alignment)

    if types.is_proper_list():
        types_list = []
        for ctype in values.from_list_iter(types):
            if not isinstance(ctype, W_CType):
                break
            types_list.append(ctype)
        else:
            return W_CStructType(types_list[:], abi, alignment)

    msg = "make-cstruct-type: expected (listof ctype?) in argument 0 got %s" % types.tostring()
    raise SchemeException(msg)

@jit.elidable
def _compiler_sizeof(ctype):
    if ctype.is_proper_list():
        acc = 0
        for type in values.from_list_iter(ctype):
            if not isinstance(type, values.W_Symbol):
                break
            size = _compiler_sizeof(type)
            acc = max(size, acc)
        else:
            return acc

    if not isinstance(ctype, values.W_Symbol):
        msg = ("compiler-sizeof: expected (or/c symbol? (listof symbol?)) in argument 0 got %s" %
               ctype.tostring())
        raise SchemeException(msg)

    for sym, size in COMPILER_SIZEOF:
        if ctype is sym:
            return size
    raise SchemeException("compiler-sizeof: %s is not a valid C type" % ctype.tostring())

@expose("compiler-sizeof", [values.W_Object])
def compiler_sizeof(obj):
    return values.W_Fixnum(_compiler_sizeof(obj))

@expose("make-stubborn-will-executor", [])
def make_stub_will_executor():
    return values.w_false

@expose("ctype-sizeof", [W_CType])
def ctype_sizeof(ctype):
    return values.W_Fixnum(ctype.sizeof())

@expose("ctype-alignof", [W_CType])
def ctype_alignof(ctype):
    return values.W_Fixnum(ctype.alignof())

@expose("ffi-lib?", [values.W_Object])
def ffi_lib(o):
    return values.W_Bool.make(isinstance(o, W_FFILib))

@expose("ffi-lib", [values.W_Object, default(values.W_Object, values.w_false), default(values.W_Object, values.w_false)])
def ffi_lib(name, fail_as_false, as_global):
    if name is values.w_false:
        return W_FFILib()
    if fail_as_false is values.w_false:
        raise FSException("ffi-lib not supported on Pycket")
    else:
        return values.w_false

@expose("malloc")
def malloc(args):
    return W_CPointer()

@expose("ptr-ref", [W_CPointer, W_CType, default(values.W_Fixnum, values.W_Fixnum.ZERO)])
def ptr_ref(cptr, ctype, offset):
    return values.w_false

@expose("ptr-set!")
def ptr_set(args):
    return values.w_void

@expose("cpointer-gcable?")
def cp_gcable(args):
    return values.w_false

@expose("ffi-obj", [values.W_Bytes, values.W_Object])
def ffi_obj(name, lib):
    raise FSException("unable to get symbol %s from %s"%(name.tostring(), lib.tostring()))

@expose("ctype-basetype", [values.W_Object])
def ctype_basetype(ctype):
    if ctype is values.w_false:
        return values.w_false
    if not isinstance(ctype, W_CType):
        msg = ("ctype-basetype: expected (or/c ctype? #f) in argument 0 got %s" %
               ctype.tostring())
        raise SchemeException(msg)
    return ctype.basetype()

@expose("ctype-scheme->c", [W_CType])
def ctype_scheme_to_c(ctype):
    return ctype.scheme_to_c()

@expose("ctype-c->scheme", [W_CType])
def ctype_c_to_scheme(ctype):
    return ctype.c_to_scheme()

# See immobile cells in pycket/foreign

_roots_for_GC = {} # int: GC

@expose("malloc-immobile-cell", [values.W_Object])
def malloc_immobile_cell(w_val):
    cell       = lltype.malloc(CELL, zero=True)
    cell.val   = cast_instance_to_base_ptr(w_val)
    rgc.pin(cell)
    addr_int = voidp_to_int(cell_to_voidp(cell))
    _roots_for_GC[addr_int] = cell
    return W_ImmobileCellPointer(int_to_voidp(addr_int))

@jit.dont_look_inside
def deref_foreign_cell(voidp):
    """Return (OBJ_PTR *)voidp[0] as a live W_Object instance."""
    ptrptr = rffi.cast(OBJ_PTR_PTR, voidp)
    return cast_base_ptr_to_instance(values.W_Object, ptrptr[0])

@jit.dont_look_inside
def set_foreign_cell(voidp, w_val):
    ptrptr = rffi.cast(OBJ_PTR_PTR, voidp)
    ptrptr[0] = cast_instance_to_base_ptr(w_val)


@expose("immobile-cell-ref", [values.W_Object])
def immobile_cell_ref(w_arg):
    voidp = voidp_from_any(w_arg, "immobile-cell-ref")
    if not voidp:
        return values.w_false

    addr_key = voidp_to_int(voidp)
    if addr_key in _roots_for_GC:
        cell_ptr = _roots_for_GC[addr_key]
        return cast_base_ptr_to_instance(values.W_Object, cell_ptr.val)
    # foreign cell
    return deref_foreign_cell(w_arg.as_voidp())

@expose("immobile-cell-set!", [values.W_Object, values.W_Object])
def immobile_cell_set(w_arg, w_new):
    voidp = voidp_from_any(w_arg, "immobile-cell-set!")
    if not voidp:
        return values.w_false

    addr_key = voidp_to_int(voidp)
    if addr_key in _roots_for_GC:
        cell_ptr = _roots_for_GC[addr_key]
        if cell_ptr:
            cell_ptr.val = cast_instance_to_base_ptr(w_new)
            return values.w_void
    set_foreign_cell(w_arg.as_voidp(), w_new)

@expose("free-immobile-cell", [values.W_Object])
def free_immobile_cell(w_arg):
    voidp = voidp_from_any(w_arg, "free-immobile-cell")
    if not voidp:
        return values.w_false

    addr_int = voidp_to_int(voidp)
    if addr_int not in _roots_for_GC:
        raise SchemeException("free-immobile-cell: address already freed")
    cell     = _roots_for_GC.pop(addr_int)
    rgc.unpin(cell)
    cell.val = lltype.nullptr(OBJ_PTR.TO)

@expose("address->immobile-cell", [types.W_R_PTR])
def address_to_immobile_cell(w_ptr):
    voidp = w_ptr.as_voidp()
    if not voidp:
        return values.w_false

    return W_ImmobileCellPointer(voidp)

@expose("immobile-cell->address", [W_ImmobileCellPointer])
def immobile_cell_to_address(w_icptr):
    return types.W_R_PTR(w_icptr.as_voidp())


