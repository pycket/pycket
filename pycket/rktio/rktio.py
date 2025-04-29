import os

from pycket import values, values_string
from pycket import vector as values_vector

from pycket.foreign import W_CStructType, W_CPointer
from pycket.prims import foreign as prim_foreign
from pycket.prims.expose import expose, expose_val

from rpython.translator.tool.cbuild import ExternalCompilationInfo
from rpython.rtyper.lltypesystem import rffi, lltype
from pycket.rktio.types import *
from pycket.rktio._rktio_bootstrap import *

# Load the librktio.a
# TODO: make this absolute (pycket/rktio), instead of "this file"
RKTIO_DIR = os.path.dirname(os.path.abspath(__file__))
librktio_a = ExternalCompilationInfo(
    includes=['rktio.h'],
    include_dirs=[RKTIO_DIR],
    libraries=['rktio'],
    library_dirs=[RKTIO_DIR],
)



FUNCTIONS = [
    ("rktio_init",  [], RKTIO_T_PTR),
    ("rktio_delete_file",   [RKTIO_T_PTR, RKTIO_CONST_STRING_T, RKTIO_BOOL_T],  RKTIO_OK_T),
]

TYPES_RFFI_TO_PYCKET = {
        INT:                    values.W_Fixnum,
        UNSIGNED:               values.W_Fixnum,
        CHAR:                   values.W_Character,
        UINTPTR_T:              values.W_Fixnum,

        RKTIO_OK_T:             values.W_Fixnum,
        RKTIO_TRI_T:            values.W_Fixnum,
        RKTIO_BOOL_T:           values.W_Fixnum,
        RKTIO_CHAR16_T:         values.W_Fixnum,
        RKTIO_CONST_STRING_T:   values_string.W_String,
        RKTIO_FILESIZE_T:       values.W_Fixnum,
}


def define_function(name):
    def _inner():
        print("I am function %s!" % name)

    # Assign to global scope with new name
    _inner.__name__ = name
    globals()[name] = _inner

# Example usage
define_function("c_rktio_free")

# Now greet() is callable globally


########### rktio_init ########################
###############################################
###############################################


c_rktio_init = rffi.llexternal('rktio_init',
                               [],
                               RKTIO_T_PTR,
                               compilation_info=librktio_a)


@expose("rktio_init", [])
def rktio_init():
    rktio_ptr = c_rktio_init()
    # return W_RKTIO_Ptr(rffi.cast(rffi.VOIDP, rktio_ptr))
    return W_RKTIO_T_PTR(rktio_ptr)

########### rktio_delete_file #################
###############################################
###############################################

c_rktio_delete_file = rffi.llexternal('rktio_delete_file',
                                      [RKTIO_T_PTR, RKTIO_CONST_STRING_T, RKTIO_BOOL_T],
                                      RKTIO_OK_T,
                                      compilation_info=librktio_a)

@expose("rktio_delete_file",
        [W_RKTIO_T_PTR, values_string.W_String])
def rktio_delete_file(w_rktio_t, w_path):
    _p_str = w_path.as_str_utf8()
    p_str = _p_str if  _p_str else ""
    c_path = rffi.str2charp(p_str)
    c_flag = rffi.cast(rffi.INT, 1) # don't care about value
    # c_ptr = w_rktio_t.cast_to_rffi()
    c_ptr = w_rktio_t.to_rffi()

    try:
        res = c_rktio_delete_file(c_ptr, c_path, c_flag)
        return values.W_Fixnum(res)
    finally:
        rffi.free_charp(c_path)


###############################################
###############################################
###############################################

caner_function_lib = ExternalCompilationInfo(
    includes=['function.h'],
    include_dirs=['/home/caner/pycket'],
    libraries=['function'],
    library_dirs=['/home/caner/pycket'],
)

caner_function = rffi.llexternal(
    "add",
    [lltype.Signed, lltype.Signed], # arguments
    lltype.Signed, # return type
    compilation_info=caner_function_lib,
)

@expose("caner-add", [values.W_Fixnum, values.W_Fixnum])
def caner_add(a, b):

    res = caner_function(a.value, b.value)
    return values.W_Fixnum(res)


###############################################

_double = prim_foreign.CTYPES['_double']
_int32 = prim_foreign.CTYPES['_int32']

# Describe foo_t
w_foo_ctype = W_CStructType([_int32, _double], abi=values.w_false, alignment=8)
FOO_T = lltype.Struct('foo_t', ('x', rffi.INT), ('y', rffi.DOUBLE))
FOO_T_PTR = lltype.Ptr(FOO_T)

# Racket value to point to foo_t
class W_FooCPtr(W_CPointer):
    _immutable_fields_ = ['addr', 'ctype']
    def __init__(self, addr):
        self.addr = addr
        self.ctype = w_foo_ctype

    def tostring(self):
        return "#<cpointer:foo_t>"

c_sumfoo = rffi.llexternal('sum_foo',
                           [FOO_T_PTR], # input type
                           rffi.DOUBLE,
                           compilation_info=caner_function_lib)

@expose("make-foo", [values.W_Fixnum, values.W_Fixnum])
def make_foo(w_x, w_y):
    foo = lltype.malloc(FOO_T, flavor='raw')
    foo.x = rffi.cast(rffi.INT, w_x.value)
    foo.y = rffi.cast(rffi.DOUBLE, w_y.value)
    return W_FooCPtr(rffi.cast(rffi.VOIDP, foo))

c_makefoo = rffi.llexternal('make_foo',
                            [rffi.INT, rffi.DOUBLE],
                            FOO_T_PTR,
                            compilation_info=caner_function_lib)

@expose("make-foo*", [values.W_Fixnum, values.W_Fixnum])
def make_foo_c(w_x, w_y):
    _foo_x = rffi.cast(rffi.INT, w_x.value)
    _foo_y = rffi.cast(rffi.DOUBLE, w_y.value)
    foo_t_ptr = c_makefoo(_foo_x, _foo_y) # returns FOO_T_PTR
    return W_FooCPtr(rffi.cast(rffi.VOIDP, foo_t_ptr))

@expose("sum-foo*", [W_FooCPtr])
def call_sumfoo(w_ptr):
    res = c_sumfoo(rffi.cast(FOO_T_PTR, w_ptr.addr))
    return values.W_Flonum(res)

@expose("free-foo", [W_FooCPtr])
def free_foo(w_ptr):
    lltype.free(rffi.cast(FOO_T_PTR, w_ptr.addr), flavor='raw')
    return values.w_void

###############################################

