from pycket.values import w_false
from pycket.foreign import make_w_pointer_class
from rpython.rtyper.lltypesystem import rffi, lltype

###############################################
######## Base Types and Pointers ##############
###############################################


INT                     = rffi.INT
UNSIGNED                = rffi.UNSIGNED
UNSIGNED_8              = rffi.UINT
CHAR                    = rffi.CHAR
DOUBLE                  = rffi.DOUBLE
FLOAT                   = rffi.FLOAT
CCHARP			= rffi.CCHARP
CCHARPP			= rffi.CCHARPP
ARR_PTR			= rffi.CArrayPtr
VOID                    = rffi.VOIDP
NULL                    = w_false
W_FALSE                 = w_false
RKTIO_OK_T              = rffi.INT
RKTIO_TRI_T             = rffi.INT
RKTIO_BOOL_T            = rffi.INT
RKTIO_CHAR16_T          = rffi.USHORT
RKTIO_CONST_STRING_T    = rffi.CCHARP
RKTIO_INT64_T           = rffi.LONGLONG
RKTIO_FILESIZE_T        = RKTIO_INT64_T
INTPTR_T                = rffi.SSIZE_T # pointer-sized signed int
# integer large enough to hold any native pointer
UINTPTR_T               = rffi.SIZE_T # unsigned
RKTIO_TIMESTAMP_T       = INTPTR_T

def bootstrap_struct_type(name, type_tuple):
    from rpython.rtyper.lltypesystem import rffi
    from pycket.foreign import make_w_pointer_class

    """
        Input is a key-value pair from the STRUCT_POINTERS map
        in _rktio_bootstrap.
        type_tuple has (rffi_type, w_pycket_type)
    """
    globals()[type_tuple[0]] = rffi.COpaquePtr(name)
    globals()[type_tuple[1]] = make_w_pointer_class(name)

def define_constant(name, val):
    globals()[name] = val

"""
ref: opaque to Racket: callee (C) allocates, and caller must
eventually free it (rktio_free, free, etc).

*ref: transparent to Racket: caller provides the bytes, callee
never tries to free them.
"""
def ptr_of(base):
    return rffi.CArrayPtr(base)


"""
(array 5 unsigned) ---> rffi.CFixedArray(UINT, 5)
(array 64 unsigned-8) -> rffi.CFixedArray(UINT8, 64)
"""
def array_of(base, size):
    return rffi.CFixedArray(base, size)

# Types needed for the connector layer
RKTIO_FILESIZE_PTR      = ptr_of(RKTIO_FILESIZE_T)
W_RKTIO_FILESIZE_PTR    = make_w_pointer_class("rktio_filesize_t")

RKTIO_TIMESTAMP_PTR     = ptr_of(RKTIO_TIMESTAMP_T)
W_RKTIO_TIMESTAMP_PTR   = make_w_pointer_class("rktio_timestamp_t")


