
from pycket			    import values
from pycket.foreign		    import make_w_pointer_class, W_CPointer
from pycket.error		    import SchemeException

from rpython.rtyper.lltypesystem    import rffi, lltype

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
VOIDP			= rffi.VOIDP
ARR_PTR			= rffi.CArrayPtr
VOID                    = lltype.Void
NULL                    = rffi.NULL
W_FALSE                 = 0
RKTIO_OK_T              = rffi.INT
RKTIO_TRI_T             = rffi.INT
RKTIO_BOOL_T            = rffi.INT
RKTIO_CHAR16_T          = rffi.INT
RKTIO_CONST_STRING_T    = rffi.CCHARP
RKTIO_INT64_T           = rffi.LONGLONG
RKTIO_FILESIZE_T        = RKTIO_INT64_T
INTPTR_T                = rffi.SSIZE_T # pointer-sized signed int
# integer large enough to hold any native pointer
UINTPTR_T               = rffi.SIZE_T # unsigned
RKTIO_TIMESTAMP_T       = INTPTR_T

W_CCHARP                = make_w_pointer_class("ccharp")
W_CCHARPP               = make_w_pointer_class("ccharpp")

# We could make separate opaque pointers for every typedef
# in the included h files, but that wouldn't give us extra
# benefit as they will all be opaque to rffi anyways.
# So we use a generic \"any\" pointer for all of them as
# much as we can.
R_PTR	= rffi.VOIDP # rffi.COpaquePtr('void *')
W_R_PTR = make_w_pointer_class('voidp')


###############################################
############## used struct-types ##############
###############################################
#
# These are some structs that are used in bootstrap.
# Pycket needs to know these intimitely, i.e. define
# rffi.CStruct for them, because it'll expose functions
# (as part of the bootstrap layer) that dereference some
# of the fields of these.

RKTIO_PROCESS_T_PTR     = R_PTR
W_RKTIO_PROCESS_T_PTR   = W_R_PTR
RKTIO_FD_T_PTR          = R_PTR
W_RKTIO_FD_T_PTR        = W_R_PTR


"""
ref: opaque to Racket: callee (C) allocates, and caller must
eventually free it (rktio_free, free, etc).

*ref: transparent to Racket: caller provides the bytes, callee
never tries to free them.
"""
def ptr_of(base):
    return rffi.CArrayPtr(base)

# For *ref
INTPTR_T_PTR		= ptr_of(INTPTR_T)
STAR_REF_CCHARP		= rffi.CCHARP



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

# Helpers for *ref arguments to coerce arg into ccharp, fixnum, etc.
# Takes a W_Object and produces an rffi pointer
# We could get just a value, in which case we need to create (malloc)
# a cell and output its address to whoever's expecting the (*ref T)

# For char*
def extract_ccharp(w_obj):
    # Null ptr
    if isinstance(w_obj, values.w_false):
        return rffi.cast(CCHARP, 0)
    # Actual C pointer
    if isinstance(w_obj, W_CPointer):
        return w_obj.to_rffi()
    # Racket value
    if isinstance(w_obj, values.W_MutableBytes):
        return rffi.str2charp(w_obj.as_bytes_list())

    raise SchemeException("expected bytes, cpointer, or #f for char*")

# For fixnum
def extract_intptr_t_ptr(w_obj):
    # Null ptr
    if isinstance(w_obj, values.w_false):
        return rffi.cast(INTPTR_T_PTR, 0)
    # Actual C pointer
    if isinstance(w_obj, W_CPointer):
        return w_obj.to_rffi()
    # Racket value
    if isinstance(w_obj, values.W_Fixnum):
	# FIXME: does rpython GC collect this?
	cell = lltype.malloc(rffi.CArray(INTPTR_T), 1, flavor='raw',
                             zero=False, track_allocation=False)
	cell[0] = rffi.cast(INTPTR_T, w_obj.value)
        return rffi.cast(INTPTR_T_PTR, cell)

    raise SchemeException("expected fixnum, cpointer, or #f for intptr_t*")


