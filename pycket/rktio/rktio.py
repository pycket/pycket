from pycket import values, values_string
from pycket import vector as values_vector
from pycket import _rktio_bootstrap as _rktio

from pycket.foreign import W_CStructType, W_CPointer, make_w_pointer_class
from pycket.prims import foreign as prim_foreign
from pycket.prims.expose import expose, expose_val

from rpython.translator.tool.cbuild import ExternalCompilationInfo
from rpython.rtyper.lltypesystem import rffi, lltype

# Load the librktio.a
librktio_a = ExternalCompilationInfo(
    includes=['rktio.h'],
    include_dirs=['/home/caner/pycket/bootstrap-linklets/rktio'],
    libraries=['rktio'],
    library_dirs=['/home/caner/pycket/bootstrap-linklets/rktio'],
)

###############################################
######## Base Types and Pointers ##############
###############################################

INT                     = rffi.INT
UNSIGNED                = rffi.UNSIGNED
UNSIGNED_8              = rffi.UINT
CHAR                    = rffi.CHAR
VOID                    = rffi.VOIDP
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


RKTIO_T_PTR             = rffi.COpaquePtr("rktio_t")
RKTIO_PROCESS_T_PTR     = rffi.COpaquePtr("rktio_process_t")
RKTIO_FD_T_PTR          = rffi.COpaquePtr("rktio_fd_t")
RKTIO_DLL_T_PTR         = rffi.COpaquePtr("rktio_dll_t")
DLL_OPEN_PROC           = rffi.COpaquePtr("dll_open_proc")
DLL_FIND_OBJECT_PROC    = rffi.COpaquePtr("dll_find_object_proc")
DLL_CLOSE_PROC          = rffi.COpaquePtr("dll_close_proc")

W_RKTIO_T_PTR           = make_w_pointer_class("rktio_t")
W_RKTIO_PROCESS_T_PTR   = make_w_pointer_class("rktio_process_t")
W_RKTIO_FD_T_PTR        = make_w_pointer_class("rktio_fd_t")
W_RKTIO_DLL_T_PTR       = make_w_pointer_class("rktio_dll_t")
W_DLL_OPEN_PROC         = make_w_pointer_class("dll_open_pro")
W_DLL_FIND_OBJECT_PROC  = make_w_pointer_class("dll_find_object_proc")
W_DLL_CLOSE_PROC        = make_w_pointer_class("dll_close_pro")

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
###############################################
############## used struct-types ##############
###############################################

# str: (struct_type, struct_ptr_type, w_struct_ptr_type)
STRUCT_RFFI_TYPES = {}

"""
(define-struct-type
 rktio_length_and_addrinfo_t
 ((intptr_t len) ((ref (ref char)) address)))
"""

RKTIO_LENGTH_AND_ADDRINFO_T = rffi.CStruct(
    'rktio_length_and_addrinfo_t',
    ('len',     INTPTR_T),
    ('address', ptr_of(rffi.CCHARP)),
)
RKTIO_LENGTH_AND_ADDRINFO_PTR = lltype.Ptr(RKTIO_LENGTH_AND_ADDRINFO_T)
W_RKTIO_LENGTH_AND_ADDRINFO_PTR = make_w_pointer_class('rktio_length_and_addrinfo_t')

STRUCT_RFFI_TYPES['rktio_length_and_addrinfo_t'] = (RKTIO_LENGTH_AND_ADDRINFO_T, RKTIO_LENGTH_AND_ADDRINFO_PTR, W_RKTIO_LENGTH_AND_ADDRINFO_PTR)

"""
(define-struct-type
 rktio_process_result_t
 (((ref rktio_process_t) process)
  ((ref rktio_fd_t) stdin_fd)
  ((ref rktio_fd_t) stdout_fd)
  ((ref rktio_fd_t) stderr_fd)))
"""

RKTIO_PROCESS_RESULT_T = rffi.CStruct(
    'rktio_process_result_t',
    ('process',     RKTIO_PROCESS_T_PTR),
    ('stdin_fd',    RKTIO_FD_T_PTR),
    ('stdout_fd',   RKTIO_FD_T_PTR),
    ('stderr_fd',   RKTIO_FD_T_PTR),
)
RKTIO_PROCESS_RESULT_PTR = lltype.Ptr(RKTIO_PROCESS_RESULT_T)
W_RKTIO_PROCESS_RESULT_PTR = make_w_pointer_class('rktio_process_result_t')

STRUCT_RFFI_TYPES['rktio_process_result_t'] = (RKTIO_PROCESS_RESULT_T, RKTIO_PROCESS_RESULT_PTR, W_RKTIO_PROCESS_RESULT_PTR)

"""
(define-struct-type rktio_status_t ((rktio_bool_t running) (int result)))
"""
RKTIO_STATUS_T = rffi.CStruct(
    'rktio_status_t',
    ('running', RKTIO_BOOL_T),
    ('result',  INT),
)
RKTIO_STATUS_PTR = lltype.Ptr(RKTIO_STATUS_T)
W_RKTIO_STATUS_PTR = make_w_pointer_class('rktio_status_t')

STRUCT_RFFI_TYPES['rktio_status_t'] = (RKTIO_STATUS_T, RKTIO_STATUS_PTR, W_RKTIO_STATUS_PTR)

"""
(define-struct-type
 rktio_stat_t
 ((uintptr_t device_id)
  (uintptr_t inode)
  (uintptr_t mode)
  (uintptr_t hardlink_count)
  (uintptr_t user_id)
  (uintptr_t group_id)
  (uintptr_t device_id_for_special_file)
  (uintptr_t size)
  (uintptr_t block_size)
  (uintptr_t block_count)
  (uintptr_t access_time_seconds)
  (uintptr_t access_time_nanoseconds)
  (uintptr_t modify_time_seconds)
  (uintptr_t modify_time_nanoseconds)
  (uintptr_t ctime_seconds)
  (uintptr_t ctime_nanoseconds)
  (rktio_bool_t ctime_is_change_time)))
"""
RKTIO_STAT_T = rffi.CStruct(
    'rktio_stat_t',
    ('device_id',                   UINTPTR_T),
    ('inode',                       UINTPTR_T),
    ('mode',                        UINTPTR_T),
    ('hardlink_count',              UINTPTR_T),
    ('user_id',                     UINTPTR_T),
    ('group_id',                    UINTPTR_T),
    ('device_id_for_special_file',  UINTPTR_T),
    ('size',                        UINTPTR_T),
    ('block_size',                  UINTPTR_T),
    ('block_count',                 UINTPTR_T),
    ('access_time_seconds',         UINTPTR_T),
    ('access_time_nanoseconds',     UINTPTR_T),
    ('modify_time_seconds',         UINTPTR_T),
    ('modify_time_nanoseconds',     UINTPTR_T),
    ('ctime_seconds',               UINTPTR_T),
    ('ctime_nanoseconds',           UINTPTR_T),
    ('ctime_is_change_time',        RKTIO_BOOL_T),
)
RKTIO_STAT_PTR = lltype.Ptr(RKTIO_STAT_T)
W_RKTIO_STAT_PTR = make_w_pointer_class('rktio_stat_t')

STRUCT_RFFI_TYPES['rktio_stat_t'] = (RKTIO_STAT_T, RKTIO_STAT_PTR, W_RKTIO_STAT_PTR)

"""
(define-struct-type
 rktio_identity_t
 ((uintptr_t a)
  (uintptr_t b)
  (uintptr_t c)
  (int a_bits)
  (int b_bits)
  (int c_bits)))
"""
RKTIO_IDENTITY_T = rffi.CStruct(
    'rktio_identity_t',
    ('a',       UINTPTR_T),
    ('b',       UINTPTR_T),
    ('c',       UINTPTR_T),
    ('a_bits',  INT),
    ('b_bits',  INT),
    ('c_bits',  INT),
)
RKTIO_IDENTITY_PTR = lltype.Ptr(RKTIO_IDENTITY_T)
W_RKTIO_IDENTITY_PTR = make_w_pointer_class('rktio_identity_t')

STRUCT_RFFI_TYPES['rktio_identity_t'] = (RKTIO_IDENTITY_T, RKTIO_IDENTITY_PTR, W_RKTIO_IDENTITY_PTR)

"""
(define-struct-type
 rktio_date_t
 ((int nanosecond)
  (int second)
  (int minute)
  (int hour)
  (int day)
  (int month)
  (intptr_t year)
  (int day_of_week)
  (int day_of_year)
  (int is_dst)
  (int zone_offset)
  ((ref char) zone_name)))
"""
RKTIO_DATE_T = rffi.CStruct(
    'rktio_date_t',
    ('nanosecond',  INT),
    ('second',      INT),
    ('minute',      INT),
    ('hour',        INT),
    ('day',         INT),
    ('month',       INT),
    ('year',        INTPTR_T),
    ('day_of_week', INT),
    ('day_of_year', INT),
    ('is_dst',      INT),
    ('zone_offset', INT),
    ('zone_name',   rffi.CCHARP), # (ref char)
)
RKTIO_DATE_PTR = lltype.Ptr(RKTIO_DATE_T)
W_RKTIO_DATE_PTR = make_w_pointer_class('rktio_date_t')

STRUCT_RFFI_TYPES['rktio_date_t'] = (RKTIO_DATE_T, RKTIO_DATE_PTR, W_RKTIO_DATE_PTR)

"""
(define-struct-type
 rktio_convert_result_t
 ((intptr_t in_consumed) (intptr_t out_produced) (intptr_t converted)))
"""
RKTIO_CONVERT_RESULT_T = rffi.CStruct(
    'rktio_convert_result_t',
    ('in_consumed',     INTPTR_T),
    ('out_produced',    INTPTR_T),
    ('converted',       INTPTR_T),
)
RKTIO_CONVERT_RESULT_PTR = lltype.Ptr(RKTIO_CONVERT_RESULT_T)
W_RKTIO_CONVERT_RESULT_PTR = make_w_pointer_class('rktio_convert_result_t')

STRUCT_RFFI_TYPES['rktio_convert_result_t'] = (RKTIO_CONVERT_RESULT_T, RKTIO_CONVERT_RESULT_PTR, W_RKTIO_CONVERT_RESULT_PTR)

"""
(define-struct-type
 rktio_sha1_ctx_t
 (((array 5 unsigned) state)
  ((array 2 unsigned) count)
  ((array 64 unsigned-8) buffer)))
"""
RKTIO_SHA1_CTX_T = rffi.CStruct(
    'rktio_sha1_ctx_t',
    ('state',   array_of(UNSIGNED, 5)),
    ('count',   array_of(UNSIGNED, 2)),
    ('buffer',  array_of(UNSIGNED_8, 64)),
)
RKTIO_SHA1_CTX_PTR = lltype.Ptr(RKTIO_SHA1_CTX_T)
W_RKTIO_SHA1_CTX_PTR = make_w_pointer_class('rktio_sha1_ctx_1')

STRUCT_RFFI_TYPES['rktio_sha1_ctx_t'] = (RKTIO_SHA1_CTX_T, RKTIO_SHA1_CTX_PTR, W_RKTIO_SHA1_CTX_PTR)

"""
(define-struct-type
 rktio_sha2_ctx_t
 (((array 2 unsigned) total)
  ((array 8 unsigned) state)
  ((array 64 unsigned-8) buffer)
  (int is224)))
"""
RKTIO_SHA2_CTX_T = rffi.CStruct(
    'rktio_sha2_ctx_t',
    ('total',   array_of(UNSIGNED, 2)),
    ('state',   array_of(UNSIGNED, 8)),
    ('buffer',  array_of(UNSIGNED_8, 64)),
    ('is224',   INT),
)
RKTIO_SHA2_CTX_PTR = lltype.Ptr(RKTIO_SHA2_CTX_T)
W_RKTIO_SHA2_CTX_PTR = make_w_pointer_class('rktio_sha2_ctx_t')

STRUCT_RFFI_TYPES['rktio_sha2_ctx_t'] = (RKTIO_SHA2_CTX_T, RKTIO_SHA2_CTX_PTR, W_RKTIO_SHA2_CTX_PTR)

###############################################
############## bootstrap layer ################
###############################################
"""
This layer implements the following functions that are
not coming from the librktio.a, and need to be manually
added.

rktio_NULL
rktio_filesize_ref
rktio_timestamp_ref
rktio_is_timestamp
rktio_recv_length_ref
rktio_recv_address_ref
rktio_stat_to_vector
rktio_identity_to_vector
rktio_seconds_to_date*
rktio_convert_result_to_vector
rktio_to_bytes
rktio_to_bytes_list
rktio_to_shorts
rktio_from_bytes_list
rktio_free_bytes_list
rktio_make_sha1_ctx
rktio_make_sha2_ctx
rktio_process_result_stdin_fd
rktio_process_result_stdout_fd
rktio_process_result_stderr_fd
rktio_process_result_process
rktio_status_running
rktio_status_result
rktio_pipe_results
rktio_do_install_os_signal_handler
rktio_get_ctl_c_handler

"""

def _wrap_int(val):
    """Return the smallest Pycket integer that can hold *val*."""
    try:
        return values.W_Fixnum(val)
    except OverflowError:
        return values.W_Bignum.fromint(val)

expose_val("rktio_NULL", values.w_false)

@expose("rktio_filesize_ref", [W_RKTIO_FILESIZE_PTR])
def rktio_filesize_ref(w_filesize_ptr):
    r_filesize_ptr = rffi.cast(RKTIO_FILESIZE_PTR, w_filesize_ptr.to_rffi())
    val = r_filesize_ptr[0]
    if not val:
        return values.w_false
    return _wrap_int(val)

@expose("rktio_timestamp_ref", [W_RKTIO_TIMESTAMP_PTR])
def rktio_timestamp_ref(w_timestamp_ptr):
    r_timestamp_ptr = rffi.cast(RKTIO_TIMESTAMP_PTR, w_timestamp_ptr.to_rffi())
    val = r_timestamp_ptr[0]
    return _wrap_int(val)


@expose("rktio_is_timestamp", [values.W_Object])
def rktio_is_timestamp(w_v):
    """
    Return #t iff `v` fits in the signed range of rktio_timestamp_t
    """
    if not isinstance(w_v, values.W_Integer):
        return values.w_false

    # Try to obtain a plain Python int without overflow.
    try:
        v = w_v.toint()
    except OverflowError:
        # Too large to fit in a machine int ⇒ definitely outside timestamp range
        return values.w_false

    bits_in_timestamp = rffi.sizeof(RKTIO_TIMESTAMP_T) * 8 # 32 or 64
    radix = 1 << (bits_in_timestamp - 1) # 2^(n-1)

    if -radix <= v < radix:
        return values.w_true
    else:
        return values.w_false

@expose("rkito_recv_length_ref", [W_RKTIO_LENGTH_AND_ADDRINFO_PTR])
def rktio_recv_length_ref(w_len_and_addrinfo_ptr):
    r_struct_ptr = rffi.cast(RKTIO_LENGTH_AND_ADDRINFO_PTR,
                             w_len_and_addrinfo_ptr.to_rffi())
    return _wrap_int(r_struct_ptr.c_len)

@expose("rktio_recv_address_ref", [W_RKTIO_LENGTH_AND_ADDRINFO_PTR])
def rktio_recv_address_ref(w_len_and_addrinfo_ptr):
    ll_ptr = rffi.cast(RKTIO_LENGTH_AND_ADDRINFO_PTR,
                       w_len_and_addrinfo_ptr.to_rffi())

    charp = ll_ptr.c_address[0]

    if not charp:
        return values.w_false

    # copy into a Pycket/Racket string object
    py_str = rffi.charp2str(charp) # assumes utf-8
    return values_string.W_String.fromstr_utf8(py_str)

@expose("rktio_stat_to_vector", [W_RKTIO_STAT_PTR])
def rktio_stat_to_vector(w_stat_ptr):
    ll_ptr = rffi.cast(RKTIO_STAT_PTR, w_stat_ptr.to_rffi())

    elems = [
        _wrap_int(ll_ptr.c_device_id),
        _wrap_int(ll_ptr.c_inode),
        _wrap_int(ll_ptr.c_mode),
        _wrap_int(ll_ptr.c_hardlink_count),
        _wrap_int(ll_ptr.c_user_id),
        _wrap_int(ll_ptr.c_group_id),
        _wrap_int(ll_ptr.c_device_id_for_special_file),
        _wrap_int(ll_ptr.c_size),
        _wrap_int(ll_ptr.c_block_size),
        _wrap_int(ll_ptr.c_block_count),

        _wrap_int(ll_ptr.c_access_time_seconds),
        _wrap_int(ll_ptr.c_access_time_nanoseconds),
        _wrap_int(ll_ptr.c_modify_time_seconds),
        _wrap_int(ll_ptr.c_modify_time_nanoseconds),
        _wrap_int(ll_ptr.c_ctime_seconds),
        _wrap_int(ll_ptr.c_ctime_nanoseconds),

        values.w_true if ll_ptr.c_ctime_is_change_time else values.w_false,
    ]
    return values_vector.W_Vector.fromelements(elems)

@expose("rktio_identity_to_vector", [W_RKTIO_IDENTITY_PTR])
def rktio_identity_to_vector(w_id_ptr):
    ll_ptr = rffi.cast(RKTIO_IDENTITY_PTR, w_id_ptr.to_rffi())

    elems = [
        _wrap_int(ll_ptr.c_a),          # uintptr_t
        _wrap_int(ll_ptr.c_b),
        _wrap_int(ll_ptr.c_c),
        _wrap_int(ll_ptr.c_a_bits),     # plain int -> still wrap
        _wrap_int(ll_ptr.c_b_bits),
        _wrap_int(ll_ptr.c_c_bits),
    ]

    return values_vector.W_Vector.fromelements(elems)

"""
@expose(
    "rktio_seconds_to_date*",
    [W_RKTIO_T_PTR, values.W_Integer, values.W_Integer, values.W_Integer],
)
def rktio_seconds_to_date_star(w_rktio, w_seconds, w_nanosec, w_get_gmt):
    rktio_ptr = rffi.cast(RKTIO_T_PTR, w_rktio.to_rffi())

    seconds  = rffi.cast(RKTIO_TIMESTAMP_T, w_seconds.toint())
    nanosec  = rffi.cast(rffi.INT,          w_nanosec.toint())
    get_gmt  = rffi.cast(rffi.INT,          w_get_gmt.toint())

    date_p = c_rktio_seconds_to_date(rktio_ptr, seconds, nanosec, get_gmt)

    # return #(errno symbol) when rktio_seconds_to_date fails
    if not date_p:
        err = rffi.get_saved_errno()
        return values_vector.W_Vector.fromelements([
            _wrap_int(err),
            errno_code_to_symbol(err),            # returns a W_Symbol
        ])

    # success, build date*
    d = date_p[0]

    zone_name_val = (
        values.w_false if not d.c_zone_name else
        values_string.W_String.fromstr_utf8(rffi.charp2str(d.c_zone_name))
    )

    from pycket.prims.general import date_star_struct
    # get and call the contstructor with these
    # self.struct_type().constructor.call_with_extra_info(args...)
    # will need env and cont to call this.


    result = make_date_star(
        _wrap_int(d.c_second),
        _wrap_int(d.c_minute),
        _wrap_int(d.c_hour),
        _wrap_int(d.c_day),
        _wrap_int(d.c_month),
        _wrap_int(d.c_year),
        _wrap_int(d.c_nanosecond),
        _wrap_int(d.c_zone_offset),
        values.w_true  if d.c_is_dst else values.w_false,
        _wrap_int(d.c_day_of_week),
        _wrap_int(d.c_day_of_year),
        zone_name_val,
    )

    # free allocation performed by C
    c_rktio_free(rktio_ptr, rffi.cast(rffi.VOIDP, date_p))

    return result
"""

###############################################
############## process_result #################

def _wrap_process_null(addr):
    return values.w_false if not addr else W_RKTIO_FD_T_PTR(addr)

@expose("rktio_process_result_stdin_fd",
        [W_RKTIO_PROCESS_RESULT_PTR])
def rktio_process_result_stdin_fd(res_ptr):
    res_ll = rffi.cast(RKTIO_PROCESS_RESULT_PTR, res_ptr.to_rffi())

    greet()  # prints: I am function greet!

    return _wrap_process_null(res_ll.c_stdin_fd)

@expose("rktio_process_result_stdout_fd",
        [W_RKTIO_PROCESS_RESULT_PTR])
def rktio_process_result_stdout_fd(res_ptr):
    res_ll = rffi.cast(RKTIO_PROCESS_RESULT_PTR, res_ptr.to_rffi())
    return _wrap_process_null(res_ll.c_stdout_fd)

@expose("rktio_process_result_stderr_fd",
        [W_RKTIO_PROCESS_RESULT_PTR])
def rktio_process_result_stderr_fd(res_ptr):
    res_ll = rffi.cast(RKTIO_PROCESS_RESULT_PTR, res_ptr.to_rffi())
    return _wrap_process_null(res_ll.c_stderr_fd)

@expose("rktio_process_result_process",
        [W_RKTIO_PROCESS_RESULT_PTR])
def rktio_process_result_process(res_ptr):
    res_ll = rffi.cast(RKTIO_PROCESS_RESULT_PTR, res_ptr.to_rffi())
    return W_RKTIO_PROCESS_T_PTR(res_ll.c_process)   # never NULL, so no _wrap


###############################################
###############################################
###############################################


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

