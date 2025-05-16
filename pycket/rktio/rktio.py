from pycket import values
from pycket import vector as values_vector
from pycket import values, values_string
from pycket.prims.expose import default, expose, expose_val

from rpython.rlib.rbigint import rbigint
from rpython.rtyper.lltypesystem import rffi

from pycket.rktio.types import *
from pycket.rktio._rktio_bootstrap import *
from pycket.rktio.bootstrap_structs import *

###############################################
############## connector layer ################
###############################################
"""
This layer implements the following functions that are
not coming from the librktio.a, and need to be manually
added.

They use the struct pointers defined in bootstrap_structs,
and they use c_rffi primitives declared at _rktio_bootstrap.

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

def _wrap_int(raw):
    """Return the smallest Pycket integer that can hold *val*."""

    val = rffi.cast(lltype.Signed, raw)

    # fast path: fits in a fixnum?
    try:
        return values.W_Fixnum(intmask(val))
    except OverflowError:
        # slow path: upgrade to big-int first, then wrap
        big = rbigint.fromint(intmask(val))
        return values.W_Bignum(big)

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

@expose(
    "rktio_seconds_to_date*",
    [W_R_PTR, values.W_Fixnum, values.W_Fixnum, values.W_Fixnum], simple=False
)
def rktio_seconds_to_date_star(w_rktio, w_seconds, w_nanosec, w_get_gmt, env, cont):
    from pycket.interpreter import return_value
    from pycket.prims.general import date_star_struct

    # TODO: seconds in date range check

    p = w_rktio_seconds_to_date(w_rktio, w_seconds, w_nanosec, w_get_gmt)

    if isinstance(p, values_vector.W_Vector):
        return return_value(p, env, cont)

    assert isinstance(p, W_RKTIO_DATE_PTR)
    d = rffi.cast(RKTIO_DATE_PTR, p.to_rffi())

    # success, build date*
    # d = date_p[0]

    zone_name_val = (
        values.w_false if not d.c_zone_name else
        values_string.W_String.fromstr_utf8(rffi.charp2str(d.c_zone_name))
    )

    is_dst_signed = rffi.cast(lltype.Signed, d.c_is_dst)

    return date_star_struct.constructor.call_with_extra_info(
        [
        _wrap_int(d.c_second),
        _wrap_int(d.c_minute),
        _wrap_int(d.c_hour),
        _wrap_int(d.c_day),
        _wrap_int(d.c_month),
        _wrap_int(d.c_year),
        _wrap_int(d.c_nanosecond),
        _wrap_int(d.c_zone_offset),
        values.w_true  if is_dst_signed else values.w_false,
        _wrap_int(d.c_day_of_week),
        _wrap_int(d.c_day_of_year),
        zone_name_val,
        ], env, cont, None)

# rktio_convert_result_to_vector
@expose("rktio_convert_result_to_vector", [W_RKTIO_CONVERT_RESULT_PTR])
def rktio_convert_result_to_vector(w_ptr):
    ll_ptr = rffi.cast(RKTIO_CONVERT_RESULT_PTR, w_ptr.to_rffi())

    elems = [
        _wrap_int(ll_ptr.in_consumed),
        _wrap_int(ll_ptr.out_produced),
        _wrap_int(ll_ptr.converted),
    ]

    return values_vector.W_Vector.fromelements(elems)


# rktio_to_bytes
@expose("rktio_to_bytes", [W_CCHARP])
def rktio_to_bytes(w_ptr):
    c_char_p = rffi.cast(CCHARP, w_ptr.to_rffi())

    # copy the bytes up to the first NUL
    py_bytes = rffi.charp2str(c_char_p)

    return values.W_Bytes.from_string(py_bytes)

# rktio_to_bytes_list
@expose("rktio_to_bytes_list", [W_CCHARPP, default(values.W_Fixnum, values.W_Fixnum.ZERO)])
def rktio_to_bytes_list(w_ptr, w_len):
    length = w_len.value
    ll_tbl = rffi.cast(rffi.CCHARPP, w_ptr.to_rffi())

    if length == 0:
        tmp = 0
        while ll_tbl[tmp]:
            tmp += 1
        length = tmp

    elems = [None] * length
    for i in range(length):
        c_str   = ll_tbl[i]
        py_str  = rffi.charp2str(c_str)
        elems[i] = values.W_Bytes.from_string(py_str)
        rffi.free_charp(c_str)

    return values.to_list(elems)

# rktio_to_shorts
@expose("rktio_to_shorts", [W_R_PTR])
def rktio_to_shorts(w_ptr):
    from rpython.rlib.rstring import StringBuilder

    """
    Copy a NUL-terminated array of unsigned-16 values into a bytevector,
    preserving little-endian order (two bytes per word).
    """
    c_char_p = rffi.cast(R_PTR, w_ptr.to_rffi())

    sb      = StringBuilder()
    offset  = 0
    while True:
        lo = ord(c_char_p[offset])
        hi = ord(c_char_p[offset + 1])
        if lo == 0 and hi == 0:
            break
        sb.append(chr(lo))
        sb.append(chr(hi))
        offset += 2

    return values.W_Bytes.from_string(sb.build())

CCHARP_ARRAY = lltype.Array(rffi.CCHARP, hints={'nolength': True})

# rktio_from_bytes_list
@expose("rktio_from_bytes_list", [values.W_Object])
def rktio_from_bytes_list(w_lst):
    elems   = []
    curr    = w_lst
    while curr is not values.w_null:
        elems.append(curr.car()) # each must be W_Bytes
        curr = curr.cdr()

    length = len(elems)

    tbl = lltype.malloc(CCHARP_ARRAY, length, flavor='raw')

    for i in range(length):
        w_bstr = elems[i] # W_Bytes
        py_str = w_bstr.as_str()
        tbl[i] = rffi.str2charp(py_str)

    # The caller must remember `length` and pass it to rktio_free_bytes_list.
    return W_CCHARPP(rffi.cast(rffi.CCHARPP, tbl))

# rktio_free_bytes_list
@expose("rktio_free_bytes_list", [W_CCHARPP, values.W_Fixnum])
def rktio_free_bytes_list(w_ptr, w_len):
    """
    Free a table previously created by `rktio_from_bytes_list`.
    """
    length = w_len.value
    ll_tbl = rffi.cast(rffi.CCHARPP, w_ptr.to_rffi())

    i = 0
    while True:
        if length and i >= length:
            break
        c_str = ll_tbl[i]
        if not c_str:
            break
        rffi.free_charp(c_str)
        i += 1

    lltype.free(rffi.cast(R_PTR, ll_tbl), flavor='raw')

# rktio_make_sha1_ctx
@expose("rktio_make_sha1_ctx", [])
def rktio_make_sha1_ctx():
    """
    Allocate a fresh, zero-initialised SHA-1 context and return it
    as a Pycket bytevector (W_Bytes), exactly like Chez
    (make-bytevector (ftype-sizeof rktio_sha1_ctx_t)).
    """
    size = rffi.sizeof(RKTIO_SHA1_CTX_T)
    return values.W_Bytes.from_string("\x00" * size)

# rktio_make_sha2_ctx
@expose("rktio_make_sha2_ctx", [])
def rktio_make_sha2_ctx():
    """
    Allocate a fresh, zero-initialised SHA-2 context and return it
    as a Pycket bytevector (W_Bytes), exactly like Chez
    (make-bytevector (ftype-sizeof rktio_sha2_ctx_t)).
    """
    size = rffi.sizeof(RKTIO_SHA2_CTX_T)
    return values.W_Bytes.from_string("\x00" * size)

###############################################
############## process_result #################

def _wrap_process_null(addr):
    return values.w_false if not addr else W_RKTIO_FD_T_PTR(addr)

@expose("rktio_process_result_stdin_fd",
        [W_RKTIO_PROCESS_RESULT_PTR])
def rktio_process_result_stdin_fd(res_ptr):
    res_ll = rffi.cast(RKTIO_PROCESS_RESULT_PTR, res_ptr.to_rffi())

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

# rktio_status_running
@expose("rktio_status_running", [W_RKTIO_STATUS_PTR])
def rktio_status_running(w_stat_ptr):
    r_stat_ptr = rffi.cast(RKTIO_STATUS_PTR, w_stat_ptr.to_rffi())
    return values.W_Bool.make(r_stat_ptr.running == 1)

# rktio_status_result
@expose("rktio_status_result", [W_RKTIO_STATUS_PTR])
def rktio_status_result(w_stat_ptr):
    r_stat_ptr = rffi.cast(RKTIO_STATUS_PTR, w_stat_ptr.to_rffi())
    return _wrap_int(r_stat_ptr.result)

# rktio_pipe_results
@expose("rktio_pipe_results", [W_R_PTR])
def rktio_pipe_results(w_res_ptr):
    """
    The only indication on what this does to me is
    what I see in io.sls for Chez.
    Looks like there're two (pointer) values back to back
    in the pointer we dereference (I'm guessing file
    descriptor pointers for in and out).

    We return them as two values
    (opaque file-descriptor pointers).
    """
    arr = rffi.cast(rffi.VOIDPP, w_res_ptr.to_rffi())

    w_fd0 = W_RKTIO_FD_T_PTR(arr[0])
    w_fd1 = W_RKTIO_FD_T_PTR(arr[1])

    return values.Values.make([w_fd0, w_fd1])

# rktio_do_install_os_signal_handler
@expose("rktio_do_install_os_signal_handler", [W_R_PTR])
def rktio_do_install_os_signal_handler(w_rktio):
    c_rktio_install_os_signal_handler(w_rktio.to_rffi())
    return values.w_void

# rktio_get_ctl_c_handler
@expose("rktio_get_ctl_c_handler", [])
def rktio_get_ctl_c_handler():
    """
    This is how it's defined in src/io/host/bootstrap-rktio.rkt
    """
    return values.w_void

