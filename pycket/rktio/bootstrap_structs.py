from pycket.rktio.types import *
from pycket.rktio._rktio_bootstrap import *

from rpython.rtyper.lltypesystem import rffi

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
w_RKTIO_FD_T_PTR        = W_R_PTR


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


