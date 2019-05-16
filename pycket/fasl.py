#! /usr/bin/env python
# -*- coding: utf-8 -*-


FASL_GRAPH_DEF_TYPE = 1
FASL_GRAPH_REF_TYPE = 2

FASL_FALSE_TYPE = 3
FASL_TRUE_TYPE = 4
FASL_NULL_TYPE = 5
FASL_VOID_TYPE = 6
FASL_EOF_TYPE = 7

FASL_INTEGER_TYPE = 8
FASL_FLONUM_TYPE = 9
FASL_SINGLE_FLONUM_TYPE = 10
FASL_RATIONAL_TYPE = 11
FASL_COMPLEX_TYPE = 12
FASL_CHAR_TYPE = 13

FASL_SYMBOL_TYPE = 14
FASL_UNREADABLE_SYMBOL_TYPE = 15
FASL_UNINTERNED_SYMBOL_TYPE = 16
FASL_KEYWORD_TYPE = 17
FASL_STRING_TYPE = 18
FASL_IMMUTABLE_STRING_TYPE = 19
FASL_BYTES_TYPE = 20
FASL_IMMUTABLE_BYTES_TYPE = 21
FASL_PATH_TYPE = 22
FASL_RELATIVE_PATH_TYPE = 23

FASL_PREGEXP_TYPE = 24
FASL_REGEXP_TYPE = 25
FASL_BYTE_PREGEXP = 26
FASL_BYTE_REGEXP_TYPE = 27

FASL_LIST_TYPE = 28
FASL_LIST_STAR_TYPE = 29
FASL_PAIR_TYPE = 30
FASL_VECTOR_TYPE = 31
FASL_IMMUTABLE_VECTOR_TYPE = 32
FASL_BOX_TYPE = 33
FASL_IMMUTABLE_BOX_TYPE = 34
FASL_PREFAB_TYPE = 35
FASL_HASH_TYPE = 36
FASL_IMMUTABLE_HASH_TYPE = 37

FASL_SRCLOC = 38

FASL_EXTFLONUM_TYPE = 39

# 100 to 255 is used for small integers:
FASL_SMALL_INTEGER_START = 100

#################################################

FASL_LOWEST_SMALL_INTEGER = -10
FASL_HIGHEST_SMALL_INTEGER = 255 - ((FASL_SMALL_INTEGER_START - FASL_LOWEST_SMALL_INTEGER) - 1)
FASL_PREFIX = "racket/fasl:"
FASL_PREFIX_LENGTH = len(FASL_PREFIX)

FASL_HASH_EQ_VARIANT = 0
FASL_HASH_EQUAL_VARIANT = 1
FASL_HASH_EQV_VARIANT = 2

from rpython.rlib             import streamio as sio
import os

def fasl_to_sexp_file(file_name):
    stream = sio.open_file_as_stream(file_name, "rb", buffering=2**21)
    return fasl_to_sexp(stream)

def fasl_to_sexp(stream):

    prefix = stream.read(FASL_PREFIX_LENGTH)
    if prefix != FASL_PREFIX:
        raise Exception("unrecognized prefix")

    shared_count = read_fasl_integer_stream(stream)
    shared = [None]*shared_count

    length = read_fasl_integer_stream(stream)
    # read the entire thing and work with a byte-string and a position
    fasl_string = stream.read(length)
    pos = 0
    sexp, pos = fasl_to_sexp_recursive(fasl_string, pos)
    return sexp

# let's not worry about the CPS'in this right now
# we probably won't have any sexp deeper than the stack anyways
def fasl_to_sexp_recursive(fasl_string, pos):
    #from pycket.interpreter import *
    from pycket.values import to_list, W_Symbol, W_Fixnum

    typ, pos = read_byte_no_eof(fasl_string, pos)

    if typ == FASL_SYMBOL_TYPE:
        sym_len, pos = read_fasl_integer(fasl_string, pos)
        sym_str, pos = read_fasl_string(fasl_string, pos, sym_len)
        return W_Symbol.make(sym_str), pos
    elif typ == FASL_LIST_TYPE:
        list_len, pos = read_fasl_integer(fasl_string, pos)
        lst_chunk = fasl_string[pos:pos+list_len]
        lst = [None]*list_len
        for i in range(list_len):
            element, pos = fasl_to_sexp_recursive(fasl_string, pos)
            lst[i] = element
        return to_list(lst), pos
    else:
        if typ >= FASL_SMALL_INTEGER_START:
            return W_Fixnum((typ-FASL_SMALL_INTEGER_START)+FASL_LOWEST_SMALL_INTEGER), pos
        else:
            raise Exception("unrecognized fasl tag : %s" % typ)

def read_fasl_string(fasl_string, pos, length):
    return read_bytes_exactly(fasl_string, pos, length)
    # TODO: utf-8

def read_byte_no_eof(fasl_string, pos):
    return ord(fasl_string[pos]), pos+1

def read_byte_no_eof_stream(stream):
    b = stream.read(1)
    if not b:
        raise Exception("truncated stream - got eof")
    return b

def read_bytes_exactly(fasl_string, pos, n):
    if pos+n > len(fasl_string):
        raise Exception("truncated stream")
    return fasl_string[pos:pos+n], pos+n

def fasl_integer_inner(b):
    if b <= 127:
        return b
    elif b >= 132:
        raise Exception("NYI")

def read_fasl_integer(fasl_string, pos):
    b, new_pos = read_byte_no_eof(fasl_string, pos)
    return fasl_integer_inner(b), new_pos

def read_fasl_integer_stream(stream):
    b = ord(read_byte_no_eof_stream(stream))
    return fasl_integer_inner(b)
