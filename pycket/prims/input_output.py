#! /usr/bin/env python
# -*- coding: utf-8 -*-
from rpython.rlib             import streamio as sio
from rpython.rlib.rbigint     import rbigint
from rpython.rlib.rstring     import ParseStringError, ParseStringOverflowError
from rpython.rlib.rarithmetic import string_to_int
from pycket                   import values
from pycket.error             import SchemeException
from pycket.prims.expose      import default, expose, expose_val

class Token(object):
    def __init__(self, v):
        self.val = v

class NumberToken(Token): pass
class StringToken(Token): pass
class SymbolToken(Token): pass
class BooleanToken(Token): pass
class LParenToken(Token): pass
class RParenToken(Token): pass
class LVecToken(Token): pass
class RVecToken(Token): pass

def read_number_or_id(f, init):
    sofar = init
    while True:
        (count, c) = f.peek()
        if c == "":
            break
        c = c[0]
        if c.isalnum():
            sofar = sofar + f.read(1)
        else:
            break
    try:
        return NumberToken(values.W_Fixnum.make(string_to_int(sofar)))
    except ParseStringOverflowError:
        val = rbigint.fromdecimalstr(sofar)
        return NumberToken(values.W_Bignum(val))
    except ParseStringError:
        try:
            return NumberToken(values.W_Flonum.make(float(sofar)))
        except:
            return SymbolToken(values.W_Symbol.make(sofar))

def read_token(f):
    while True:
        c = f.read(1) # FIXME: unicode
        if c in [" ", "\n", "\t"]:
            continue
        if c in ["(", "[", "{"]:
            return LParenToken(values.W_String.make(c))
        if c in [")", "]", "}"]:
            return RParenToken(values.W_String.make(c))
        if c.isalnum():
            return read_number_or_id(f, c)
        if c == "#":
            c2 = f.read(1)
            if c2 == "t":
                return BooleanToken(values.w_true)
            if c2 == "f":
                return BooleanToken(values.w_false)
            if c2 in ["(", "[", "{"]:
                return LVecToken(values.W_String.make(c2))
            raise SchemeException("bad token in read: %s" % c2)
        raise SchemeException("bad token in read: %s" % c)

@expose("read", [default(values.W_InputPort, None)])
def read(port):
    if port is None:
        stream = sio.fdopen_as_stream(0, "rb")
    else:
        stream = port.file
    token = read_token(stream)
    if isinstance(token, NumberToken):
        return token.val
    if isinstance(token, StringToken):
        return token.val
    if isinstance(token, SymbolToken):
        return token.val
    if isinstance(token, BooleanToken):
        return token.val
    return values.w_false # fail!

@expose("read-line", [default(values.W_InputPort, None)])
def read(port):
    if port is None:
        stream = sio.fdopen_as_stream(0, "rb")
    else:
        stream = port.file
    return values.W_String(stream.readline())

text_sym   = values.W_Symbol.make("text")
binary_sym = values.W_Symbol.make("binary")
none_sym   = values.W_Symbol.make("none")
error_sym  = values.W_Symbol.make("error")

@expose("open-input-file", [values.W_String,
                            default(values.W_Symbol, binary_sym),
                            default(values.W_Symbol, none_sym)])
def open_input_file(str, mode, mod_mode):
    m = "r" if mode is text_sym else "rb"
    f = str.value
    return values.W_FileInputPort(sio.open_file_as_stream(f, mode=m))

@expose("open-output-file", [values.W_String,
                             default(values.W_Symbol, binary_sym),
                             default(values.W_Symbol, error_sym)])
def open_output_file(str, mode, exists):
    m = "w" if mode is text_sym else "wb"
    f = str.value
    return values.W_FileOutputPort(sio.open_file_as_stream(f, mode=m))

