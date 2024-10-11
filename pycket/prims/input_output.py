#! /usr/bin/env python
# -*- coding: utf-8 -*-
from rpython.rlib import jit
from rpython.rlib             import streamio as sio
from rpython.rlib.rbigint     import rbigint
from rpython.rlib.rstring     import (ParseStringError,
        ParseStringOverflowError, StringBuilder)
from rpython.rlib.rarithmetic import string_to_int, intmask
from rpython.rlib import runicode
from rpython.rlib.objectmodel import newlist_hint

from pycket.arity        import Arity
from pycket.cont         import continuation, loop_label, call_cont
from pycket.base         import W_ProtoObject
from pycket              import values
from pycket              import values_parameter
from pycket              import values_struct
from pycket              import vector as values_vector
from pycket.hash.simple  import W_EqvImmutableHashTable, W_EqMutableHashTable, W_EqvMutableHashTable, W_EqImmutableHashTable, make_simple_immutable_table
from pycket.hash.base    import W_HashTable
from pycket              import impersonators as imp
from pycket.hash.equal   import W_EqualHashTable, W_EqualAlwaysHashTable
from pycket              import values_string
from pycket.error        import SchemeException, FSException, ContractException, ArityException
from pycket.prims.expose import default, expose, expose_val, procedure, make_procedure

from sys import platform, maxint

import os

############################ Values and Parameters

stdin_port = values.W_FileInputPort(sio.fdopen_as_stream(0, "r"), None, stdin=True)
stdout_port = values.W_FileOutputPort(sio.fdopen_as_stream(1, "w", buffering=1), None, stdout=True)
stderr_port = values.W_FileOutputPort(sio.fdopen_as_stream(2, "w", buffering=1), None, stdout=True)

expose_val("eof", values.eof_object)

current_out_param = values_parameter.W_Parameter(stdout_port)
current_error_param = values_parameter.W_Parameter(stderr_port)
current_in_param = values_parameter.W_Parameter(stdin_port)
current_readtable_param = values_parameter.W_Parameter(values.w_false)

# FIXME : get all these from the io linklet
expose_val("current-readtable", current_readtable_param)
expose_val("current-output-port", current_out_param)
expose_val("current-error-port", current_error_param)
expose_val("current-input-port", current_in_param)
expose_val("current-get-interaction-input-port", values_parameter.W_Parameter(stdin_port))
expose_val("current-read-interaction", values_parameter.W_Parameter(stdin_port))


class Token(W_ProtoObject):
    _attrs_ = []

class SpecialToken(Token):
    _attrs_ = ['con']

    def __init__(self, con):
        self.con = con

    def finish(self, val):
        return values.W_Cons.make(self.con, values.W_Cons.make(val, values.w_null))

class DelimToken(Token):
    _attrs_ = ['str']

    def __init__(self, s=""):
        self.str = s

class LParenToken(DelimToken):
    _attrs_ = []

class RParenToken(DelimToken):
    _attrs_ = []

class DotToken(DelimToken):
    _attrs_ = []

class HashToken(DelimToken):
    _attrs_ = []

class HashToken(DelimToken):
    _attrs_ = []

class HashEqToken(DelimToken):
    _attrs_ = []

class HashEqvToken(DelimToken):
    _attrs_ = []

class HashAlwToken(DelimToken):
    _attrs_ = []

class NoToken(DelimToken):
    _attrs_ = []

# Some prebuilt tokens

def make_special_tokens(*names):
    for name in names:
        symbol = values.W_Symbol.make(name)
        token = SpecialToken(symbol)
        idname = name.replace("-", "_")
        globals()[idname + "_token"] = token

make_special_tokens(
    "quote",
    "quasiquote",
    "unquote",
    "unquote-splicing",
    "syntax",
    "quasisyntax",
    "unsyntax",
    "unsyntax-splicing")

dot_token = DotToken('.')

allowed_char = "!?.-_:=*$%<>+^@&~/"

def idchar(c):
    c = c[0] # tell the annotator it's really a single char
    return c.isalnum() or c in allowed_char

def read_number_or_id(f, init):
    sofar = StringBuilder(64)
    sofar.append(init)
    while True:
        c = f.peek()
        if c == "":
            break
        if idchar(c):
            v = f.read(1)
            assert v == c
            sofar.append(v)
        else:
            break
    got = sofar.build()
    try:
        val = string_to_int(got)
        return values.W_Fixnum.make_or_interned(val)
    except ParseStringOverflowError:
        val = rbigint.fromdecimalstr(got)
        return values.W_Bignum(val)
    except ParseStringError:
        try:
            return values.W_Flonum(float(got))
        except:
            return values.W_Symbol.make(got)

@expose("read-string", [values.W_Fixnum, default(values.W_InputPort, None)], simple=False)
def read_string_(amt, w_port, env, cont):
    from pycket.interpreter import return_value
    if w_port is None:
        w_port = current_in_param.get(cont)
    return return_value(read_string(w_port, amount=amt.value), env, cont)

# FIXME: replace with a string builder
# FIXME: unicode
# FIXME: If no characters are available before an end-of-file, then eof is returned.
def read_string(f, amount=maxint):
    buf = StringBuilder(64)
    isascii = True
    count = 0
    while count < amount:
        c = f.read(1)[0]
        count += 1
        if c == '"':
            string = buf.build()
            if isascii:
                return values_string.W_String.fromascii(string)
            return values_string.W_String.fromstr_utf8(string)
        elif c == '\\':
            n = f.read(1)[0]
            if n == '"' or n == '\\':
                c = n
            elif n == 'n':
                c = '\n'
            elif n == 't':
                c = '\t'
            else:
                raise SchemeException("read: bad escape character in string: %s"%n)
        else:
            isascii &= ord(c) < 128
        buf.append(c)
    string = buf.build()
    if isascii:
        return values_string.W_String.fromascii(string)
    return values_string.W_String.fromstr_utf8(string)

def is_hash_token(s):
    # already read the #, so the cursor is at position 1 (s.seek(1))
    if s.read(1) == "a" and s.read(1) == "s" and s.read(1) == "h":
        # we're on to something
        if s.peek() == "(":
            return HashToken()
        e = s.read(1)
        q = s.read(1)
        v = s.read(1)
        if e == "e" and q == "q" and v == "(":
            s.seek(7)
            return HashEqToken()
        elif e == "e" and q == "q" and v == "v" and s.peek() == "(":
            return HashEqvToken()
        elif e == "a" and q == "l" and v == "w" and s.peek() == "(":
            return HashAlwToken()
        else:
            return NoToken()
    else:
        return NoToken()

def read_hash(stream, token):
    # cursor is at the start (
    elements = read_stream(stream)

    keys = []
    vals = []

    while elements is not values.w_null:
        c = elements.car()
        assert isinstance(c, values.W_WrappedCons)
        keys.append(c.car())
        vals.append(c.cdr())
        elements = elements.cdr()

    if isinstance(token, HashToken):
        return W_EqualHashTable(keys, vals, immutable=True)
    elif isinstance(token, HashAlwToken):
        return W_EqualAlwaysHashTable(keys, vals, immutable=True)
    elif isinstance(token, HashEqToken):
        return make_simple_immutable_table(W_EqImmutableHashTable, keys, vals)
    elif isinstance(token, HashEqvToken):
        return make_simple_immutable_table(W_EqvImmutableHashTable, keys, vals)
    else:
        raise SchemeException("read: cannot read hash in string : %s" % stream.tostring())

def read_token(f):
    while True:
        c = f.read(1) # FIXME: unicode
        if not c:
            return values.eof_object
        if c == ";":
            f.readline()
            continue
        if c in [" ", "\n", "\t"]:
            continue
        if c in ["(", "[", "{"]:
            return LParenToken(c)
        if c in [")", "]", "}"]:
            return RParenToken(c)
        if c == "\"":
            v = read_string(f)
            return v
        if c == ".":
            p = f.peek()
            if p in [" ", "\n", "\t"]:
                return dot_token
            return read_number_or_id(f, c)
        if c == "'":
            return quote_token
        if c ==  "`":
            return quasiquote_token
        if c == ",":
            p = f.peek()
            if p == "@":
                p = f.read(1)
                return unquote_splicing_token
            else:
                return unquote_token
        if idchar(c):
            return read_number_or_id(f, c)
        if c == "#":
            c2 = f.read(1)
            if c2 == "s":
                c3 = f.read(1)
                if c3 == "(":
                    return LParenToken("#s(")
                raise SchemeException("bad token in read: %s reading %s" % (c+c2+c3, f))
            if c2 == "h":
                token = is_hash_token(f)
                if not isinstance(token, NoToken):
                    return token
                f.seek(1)
            if c2 == "'":
                return syntax_token
            if c2 == "`":
                return quasisyntax_token
            if c2 == ",":
                p = f.peek()
                if p == "@":
                    p = f.read(1)
                    return unsyntax_splicing_token
                return unsyntax_token
            if c2 == "t":
                return values.w_true
            if c2 == "f":
                return values.w_false
            if c2 == '"':
                v = read_string(f)
                return values.W_Bytes.from_charlist(v.as_charlist_utf8())
            if c2 in ["(", "[", "{"]:
                return LParenToken("#" + c2)
            if c2 == "\\":
                s = f.read(1)
                if not s:
                    raise SchemeException("unexpected end of file reading %s" % f)
                c = ord(s[0]) # XXX deal with unicode
                return values.W_Character(unichr(c))
            raise SchemeException("bad token in read: %s reading %s" % (c+c2,f))
        raise SchemeException("bad token in read: %s reading %s" % (c,f))

@expose("read", [default(values.W_Object, None)], simple=False)
def read(port, env, cont):
    from pycket.interpreter import return_value
    cont = read_stream_cont(env, cont)
    return get_input_port(port, env, cont)

def get_input_port(port, env, cont):
    from pycket.interpreter import return_value
    if port is None:
        port = current_in_param.get(cont)
        return return_value(port, env, cont)
    else:
        return get_port(port, values_struct.w_prop_input_port, values.W_InputPort, env, cont)

@continuation
def read_stream_cont(env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    port = check_one_val(_vals)
    rt = current_readtable_param.get(cont)
    if rt is values.w_false:
        rt = None
    else:
        assert isinstance(rt, values.W_ReadTable)
    return read_stream_rt(port, rt, env, cont)

def read_stream_rt(port, rt, env, cont):
    from pycket.interpreter import return_value
    if rt is not None:
        c = port.peek()
        i = ord(c[0])
        needed = utf8_code_length(i)
        old = port.tell()
        c = port.read(needed)
        port.seek(old)
        u = c.decode("utf-8")
        assert len(u) == 1
        if u == rt.key.value:
            port.read(needed) # since we peeked
            args = [rt.key, port, values.w_false, values.w_false, values.w_false, values.w_false]
            return rt.action.call(args, env, cont)
    # ignore the possibility that the readtable is relevant in the future
    return return_value(read_stream(port), env, cont)

@jit.dont_look_inside
def read_stream(stream):
    next_token = read_token(stream)
    if isinstance(next_token, SpecialToken):
        v = read_stream(stream)
        return next_token.finish(v)
    if isinstance(next_token, HashToken):
        v = read_hash(stream, next_token)
        return v
    if isinstance(next_token, DelimToken):
        if not isinstance(next_token, LParenToken):
            raise SchemeException("read: unexpected %s" % next_token.str)
        v = read_list(stream, next_token.str)
        if next_token.str in ["#(", "#[", "#{"]:
            return values_vector.W_Vector.fromelements(values.from_list(v))
        if next_token.str in ["#s("]:
            return values_struct.W_Struct.make_prefab(v.car(), values.from_list(v.cdr()))
        return v
    else:
        assert isinstance(next_token, values.W_Object)
        return next_token

def check_matches(s1, s2):
    assert (s1 == "(" and s2 == ")" or
            s1 == "[" and s2 == "]" or
            s1 == "{" and s2 == "}" or
            s1 == "#s(" and s2 == ")" or
            s1 == "#(" and s2 == ")" or
            s1 == "#[" and s2 == "]" or
            s1 == "#{" and s2 == "}")

def to_improper(l, curr, start=0):
    """
    This is the same code as values.to_improper but is needed to type check properly
    as values.to_improper only works for immutable lists.
    """
    assert start >= 0
    for i in range(len(l) - 1, start - 1, -1):
        curr = values.W_Cons.make(l[i], curr)
    return curr

def read_list(stream, end):
    so_far = newlist_hint(8)
    while True:
        next_token = read_token(stream)
        if next_token is dot_token:
            last = read_stream(stream)
            close = read_token(stream)
            if isinstance(close, RParenToken):
                check_matches(end, close.str)
                return to_improper(so_far, last)
            else:
                raise SchemeException("read: illegal use of `.`")
        elif isinstance(next_token, RParenToken):
            check_matches(end, next_token.str)
            return to_improper(so_far, values.w_null)
        elif isinstance(next_token, LParenToken):
            v = read_list(stream, next_token.str)
        elif isinstance(next_token, SpecialToken):
            arg = read_stream(stream)
            v = next_token.finish(arg)
        else:
            assert isinstance(next_token, values.W_Object)
            v = next_token
        so_far.append(v)

linefeed_sym = values.W_Symbol.make("linefeed")

@continuation
def do_read_line(mode, as_bytes, env, cont, _vals):
    # FIXME: respect mode
    from pycket.interpreter import return_value, check_one_val
    port = check_one_val(_vals)
    line = port.readline()
    stop = len(line) - 1
    if stop >= 0:
        # chomp
        if line[stop] == "\n":
            line = line[:stop]
        if as_bytes:
            return return_value(values.W_Bytes.from_string(line), env, cont)
        else:
            return return_value(values_string.W_String.fromstr_utf8(line), env, cont)
    else:
        return return_value(values.eof_object, env, cont)

@expose("read-line",[default(values.W_Object, None),
                     default(values.W_Symbol, linefeed_sym)],
                    simple=False)
def read_line(port, mode, env, cont):
    cont = do_read_line(mode, False, env, cont)
    return get_input_port(port, env, cont)


@expose("read-bytes-line", [default(values.W_Object, None),
                            default(values.W_Symbol, linefeed_sym)],
                           simple=False)
def read_bytes_line(w_port, w_mode, env, cont):
    cont = do_read_line(w_mode, True, env, cont)
    return get_input_port(w_port, env, cont)

@continuation
def custom_port_read_peek_cont(bstr, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    from pycket.env import w_global_config as glob

    read_peek_result = check_one_val(_vals) # this should be Fixnum(1)

    assert not isinstance(read_peek_result, values.W_Procedure) # For now

    return return_value(bstr.ref(0), env, cont)


@continuation
def do_read_one_cont(as_bytes, peek, env, cont, _vals):
    from pycket.interpreter import check_one_val
    w_port = check_one_val(_vals)

    if isinstance(w_port, values.W_CustomInputPort):
        w_port_or_proc = w_port.w_read_is_port()
        if isinstance(w_port_or_proc, values.W_InputPort):
            # redirect the read to the given port
            return do_read_one(w_port_or_proc, as_bytes, peek, env, cont)
        else:
            # w_port_or_proc is a procedure, so let's call it
            _bstr = [chr(0)]
            bstr = values.W_MutableBytes(_bstr)
            if peek:
                return w_port._call_peek(bstr, values.W_Fixnum.ZERO, values.w_false, env, custom_port_read_peek_cont(bstr, env, cont))
            else:
                return w_port._call_read_in(bstr,env,custom_port_read_peek_cont(bstr, env, cont))

    return do_read_one(w_port, as_bytes, peek, env, cont)

def utf8_code_length(i):
    if i < 0x80:
        return 1
    return ord(runicode._utf8_code_length[i - 0x80])

def do_read_one(w_port, as_bytes, peek, env, cont):
    from pycket.interpreter import return_value
    if peek:
        c = w_port.peek()
    else:
        c = w_port.read(1)

    if len(c) == 0:
        return return_value(values.eof_object, env, cont)

    i = ord(c[0])
    if as_bytes:
        return return_value(values.W_Fixnum(i), env, cont)
    else:
        # hmpf, poking around in internals
        needed = utf8_code_length(i)
        if peek:
            if w_port.is_stdin():
                c = c[:needed]
            else:
                old = w_port.tell()
                c = w_port.read(needed)
                w_port.seek(old)
        elif needed > 1:
            c += w_port.read(needed - 1)
        u = c.decode("utf-8")
        assert len(u) == 1
        return return_value(values.W_Character(u[0]), env, cont)

@expose("read-char-or-special", [values.W_Object, default(values.W_Object, values.w_false), default(values.W_Object, values.w_false)], simple=False)
def read_char_or_special(in_port, special_wrap, source_name, env, cont):
    try:
        cont = do_read_one_cont(False, False, env, cont)
        return get_input_port(in_port, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("read-char: string is not a well-formed UTF-8 encoding")

@expose("read-char", [default(values.W_Object, None)], simple=False)
def read_char(w_port, env, cont):
    try:
        cont = do_read_one_cont(False, False, env, cont)
        return get_input_port(w_port, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("read-char: string is not a well-formed UTF-8 encoding")

@expose("read-byte", [default(values.W_Object, None)], simple=False)
def read_byte(w_port, env, cont):
    try:
        cont = do_read_one_cont(True, False, env, cont)
        return get_input_port(w_port, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("read-byte: string is not a well-formed UTF-8 encoding")

@continuation
def do_peek_cont(as_bytes, skip, env, cont, _vals):
    from pycket.interpreter import check_one_val
    w_port = check_one_val(_vals)
    if isinstance(w_port, values.W_CustomInputPort):
        w_port_or_proc = w_port.w_read_is_port()
        if isinstance(w_port_or_proc, values.W_InputPort):
            # redirect the read to the given port
            return do_peek(w_port_or_proc, as_bytes, skip, env, cont)
        else:
            # w_port_or_proc is a procedure, so let's call it
            _bstr = [chr(0)]
            bstr = values.W_MutableBytes(_bstr)
            return w_port._call_peek(bstr, values.W_Fixnum.make(skip), values.w_false, env, custom_port_read_peek_cont(bstr, env, cont))

    return do_peek(w_port, as_bytes, skip, env, cont)

def do_peek(w_port, as_bytes, skip, env, cont):
    if skip == 0:
        return do_read_one(w_port, as_bytes, True, env, cont)
    else:
        # FIXME: put into port.
        old = w_port.tell()
        w_port.seek(old + skip)
        ret = do_read_one(w_port, as_bytes, True, env, cont)
        w_port.seek(old)
        return ret

@expose("peek-char-or-special", [default(values.W_Object, None),
                                 default(values.W_Fixnum, values.W_Fixnum.ZERO),
                                 default(values.W_Object, values.w_false),
                                 default(values.W_Object, values.w_false)],
        simple=False)
def peek_char_or_special(w_port, w_skip, special_wrap, source_name, env, cont):
    # FIXME: exactly same with peek-char
    try:
        cont = do_peek_cont(False, w_skip.value, env, cont)
        return get_input_port(w_port, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("peek-char: string is not a well-formed UTF-8 encoding")



@expose("peek-char", [default(values.W_Object, stdin_port),
                      default(values.W_Fixnum, values.W_Fixnum.ZERO)],
                    simple=False)
def peek_char(w_port, w_skip, env, cont):
    try:
        cont = do_peek_cont(False, w_skip.value, env, cont)
        return get_input_port(w_port, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("peek-char: string is not a well-formed UTF-8 encoding")

@expose("peek-byte", [default(values.W_Object, None),
                      default(values.W_Fixnum, values.W_Fixnum.ZERO)],
                    simple=False)
def peek_byte(w_port, w_skip, env, cont):
    try:
        cont = do_peek_cont(True, w_skip.value, env, cont)
        return get_input_port(w_port, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("peek-byte: string is not a well-formed UTF-8 encoding")

@continuation
def do_peek_bytes_cont(amt, skip, env, cont, _vals):
    from pycket.interpreter import check_one_val
    w_port = check_one_val(_vals)
    return do_peek_bytes(w_port, amt, skip, env, cont)

def do_peek_bytes(w_port, amt, skip, env, cont):
    from pycket.interpreter import return_value

    if amt == 0:
        return return_value(values.W_Bytes.from_string(""), env, cont)

    old = w_port.tell()
    w_port.seek(old + skip)

    res = w_port.read(amt)

    w_port.seek(old)

    if len(res) == 0:
        return return_value(values.eof_object, env, cont)

    return return_value(values.W_Bytes.from_string(res), env, cont)

@expose("peek-bytes", [values.W_Fixnum,
                       values.W_Fixnum,
                       default(values.W_InputPort, None)], simple=False)
def peek_bytes(w_amt, w_skip, w_port, env, cont):
    amt = w_amt.value
    skip = w_skip.value
    if amt < 0 or skip < 0:
        raise SchemeException("peek-bytes : expected non-negative integer for arguments : amt : %s - skip-bytes-amt : %s" % (amt, skip))

    try:
        cont = do_peek_bytes_cont(amt, skip, env, cont)
        return get_input_port(w_port, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("peek-byte: string is not a well-formed UTF-8 encoding")

w_text_sym   = values.W_Symbol.make("text")
w_binary_sym = values.W_Symbol.make("binary")
w_none_sym   = values.W_Symbol.make("none")
w_error_sym  = values.W_Symbol.make("error")

@expose("open-input-file", [values.W_Object,
                            default(values.W_Symbol, w_binary_sym),
                            default(values.W_Symbol, w_none_sym)])
def open_input_file(path, mode, mod_mode):
    if not isinstance(path, values_string.W_String) and not isinstance(path, values.W_Path):
        raise ContractException("open-input-file: expected path-string for argument 0")
    m = "r" if mode is w_text_sym else "rb"
    return open_infile(path, m)

w_error_sym = values.W_Symbol.make("error")
w_append_sym = values.W_Symbol.make("append")
w_update_sym = values.W_Symbol.make("update")
w_replace_sym = values.W_Symbol.make("replace")
w_truncate_sym = values.W_Symbol.make("truncate")
w_truncate_replace_sym = values.W_Symbol.make("truncate/replace")

@expose("open-output-file", [values.W_Object,
                             default(values.W_Symbol, w_binary_sym),
                             default(values.W_Symbol, w_error_sym),
                             default(values.W_Fixnum, values.W_Fixnum(0o666)),
                             default(values.W_Bool, values.w_false)])
def open_output_file(path, mode, exists, perms, replace_perms):
    if not isinstance(path, values_string.W_String) and not isinstance(path, values.W_Path):
        raise ContractException("open-input-file: expected path-string for argument 0")
    m = "w" if mode is w_text_sym else "wb"
    return open_outfile(path, m, exists, perms, replace_perms)

@expose("close-input-port", [values.W_Object], simple=False)
def close_input_port(port, env, cont):
    cont = close_port_cont(env, cont)
    return get_input_port(port, env, cont)

@continuation
def custom_port_close_cont(port, env, cont, _vals):
    from pycket.interpreter import return_multi_vals
    port.close()
    return return_multi_vals(_vals, env, cont)

@continuation
def close_port_cont(env, cont, _vals):
    from pycket.interpreter import check_one_val
    port = check_one_val(_vals)
    # FIXME : Call "w_close" if CustomPort
    if isinstance(port, values.W_CustomInputPort):
        return port._call_close(env, custom_port_close_cont(port, env, cont))
    try:
        port.close()
    except OSError as err:
        #import pdb; pdb.set_trace()
        raise FSException("close-*-port: cannot close port : %s %s" % (port,err.strerror))
    return return_void(env, cont)

@expose("close-output-port", [values.W_Object], simple=False)
def close_output_port(port, env, cont):
    cont = close_port_cont(env, cont)
    return get_output_port(port, env, cont)

@expose("port-closed?", [values.W_Port])
def port_closedp(p):
    return values.W_Bool.make(p.closed)

@expose("eof-object?", [values.W_Object])
def eofp(e):
    return values.W_Bool.make(e is values.eof_object)

def extract_path(obj):

    if isinstance(obj, values_string.W_String):
        try:
            result = obj.as_str_utf8()
        except UnicodeDecodeError as e:
            msg = str(e)
            raise SchemeException(msg)
    elif isinstance(obj, values.W_Path):
        result = obj.path
    elif isinstance(obj, values.W_Bytes):
        result = obj.as_str()
    else:
        raise ContractException("expected path-like values but got %s" % obj.tostring())
    return result if result is not None else "."

@expose("directory-exists?", [values.W_Object])
def directory_exists(w_str):
    s = extract_path(w_str)
    return values.W_Bool.make(os.path.isdir(s))

@expose("make-directory", [values.W_Object])
def make_directory(p):
    s = extract_path(p)
    if os.path.isdir(s):
        raise FSException("make-directory: cannot make directory; path already exists : %s" % s)

    try:
        os.mkdir(s)
    except OSError:
        raise FSException("make-directory: cannot make directory : %s" % s)

    return values.w_void

@expose("file-exists?", [values.W_Object])
def file_exists(w_str):
    s = extract_path(w_str)
    return values.W_Bool.make(os.path.isfile(s))

@expose("file-or-directory-modify-seconds", [values.W_Object, default(values.W_Object, None), default(values.W_Object, None)], simple=False)
def file_or_dir_mod_seconds(w_path, secs_n, fail, env, cont):
    from pycket.prims.general import detect_platform, w_windows_sym
    from pycket.interpreter import return_value

    platform = detect_platform()
    if platform is w_windows_sym:
        raise SchemeException("Not yet implemented")

    path_str = extract_path(w_path)

    if not os.path.isdir(path_str) and not os.path.isfile(path_str):
        if fail is not None:
            return fail.call([], env, cont)
        else:
            raise FSException("No such file or directory exists : %s" % path_str)

    # secs_n can also be w_false
    if secs_n is not None and isinstance(secs_n, values.W_Fixnum):
        # Set the access and modify times of path to the given time
        t = secs_n.toint() # seconds
        os.utime(path_str, (t,t))
        return return_void(env, cont)
    else:
        # Get the last modification date of path (in seconds)
        m_time = int(os.path.getmtime(path_str))
        return return_value(values.W_Fixnum(m_time), env, cont)

@expose("sync", arity=Arity.geq(1))
def sync(args):
    # FIXME : actually check if any event is ready
    return args[0]

@expose("sync/timeout", simple=False)
def sync_timeout(args, env, cont):
    from pycket.interpreter import return_value
    w_timeout = args[0]
    w_events = args[1:]

    # FIXME : actually check if any event is ready

    if isinstance(w_timeout, values.W_Number):
        # import time
        # time.sleep(w_timeout.value)
        return return_value(values.w_false, env, cont)
    elif isinstance(w_timeout, values.W_Procedure) and w_timeout.iscallable():
        return w_timeout.call([], env, cont)
    else:
        raise Exception("Unsupported timeout type : %s" % w_timeout.tostring())

@expose("directory-list", [values.W_Object])
def dir_list(w_str):
    s = extract_path(w_str)
    dir = [values.W_Path(p) for p in os.listdir(s)]
    return values.to_list(dir)

UP = values.W_Symbol.make("up")
SAME = values.W_Symbol.make("same")
RELATIVE = values.W_Symbol.make("relative")
ROOT = SEP = values.W_Path(os.sep)

def _explode_element(s):
    if not s:
        return SEP
    if s == ".":
        return SAME
    if s == "..":
        return UP
    return values.W_Path(s)

@expose("explode-path", [values.W_Object])
def explode_path(w_path):
    sep = os.sep
    path = extract_path(w_path)
    parts = [_explode_element(p) for p in path.split(sep) if p]
    if path[0] == sep:
        return values.W_Cons.make(SEP, values.to_list(parts))
    else:
        return values.to_list(parts)

def _strip_path_seps(path):
    i = len(path)
    while i > 0 and path[i-1] == os.path.sep:
        i -= 1
    assert i >= 0
    return path[:i]

def _dirname(path):
    path = _strip_path_seps(path)
    components = path.split(os.path.sep)[:-1]
    if components == ['']:
        return os.path.sep
    return os.path.sep.join(components)

def _basename(path):
    path = _strip_path_seps(path)
    components = path.split(os.path.sep)
    return components[-1]

def _must_be_dir(path):
    return values.W_Bool.make(bool(path) and path[-1] == os.path.sep)

def _split_path(path):
    dirname  = _dirname(path)
    basename = _basename(path)
    name = _explode_element(basename)
    if dirname == os.path.sep:
        base = values.W_Path(os.path.sep)
        must_be_dir = _must_be_dir(path)
    elif dirname == '' and basename == '':
        base = values.w_false
        must_be_dir = values.w_true
    elif dirname == '':
        if basename == '':
            base = values.w_false
        else:
            base = RELATIVE
        if name is UP or name is SAME:
            must_be_dir = values.w_true
        else:
            must_be_dir = _must_be_dir(path)
    elif basename == '':
        base = RELATIVE
        second_name = _explode_element(dirname)
        if second_name is UP or second_name is SAME:
            name = second_name
        else:
            name = values.W_Path(dirname + os.path.sep)
        must_be_dir = values.w_true
    elif basename == ".." or basename == ".":
        base = values.W_Path(dirname + os.path.sep)
        must_be_dir = values.w_true
    else:
        base = values.W_Path(dirname + os.path.sep)
        must_be_dir = values.w_false
    return base, name, must_be_dir

@expose("split-path", [values.W_Object], simple=False)
def split_path(w_path, env, cont):
    from pycket.interpreter import return_multi_vals
    path = extract_path(w_path)
    base, name, must_be_dir = _split_path(path)
    result = values.Values.make([base, name, must_be_dir])
    return return_multi_vals(result, env, cont)


def build_path(args):
    # XXX Does not check that we are joining absolute paths
    # Sorry again Windows
    if not args:
        raise ArityException("build-path: expected at least 1 argument")
    result = [None] * len(args)
    for i, s in enumerate(args):
        if s is UP:
            part = ".."
        elif s is SAME:
            part = "."
        else:
            part = extract_path(s)
        if not part:
            raise ContractException("build-path: path element is empty")
        if part == os.path.sep:
            part = ""
        result[i] = part

    path = os.path.sep.join(result)

    if not path:
        return ROOT

    return values.W_Path(path)

expose("build-path")(build_path)

@expose("simplify-path", [values.W_Object, default(values.W_Bool, values.w_false)])
def simplify_path(path, use_filesystem):
    path_str = os.path.normpath(extract_path(path))
    return values.W_Path(path_str)

@expose("path<?", [values.W_Path, values.W_Path])
def path_less_than(p1, p2):
    return values.W_Bool.make(p1.path < p2.path)

@expose("use-user-specific-search-paths", [])
def use_user_specific_search_paths():
    return values.w_false

@expose("path->complete-path", [values.W_Object, default(values.W_Object, None)])
def path_to_path_complete_path(path, _base):
    if _base is None:
        base = os.getcwd()
    else:
        base = extract_path(_base)
    p = extract_path(path)
    if p and p[0] == os.path.sep:
        return values.W_Path(p)
    return values.W_Path(base + os.path.sep + p)

@expose("path->directory-path", [values.W_Object])
def path_to_path_complete_path(path):
    p = extract_path(path)
    if p and p[-1] == os.path.sep:
        return values.W_Path(p)
    return values.W_Path(p + os.path.sep)

@expose("path-convention-type", [values.W_Path])
def path_convention_type(path):
    from pycket.prims.general import detect_platform, w_macosx_sym, w_unix_sym
    platform = detect_platform()
    if platform is w_macosx_sym:
        platform = w_unix_sym
    return platform

def _path_for_some_systemp(path):
    # XXX Really only handles UNIX paths
    # https://github.com/racket/racket/blob/827fc4559879c73d46268fc72f95efe0009ff905/racket/src/racket/include/scheme.h#L493
    # This seems to be the closest implementation we can achieve.
    return isinstance(path, values.W_Path)

@expose("path-for-some-system?", [values.W_Object])
def path_for_some_systemp(path):
    return values.W_Bool.make(_path_for_some_systemp(path))

@expose("relative-path?", [values.W_Object], simple=False)
def relative_path(obj, env, cont):
    from pycket.interpreter import return_value
    string = extract_path(obj)
    return return_value(values.W_Bool.make(not os.path.isabs(string)), env, cont)

@expose("absolute-path?", [values.W_Object])
def absolute_path(obj):
    string = extract_path(obj)
    return values.W_Bool.make(os.path.isabs(string))

@expose("resolve-path", [values.W_Object])
def resolve_path(obj):
    if (not isinstance(obj, values_string.W_String) and
        not isinstance(obj, values.W_Path)):
        raise ContractException("resolve-path: expected path-string")
    str = extract_path(obj)
    return values.W_Path(os.path.normpath(str))

@expose("path-string?", [values.W_Object])
def path_stringp(v):
    # FIXME: handle zeros in string
    return values.W_Bool.make(
        isinstance(v, values_string.W_String) or isinstance(v, values.W_Path))

@expose("complete-path?", [values.W_Object])
def complete_path(v):
    if not isinstance(v, values_string.W_String) and not isinstance(v, values.W_Path):
        raise ContractException("complete-path?: expected a path? or path-string?")

    path_str = extract_path(v)
    if path_str[0] == os.path.sep:
        return values.w_true
    return values.w_false

@expose("expand-user-path", [values.W_Object])
def expand_user_path(p):
    if isinstance(p, values.W_Path):
        path_str = p.path
    elif isinstance(p, values_string.W_String):
        path_str = p.as_escaped_utf8()
    else:
        raise ContractException("expand_user_path expects a string or a path")

    if "~" in path_str:
        if os.environ.get('HOME') is None:
            raise Exception("HOME is not found among the os environment variables")

        home_dir = os.environ.get('HOME')
        # assumes a) there's exactly one ~ and b) nothing is there behind the ~
        path_str = home_dir.join(path_str.split("~"))

    return values.W_Path(path_str)

@expose("path->string", [values.W_Path])
def path2string(p):
    return values_string.W_String.fromstr_utf8(p.path)

@expose("path->bytes", [values.W_Path])
def path2bytes(p):
    return values.W_Bytes.from_string(p.path)

@expose("cleanse-path", [values.W_Object])
def cleanse_path(p):
    if isinstance(p, values_string.W_String):
        return values.W_Path(p.as_str_ascii())
    if isinstance(p, values.W_Path):
        return p
    raise ContractException("cleanse-path expects string or path")

def _path_elementp(p):
    """
    see path.rkt
    (define (path-element? path)
      (and (path-for-some-system? path)
           (let-values ([(base name d?) (split-path path)])
             (and (eq? base 'relative)
                  (path-for-some-system? name)))))
    """
    if not _path_for_some_systemp(p):
        return False
    path = extract_path(p)
    base, name, _ = _split_path(path)
    return base is RELATIVE and _path_for_some_systemp(name)

@expose("path-element->string", [values.W_Object])
def path_element2string(p):
    if not _path_elementp(p):
        raise ContractException("path-element->string expects path, got %s"%p.tostring())
    path = extract_path(p)
    return values_string.W_String.fromstr_utf8(path)

@expose("path-element->bytes", [values.W_Object])
def path_element2bytes(p):
    if not _path_elementp(p):
        raise ContractException("path-element->bytes expects path, got %s"%p.tostring())
    path = extract_path(p)
    return values.W_Bytes.from_string(path)

@continuation
def close_cont(port, env, cont, vals):
    from pycket.interpreter import return_multi_vals
    port.close()
    return return_multi_vals(vals, env, cont)

def open_infile(w_str, mode):
    s = extract_path(w_str)
    if not os.path.exists(s):
        raise FSException("No such file or directory : %s" % s)

    return values.W_FileInputPort(sio.open_file_as_stream(s, mode=mode, buffering=2**21), path=os.path.abspath(s))

def open_outfile(w_str, mode, exists, w_perms=values.W_Fixnum(0o666), w_replace_perms=values.w_false):
    # TODO (cderici-10-10-2024): w_perms and w_replace_perms are ignored
    s = extract_path(w_str)
    if exists is w_error_sym and os.path.exists(s):
        raise FSException("File exists : %s" % s)
    if not os.path.exists(s):
        # then touch the file
        # it's not clear to me at the moment how to create while
        # opening the file through the rlib.streamio
        try:
            open(s, 'a').close()
        except OSError:
            raise FSException("open-output-file : cannot open file : %s" % s)

    # FIXME : handle different exists modes (e.g. replace)
    return values.W_FileOutputPort(sio.open_file_as_stream(s, mode=mode), path=os.path.abspath(s))

@expose("file-or-directory-type", [values.W_Object, default(values.W_Object, values.w_false)])
def file_or_directory_type(path, must_exist):
    # (or/c 'file 'directory 'link 'directory-link #f)
    p = extract_path(path)
    if not os.path.exists(p):
        if must_exist is values.w_false:
            return values.w_false
        else:
            raise FSException("file-or-directory-type: access failed\n path: %s" % p)
    if os.path.isfile(p):
        return values.W_Symbol.make('file')
    if os.path.isdir(p):
        return values.W_Symbol.make('directory')
    if os.path.islink(p):
        return values.W_Symbol.make('link')
    # FIXME : add directory-link for Windows junctions etc

@expose("rename-file-or-directory", [values.W_Object, values.W_Object, default(values.W_Object, values.w_false)])
def rename_file_or_directory(o, n, exists_ok):

    old = extract_path(o)
    new = extract_path(n)

    # Unless exists-ok? is provided as a true value, new cannot refer
    # to an existing file or directory, but the check is not atomic
    # with the rename operation on Unix and Mac OS. Even if exists-ok?
    # is true, new cannot refer to an existing file when old is a
    # directory, and vice versa.

    if exists_ok is values.w_false and os.path.exists(new):
        raise FSException("%s already exists" % new)

    if exists_ok is not values.w_false:
        if os.path.isdir(old) and os.path.isfile(new):
            raise FSException("%s is an existing file while %s is a directory" % (new, old))

        if os.path.isdir(new) and os.path.isfile(old):
            raise FSException("%s is an existing file while %s is a directory" % (old, new))

    try:
        os.rename(old, new)
    except OSError:
        raise FSException("rename-file-or-directory : cannot move file : %s to %s" % (old, new))

    return values.w_void

@expose("delete-file", [values.W_Object])
def delete_file(p):
    from pycket.prims.general import exn_fail_fs

    path = extract_path(p)
    if not os.path.exists(path):
        raise FSException("No such file : %s" % (path))

    try:
        os.remove(path)
    except OSError:
        raise FSException("cannot remove file : %s" % (path))

    return values.w_void

@expose("call-with-input-file", [values.W_Object,
                                 values.W_Object,
                                 default(values.W_Symbol, w_binary_sym)],
                                simple=False)
def call_with_input_file(s, proc, mode, env, cont):
    m = "r" if mode is w_text_sym else "rb"
    port = open_infile(s, m)
    return proc.call([port], env, close_cont(port, env, cont))

@expose("call-with-output-file", [values.W_Object,
                                  values.W_Object,
                                  default(values.W_Symbol, w_binary_sym),
                                  default(values.W_Symbol, w_error_sym),
                                  default(values.W_Fixnum, values.W_Fixnum(0o666)),
                                  default(values.W_Bool, values.w_false)],
                                simple=False)
def call_with_output_file(path, proc, mode, exists, perms, replace_perms, env, cont):
    m = ""
    if exists is w_append_sym:
        m += "a"
    elif exists is w_truncate_sym or w_truncate_replace_sym:
        m += "w"
    else:
        raise SchemeException("mode not yet supported: %s" % exists.tostring())
    if mode is not w_text_sym:
        m += "b"
    port = open_outfile(path, m, exists, perms, replace_perms)
    return proc.call([port], env, close_cont(port, env, cont))

@expose("with-input-from-file", [values.W_Object, values.W_Object,
                                 default(values.W_Symbol, w_binary_sym)],
        simple=False)
def with_input_from_file(s, proc, mode, env, cont):
    from pycket.prims.parameter import call_with_extended_paramz
    m = "rb" if mode is w_binary_sym else "r"
    port = open_infile(s, m)
    return call_with_extended_paramz(proc, [], [current_in_param], [port],
                                     env, close_cont(port, env, cont))

@expose("with-output-to-file",
        [values_string.W_String, values.W_Object,
         default(values.W_Object, None),
         default(values.W_Object, None),
         default(values.W_Fixnum, values.W_Fixnum(0o666)),
         default(values.W_Bool, values.w_false)], simple=False)
def with_output_to_file(s, proc, mode, exists, perms, replace_perms, env, cont):
    # XXX mode and exists are currently ignored, they need to be translated into
    # the proper mode string.
    from pycket.prims.parameter import call_with_extended_paramz
    port = open_outfile(s, "wb", exists, perms, replace_perms)
    return call_with_extended_paramz(proc, [], [current_out_param], [port],
                                     env, close_cont(port, env, cont))


@expose("file-position")
def file_position(args):
    if len(args) == 1:
        w_port = args[0]
        assert isinstance(w_port, values.W_Port)
        told = w_port.tell()
        assert told >= 0
        return values.W_Integer.frombigint(
            rbigint.fromint(told))
    elif len(args) == 2:
        w_port = args[0]
        assert isinstance(w_port, values.W_Port)
        w_offset = args[1]
        if isinstance(w_offset, values.W_Fixnum):
            assert w_offset.value >= 0
            w_port.seek(w_offset.value)
        elif isinstance(w_offset, values.W_Bignum):
            # XXX this means we can only deal with 4GiB files on 32bit systems
            v = w_offset.value.toint()
            w_port.seek(v)
        elif w_offset is values.eof_object:
            w_port.seek(0, end=True)
        else:
            assert 0
        return values.w_void


    raise ContractException(
        "printf expected one or two arguments, got %s" % len(args))

###############################################################################
@expose("display", [values.W_Object, default(values.W_OutputPort, None)], simple=False)
def display(datum, out, env, cont):
    if isinstance(datum, values.W_Bytes):
        bytes = datum.as_bytes_list()
        port = current_out_param.get(cont) if out is None else out
        write_bytes_avail(bytes, port , 0, len(bytes))
        return return_void(env, cont)
    return do_print(datum.tostring(), out, env, cont)

@expose("newline", [default(values.W_OutputPort, None)], simple=False)
def newline(out, env, cont):
    return do_print("\n", out, env, cont)

@expose("write", [values.W_Object, default(values.W_OutputPort, None)], simple=False)
def write(o, p, env, cont):
    from pycket.values_struct import w_prop_custom_write, W_Struct

    if isinstance(o, W_Struct):
        w_custom_writer = o.struct_type().read_property(w_prop_custom_write)
        if w_custom_writer is not None and w_custom_writer.iscallable():
            if not p:
                p = current_out_param.get(cont)
            return w_custom_writer.call([o, p, values.w_true], env, cont)

    cont = do_write_cont(o, env, cont)
    return get_output_port(p, env, cont)

@continuation
def do_write_cont(o, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    port = check_one_val(_vals)
    assert isinstance(port, values.W_OutputPort)
    write_loop(o, port, env)
    return return_void(env, cont)

def write_linklet_bundle(lb, port, env):
    from pycket.racket_entry import get_primitive
    from pycket.prims.linklet import ast_to_sexp
    from pycket.util import console_log, PerfRegion

    s_exp_to_fasl = get_primitive("s-exp->fasl")

    console_log("BUNDLE AST TO BE SERIALIZED: %s" % lb.tostring(), 8)

    with PerfRegion("ast_to_sexp"):
        bundle_s_exp = ast_to_sexp(lb)

    console_log("WRITING BUNDLE SEXP : %s" % bundle_s_exp.tostring(), 8)

    with PerfRegion("s-exp->fasl"):
        s_exp_to_fasl.call_interpret([bundle_s_exp, port, values.w_false])

def write_linklet_directory(ld, port, env):
    from pycket.racket_entry import get_primitive
    from pycket.prims.linklet import ast_to_sexp
    from pycket.util import console_log, PerfRegion

    s_exp_to_fasl = get_primitive("s-exp->fasl")

    console_log("DIRECTORY AST TO BE SERIALIZED: %s" % ld.tostring(), 8)

    with PerfRegion("ast_to_sexp"):
        directory_s_exp = ast_to_sexp(ld)

    console_log("WRITING DIRECTORY : %s" % directory_s_exp.tostring(), 8)

    with PerfRegion("s-exp->fasl"):
        s_exp_to_fasl.call_interpret([directory_s_exp, port, values.w_false])

def write_linklet(v, port, env):
    from pycket.util import console_log
    console_log(v.tostring(), 2)
    port.write("(linklet")
    port.write(" ")
    write_loop(v.name, port, env)
    port.write(" ")
    port.write("(")
    importss = v.importss
    for imp_group in importss:
        port.write("(")
        for imp_obj in imp_group:
            port.write("(")
            write_loop(imp_obj.ext_id, port, env)
            port.write(" . ")
            write_loop(imp_obj.int_id, port, env)
            port.write(")")
        port.write(")")
    port.write(")")
    port.write(" ")
    port.write("(")
    exports = v.exports
    for exp_sym, exp_obj in exports.iteritems():
        port.write("(")
        write_loop(exp_sym, port, env)
        port.write(" ")
        write_loop(exp_obj.ext_id, port, env)
        port.write(")")
    port.write(")")

    forms = v.forms
    for form in forms:
        port.write(" ")
        form.write(port, env)
    port.write(")")

def write_hash_table(v, port, env):
    from pycket.prims.linklet import is_bundle, is_directory, W_Linklet
    ht = v
    if isinstance(v, imp.W_ImpHashTable) or isinstance(v, imp.W_ChpHashTable):
        ht = v.get_proxied()

    if isinstance(ht, W_EqvImmutableHashTable):
        port.write("#hasheqv(")
        for k, v in ht.iteritems():
            port.write("(")
            write_loop(k, port, env)
            port.write(" . ")
            write_loop(v, port, env)
            port.write(")")
        port.write(")")
    elif isinstance(ht, W_EqImmutableHashTable):
        port.write("#hasheq(")
        for k, v in ht.iteritems():
            port.write("(")
            write_loop(k, port, env)
            port.write(" . ")
            if is_bundle(v):
                write_linklet_bundle(v, port, env)
            elif is_directory(v):
                write_linklet_directory(v, port, env)
            else:
                write_loop(v, port, env)
            port.write(")")
        port.write(")")
    elif isinstance(ht, W_EqMutableHashTable):
        port.write("#hasheq(")
        for k, v in ht.data.iteritems():
            port.write("(")
            write_loop(k, port, env)
            port.write(" . ")
            write_loop(v, port, env)
            port.write(")")
        port.write(")")
    elif isinstance(ht, W_EqvMutableHashTable):
        port.write("#hasheqv(")
        for k, v in ht.data.iteritems():
            port.write("(")
            write_loop(k, port, env)
            port.write(" . ")
            write_loop(v, port, env)
            port.write(")")
        port.write(")")
    elif isinstance(ht, W_EqualHashTable):
        port.write("#hash(")
        for k, v in ht.hash_items():
            port.write("(")
            write_loop(k, port, env)
            port.write(" . ")
            if is_bundle(v):
                write_linklet_bundle(v, port, env)
            elif is_directory(v):
                write_linklet_directory(v, port, env)
            else:
                write_loop(v, port, env)
            port.write(")")
        port.write(")")
    elif isinstance(ht, W_EqualAlwaysHashTable):
        port.write("#hashalw(")
        for k, v in ht.hash_items():
            port.write("(")
            write_loop(k, port, env)
            port.write(" . ")
            if is_bundle(v):
                write_linklet_bundle(v, port, env)
            elif is_directory(v):
                write_linklet_directory(v, port, env)
            else:
                write_loop(v, port, env)
            port.write(")")
        port.write(")")

def write_loop(v, port, env):
    from pycket.prims.linklet import is_bundle, is_directory, W_Linklet
    from pycket.vector import W_Vector
    from pycket.values_struct import W_Struct

    if isinstance(v, values.W_Cons):
        cur = v
        port.write("(")
        while isinstance(cur, values.W_Cons):
            write_loop(cur.car(), port, env)
            cur = cur.cdr()
            if isinstance(cur, values.W_Cons):
                # there will be more elements
                port.write(" ")

        # Are we a dealing with a proper list?
        if cur is values.w_null:
            port.write(")")
        else:
            port.write(" . ")
            write_loop(cur, port, env)
            port.write(")")
    elif isinstance(v, values.W_MBox):
        port.write("#&")
        write_loop(v.value, port, env)
    elif isinstance(v, values.W_IBox):
        port.write("#&")
        write_loop(v.value, port, env)

    elif isinstance(v, values.W_MCons):
        port.write("{")
        write_loop(v.car(), port, env)
        port.write(" . ")
        write_loop(v.cdr(), port, env)
        port.write("}")

    elif isinstance(v, W_Vector):
        items = v.get_strategy().ref_all(v)
        port.write("#(")
        for obj in items:
            port.write(" ")
            write_loop(obj, port, env)
        port.write(")")

    elif isinstance(v, values.W_Character):
        #from rpython.rlib import runicode

        if v.value == u'\x00':
            port.write("#\\nul")
        elif v.value == u'\x08':
            port.write("#\\backspace")
        elif v.value == u'\t':
            port.write("#\\tab")
        elif v.value == u'\n':
            port.write("#\\newline")
        elif v.value == u'\x0b':
            port.write("#\\vtab")
        elif v.value == u'\x0c':
            port.write("#\\page")
        elif v.value == u'\r':
            port.write("#\\return")
        elif v.value == u' ':
            port.write("#\\space")
        elif v.value == u'\x7f':
            port.write("#\\rubout")


        else:
            port.write(v.tostring())

    elif isinstance(v, values_string.W_String):
        from pypy.objspace.std.bytesobject import string_escape_encode
        port.write(string_escape_encode(v.as_str_utf8(), '"'))

    elif isinstance(v, values.W_Bytes):
        port.write(v.tostring()) # FIXME: need to encode special chars
    elif isinstance(v, values.W_Symbol):
        s = v.tostring()
        if v.is_bar_quoted():
            s = "|%s|" % s
        port.write(s) # FIXME: handle special chars
    elif isinstance(v, W_HashTable):
        write_hash_table(v, port, env)

    elif isinstance(v, W_Linklet):
        write_linklet(v, port, env)

    elif is_bundle(v):
        from pycket.env import w_version
        port.write("#~")
        version = w_version.get_version()
        len_version = len(version)
        assert len_version < 10
        port.write("%s%s" % (len_version, version))
        write_linklet_bundle(v, port, env)

    elif is_directory(v):
        from pycket.env import w_version
        port.write("#~")
        version = w_version.get_version()
        len_version = len(version)
        assert len_version < 10
        port.write("%s%s" % (len_version, version))
        write_linklet_directory(v, port, env)

    elif isinstance(v, W_Struct):
        v.write(port, env)

    elif isinstance(v, values.W_Path):
        v.write(port, env)
    else:
        port.write(v.tostring())

@expose("print", [values.W_Object, default(values.W_OutputPort, None)], simple=False)
def _print(o, p, env, cont):
    return do_print(o.tostring(), p, env, cont)

def do_print(str, port, env, cont):
    cont = do_print_cont(str, env, cont)
    return get_output_port(port, env, cont)

@continuation
def do_print_cont(str, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    port = check_one_val(_vals)
    assert isinstance(port, values.W_OutputPort)
    port.write(str)
    return return_void(env, cont)

# XXX: Might need to be careful with this heuristic due to mutable strings, but
# mutable strings are unlikely to be constant, as they are not interned.
@jit.look_inside_iff(lambda form, vals, name: jit.isconstant(form))
def format(form, vals, name):
    fmt = form.as_str_utf8() # XXX for now
    i = 0
    j = 0
    result = []
    len_fmt = len(fmt)
    while True:
        start = i
        while i < len_fmt:
            if fmt[i] == '~':
                break
            i += 1
        else:
            # not left via break, so we're done
            result.append(fmt[start:len_fmt])
            break
        result.append(fmt[start:i])
        if i+1 == len_fmt:
            raise ContractException(name + ": bad format string")
        s = fmt[i+1]
        if (s == '.'):
            i += 1
            s = fmt[i+1]
            if (s == 'a' or # turns into switch
                s == 'A' or
                s == 's' or
                s == 'S' or
                s == 'v' or
                s == 'V'):
                #FIXME: use error-print-width
                if j >= len(vals):
                    raise ContractException(name + ": not enough arguments for format string")
                if isinstance(vals[j], values_string.W_String):
                    result.append(vals[j].as_escaped_utf8())
                elif isinstance(vals[j], values.W_Character):
                    result.append(vals[j].get_value_utf8())
                else:
                    result.append(vals[j].tostring())
                j += 1
        elif (s == 'a' or # turns into switch
            s == 'A' or
            s == 's' or
            s == 'S' or
            s == 'x' or
            s == 'X' or
            s == 'b' or
            s == 'B' or
            s == 'c' or
            s == 'C' or
            s == 'o' or
            s == 'O' or
            s == 'v' or
            s == 'V' or
            s == 'e' or
            s == 'E' or
            s == '.'):
            if j >= len(vals):
                raise ContractException(name + ": not enough arguments for format string")
            if isinstance(vals[j], values_string.W_String):
                result.append(vals[j].as_escaped_utf8())
            elif isinstance(vals[j], values.W_Character):
                result.append(vals[j].get_value_utf8())
            else:
                result.append(vals[j].tostring())
            j += 1
        elif s == 'n' or s == '%':
            result.append("\n") # newline
        elif s == '~':
            result.append("~")
        else:
            raise ContractException("%s: undexpected format character '%s'" % (name, s))
        i += 2
    if j != len(vals):
        raise ContractException(name + ": not all values used")
    return "".join(result)

@expose("printf", simple=False)
def printf(args, env, cont):
    if not args:
        raise ArityException("printf: expected at least one argument, got 0")
    fmt = args[0]
    if not isinstance(fmt, values_string.W_String):
        raise ContractException("printf: expected a format string, got something else")
    return do_print(format(fmt, args[1:], "printf"), None, env, cont)

@expose("eprintf", simple=False)
def eprintf(args, env, cont):
    if not args:
        raise ArityException("eprintf: expected at least one argument, got 0")
    fmt = args[0]
    if not isinstance(fmt, values_string.W_String):
        raise ContractException("eprintf: expected a format string, got something else")
    return do_print(format(fmt, args[1:], "eprintf"), current_error_param.get(cont), env, cont)

@expose("format")
def do_format(args):
    if len(args) == 0:
        raise ContractException("format: expects format string")
    fmt = args[0]
    if not isinstance(fmt, values_string.W_String):
        raise ContractException("format: expected a format string, got something else")
    vals = args[1:]
    return values_string.W_String.fromstr_utf8(format(fmt, vals, "format"))

@expose("fprintf", simple=False)
def do_fprintf(args, env, cont):
    out, form, v = args[0], args[1], args[2:]
    assert isinstance(out, values.W_OutputPort)
    assert isinstance(form, values_string.W_String)
    out.write(format(form, v, "fprintf"))
    return return_void(env, cont)

# Why is this different than format/fprintf?
# @expose("printf", simple=False)
# def do_printf(args, env, cont):
#     port = current_out_param.get(cont)
#     return do_fprintf([port] + args, env, cont)

def return_void(env, cont):
    from pycket.interpreter import return_value
    return return_value(values.w_void, env, cont)

@expose("flush-output", [default(values.W_OutputPort, None)], simple=False)
def flush_output(port, env, cont):
    if port is None:
        port = current_out_param.get(cont)
    port.flush()
    return return_void(env, cont)

def cur_print_proc(args, env, cont, extra_call_info):
    from pycket.interpreter import return_value
    v = args[0]
    port = current_out_param.get(cont)
    assert isinstance(port, values.W_OutputPort)
    if v is not values.w_void:
        port.write(v.tostring())
        port.write("\n")
    return return_void(env, cont)

standard_printer = values.W_Prim("current-print", cur_print_proc)
current_print_param = values_parameter.W_Parameter(standard_printer)
expose_val("current-print", current_print_param)

@expose(["open-output-string", "open-output-bytes"], [])
def open_output_string():
    # FIXME: actual implementation for bytes and string
    return values.W_StringOutputPort()

string_sym  = values.W_Symbol.make("string")

@expose("open-input-bytes", [values.W_Bytes, default(values.W_Symbol, string_sym)])
def open_input_bytes(bstr, name):
    # FIXME: name is ignore
    return values.W_StringInputPort(bstr.as_str())

@expose("open-input-string", [values_string.W_String, default(values.W_Symbol, string_sym)])
def open_input_string(w_str, name):
    # FIXME: name is ignore
    return values.W_StringInputPort(w_str.as_str_utf8())

@expose("get-output-string", [values.W_StringOutputPort])
def get_output_string(w_port):
    return values_string.W_String.fromascii(w_port.contents()) # XXX

@expose("get-output-bytes", [values.W_StringOutputPort,
                             default(values.W_Object, values.w_false),
                             default(values.W_Fixnum, values.W_Fixnum.ZERO),
                             default(values.W_Object, values.w_false)])
def get_output_bytes(w_port, reset_huh, start_pos, end_pos):
    if start_pos is not values.W_Fixnum.ZERO or end_pos is not values.w_false:
        raise SchemeException("get-output-bytes : handle start-pos and end-pos arguments")
    reset = True if reset_huh is not values.w_false else False
    return values.W_Bytes.from_string(w_port.contents(reset),
                                      immutable=False) # XXX

# FIXME: implementation
@expose("make-output-port",
        [values.W_Object, values.W_Object,
         values.W_Object, values.W_Object,
         default(values.W_Object, None), default(values.W_Object, None),
         default(values.W_Object, None), default(values.W_Object, None),
         default(values.W_Object, None), default(values.W_Object, None),
         default(values.W_Object, None)])
def make_output_port(name, evt, write_out, close, write_out_special,
                     get_write_evt, get_write_special_evt, get_location,
                     count_lines, init_position, buffer_mode):
    return values.W_StringOutputPort()

# FIXME: implementation
@expose("port-display-handler", [values.W_OutputPort, default(procedure, None)])
def port_display_handler(out, proc):
    return standard_printer

# FIXME: implementation
@expose("port-write-handler", [values.W_OutputPort, default(procedure, None)])
def port_write_handler(out, proc):
    return standard_printer

# FIXME: implementation
@expose("port-print-handler", [values.W_OutputPort, default(procedure, None)])
def port_print_handler(out, proc):
    return standard_printer

# FIXME: implementation
@expose("port-count-lines!", [values.W_Port])
def port_count_lines_bang(p):
    return values.w_void

# FIXME: implementation
@expose("port-counts-lines?", [values.W_Object])
def port_count_lines_huh(p):
    from pycket.prims.general import struct_port_huh
    if not isinstance(p, values.W_Port) and \
        (isinstance(p, values_struct.W_RootStruct) and not struct_port_huh(p)):
        raise ContractException("port-counts-lines?: expected port, got : %s" % p.tostring())

    return values.w_true

def is_path_string(path):
    return isinstance(path, values.W_Path) or isinstance(path, values_string.W_String)

@expose("file-size", [values.W_Object])
def file_size(obj):
    if not is_path_string(obj):
        raise ContractException("file-size: expected path string")
    path = extract_path(obj)
    try:
        size = os.path.getsize(path)
    except OSError:
        raise FSException("file-size: file %s does not exists" % path)

    intsize = intmask(size)
    if intsize == size:
        return values.W_Fixnum(intsize)
    return values.W_Bignum(rbigint.fromrarith_int(size))

@expose("read-bytes", [values.W_Fixnum, default(values.W_InputPort, None)],
        simple=False)
def read_bytes(amt, w_port, env, cont):
    from pycket.interpreter import return_value

    n = amt.value
    if n < 0:
        raise ContractException("read-bytes: expected non-negative integer for argument 0")
    if n == 0:
        return return_value(values.W_Bytes.from_string(""), env, cont)

    if w_port is None:
        w_port = current_in_param.get(cont)

    res = w_port.read(n)
    reslen = len(res)

    if reslen == 0:
        return return_value(values.eof_object, env, cont)

    return return_value(values.W_Bytes.from_string(res), env, cont)

@expose(["read-bytes!", "read-bytes-avail!"],
        [values.W_Bytes, default(values.W_InputPort, None),
         default(values.W_Fixnum, values.W_Fixnum.ZERO),
         default(values.W_Fixnum, None)], simple=False)
def read_bytes_avail_bang(w_bstr, w_port, w_start, w_end, env, cont):
    # FIXME: discern the available from the non-available form
    from pycket.interpreter import return_value

    # FIXME: custom ports
    if w_bstr.immutable():
        raise ContractException("read-bytes-avail!: given immutable byte string")
    if w_port is None:
        w_port = current_in_param.get(cont)
    start = w_start.value
    bytes = w_bstr.as_bytes_list()
    stop = len(bytes) if w_end is None else w_end.value
    if stop == start:
        return return_value(values.W_Fixnum.ZERO, env, cont)


    # FIXME: assert something on indices
    assert start >= 0 and stop <= len(bytes)
    n = stop - start

    res = w_port.read(n)
    reslen = len(res)

    # shortcut without allocation when complete replace
    if isinstance(w_bstr, values.W_MutableBytes):
        if start == 0 and stop == len(bytes) and reslen == n:
            w_bstr.value = list(res)
            return return_value(values.W_Fixnum(reslen), env, cont)

    if reslen == 0:
        return return_value(values.eof_object, env, cont)

    for i in range(0, reslen):
        bytes[start + i] = res[i]
    return return_value(values.W_Fixnum(reslen), env, cont)

@continuation
def peek_bytes_custom_port_cont(dest_bstr, w_bstr, start, stop, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    w_reslen = check_one_val(_vals)
    n = stop - start
    bytes = w_bstr.as_bytes_list()

    if w_reslen is values.eof_object:
        return return_value(values.eof_object, env, cont)

    assert isinstance(w_reslen, values.W_Fixnum)
    # shortcut without allocation when complete replace
    if isinstance(w_bstr, values.W_MutableBytes):
        if start == 0 and stop == len(bytes) and w_reslen.value == n:
            w_bstr.value = dest_bstr.value
            return return_value(w_reslen, env, cont)

    for i in range(0, w_reslen.value):
        bytes[start + i] = dest_bstr.value[i]
    return return_value(w_reslen, env, cont)


@expose("peek-bytes-avail!", [values.W_Bytes, values.W_Fixnum,
                              default(values.W_Object, values.w_false),
                              default(values.W_InputPort, None),
                              default(values.W_Fixnum, values.W_Fixnum.ZERO),
                              default(values.W_Fixnum, None)], simple=False)
def peek_bytes_avail_bang(w_bstr, skip_bytes_amt, progress, w_in, start_pos, end_pos, env, cont):
    from pycket.interpreter import return_value

    w_port = w_in if w_in else current_in_param.get(cont)

    start = start_pos.value
    stop = end_pos.value if end_pos else w_bstr.length()
    if w_bstr.immutable():
        raise ContractException("peek-bytes-avail!: given immutable byte string")
    bytes = w_bstr.as_bytes_list()

    if stop == start:
        return return_value(values.W_Fixnum.ZERO, env, cont)

    # FIXME: assert something on indices
    assert start >= 0 and stop <= len(bytes)
    n = stop - start

    if isinstance(w_in, values.W_CustomInputPort):
        _bstr = [chr(0)]*n
        # dest_bstr is a temporary bytes for custom port to peek the bytes into
        dest_bstr = values.W_MutableBytes(_bstr)

        return w_in._call_peek(dest_bstr, values.W_Fixnum(skip_bytes_amt.value), progress, env, peek_bytes_custom_port_cont(dest_bstr, w_bstr, start, stop, env, cont))

    old = w_port.tell()
    res = w_port.read(n)
    w_port.seek(old)
    reslen = len(res)

    # shortcut without allocation when complete replace
    if isinstance(w_bstr, values.W_MutableBytes):
        if start == 0 and stop == len(bytes) and reslen == n:
            w_bstr.value = list(res)
            return return_value(values.W_Fixnum(reslen), env, cont)

    if reslen == 0:
        return return_value(values.eof_object, env, cont)

    for i in range(0, reslen):
        bytes[start + i] = res[i]
    return return_value(values.W_Fixnum(reslen), env, cont)


# FIXME: implementation
@expose("write-string",
        [values_string.W_String, default(values.W_Object, None),
         default(values.W_Fixnum, values.W_Fixnum.ZERO),
         default(values.W_Fixnum, None)],
        simple=False)
def do_write_string(w_str, port, start_pos, end_pos, env, cont):
    from pycket.interpreter import return_value
    start = start_pos.value
    assert start >= 0
    if end_pos:
        end_pos = end_pos.value
        if end_pos < 0 or end_pos > w_str.length():
            raise ContractException("write-string: ending index out of range")
    else:
        end_pos = w_str.length()
    cont = write_string_cont(w_str, start, end_pos, env, cont)
    return get_output_port(port, env, cont)

@continuation
def write_string_cont(w_str, start, end_pos, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    port = check_one_val(_vals)
    assert isinstance(port, values.W_OutputPort)
    port.write(w_str.getslice(start, end_pos).as_str_utf8())
    return return_value(values.W_Fixnum(end_pos - start), env, cont)

def get_output_port(port, env, cont):
    from pycket.interpreter import return_value
    if port is None:
        port = current_out_param.get(cont)
        return return_value(port, env, cont)
    else:
        return get_port(port, values_struct.w_prop_output_port, values.W_OutputPort, env, cont)


def get_port(port, prop, typ, env, cont):
    cont = get_port_cont(prop, typ, env, cont)
    return _get_port(port, prop, typ, env, cont)

def _get_port(port, prop, typ, env, cont):
    from pycket.interpreter import return_value
    if isinstance(port, values_struct.W_RootStruct):
        cont = get_port_from_property(port, env, cont)
        return port.get_prop(prop, env, cont)
    else:
        return return_value(port, env, cont)

@continuation
def get_port_cont(prop, typ, env, cont, _vals):
    from pycket.interpreter import return_value, check_one_val
    val = check_one_val(_vals)
    if isinstance(val, values_struct.W_RootStruct):
        return get_port(val, prop, typ, env, cont)
    else:
        return return_value(val, env, cont)

@continuation
def get_port_from_property(port, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    val = check_one_val(_vals)
    if isinstance(val, values.W_Fixnum):
        return port.ref(val.value, env, cont)
    return return_value(port, env, cont)

@expose("write-byte",
        [values.W_Fixnum, default(values.W_Object, None)], simple=False)
def write_byte(b, out, env, cont):
    s = b.value
    if s < 0 or s > 255:
        raise ContractException("%s is not a byte"%s)
    return do_print(chr(s), out, env, cont)

@expose("write-char",
        [values.W_Character, default(values.W_Object, None)], simple=False)
def write_char(w_char, w_port, env, cont):
    c = w_char.value
    from rpython.rlib.runicode import unicode_encode_utf_8
    s = unicode_encode_utf_8(c, len(c), "strict")
    return do_print(s, w_port, env, cont)

def write_bytes_avail(w_bstr, w_port, start, stop):
    # FIXME: discern the available from the non-available form

    if start == stop:
        w_port.flush()
        return 0

    if start == 0 and stop == len(w_bstr):
        to_write = w_bstr
    else:
        slice_stop = stop
        assert start >= 0 and slice_stop <= len(w_bstr)
        assert slice_stop >= 0
        to_write = w_bstr[start:slice_stop]

    # FIXME: we fake here
    assert isinstance(w_port, values.W_OutputPort)
    w_port.write("".join(to_write))
    return stop - start

@expose(["write-bytes", "write-bytes-avail"],
         [values.W_Bytes, default(values.W_OutputPort, None),
          default(values.W_Fixnum, values.W_Fixnum.ZERO),
          default(values.W_Fixnum, None)], simple=False)
def wrap_write_bytes_avail(w_bstr, w_port, w_start, w_end, env, cont):
    from pycket.interpreter import return_value
    # FIXME: custom ports
    if w_port is None:
        w_port = current_out_param.get(cont)
    bytes = w_bstr.as_bytes_list()
    start = 0 if w_start is None else w_start.value
    stop = len(bytes) if w_end is None else w_end.value
    n = write_bytes_avail(bytes, w_port, start, stop)
    return return_value(values.W_Fixnum(n), env, cont)

# FIXME:
@expose("custom-write?", [values.W_Object])
def do_has_custom_write(v):
    return values.w_false


def bytes_to_path_element(bytes, path_type=None, false_on_non_element=None):
    from pycket.prims.general import w_unix_sym, w_windows_sym
    if path_type is None:
        path_type = w_windows_sym if platform in ('win32', 'cygwin') else w_unix_sym
    if path_type not in (w_unix_sym, w_windows_sym):
        raise SchemeException("bytes->path-element: unknown system type %s" % path_type.tostring())
    str = bytes.as_str()
    if os.sep in str:
        if false_on_non_element is values.w_true:
            return values.w_false
        raise SchemeException("bytes->path-element: cannot be converted to a path element %s" % str)
    return values.W_Path(str)

expose("bytes->path-element", [values.W_Bytes, default(values.W_Symbol, None), default(values.W_Bool, None)])(bytes_to_path_element)

def shutdown(env):
    # called before the interpreter exits
    stdout_port.flush()

@make_procedure("mock-prompt-thunk", [], arity=Arity.ZERO, simple=False)
def mock_prompt_thunk(env, cont):
    return mock_prompt_thunk_worker(env, cont)

# to be able to call it internally
def mock_prompt_thunk_worker(env, cont):
    """
    (lambda ()
    (display "> ")
    (let ([in ((current-get-interaction-input-port))])
    ((current-read-interaction) (object-name in) in)))"""

    pycketconfig = env.toplevel_env()._pycketconfig
    from pycket.interpreter import return_value

    stdout_port.write("> ")

    from pycket.racket_entry import get_primitive
    rs = get_primitive("read-syntax")
    obj_name = values.W_Symbol.make("readline-input")

    return rs.call([obj_name, stdin_port], env, cont)

@expose("current-prompt-read", [], simple=True)
def mock_current_prompt_read():
    return mock_prompt_thunk

@expose("file-stream-buffer-mode", [values.W_Port, default(values.W_Object, None)])
def file_stream_buffer_mode(p, mode):
    # FIXME: doesn't actually do anything
    if mode is None:
        # getting
        return values.w_false
    else:
        # setting
        return values.w_void

@expose("file-stream-port?", [values.W_Port])
def file_stream_port_p(p):
    return values.W_Bool.make(isinstance(p, values.W_FileOutputPort) or
                              isinstance(p, values.W_FileInputPort))

MIP_ARGS = [
    values.W_Object, # name
    values.W_Object, # read_in : (or input-port (bytes -> (or number eof procedure event)))
    values.W_Object, # peek : (or input-port (bytes number (or evt #f) -> (or number eof procedure evt #f)))
    values.W_Object, # close : (-> any)
    default(values.W_Object, values.w_false), # get_progress_evt : (or (-> evt) #f)
    default(values.W_Object, values.w_false), # commit : (or (number evt evt -> any) #f)
    default(values.W_Object, values.w_false), # get_location : (or (-> (values number/#f number/#f number/#f)) #f)
    default(values.W_Object, values.w_void), # count_lines! : (-> any)
    default(values.W_Object, values.W_Fixnum.ONE), # init_position : (or number port #f (-> (or number #f)))
    default(values.W_Object, values.w_false), # buffer_mode : (or (case-> ((or 'block 'none) -> any) (-> (or 'block 'none #f))) #f)
    ]

@expose("make-input-port", MIP_ARGS)
def make_input_port(name, read_in, peek, close,
                    get_progress_evt, commit, get_location, count_lines_bang, init_position, buffer_mode):
    # FIXME : implementation (or get the IO linklet)
    #return values.W_StringInputPort("")
    return values.W_CustomInputPort(name, read_in, peek, close, get_progress_evt, commit, get_location,
                                    count_lines_bang, init_position, buffer_mode)

print_graph_param = values_parameter.W_Parameter(values.w_false)
print_struct_param = values_parameter.W_Parameter(values.w_false)
print_box_param = values_parameter.W_Parameter(values.w_false)
print_vector_length_param = values_parameter.W_Parameter(values.w_false)
print_hash_table_param = values_parameter.W_Parameter(values.w_false)
print_boolean_long_form_param = values_parameter.W_Parameter(values.w_false)
print_as_expression_param = values_parameter.W_Parameter(values.w_true)

expose_val("print-graph", print_graph_param)
expose_val("print-struct", print_struct_param)
expose_val("print-box", print_box_param)
expose_val("print-vector-length", print_vector_length_param)
expose_val("print-hash-table", print_hash_table_param)
expose_val("print-boolean-long-form", print_boolean_long_form_param)
expose_val("print-as-expression", print_as_expression_param)

w_read_case_sensitive = values_parameter.W_Parameter(values.w_true)
expose_val("read-case-sensitive", w_read_case_sensitive)
