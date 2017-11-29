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
from pycket              import values_string
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, expose_val, procedure

from sys import platform

import os

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

    def __init__(self, s):
        self.str = s

class LParenToken(DelimToken):
    _attrs_ = []

class RParenToken(DelimToken):
    _attrs_ = []

class DotToken(DelimToken):
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
    "quote-syntax",
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

# FIXME: replace with a string builder
# FIXME: unicode
def read_string(f):
    buf = StringBuilder(64)
    isascii = True
    while True:
        c = f.read(1)[0]
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
            if c2 == "'":
                return quote_syntax_token
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
            if c2 in ["(", "[", "{"]:
                return LParenToken("#" + c2)
            if c2 == "\\":
                s = f.read(1)
                if not s:
                    raise SchemeException("unexpected end of file")
                c = ord(s[0]) # XXX deal with unicode
                return values.W_Character(unichr(c))
            raise SchemeException("bad token in read: %s" % c2)
        raise SchemeException("bad token in read: %s" % c)

#@expose("read", [default(values.W_Object, None)], simple=False)
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
        c = c[0]
        if c == rt.key.value:
            port.read(1) # since we peeked
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
    if isinstance(next_token, DelimToken):
        if not isinstance(next_token, LParenToken):
            raise SchemeException("read: unexpected %s" % next_token.str)
        v = read_list(stream, next_token.str)
        return v
    else:
        assert isinstance(next_token, values.W_Object)
        return next_token

def check_matches(s1, s2):
    assert (s1 == "(" and s2 == ")" or
            s1 == "[" and s2 == "]" or
            s1 == "{" and s2 == "}")

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
def do_read_one_cont(as_bytes, peek, env, cont, _vals):
    from pycket.interpreter import check_one_val
    w_port = check_one_val(_vals)
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
            old = w_port.tell()
            c = w_port.read(needed)
            w_port.seek(old)
        elif needed > 1:
            c += w_port.read(needed - 1)
        c = c.decode("utf-8")
        assert len(c) == 1
        return return_value(values.W_Character(c[0]), env, cont)

@expose("read-char-or-special", [values.W_InputPort, default(values.W_Object, values.w_false), default(values.W_Object, values.w_false)], simple=False)
def read_char_or_special(in_port, special_wrap, source_name, env, cont):
    # FIXME: ignoring special values and custom ports for now

    #return read_char(in_port, env, cont)
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
    
@expose("peek-char", [default(values.W_Object, None),
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
        raise SchemeException("open-input-file: expected path-string for argument 0")
    m = "r" if mode is w_text_sym else "rb"
    return open_infile(path, m)

@expose("open-output-file", [values.W_Object,
                             default(values.W_Symbol, w_binary_sym),
                             default(values.W_Symbol, w_error_sym)])
def open_output_file(path, mode, exists):
    if not isinstance(path, values_string.W_String) and not isinstance(path, values.W_Path):
        raise SchemeException("open-input-file: expected path-string for argument 0")
    m = "w" if mode is w_text_sym else "wb"
    return open_outfile(path, m)

@expose("close-input-port", [values.W_Object], simple=False)
def close_input_port(port, env, cont):
    cont = close_port_cont(env, cont)
    return get_input_port(port, env, cont)

@continuation
def close_port_cont(env, cont, _vals):
    from pycket.interpreter import check_one_val
    port = check_one_val(_vals)
    port.close()
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
        result = obj.as_str_utf8()
    elif isinstance(obj, values.W_Path):
        result = obj.path
    elif isinstance(obj, values.W_Bytes):
        result = obj.as_str()
    else:
        raise SchemeException("expected path-like values but got %s" % obj.tostring())
    return result if result is not None else "."

@expose("directory-exists?", [values.W_Object])
def directory_exists(w_str):
    s = extract_path(w_str)
    return values.W_Bool.make(os.path.isdir(s))

@expose("file-exists?", [values.W_Object])
def file_exists(w_str):
    s = extract_path(w_str)
    return values.W_Bool.make(os.path.isfile(s))

@expose("file-or-directory-modify-seconds", [values.W_Object, default(values.W_Object, None), default(values.W_Object, None)], simple=False)
def file_or_dir_mod_seconds(w_path, secs_n, fail, env, cont):
    from pycket.prims.general import detect_platform, w_unix_sym
    from pycket.interpreter import return_value
    
    platform = detect_platform()
    if platform is not w_unix_sym:
        raise Exception("Not yet implemented")

    path_str = extract_path(w_path)

    if not os.path.isdir(path_str) and not os.path.isfile(path_str):
        if fail is not None:
            return fail.call([], env, cont)
        else:
            raise SchemeException("No such file or directory exists : %s" % path_str)

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
    parts = [_explode_element(p) for p in path.split(sep)]
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

@expose("build-path")
def build_path(args):
    # XXX Does not check that we are joining absolute paths
    # Sorry again Windows
    if not args:
        raise SchemeException("build-path: expected at least 1 argument")
    result = [None] * len(args)
    for i, s in enumerate(args):
        if s is UP:
            part = ".."
        elif s is SAME:
            part = "."
        else:
            part = extract_path(s)
        if not part:
            raise SchemeException("build-path: path element is empty")
        if part == os.path.sep:
            part = ""
        result[i] = part
    path = os.path.normpath(os.path.sep.join(result))
    if not path:
        return ROOT
    return values.W_Path(path)

@expose("simplify-path", [values.W_Object, default(values.W_Bool, values.w_false)])
def simplify_path(path, use_filesystem):
    path_str = extract_path(path)
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

@expose("relative-path?", [values.W_Object])
def relative_path(obj):
    string = extract_path(obj)
    return values.W_Bool.make(not os.path.isabs(string))

@expose("absolute-path?", [values.W_Object])
def absolute_path(obj):
    string = extract_path(obj)
    return values.W_Bool.make(os.path.isabs(string))

@expose("resolve-path", [values.W_Object])
def resolve_path(obj):
    if (not isinstance(obj, values_string.W_String) and
        not isinstance(obj, values.W_Path)):
        raise SchemeException("resolve-path: expected path-string")
    str = extract_path(obj)
    return values.W_Path(os.path.normpath(str))

@expose("path-string?", [values.W_Object])
def path_stringp(v):
    # FIXME: handle zeros in string
    return values.W_Bool.make(
        isinstance(v, values_string.W_String) or isinstance(v, values.W_Path))

@expose("complete-path?", [values.W_Object])
def complete_path(v):
    # FIXME: stub
    return values.w_true

@expose("expand-user-path", [values.W_Object])
def expand_user_path(p):
    if isinstance(p, values.W_Path):
        path_str = p.path
    elif isinstance(p, values_string.W_String):
        path_str = p.tostring()
    else:
        raise SchemeException("expand_user_path expects a string or a path")

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
    raise SchemeException("cleanse-path expects string or path")

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
        raise SchemeException("path-element->string expects path")

    path = extract_path(p)
    return values_string.W_String.fromstr_utf8(path)

@expose("path-element->bytes", [values.W_Object])
def path_element2bytes(p):
    if not _path_elementp(p):
        raise SchemeException("path-element->string expects path")
    path = extract_path(p)
    return values.W_Bytes.from_string(path)

@continuation
def close_cont(port, env, cont, vals):
    from pycket.interpreter import return_multi_vals
    port.close()
    return return_multi_vals(vals, env, cont)

def open_infile(w_str, mode):
    s = extract_path(w_str)
    return values.W_FileInputPort(sio.open_file_as_stream(s, mode=mode, buffering=2**21))

def open_outfile(w_str, mode):
    s = extract_path(w_str)
    return values.W_FileOutputPort(sio.open_file_as_stream(s, mode=mode))

@expose("call-with-input-file", [values.W_Object,
                                 values.W_Object,
                                 default(values.W_Symbol, w_binary_sym)],
                                simple=False)
def call_with_input_file(s, proc, mode, env, cont):
    m = "r" if mode is w_text_sym else "rb"
    port = open_infile(s, m)
    return proc.call([port], env, close_cont(port, env, cont))

w_error_sym = values.W_Symbol.make("error")
w_append_sym = values.W_Symbol.make("append")
w_update_sym = values.W_Symbol.make("update")
w_replace_sym = values.W_Symbol.make("replace")
w_truncate_sym = values.W_Symbol.make("truncate")
w_truncate_replace_sym = values.W_Symbol.make("truncate/replace")

@expose("call-with-output-file", [values.W_Object,
                                  values.W_Object,
                                  default(values.W_Symbol, w_binary_sym),
                                  default(values.W_Symbol, w_error_sym)],
                                simple=False)
def call_with_output_file(s, proc, mode, exists, env, cont):
    m = ""
    if exists is w_append_sym:
        m += "a"
    elif exists is w_truncate_sym or w_truncate_replace_sym:
        m += "w"
    else:
        raise SchemeException("mode not yet supported: %s" % exists.tostring())
    if mode is not w_text_sym:
        m += "b"
    port = open_outfile(s, m)
    return proc.call([port], env, close_cont(port, env, cont))

@expose("with-input-from-file", [values_string.W_String, values.W_Object,
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
         default(values.W_Object, None)], simple=False)
def with_output_to_file(s, proc, mode, exists, env, cont):
    # XXX mode and exists are currently ignored, they need to be translated into
    # the proper mode string.
    from pycket.prims.parameter import call_with_extended_paramz
    port = open_outfile(s, "wb")
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


    raise SchemeException(
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
    return do_print(o.tostring(), p, env, cont)

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
            raise SchemeException(name + ": bad format string")
        s = fmt[i+1]
        if (s == 'a' or # turns into switch
            s == 'A' or
            s == 's' or
            s == 'S' or
            s == 'v' or
            s == 'V' or
            s == 'e' or
            s == 'E' or
            s == '.'):
            if j >= len(vals):
                raise SchemeException(name + ": not enough arguments for format string")
            result.append(vals[j].tostring())
            j += 1
        elif s == 'n' or s == '%':
            result.append("\n") # newline
        elif s == '~':
            result.append("~")
        else:
            raise SchemeException("%s: undexpected format character '%s'" % (name, s))
        i += 2
    if j != len(vals):
        raise SchemeException(name + ": not all values used")
    return "".join(result)

@expose("printf", simple=False)
def printf(args, env, cont):
    if not args:
        raise SchemeException("printf: expected at least one argument, got 0")
    fmt = args[0]
    if not isinstance(fmt, values_string.W_String):
        raise SchemeException("printf: expected a format string, got something else")
    return do_print(format(fmt, args[1:], "printf"), None, env, cont)

@expose("eprintf", simple=False)
def eprintf(args, env, cont):
    if not args:
        raise SchemeException("eprintf: expected at least one argument, got 0")
    fmt = args[0]
    if not isinstance(fmt, values_string.W_String):
        raise SchemeException("eprintf: expected a format string, got something else")
    return do_print(format(fmt, args[1:], "eprintf"), current_error_param.get(cont), env, cont)

@expose("format")
def do_format(args):
    if len(args) == 0:
        raise SchemeException("format: expects format string")
    fmt = args[0]
    if not isinstance(fmt, values_string.W_String):
        raise SchemeException("format: expected a format string, got something else")
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
    if v is not values.w_void:
        port.write(v.tostring())
        port.write("\n")
    return return_void(env, cont)

standard_printer = values.W_Prim("current-print", cur_print_proc)

string_sym  = values.W_Symbol.make("string")

@expose(["open-output-string", "open-output-bytes"], [])
def open_output_string():
    # FIXME: actual implementation for bytes and string
    return values.W_StringOutputPort()

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

@expose("get-output-bytes", [values.W_StringOutputPort])
def get_output_bytes(w_port):
    return values.W_Bytes.from_string(w_port.contents(),
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

def is_path_string(path):
    return isinstance(path, values.W_Path) or isinstance(path, values_string.W_String)

@expose("file-size", [values.W_Object])
def file_size(obj):
    if not is_path_string(obj):
        raise SchemeException("file-size: expected path string")
    path = extract_path(obj)
    try:
        size = os.path.getsize(path)
    except OSError:
        raise SchemeException("file-size: file %s does not exists" % path)

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
        raise SchemeException("read-bytes: expected non-negative integer for argument 0")
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
        raise SchemeException("read-bytes-avail!: given immutable byte string")
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
            raise SchemeException("write-string: ending index out of range")
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
        raise SchemeException("%s is not a byte"%s)
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

@expose("bytes->path-element", [values.W_Bytes, default(values.W_Symbol, None)])
def bytes_to_path_element(bytes, path_type):
    from pycket.prims.general import w_unix_sym, w_windows_sym
    if path_type is None:
        path_type = w_windows_sym if platform in ('win32', 'cygwin') else w_unix_sym
    if path_type not in (w_unix_sym, w_windows_sym):
        raise SchemeException("bytes->path-element: unknown system type %s" % path_type.tostring())
    str = bytes.as_str()
    if os.sep in str:
        raise SchemeException("bytes->path-element: cannot be converted to a path element %s" % str)
    return values.W_Path(str)

def shutdown(env):
    # called before the interpreter exits
    stdout_port.flush()

############################ Values and Parameters

expose_val("eof", values.eof_object)

current_print_param = values_parameter.W_Parameter(standard_printer)
expose_val("current-print", current_print_param)

# line buffer stdout
stdout_port = values.W_FileOutputPort(sio.fdopen_as_stream(1, "w", buffering=1))
stderr_port = values.W_FileOutputPort(sio.fdopen_as_stream(2, "w", buffering=1))
stdin_port = values.W_FileInputPort(sio.fdopen_as_stream(0, "r"))
current_out_param = values_parameter.W_Parameter(stdout_port)
current_error_param = values_parameter.W_Parameter(stderr_port)
current_in_param = values_parameter.W_Parameter(stdin_port)
current_readtable_param = values_parameter.W_Parameter(values.w_false)
expose_val("current-readtable", current_readtable_param)

expose_val("current-output-port", current_out_param)
expose_val("current-error-port", current_error_param)
expose_val("current-input-port", current_in_param)

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

