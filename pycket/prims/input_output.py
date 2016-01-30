#! /usr/bin/env python
# -*- coding: utf-8 -*-
from rpython.rlib import jit
from rpython.rlib             import streamio as sio
from rpython.rlib.rbigint     import rbigint
from rpython.rlib.rstring     import (ParseStringError,
        ParseStringOverflowError, StringBuilder)
from rpython.rlib.rarithmetic import string_to_int, intmask
from rpython.rlib import runicode

from pycket.cont         import continuation, loop_label, call_cont
from pycket              import values
from pycket              import values_parameter
from pycket              import values_struct
from pycket              import values_string
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, expose_val, procedure
import os

w_quote_symbol = values.W_Symbol.make("quote")
w_quasiquote_symbol = values.W_Symbol.make("quasiquote")
w_unquote_symbol = values.W_Symbol.make("unquote")
w_unquote_splicing_symbol = values.W_Symbol.make("unquote-splicing")

w_quote_syntax_symbol = values.W_Symbol.make("quote-syntax")
w_quasiquote_syntax_symbol = values.W_Symbol.make("quasisyntax")
w_unquote_syntax_symbol = values.W_Symbol.make("unsyntax")
w_unquote_syntax_splicing_symbol = values.W_Symbol.make("unsyntax-splicing")

class Token(object): pass

class ValueToken(Token):
    def __init__(self, v):
        assert isinstance(v, values.W_Object)
        self.val = v

class SpecialToken(Token):
    def __init__(self, s, con):
        self.str = s
        self.con = con
    def finish(self, val):
        return values.W_Cons.make(self.con, values.W_Cons.make(val, values.w_null))

class NumberToken(ValueToken): pass
class StringToken(ValueToken): pass
class SymbolToken(ValueToken): pass
class BooleanToken(ValueToken): pass
class CharToken(ValueToken): pass
class EOFToken(ValueToken): pass


class DelimToken(Token):
    def __init__(self, s):
        self.str = s

class LParenToken(DelimToken): pass
class RParenToken(DelimToken): pass
class DotToken(DelimToken): pass

allowed_char = "!?.-_:=*$%<>+^@&~/"

def idchar(c):
    c = c[0] # tell the annotator it's really a single char
    if c.isalnum() or (c in allowed_char):
        return True
    return False

def read_number_or_id(f, init):
    sofar = [init]
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
    got = "".join(sofar)
    try:
        return NumberToken(values.W_Fixnum.make(string_to_int(got)))
    except ParseStringOverflowError:
        val = rbigint.fromdecimalstr(got)
        return NumberToken(values.W_Bignum(val))
    except ParseStringError:
        try:
            return NumberToken(values.W_Flonum.make(float(got)))
        except:
            return SymbolToken(values.W_Symbol.make(got))

# FIXME: replace with a string builder
# FIXME: unicode
def read_string(f):
    buf = []
    while True:
        c = f.read(1)
        if c == '"':
            return values_string.W_String.fromstr_utf8("".join(buf))
        elif c == "\\":
            n = f.read(1)
            if n in ['"', "\\"]:
                c = n
            elif n == "n":
                c = "\n"
            elif n == "t":
                c = "\t"
            else:
                raise SchemeException("read: bad escape character in string: %s"%n)
        buf.append(c)

def read_token(f):
    while True:
        c = f.read(1) # FIXME: unicode
        if not c:
            return EOFToken(values.eof_object)
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
            return ValueToken(v)
        if c == ".":
            p = f.peek()
            if p in [" ", "\n", "\t"]:
                return DotToken(c)
            return read_number_or_id(f, c)
        if c == "'":
            return SpecialToken(c, w_quote_symbol)
        if c ==  "`":
            return SpecialToken(c, w_quasiquote_symbol)
        if c == ",":
            p = f.peek()
            if p == "@":
                p = f.read(1)
                return SpecialToken(c + p, w_unquote_splicing_symbol)
            else:
                return SpecialToken(c, w_unquote_symbol)
        if idchar(c):
            return read_number_or_id(f, c)
        if c == "#":
            c2 = f.read(1)
            if c2 == "'":
                return SpecialToken(c + c2, w_quote_syntax_symbol)
            if c2 == "`":
                return SpecialToken(c + c2, w_quasiquote_syntax_symbol)
            if c2 == ",":
                p = f.peek()
                if p == "@":
                    p = f.read(1)
                    return SpecialToken(c + c2, w_unquote_syntax_splicing_symbol)
                return SpecialToken(c + c2, w_unquote_syntax_symbol)
            if c2 == "t":
                return BooleanToken(values.w_true)
            if c2 == "f":
                return BooleanToken(values.w_false)
            if c2 in ["(", "[", "{"]:
                return LParenToken("#" + c2)
            if c2 == "\\":
                s = f.read(1)
                if not s:
                    raise SchemeException("unexpected end of file")
                c = ord(s[0]) # XXX deal with unicode
                return CharToken(values.W_Character.make(unichr(c)))
            raise SchemeException("bad token in read: %s" % c2)
        raise SchemeException("bad token in read: %s" % c)

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
    v = read_stream(port)
    return return_value(v, env, cont)

def read_stream(stream):
    next_token = read_token(stream)
    if isinstance(next_token, SpecialToken):
        v = read_stream(stream)
        return next_token.finish(v)
    if isinstance(next_token, DelimToken):
        if not isinstance(next_token, LParenToken):
            raise SchemeException("read: unexpected %s"%next_token.str)
        v = read_list(stream, values.w_null, next_token.str)
        return v
    else:
        return next_token.val

# assumes a proper list
def reverse(w_l, acc=values.w_null):
    while isinstance(w_l, values.W_Cons):
        val, w_l = w_l.car(), w_l.cdr()
        acc = values.W_Cons.make(val, acc)
    return acc

def check_matches(s1, s2):
    if s1 == "(":
        assert s2 == ")"
    if s1 == "[":
        assert s2 == "]"
    if s1 == "{":
        assert s2 == "}"

def read_list(stream, so_far, end):
    while True:
        next_token = read_token(stream)
        if isinstance(next_token, DotToken):
            last = read_stream(stream)
            close = read_token(stream)
            if isinstance(close, RParenToken):
                check_matches(end, close.str)
                return reverse(so_far, acc=last)
            else:
                raise SchemeException("read: illegal use of `.`")
        elif isinstance(next_token, RParenToken):
            check_matches(end, next_token.str)
            return reverse(so_far)
        elif isinstance(next_token, LParenToken):
            v = read_list(stream, values.w_null, next_token.str)
        elif isinstance(next_token, SpecialToken):
            arg = read_stream(stream)
            v = next_token.finish(arg)
        else:
            assert isinstance(next_token, ValueToken)
            v = next_token.val
        so_far = values.W_Cons.make(v, so_far)


linefeed_sym        = values.W_Symbol.make("linefeed")
return_sym          = values.W_Symbol.make("return")
return_linefeed_sym = values.W_Symbol.make("return-linefeed")
any_sym             = values.W_Symbol.make("any")
any_one_sym         = values.W_Symbol.make("any-one")

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


def do_read_one(w_port, as_bytes, peek, env, cont):
    from pycket.interpreter import return_value
    if w_port is None:
        w_port = current_in_param.get(cont)
    assert isinstance(w_port, values.W_InputPort)
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
        needed = runicode.utf8_code_length[i]
        if peek:
            old = w_port.tell()
            c = w_port.read(needed)
            w_port.seek(old)
        else:
            c += w_port.read(needed - 1)
        c = c.decode("utf-8")
        assert len(c) == 1
        return return_value(values.W_Character(c[0]), env, cont)

@expose("read-char", [default(values.W_InputPort, None)], simple=False)
def read_char(w_port, env, cont):
    try:
        return do_read_one(w_port, False, False, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("read-char: string is not a well-formed UTF-8 encoding")

@expose("read-byte", [default(values.W_InputPort, None)], simple=False)
def read_byte(w_port, env, cont):
    try:
        return do_read_one(w_port, True, False, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("read-byte: string is not a well-formed UTF-8 encoding")

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

@expose("peek-char", [default(values.W_InputPort, None),
                      default(values.W_Fixnum, values.W_Fixnum.ZERO)],
                    simple=False)
def peek_char(w_port, w_skip, env, cont):
    try:
        return do_peek(w_port, False, w_skip.value, env, cont)
    except UnicodeDecodeError:
        raise SchemeException("peek-char: string is not a well-formed UTF-8 encoding")

@expose("peek-byte", [default(values.W_InputPort, None),
                      default(values.W_Fixnum, values.W_Fixnum.ZERO)],
                    simple=False)
def peek_byte(w_port, w_skip, env, cont):
    try:
        return do_peek(w_port, True, w_skip.value, env, cont)
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

@expose("close-input-port", [values.W_InputPort])
def close_input_port(port):
    port.close()
    return values.w_void

@expose("close-output-port", [values.W_OutputPort])
def close_output_port(port):
    port.close()
    return values.w_void

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

@expose("directory-list", [values.W_Object])
def dir_list(w_str):
    s = extract_path(w_str)
    dir = [values.W_Path(p) for p in os.listdir(s)]
    return values.to_list(dir)

UP = values.W_Symbol.make("up")
SAME = values.W_Symbol.make("same")
RELATIVE = values.W_Symbol.make("relative")
SEP = values.W_Path(os.sep)

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

def _dirname(path):
    components = path.split(os.path.sep)
    return os.path.sep.join(components[:-1])

def _basename(path):
    components = path.split(os.path.sep)
    return components[-1]

@expose("split-path", [values.W_Object], simple=False)
def split_path(w_path, env, cont):
    from pycket.interpreter import return_multi_vals
    path = extract_path(w_path)
    dirname  = _dirname(path)
    basename = _basename(path)
    name = _explode_element(basename)
    if dirname == os.path.sep:
        base = values.w_false
        must_be_dir = values.w_false
    elif name is UP or name is SAME:
        base = RELATIVE
        must_be_dir = values.w_true
    else:
        base = values.W_Path(dirname + os.path.sep)
        must_be_dir = values.w_false
    result = values.Values.make([base, name, must_be_dir])
    return return_multi_vals(result, env, cont)

@expose("build-path")
def build_path(args):
    # XXX Does not check that we are joining absolute paths
    # Sorry again Windows
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
        result[i] = part
    return values.W_Path("/".join(result))

@expose("simplify-path", [values.W_Object, default(values.W_Bool, values.w_false)])
def simplify_path(path, use_filesystem):
    path_str = extract_path(path)
    return values.W_Path(path_str)

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
    if p and p[0] == '/':
        return path
    return values.W_Path(base + '/' + p)

@expose("path-for-some-system?", [values.W_Object])
def path_for_some_system(path):
    # XXX Really only handles UNIX paths
    # https://github.com/racket/racket/blob/827fc4559879c73d46268fc72f95efe0009ff905/racket/src/racket/include/scheme.h#L493
    # This seems to be the closest implementation we can achieve.
    return values.W_Bool.make(isinstance(path, values.W_Path))

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
    if not isinstance(obj, values_string.W_String) and not isinstance(obj, values.W_Path):
        raise SchemeException("resolve-path: expected path-string")
    str = extract_path(obj)
    return values.W_Path(os.path.normpath(str))

@continuation
def close_cont(port, env, cont, vals):
    from pycket.interpreter import return_multi_vals
    port.close()
    return return_multi_vals(vals, env, cont)

def open_infile(w_str, mode):
    s = extract_path(w_str)
    return values.W_FileInputPort(sio.open_file_as_stream(s, mode=mode))

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
        bytes = datum.value
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
    if port is None:
        port = current_out_param.get(cont)
    port.write(str)
    return return_void(env, cont)

@jit.unroll_safe
def format(form, vals, name):
    fmt = form.as_str_utf8() # XXX for now
    i = 0
    j = 0
    result = []
    len_fmt = len(fmt)
    while True:
        i0 = i
        while i < len_fmt:
            if fmt[i] == '~':
                break
            i += 1
        else:
            # not left via break, so we're done
            result.append(fmt[i0:len_fmt])
            break
        result.append(fmt[i0:i])
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
            # print a value
            # FIXME: different format chars
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
@jit.look_inside_iff(lambda args: jit.isconstant(args[0]))
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
    stop = len(w_bstr.value) if w_end is None else w_end.value
    if stop == start:
        return return_value(values.W_Fixnum.ZERO, env, cont)


    # FIXME: assert something on indices
    assert start >= 0 and stop <= len(w_bstr.value)
    n = stop - start

    res = w_port.read(n)
    reslen = len(res)

    # shortcut without allocation when complete replace
    if start == 0 and stop == len(w_bstr.value) and reslen == n:
        w_bstr.value = list(res)
        return return_value(values.W_Fixnum(reslen), env, cont)

    if reslen == 0:
        return return_value(values.eof_object, env, cont)

    for i in range(0, reslen):
        w_bstr.value[start + i] = res[i]
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
        [values.W_Fixnum, default(values.W_OutputPort, None)], simple=False)
def write_byte(b, out, env, cont):
    s = b.value
    if s < 0 or s > 255:
        raise SchemeException("%s is not a byte"%s)
    return do_print(chr(s), out, env, cont)

@expose("write-char",
        [values.W_Character, default(values.W_OutputPort, None)], simple=False)
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
    bytes = w_bstr.value
    start = 0 if w_start is None else w_start.value
    stop = len(bytes) if w_end is None else w_end.value
    n = write_bytes_avail(bytes, w_port, start, stop)
    return return_value(values.W_Fixnum(n), env, cont)

# FIXME:
@expose("custom-write?", [values.W_Object])
def do_has_custom_write(v):
    return values.w_false

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

