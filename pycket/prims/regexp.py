from pycket.prims.expose import (unsafe, default, expose, expose_val,
                                 procedure, make_call_method, define_nyi)
from pycket import values, values_string, values_regex
from pycket.error import SchemeException

from rpython.rlib import jit

@expose("regexp", [values_string.W_String])
def regrexp(w_str):
    return values_regex.W_Regexp(w_str.as_str_utf8())

@expose("pregexp", [values_string.W_String])
def pregrexp(w_str):
    return values_regex.W_PRegexp(w_str.as_str_utf8())

@expose("byte-regexp", [values.W_Bytes])
def byte_regrexp(w_str):
    return values_regex.W_ByteRegexp(w_str.as_str())

@expose("byte-pregexp", [values.W_Bytes])
def byte_pregrexp(w_str):
    return values_regex.W_BytePRegexp(w_str.as_str())


@expose("regexp-match", [values.W_Object, values.W_Object])
@jit.unroll_safe
def regexp_match(w_re, w_str):
    result = match(w_re, w_str)
    if result is None:
        return values.w_false
    elif (isinstance(w_str, values_string.W_String) or \
          isinstance(w_str, values.W_StringInputPort)) \
        and \
         (isinstance(w_re, values_regex.W_PRegexp) or \
          isinstance(w_re, values_regex.W_Regexp) or \
          isinstance(w_re, values_string.W_String)):
        return values.to_list([values_string.W_String.fromascii(r)
                               if r else values.w_false
                               for r in result])
    else:
        return values.to_list([values.W_Bytes.from_string(r)
                               if r else values.w_false
                               for r in result])

def promote_to_regexp(w_re):
    if isinstance(w_re, values_string.W_String):
        return values_regex.W_Regexp(w_re.as_str_ascii()) # XXX for now
    if isinstance(w_re, values.W_Bytes):
        return values_regex.W_Regexp(w_re.as_str())
    if isinstance(w_re, values_regex.W_AnyRegexp):
        return w_re
    raise SchemeException("regexp-match: unknown kind of regexp")

def match(w_re, w_str):
    w_re = promote_to_regexp(w_re)
    if isinstance(w_str, values_string.W_String):
        s = w_str.as_str_ascii() # XXX for now
        result = w_re.match_string(s)
        return result
    if isinstance(w_str, values.W_Bytes):
        result = w_re.match_string(w_str.as_str())
        return result
    if isinstance(w_str, values.W_InputPort):
        result = w_re.match_port(w_str)
        return result
    raise SchemeException("regexp-match: can't deal with this type")

def match_positions(w_re, w_str):
    w_re = promote_to_regexp(w_re)
    if isinstance(w_str, values_string.W_String):
        s = w_str.as_str_ascii() # XXX for now
        result = w_re.match_string_positions(s)
        return result
    if isinstance(w_str, values.W_Bytes):
        result = w_re.match_string_positions(w_str.as_str())
        return result
    if isinstance(w_str, values.W_InputPort):
        result = w_re.match_port_positions(w_str)
        return result
    raise SchemeException("regexp-match-positions: can't deal with this type")

@expose("regexp-match-positions", [values.W_Object, values.W_Object])
@jit.unroll_safe
def rmp(pat, input):
    matches = match_positions(pat, input)
    if matches is None:
        return values.w_false
    xs = []
    for start, end in matches:
        s = values.W_Fixnum(start)
        e = values.W_Fixnum(end)
        xs.append(values.W_Cons.make(s, e))
    return values.to_list(xs)

@expose("regexp-match-positions-end", [values.W_Object, values.W_Object])
@jit.unroll_safe
def rmp(pat, input):
    matches = match_positions(pat, input)
    if matches is None:
        return values.w_false
    xs = []
    for start, end in matches:
        s = values.W_Fixnum(start)
        e = values.W_Fixnum(end)
        xs.append(values.W_Cons.make(s, e))
    return values.to_list(xs)

@expose("regexp-match?", [values.W_Object, values.W_Object])
def regexp_matchp(w_r, w_o):
    result = match(w_r, w_o)
    if result:
        return values.w_true
    else:
        return values.w_false

@expose("regexp-max-lookbehind", [values.W_Object])
def regexp_max_lookbehind(obj):
    if not isinstance(obj, values_regex.W_Regexp) and not isinstance(obj, values_regex.W_ByteRegexp):
        raise SchemeException("regexp-max-lookbehind: expected regexp or bytes-regexp")
    return values.W_Fixnum(1000)

# FIXME: implementation
define_nyi("regexp-replace", [values.W_Object, values.W_Object, values.W_Object,
                           default(values.W_Bytes, None)])
# def regexp_replace(pattern, input, insert, input_prefix):
#     raise NotImplementedError()
#     return input
