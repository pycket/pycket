from pycket.prims.expose import (unsafe, default, expose, expose_val,
                                 procedure, make_call_method, define_nyi)
from pycket import values, values_string, values_regex
from pycket.error import SchemeException

@expose("regexp-match", [values.W_Object, values.W_Object])
def regexp_match(w_re, w_str):
    result = match(w_re, w_str)
    if result is None:
        return values.w_false
    elif isinstance(w_re, values_regex.W_Regexp) or \
         isinstance(w_re, values_regex.W_PRegexp):
        return values.to_list([values_string.W_String.fromascii(r)
                for r in result])
    elif isinstance(w_re, values_regex.W_ByteRegexp) or \
         isinstance(w_re, values_regex.W_BytePRegexp):
        return values.to_list([values.W_Bytes.from_string(r)
                for r in result])
    else:
        assert 0 # Unreachable

def match(w_re, w_str):
    # FIXME: more error checking
    if isinstance(w_re, values_string.W_String):
        w_re = values_regex.W_Regexp(w_re.as_str_ascii()) # XXX for now
    elif isinstance(w_re, values.W_Bytes):
        w_re = values_regex.W_Regexp(w_re.as_str())
    elif isinstance(w_re, values_regex.W_AnyRegexp):
        pass
    else:
        raise SchemeException("regexp-match: unknown kind of regexp")
    if isinstance(w_str, values_string.W_String):
        s = w_str.as_str_ascii() # XXX for now
        result = w_re.match_string(s)
        return result
    if isinstance(w_str, values.W_Bytes):
        result = w_re.match_string(w_str.as_str())
        return result
    if isinstance(w_str, values.W_InputPort):
        result = match_port(w_re, w_str)
        return result
    raise SchemeException("regexp-match: can't deal with this type")


def match_port(w_re, w_port):
    max_match = w_port._length_up_to_end()
    pos = w_port.tell()
    for i in range(max_match):
        w_port.seek(pos)
        s = w_port.read(i)
        result = w_re.match_string(s)
        if result:
            return result
    return None

@expose("regexp-match?", [values.W_Object, values.W_Object])
def regexp_matchp(w_r, w_o):
    result = match(w_r, w_o)
    if result:
        return values.w_true
    else:
        return values.w_false

# FIXME: implementation
define_nyi("regexp-replace", [values.W_Object, values.W_Object, values.W_Object,
                           default(values.W_Bytes, None)])
# def regexp_replace(pattern, input, insert, input_prefix):
#     raise NotImplementedError()
#     return input
