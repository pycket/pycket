from pycket.prims.expose import (unsafe, default, expose, expose_val,
                                 procedure, make_call_method, define_nyi)
from pycket import values, values_string, values_regex
from pycket.error import SchemeException

@expose("regexp-match", [values.W_Object, values.W_Object])
def regexp_match(w_re, w_str):
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
        if result is None:
            return values.w_false
        else:
            return values.to_list([values_string.W_String.fromascii(r)
                    for r in result])
    if isinstance(w_str, values.W_Bytes):
        result = w_re.match_string(w_str.as_str())
        if result is None:
            return values.w_false
        else:
            return values.to_list([values.W_Bytes.from_string(r)
                    for r in result])


define_nyi("regexp-match?", [values.W_Object, values.W_Object])
# def regexp_matchp(r, o):
#     # FIXME: more error checking
#     assert isinstance(r, values.W_AnyRegexp) \
#         or isinstance(r, values_string.W_String) or isinstance(r, values.W_Bytes)
#     assert isinstance(o, values_string.W_String) or isinstance(o, values.W_Bytes) \
#         or isinstance(o, values.W_InputPort) or isinstance(o, values.W_Path)
#     # ack, this is wrong
#     return values.w_true # Back to one problem

# FIXME: implementation
define_nyi("regexp-replace", [values.W_Object, values.W_Object, values.W_Object,
                           default(values.W_Bytes, None)])
# def regexp_replace(pattern, input, insert, input_prefix):
#     raise NotImplementedError()
#     return input
