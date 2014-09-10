#! /usr/bin/env python
# -*- coding: utf-8 -*-
import operator as op
from pycket import values
from pycket.error import SchemeException
from pycket.prims.expose import default, expose

def define_string_comp(name, op):
    @expose(name)
    def comp(args):
        if len(args) < 2:
            raise SchemeException(name + ": requires at least 2 arguments")
        head, tail = args[0], args[1:]
        if not isinstance(head, values.W_String):
            raise SchemeException(name + ": not given a string")
        for t in tail:
            if not isinstance(t, values.W_String):
                raise SchemeException(name + ": not given a string")
            if not op(head.value, t.value):
                return values.w_false
            head = t
        return values.w_true

# FIXME: Doing case insensitives like this will perform the lower operation
# every time a value is used for a comparison.
def make_ci(op):
    def lower(a, b):
        return op(a.lower(), b.lower())
    return lower

for a in [("string<?", op.lt),
          ("string<=?", op.le),
          ("string=?", op.eq),
          ("string>=?", op.ge),
          ("string>?", op.gt),
          ("string-ci<?", make_ci(op.lt)),
          ("string-ci<=?", make_ci(op.le)),
          ("string-ci=?", make_ci(op.eq)),
          ("string-ci>=?", make_ci(op.ge)),
          ("string-ci>?", make_ci(op.gt)),
          ]:
    define_string_comp(*a)

@expose("make-string", [values.W_Fixnum, default(values.W_Character, values.w_null)])
def string_to_list(k, char):
    char = str(char.value) if isinstance(char, values.W_Character) else '\0'
    return values.W_String(char * k.value)

# FIXME: this implementation sucks
@expose("string-append")
def string_append(args):
    if not args:
        return values.W_String("")
    l = []
    for a in args:
        if not isinstance(a, values.W_String):
            raise SchemeException("string-append: expected a string")
        l.append(a.value)
    return values.W_String(''.join(l))

@expose("string-length", [values.W_String])
def string_length(s1):
    return values.W_Fixnum(len(s1.value))

@expose("substring", [values.W_String, values.W_Fixnum, default(values.W_Fixnum, None)])
def substring(w_string, w_start, w_end):
    """
    (substring str start [end]) → string?
        str : string?
        start : exact-nonnegative-integer?
        end : exact-nonnegative-integer? = (string-length str)
    """
    string = w_string.value
    start = w_start.value
    if start > len(string) or start < 0:
        raise SchemeException("substring: end index out of bounds")
    if w_end is not None:
        end = w_end.value
        if end > len(string) or end < 0:
            raise SchemeException("substring: end index out of bounds")
    else:
        end = len(string)
    if end < start:
        raise SchemeException(
            "substring: ending index is smaller than starting index")
    return values.W_String(string[start:end])

@expose("string-ref", [values.W_String, values.W_Fixnum])
def string_ref(s, n):
    idx = n.value
    st  = s.value
    if idx < 0 or idx >= len(st):
        raise SchemeException("string-ref: index out of range")
    return values.W_Character(st[idx])

@expose("string-set!", [values.W_String, values.W_Fixnum, values.W_Character])
def string_set(str, k, char):
    if str.immutable():
        raise SchemeException("string-set!: given immutable string")
    idx = k.value
    v = [i for i in str.value]
    if not (0 <= idx < len(v)):
        raise SchemeException("string-set!: given index is out of range")
    v[idx] = char.value
    str.value = "".join(v)
    return values.w_void
