#! /usr/bin/env python
# -*- coding: utf-8 -*-

import re
from pycket.values import w_false, W_Cons, W_Symbol, w_null, W_Flonum, W_Fixnum, w_true, W_Bignum, W_Complex, W_Rational, W_Character, W_Bytes
from pycket import values_regex
from pycket.values_string import W_String
from rpython.rlib.rbigint import rbigint
from rpython.rlib.rarithmetic import string_to_int
from rpython.rlib.rstring import ParseStringError, ParseStringOverflowError

def debug_out(p):
    open("debug_out.txt", "a").write(p)

def wrap_linklet_sexp(body_sexp):
    return W_Cons.make(W_Symbol.make("linklet"),
                       W_Cons.make(w_null, # imports
                                   W_Cons.make(w_null, # exports
                                               W_Cons.make(body_sexp, w_null))))

def to_w_list(lst):
    out = w_null
    while lst != []:
        h = lst.pop()
        if isinstance(h, list):
            h = to_w_list(h)
        out = W_Cons.make(h, out)

    return out

dbg = False

## Based on :
## https://rosettacode.org/wiki/S-Expressions#Python

term_regex = r'''(?mx)
    \s*(?:
        (?P<l_paren>\()|
        (?P<ll_paren>\[)|
        (?P<r_paren>\))|
        (?P<rr_paren>\])|
        (?P<char>\#\\[a-zA-Z0-9])|
        (?P<compnum>\-?(\d+[/.])?\d+[+-](\d+[/.])?\d+[ij])|
        (?P<rational>\-?\d+/\d+)|
        (?P<num>(\-?\d+\.\d+|\-?\d+)|\+inf.0|-inf.0|\+nan.0)|
        (?P<bool>\btrue\b|\bfalse\b|\#[tTfF]\b|\#true\b|\#false\b)|
        (?P<string>"[^"]*")|
        (?P<bytes>\#"[^"]*")|
        (?P<byte_regexp>\#rx\#"[^"]*")|
        (?P<byte_pregexp>\#px\#"[^"]*")|
        (?P<regexp>\#rx"[^"]*")|
        (?P<pregexp>\#px"[^"]*")|
        (?P<sym>[^(^)\s]+)
       )'''

def to_num(value):
    if value[0] == "+":
        value = value[1:]

    if value == "inf.0":
        return W_Flonum.INF
    elif value == "-inf.0":
        return W_Flonum.NEGINF
    elif value == "nan.0":
        return W_Flonum.NAN

    if "/" in value:
        if len(re.findall('/', value)) > 1:
            raise Exception("Something's wrong with this rational number : %s" % value)

        sides = value.split("/")
        numer = sides[0]
        denom = sides[1]
        return W_Rational.make(to_num(numer), to_num(denom))


    v = float(value)

    if "." in value:
        return W_Flonum(v)

    else:
        try:
            return W_Fixnum.make(string_to_int(value))
        except ParseStringOverflowError:
            val = rbigint.fromdecimalstr(value)
            return W_Bignum(val)

    raise Exception("Didn't know what to do with this number : %s" % value)

def split_complex(comp_num_str):
    split_indices = [m.start() for m in re.finditer('[+-]?(\d+[/.])?\d+', comp_num_str)]

    if len(split_indices) != 2:
        raise Exception("Somthing must be wrong with the regex, can't seem to handle this complex number : %s" % comp_num_str)
    return comp_num_str[split_indices[0]:split_indices[1]], comp_num_str[split_indices[1]:-1]

def string_to_sexp(sexp):
    stack = []
    out = []
    if dbg: print("%-6s %-14s %-44s %-s" % tuple("term value out stack".split()))
    for termtypes in re.finditer(term_regex, sexp):
        term, value = [(t,v) for t,v in termtypes.groupdict().items() if v][0]
        if dbg: print("%-7s %-14s %-44r %-r" % (term, value, out, stack))
        if   term == 'l_paren' or term == 'll_paren':
            stack.append(out)
            out = []
        elif term == 'r_paren' or term == 'rr_paren':
            assert stack, "Trouble with nesting of brackets"
            tmpout, out = out, stack.pop(-1)
            out.append(tmpout)
        elif term == 'rational':
            v = to_num(value)
            out.append(v)
        elif term == 'compnum':
            real_part, imag_part = split_complex(value)
            v = W_Complex.make(to_num(real_part), to_num(imag_part))
            out.append(v)
        elif term == 'num':
            v = to_num(value)
            out.append(v)
        elif term == 'bytes':
            out.append(W_Bytes.from_string(value[2:-1]))
        elif term == 'regexp':
            out.append(values_regex.W_Regexp(value[4:-1]))
        elif term == 'pregexp':
            out.append(values_regex.W_PRegexp(value[4:-1]))
        elif term == 'byte_regexp':
            out.append(values_regex.W_ByteRegexp(value[5:-1]))
        elif term == 'byte_pregexp':
            out.append(values_regex.W_BytePRegexp(value[5:-1]))
        elif term == 'string':
            s = W_String.make(value[1:-1])
            out.append(s)
        elif term == 'char':
            c = W_Character(unicode(value[2:], 'utf-8'))
            out.append(c)
        elif term == 'bool':
            if value in ['#t', '#T', 'true', '#true']:
                b = w_true
            else:
                b = w_false
            out.append(b)
        elif term == 'sym':
            s = W_Symbol.make(value)
            out.append(s)
        else:
            raise NotImplementedError("Error: %r" % (term, value))
    assert not stack, "Trouble with nesting of brackets"
    return to_w_list(out[0])
