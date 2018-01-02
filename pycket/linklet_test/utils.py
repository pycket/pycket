#! /usr/bin/env python
# -*- coding: utf-8 -*-

import re
from pycket.values import w_false, W_Cons, W_Symbol, w_null, W_Flonum, W_Fixnum, w_true

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
        (?P<r_paren>\))|
        (?P<num>\-?\d+\.\d+|\-?\d+)|
        (?P<bool>\btrue\b|\bfalse\b|\#[tTfF]\b)|
        (?P<string>"[^"]*")|
        (?P<sym>[^(^)\s]+)
       )'''

def string_to_sexp(sexp):
    stack = []
    out = []
    if dbg: print("%-6s %-14s %-44s %-s" % tuple("term value out stack".split()))
    for termtypes in re.finditer(term_regex, sexp):
        term, value = [(t,v) for t,v in termtypes.groupdict().items() if v][0]
        if dbg: print("%-7s %-14s %-44r %-r" % (term, value, out, stack))
        if   term == 'l_paren':
            stack.append(out)
            out = []
        elif term == 'r_paren':
            assert stack, "Trouble with nesting of brackets"
            tmpout, out = out, stack.pop(-1)
            out.append(tmpout)
        elif term == 'num':
            v = float(value)
            if v.is_integer():
                v = W_Fixnum(int(v))
            else:
                v = W_Flonum(v)
            out.append(v)
        elif term == 'string':
            s = W_String.make(value[1:-1])
            out.append(s)
        elif term == 'bool':
            if value in ['#t', '#T', 'true']:
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
    #import pdb;pdb.set_trace()
    return to_w_list(out[0])
