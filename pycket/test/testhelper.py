#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Assorted test helpers
#

import os

from pycket.expand import expand, to_ast, expand_string, parse_module
from pycket.json import loads
from pycket.interpreter import *
from pycket import values

#
# basic runners
#

# This is where all the work happens

def run_mod(m, stdlib=False):
    mod = interpret_module(parse_module(expand_string(m, stdlib=stdlib)))
    return mod

def format_pycket_mod(s, extra=""):
    # pycket-lang is just a trivial do-nothing-interesting language
    str = "#lang s-exp pycket-lang\n%s\n%s"%(extra,s)
    return str

def run_mod_defs(m, extra="",stdlib=False):
    str = format_pycket_mod(m, extra=extra)
    mod = run_mod(str, stdlib=stdlib)
    return mod

def run_mod_expr(e, v=None, stdlib=False, wrap=False, extra=""):
    # this (let () e) wrapping is needed if e is `(begin (define x 1) x)`, for example
    expr = "(let () %s)"%e if wrap else e
    defn = "(define #%%pycket-expr %s)"%expr
    mod = run_mod_defs(defn, stdlib=stdlib, extra=extra)
    ov = mod.defs[values.W_Symbol.make("#%pycket-expr")]
    if v:
        assert ov.equal(v)
    return ov


def execute(p, stdlib=False, extra=""):
    return run_mod_expr(p, stdlib=stdlib, extra=extra)

def run_fix(p, v, stdlib=False, extra=""):
    ov = run_mod_expr(p,stdlib=stdlib, extra=extra)
    assert isinstance(ov, values.W_Fixnum)
    assert ov.value == v
    return ov.value

def run_flo(p, v, stdlib=False, extra=""):
    ov = run_mod_expr(p,stdlib=stdlib, extra=extra)
    assert isinstance(ov, values.W_Flonum)
    assert ov.value == v
    return ov.value

def run(p, v=None, stdlib=False, extra=""):
    return run_mod_expr(p,v=v,stdlib=stdlib, extra=extra)

def run_top(p, v=None, stdlib=False, extra=""):
    return run_mod_expr(p,v=v,stdlib=stdlib, wrap=True, extra=extra)

def run_values(p, stdlib=False):
    e = "(call-with-values (lambda () %s) list)"%p
    v = run_mod_expr(e, stdlib=stdlib)
    return values.from_list(v)

def run_std(c, v):
    return run_top(c, v, stdlib=True)

def run_file(fname, *replacements):
    ast = parse_file(fname, *replacements)
    val = interpret([ast])


def parse_file(fname, *replacements):
    fname = os.path.join(os.path.dirname(__file__), fname)
    with file(fname) as f:
        s = f.read()
    for replace, with_ in replacements:
        assert s.count(replace) == 1
        s = s.replace(replace, with_)
    s = expand_string(s, wrap=True, stdlib=True)
    e = loads(s)
    ast = to_ast(e)
    return ast




#
# Combined checking
#
def check_all(*snippets_returning_true):
    code = []
    tail = []
    for i, snippet in enumerate(snippets_returning_true):
        code.append("  " * i + "(if %s" % snippet)
        tail.append("  " * (i + 1) + "%s)" % i)
    code.append("  " * (i + 1) + "#t")
    code = "\n".join(code) + "\n" + "\n".join(reversed(tail))
    print code
    res = execute(code)
    if res is not values.w_true:
        assert 0, "%s returned a non-true value" % snippets_returning_true[res.value]

def check_none(*snippets_returning_false):
    code = []
    tail = []
    for i, snippet in enumerate(snippets_returning_false):
        code.append("  " * i + "(if %s %s" % (snippet, i))
        tail.append("  " * (i + 1) + ")")
    code.append("  " * (i + 1) + "#t")
    code = "\n".join(code) + "\n" + "\n".join(reversed(tail))
    print code
    res = execute(code)
    if res is not values.w_true:
        assert 0, "%s returned a true value" % snippets_returning_false[res.value]

def check_equal(*pairs_of_equal_stuff):
    code = []
    tail = []
    assert len(pairs_of_equal_stuff) % 2 == 0
    for i in range(len(pairs_of_equal_stuff) // 2):
        a = pairs_of_equal_stuff[i * 2]
        b = pairs_of_equal_stuff[i * 2 + 1]
        snippet = "(equal? %s %s)" % (a, b)
        code.append("  " * i + "(if %s" % snippet)
        tail.append("  " * (i + 1) + "%s)" % i)
    code.append("  " * (i + 1) + "#t")
    code = "\n".join(code) + "\n" + "\n".join(reversed(tail))
    print code
    res = execute(code)
    if res is not values.w_true:
        assert 0, "%s is different from %s" % (
                pairs_of_equal_stuff[res.value * 2], pairs_of_equal_stuff[res.value * 2 + 1])
