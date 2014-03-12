#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Assorted test helpers
#

import os

from pycket.expand import expand, to_ast, expand_string
from pycket.json import loads
from pycket.interpreter import *
from pycket import values

#
# basic runners
#
def execute(p, stdlib=False):
    e = expand(p, stdlib=stdlib)
    ast = to_ast(e)
    val = interpret_one(ast)
    return val

def run_fix(p, v, stdlib=False):
    val = execute(p, stdlib=stdlib)
    ov = check_one_val(val)
    assert isinstance(ov, values.W_Fixnum)
    assert ov.value == v
    return ov.value

def run_flo(p, v, stdlib=False):
    val = execute(p, stdlib=stdlib)
    ov = check_one_val(val)
    assert isinstance(ov, values.W_Flonum)
    assert ov.value == v
    return ov.value

def run(p, v=None, stdlib=False):
    val = execute(p, stdlib=stdlib)
    ov = check_one_val(val)
    if v is not None:
        assert ov.equal(v)
    return ov

def run_top(p, v=None, stdlib=False):
    e = expand(p, wrap=True, stdlib=stdlib)
    ast = to_ast(e)
    val = interpret([ast])
    ov = check_one_val(val)
    if v:
        assert ov.equal(v)
    return ov

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
    res = execute(code)._get_list(0)
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
    res = execute(code)._get_list(0)
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
    res = execute(code)._get_list(0)
    if res is not values.w_true:
        assert 0, "%s is different from %s" % (
                pairs_of_equal_stuff[res.value * 2], pairs_of_equal_stuff[res.value * 2 + 1])
