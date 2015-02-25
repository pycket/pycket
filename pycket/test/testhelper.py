#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Assorted test helpers
#
import os
from pycket.expand import expand, to_ast, expand_string, parse_module
from pycket.pycket_json import loads
from pycket.interpreter import *
from pycket.env import ToplevelEnv
from pycket import values

#
# basic runners
#

# This is where all the work happens

def run_ast(ast):
    env = ToplevelEnv()
    env.globalconfig.load(ast)
    mod = interpret_module(ast, env)
    return mod

def run_mod(m, stdlib=False, srcloc=True):
    assert (not stdlib)
    ast = parse_module(expand_string(m, srcloc=srcloc))
    return run_ast(ast)

def format_pycket_mod(s, stdlib=False, extra=""):
    # pycket handles the stdlib, various requires
    str = "#lang pycket%s\n%s\n%s"%(" #:stdlib" if stdlib else "", extra, s)
    return str

def run_mod_defs(m, extra="",stdlib=False, srcloc=True):
    str = format_pycket_mod(m, extra=extra, stdlib=stdlib)
    mod = run_mod(str, srcloc=srcloc)
    return mod

def run_mod_expr(e, v=None, stdlib=False, wrap=False, extra="", srcloc=False):
    # this (let () e) wrapping is needed if e is `(begin (define x 1) x)`, for example
    # FIXME: this should get moved into a language
    expr = "(let () %s)"%e if wrap else e
    defn = "(define #%%pycket-expr %s)"%expr
    mod = run_mod_defs(defn, stdlib=stdlib, extra=extra, srcloc=srcloc)
    ov = mod.defs[values.W_Symbol.make("#%pycket-expr")]
    if v:
        assert ov.equal(v)
    return ov

def execute(p, stdlib=False, extra=""):
    return run_mod_expr(p, stdlib=stdlib, extra=extra)

def run_fix(p, v=None, stdlib=False, extra=""):
    ov = run_mod_expr(p,stdlib=stdlib, extra=extra)
    assert isinstance(ov, values.W_Fixnum)
    if v is not None:
        assert ov.value == v
    return ov.value

def run_flo(p, v=None, stdlib=False, extra=""):
    ov = run_mod_expr(p,stdlib=stdlib, extra=extra)
    assert isinstance(ov, values.W_Flonum)
    if v is not None:
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
    return run_ast(ast)

def parse_file(fname, *replacements):
    fname = os.path.join(os.path.dirname(__file__), fname)
    with file(fname) as f:
        s = f.read()
    for replace, with_ in replacements:
        assert s.count(replace) == 1
        s = s.replace(replace, with_)
    s = s.decode("utf-8")
    s = expand_string(s)
    ast = parse_module(s)
    return ast

#
# Combined checking
#
def check_all(*snippets_returning_true, **kwargs):
    code = []
    tail = []
    for i, snippet in enumerate(snippets_returning_true):
        code.append("  " * i + "(if %s" % snippet)
        tail.append("  " * (i + 1) + "%s)" % i)
    code.append("  " * (i + 1) + "#t")
    code = "\n".join(code) + "\n" + "\n".join(reversed(tail))
    print code
    res = execute(code, extra=kwargs.get("extra", ""))
    if res is not values.w_true:
        assert 0, "%s returned a non-true value" % snippets_returning_true[res.value]

def check_none(*snippets_returning_false, **kwargs):
    code = []
    tail = []
    for i, snippet in enumerate(snippets_returning_false):
        code.append("  " * i + "(if %s %s" % (snippet, i))
        tail.append("  " * (i + 1) + ")")
    code.append("  " * (i + 1) + "#t")
    code = "\n".join(code) + "\n" + "\n".join(reversed(tail))
    print code
    res = execute(code, extra=kwargs.get("extra", ""))
    if res is not values.w_true:
        assert 0, "%s returned a true value" % snippets_returning_false[res.value]

def check_equal(*pairs_of_equal_stuff, **kwargs):
    code = []
    tail = []
    assert len(pairs_of_equal_stuff) % 2 == 0
    ind = 0
    for i in range(len(pairs_of_equal_stuff) // 2):
        a = pairs_of_equal_stuff[i * 2]
        b = pairs_of_equal_stuff[i * 2 + 1]
        ind += 1
        if isinstance(a, list):
            code.append("  " * ind + "(let ()")
            ind += 1
            code.extend(["  " * ind + x for x in a[:-1]])
            code.append( "  " * ind + ("(if (equal? %s %s)" % (a[-1], b)))
            ind += 1
            tail.append("  " * ind + "%s))" % i)
        else:
            snippet = "(equal? %s %s)" % (a, b)
            code.append("  " * ind + "(if %s" % snippet)
            tail.append("  " * (ind + 1) + "%s)" % i)
    code.append("  " * (ind + 1) + "#t")
    code = "\n".join(code) + "\n" + "\n".join(reversed(tail))
    try:
        print code
    except UnicodeEncodeError:
        print code.encode("ascii", 'replace')

    extra = kwargs.get("extra", "")
    res = execute(code, extra=extra)
    if res is not values.w_true:
        src = pairs_of_equal_stuff[res.value * 2]
        if isinstance(src, list):
            src = "(let ()\n" + "\n".join(src) + ")"
        wrong = execute(src, extra=extra)
        assert 0, u"%s is %s, which is different from %s" % (
            pairs_of_equal_stuff[res.value * 2], wrong.tostring(),
            pairs_of_equal_stuff[res.value * 2 + 1])
