import pytest
from pycket.expand import expand, expand_string
from pycket.values import W_Symbol
from pycket.expand import _to_ast, parse_module
from pycket.test.testhelper import format_pycket_mod

def make_symbols(d):
    return {W_Symbol.make(i): j for i, j in d.iteritems()}

def expr_ast(s):
    m = parse_module(expand_string(format_pycket_mod(s, extra="(define x 0)")))
    return m.body[0]

def test_mutvars():
    p = expr_ast("(lambda (x) (set! x 2))")
    assert p.mutated_vars() == {}
    p = expr_ast(("(lambda (y) (set! x 2))"))
    assert p.mutated_vars() == make_symbols({"x": None})
    p = expr_ast(("(let ([y 1]) (set! x 2))"))
    assert p.mutated_vars() == make_symbols({"x": None})
    p = expr_ast(("(let ([x 1]) (set! x 2))"))
    assert p.mutated_vars() == make_symbols({})

def test_cache_lambda_if_no_frees():
    from pycket.interpreter import ToplevelEnv
    from pycket.values import W_PromotableClosure
    lamb = expr_ast(("(lambda (y) (set! y 2))")).body[0]
    toplevel = ToplevelEnv()
    w_cl1 = lamb.interpret_simple(toplevel)
    assert isinstance(w_cl1, W_PromotableClosure)
    w_cl2 = lamb.interpret_simple(toplevel)
    assert w_cl1 is w_cl2
    assert w_cl1.env.toplevel_env is toplevel
