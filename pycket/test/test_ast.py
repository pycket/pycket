import pytest
from pycket.expand import expand
from pycket.values import W_Symbol
from pycket.expand import _to_ast

def make_symbols(d):
    return {W_Symbol.make(i): j for i, j in d.iteritems()}

def test_mutvars():
    p = _to_ast(expand("(lambda (x) (set! x 2))"))
    assert p.mutated_vars() == {}
    p = _to_ast(expand("(lambda (y) (set! x 2))"))
    assert p.mutated_vars() == make_symbols({"x": None})
    p = _to_ast(expand("(let ([y 1]) (set! x 2))"))
    assert p.mutated_vars() == make_symbols({"x": None})
    p = _to_ast(expand("(let ([x 1]) (set! x 2))"))
    assert p.mutated_vars() == make_symbols({})

def test_cache_lambda_if_no_frees():
    from pycket.interpreter import ToplevelEnv
    from pycket.values import W_PromotableClosure
    lamb = _to_ast(expand("(lambda (y) (set! y 2))")).body[0]
    toplevel = ToplevelEnv()
    w_cl1 = lamb.interpret_simple(toplevel)
    assert isinstance(w_cl1, W_PromotableClosure)
    w_cl2 = lamb.interpret_simple(toplevel)
    assert w_cl1 is w_cl2
    assert w_cl1.env.toplevel_env is toplevel
