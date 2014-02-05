import pytest
from pycket.expand import expand
from pycket.values import W_Symbol
from pycket.expand import _to_ast, to_ast

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

def test_cache_closure():
    from pycket import interpreter
    p = to_ast(expand("(let ([a 1] [b 2] [c 4]) (lambda (x) x a b c))"))
    lam = p.body[0]
    lam.recursive_sym = lam.body[3].sym
    w_closure = lam._make_or_retrieve_closure(interpreter.ConsEnv.make([1, 2, 3, 4], None, None))
    assert 1 in w_closure.env._get_full_list()
    assert 2 in w_closure.env._get_full_list()
    assert w_closure in w_closure.env._get_full_list()

    # check caching
    w_closure1 = lam._make_or_retrieve_closure(interpreter.ConsEnv.make([1, 2, 3, 4], None, None))
    assert w_closure1 is w_closure
    w_closure1 = lam._make_or_retrieve_closure(interpreter.ConsEnv.make([1, 2, 3, 5], None, None))
    assert w_closure1 is w_closure

    # cache invalid:
    w_closure1 = lam._make_or_retrieve_closure(interpreter.ConsEnv.make([7, 2, 3, 5], None, None))
    assert w_closure1 is not w_closure
    # don't attempt to cache again
    w_closure2 = lam._make_or_retrieve_closure(interpreter.ConsEnv.make([7, 2, 3, 5], None, None))
    assert w_closure1 is not w_closure2
