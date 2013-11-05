import pytest
from pycket.expand import expand
from pycket.values import W_Symbol
from pycket.interpreter import _to_ast

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

