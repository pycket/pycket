import pytest
from pycket.expand import expand, expand_string
from pycket.values import W_Symbol
from pycket.expand import _to_ast, parse_module
from pycket.interpreter import LexicalVar, ModuleVar, Done
from pycket.test.testhelper import format_pycket_mod

def make_symbols(d):
    return {ModuleVar(W_Symbol.make(i), None, W_Symbol.make(i)): j for i, j in d.iteritems()}

def expr_ast(s):
    m = parse_module(expand_string(format_pycket_mod(s, extra="(define x 0)")))
    return m.body[1]

# this is a ridiculous hack
# I hope someone who knows more python than me can fix it
def compare_mutvars(a, b):
    h1 = {}
    h2 = {}
    for k in a:
        if not(k.srcmod in h1):
            h1[k.srcmod] = {}
        h1[k.srcmod][k.srcsym] = None
        
    for k in b:
        if not(k.srcmod in h2):
            h2[k.srcmod] = {}
        h2[k.srcmod][k.srcsym] = None

    assert h1 == h2

def test_mutvars():
    p = expr_ast("(lambda (x) (set! x 2))")
    assert p.mutated_vars() == {}
    p = expr_ast(("(lambda (y) (set! x 2))"))
    print p
    compare_mutvars(p.mutated_vars(), make_symbols({"x": None}))
    p = expr_ast(("(let ([y 1]) (set! x 2))"))
    compare_mutvars(p.mutated_vars(), make_symbols({"x": None}))
    #    assert p.mutated_vars() == make_symbols({"x": None})
    p = expr_ast(("(let ([x 1]) (set! x 2))"))
    assert p.mutated_vars() == make_symbols({})

def test_cache_lambda_if_no_frees():
    from pycket.interpreter import ToplevelEnv
    from pycket.values import W_PromotableClosure
    lamb = expr_ast("(lambda (y) (set! y 2))")
    toplevel = ToplevelEnv()
    w_cl1 = lamb.interpret_simple(toplevel)
    assert isinstance(w_cl1, W_PromotableClosure)
    w_cl2 = lamb.interpret_simple(toplevel)
    assert w_cl1 is w_cl2
    assert w_cl1.env.toplevel_env is toplevel
