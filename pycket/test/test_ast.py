import pytest
from pycket.expand import expand, expand_string
from pycket.values import W_Symbol
from pycket.expand import _to_ast, to_ast, parse_module
from pycket.interpreter import (LexicalVar, ModuleVar, Done, CaseLambda,
                                variable_set, variables_equal,
                                Lambda, Letrec, Let, Quote, App, If,
                                )
from pycket.test.testhelper import format_pycket_mod

def make_symbols(d):
    v = variable_set()
    for i, j in d.iteritems():
        v[ModuleVar(W_Symbol.make(i), None, W_Symbol.make(i))] = j
    return v

def expr_ast(s):
    m = parse_module(expand_string(format_pycket_mod(s, extra="(define x 0)")))
    return m.body[-1]

def test_mutvars():
    p = expr_ast("(lambda (x) (set! x 2))")
    assert len(p.mutated_vars()) == 0
    p = expr_ast(("(lambda (y) (set! x 2))"))
    print p
    assert variables_equal(p.mutated_vars(), make_symbols({"x": None}))
    p = expr_ast(("(let ([y 1]) (set! x 2))"))
    assert variables_equal(p.mutated_vars(), make_symbols({"x": None}))
    #    assert p.mutated_vars() == make_symbols({"x": None})
    p = expr_ast(("(let ([x 1]) (set! x 2))"))
    assert variables_equal(p.mutated_vars(), make_symbols({}))

def test_cache_lambda_if_no_frees():
    from pycket.interpreter import ToplevelEnv
    from pycket.values import W_PromotableClosure
    lamb = expr_ast("(lambda (y) (set! y 2))")
    toplevel = ToplevelEnv()
    w_cl1 = lamb.interpret_simple(toplevel)
    assert isinstance(w_cl1, W_PromotableClosure)
    w_cl2 = lamb.interpret_simple(toplevel)
    assert w_cl1 is w_cl2
    assert w_cl1.closure._get_list(0).toplevel_env() is toplevel

def test_remove_let():
    p = expr_ast("(let ([a 1]) a)")
    assert isinstance(p, Quote)

    p = expr_ast("(let ([g cons]) (g 5 5))")
    assert isinstance(p, App)

    p = expr_ast("(let ([a 1]) (if a + -))")
    assert isinstance(p, If)


def test_reclambda():
    # simple case:
    p = expr_ast("(letrec ([a (lambda () a)]) a)")
    assert isinstance(p, CaseLambda)
    assert p.recursive_sym is not None

    # immediate application
    p = expr_ast("(letrec ([a (lambda () a)]) (a))")
    assert isinstance(p.rator, CaseLambda)
    assert p.rator.recursive_sym is not None

    # immediate application
    p = expr_ast("(letrec ([a (lambda (b) (a b))]) (a 1))")
    assert isinstance(p.rator, CaseLambda)
    assert p.rator.recursive_sym is not None

    # immediate application, need a let because the variable appears not just
    # once (but not a letrec)
    p = expr_ast("(letrec ([a (lambda (b) (a b))]) (a (a 1)))")
    assert isinstance(p, Let)
    assert isinstance(p.rhss[0], CaseLambda)
    assert p.rhss[0].recursive_sym is not None

def test_asts_know_surrounding_lambda():
    from pycket.interpreter import ToplevelEnv
    caselam = expr_ast("(lambda (y a b) (if y a b))")
    lam = caselam.lams[0]
    assert lam.body[0].surrounding_lambda is lam

    caselam = expr_ast("(lambda (y) (lambda (z) (+ y z)))")
    lam = caselam.lams[0]

    inner_caselam = lam.body[0]
    assert inner_caselam.surrounding_lambda is lam

    inner_lam = inner_caselam.lams[0]
    assert inner_lam.body[0].surrounding_lambda is inner_lam

def test_cont_fusion():
    from pycket.env import SymList
    from pycket.interpreter import (
        LetCont, BeginCont,
        FusedLet0Let0Cont, FusedLet0BeginCont,
    )
    args = SymList([])
    counts = [1]
    rhss = 1
    letast1 = Let(args, counts, [1], [2])
    letast2 = Let(args, counts, [1], [2])
    env = object()
    prev = object()
    let2 = LetCont.make([], letast2, 0, env, prev)
    let1 = LetCont.make([], letast1, 0, env, let2)
    assert isinstance(let1, FusedLet0Let0Cont)
    assert let1.prev is prev
    assert let1.env is env

    let2 = BeginCont(letast2.counting_asts[0], env, prev)
    let1 = LetCont.make([], letast1, 0, env, let2)
    assert isinstance(let1, FusedLet0BeginCont)
    assert let1.prev is prev
    assert let1.env is env
