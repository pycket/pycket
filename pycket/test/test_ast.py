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

def test_symlist_depth():
    from pycket.env import SymList
    s = SymList([1, 2, 3, 4], SymList([], None))
    assert s.depth_and_size() == (2, 4)

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

def test_let_remove_num_envs():
    p = expr_ast("(let ([b 1]) (let ([a (+ b 1)]) (sub1 a)))")
    assert isinstance(p, Let)
    assert p.remove_num_envs == [0, 0]
    assert p.body[0].remove_num_envs == [0, 1]

    p = expr_ast("(let ([c 7]) (let ([b (+ c 1)]) (let ([a (b + 1)] [d (- c 5)]) (+ a d))))")
    assert p.body[0].body[0].remove_num_envs == [0, 1, 2]

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
    from pycket.env import SymList, ToplevelEnv
    from pycket.interpreter import (
        LetCont, BeginCont,
        FusedLet0Let0Cont, FusedLet0BeginCont,
    )
    args = SymList([])
    counts = [1]
    rhss = 1
    letast1 = Let(args, counts, [1], [2])
    letast2 = Let(args, counts, [1], [2])
    env = ToplevelEnv()
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

def test_bottom_up_let_conversion():
    caselam = expr_ast("(lambda (f1 f2 f3 x) (f1 (f2 (f3 x))))")
    lam = caselam.lams[0]
    f1, f2, f3, x = lam.args.elems
    let = lam.body[0]
    for fn in [f3, f2]:
        assert let.rhss[0].rator.sym is fn
        let = let.body[0]
    assert let.rator.sym is f1

    caselam = expr_ast("(lambda (f g1 g2 h1 h2 a b) (f (g1 (g2 a)) (h1 (h2 b))))")
    lam = caselam.lams[0]
    f, g1, g2, h1, h2, a, b = lam.args.elems
    let = lam.body[0]
    for fn in [g2, g1, h2, h1]:
        assert let.rhss[0].rator.sym is fn
        let = let.body[0]
    assert let.rator.sym is f

    caselam = expr_ast("(lambda (f f2 x) %s x %s)" % ("(f (f2 " * 10, "))" * 10))
    lam = caselam.lams[0]
    f, f2, x = lam.args.elems
    let = lam.body[0]
    for fn in [f2, f] * 9 + [f2]:
        assert let.rhss[0].rator.sym is fn
        let = let.body[0]
    assert let.rator.sym is f


def test_bottom_up_let_conversion_bug_append():
    caselam = expr_ast("(lambda (cons car cdr a b append) (cons (car a) (append (cdr a) b)))")
    lam = caselam.lams[0]
    cons, car, cdr, a, b, append = lam.args.elems
    let = lam.body[0]
    for fn in [car, cdr, append]:
        assert let.rhss[0].rator.sym is fn
        let = let.body[0]
    assert let.rator.sym is cons
