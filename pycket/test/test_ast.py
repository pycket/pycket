#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest
from pycket.expand import expand, expand_string
from pycket.values import W_Symbol, W_Fixnum, w_false, w_true
from pycket.expand import parse_module
from pycket.interpreter import (LexicalVar, ModuleVar, Done, CaseLambda,
                                variable_set, variables_equal,
                                Lambda, Letrec, Let, Quote, App, If, Begin,
                                SimplePrimApp1, SimplePrimApp2,
                                WithContinuationMark, SetBang,
                                )
from pycket.test.testhelper import format_pycket_mod, run_mod

skip = pytest.mark.skipif("True")

def make_symbols(d):
    v = variable_set()
    for i, j in d.iteritems():
        v[ModuleVar(W_Symbol.make(i), None, W_Symbol.make(i))] = j
    return v

def expr_ast(s, const_prop=True):
    m = expand_string(format_pycket_mod(s, extra="(define x 0)"))
    m = parse_module(m, const_prop=const_prop)
    return m.body[-1]

def test_symlist_depth():
    from pycket.env import SymList
    s = SymList([1, 2, 3, 4], SymList([], None))
    assert s.depth_and_size() == (2, 4)

# def test_constant_prop():
    # p = expr_ast("(let ([x 1]) (+ x 2))")
    # import pdb; pdb.set_trace()

def test_mutvars():
    p = expr_ast("(lambda (x) (set! x 2))")
    assert len(p.mutated_vars()) == 0
    assert p.lams[0]._mutable_var_flags[0]
    p = expr_ast(("(lambda (y) (set! x 2))"))
    assert variables_equal(p.mutated_vars(), make_symbols({"x": None}))
    assert p.lams[0]._mutable_var_flags is None
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

def test_cache_recursive_lambda_if_no_frees():
    from pycket.interpreter import interpret_one
    from pycket.values import W_PromotableClosure
    letrec = expr_ast("(letrec ([self (lambda (y) (set! y (self 2)))]) self)")
    assert isinstance(letrec, Let) and isinstance(letrec.rhss[0], CaseLambda)

    lamb = letrec.rhss[0]
    assert lamb.recursive_sym is W_Symbol.make("self")

    w_cl1 = interpret_one(lamb)
    assert isinstance(w_cl1, W_PromotableClosure)

@skip
def test_remove_let():
    p = expr_ast("(let ([g cons]) (g 5 5))")
    assert isinstance(p, Let)

    p = expr_ast("(let ([a 1]) (if a + -))")
    assert isinstance(p, Let)

def test_remove_simple_if():
    p = expr_ast("(if #t 'then 'else)")
    assert isinstance(p, Quote) and p.w_val is W_Symbol.make("then")
    p = expr_ast("(if #f 'then 'else)")
    assert isinstance(p, Quote) and p.w_val is W_Symbol.make("else")

def test_remove_simple_begin():
    p = expr_ast("(begin #f #t)", const_prop=False)
    assert isinstance(p, Quote) and p.w_val is w_true
    p = expr_ast("(let ([a 1]) a a a)", const_prop=False)
    assert isinstance(p, Let) and len(p.body) == 1
    p = expr_ast("(begin0 #t #f #f #f)", const_prop=False)
    assert isinstance(p, Quote) and p.w_val is w_true
    p = expr_ast("(let ([a 1]) (equal? 1 2) (let ([b 2]) (equal? 1 2) (let ([c 3]) (equal? 1 2) (begin (equal? b c) (equal? a b)))))", const_prop=False)
    assert isinstance(p, Let)
    assert p.body[-1].body[-1]._sequenced_remove_num_envs == [0, 0, 1]

def test_remove_simple_values():
    p = expr_ast("(letrec ([rec (values (lambda (x) (rec x)))]) (rec 10))")
    assert isinstance(p, Let)
    assert isinstance(p.rhss[0], CaseLambda)
    p = expr_ast("(+ (values 1) (values 2) (values 3))")
    assert isinstance(p, Quote) and p.w_val.value == 6
    # p = expr_ast("(let-values ([(a b c) (values 1 2 3)]) (+ a b c))")
    # assert isinstance(p, Quote) and p.w_val.value == 6

def test_let_remove_num_envs():
    p = expr_ast("(let ([b 1]) (let ([a (+ b 1)]) (sub1 a)))", const_prop=False)
    assert isinstance(p, Let)
    assert p.remove_num_envs == [0, 0]
    assert p.body[0].remove_num_envs == [0, 1]

    p = expr_ast("(let ([c 7]) (let ([b (+ c 1)]) (let ([a (b + 1)] [d (- c 5)]) (+ a d))))", const_prop=False)
    assert p.body[0].body[0].remove_num_envs == [0, 1, 2]

def test_let_remove_num_envs_edge_case():
    m = run_mod(
    """
    #lang pycket
    (define d
      (let-values (((es) values))
        (let-values (((adj) '0))
          (let-values ((() (es)))
            adj))))
    """)
    d = W_Symbol.make("d")
    assert type(m.defs[d]) is W_Fixnum and m.defs[d].value == 0

    m = run_mod(
    """
    #lang pycket
    (define d
      (let-values (((es) '0))
        (let-values (((adj) '1))
          (let-values (((a b c) (begin es (values '2 '3 '4))))
            (+ adj a)))))
    """)
    assert type(m.defs[d]) is W_Fixnum and m.defs[d].value == 3

def test_copy_to_env():
    p = expr_ast("(let ([c 7]) (let ([b (+ c 1)]) (let ([a (b + 1)] [d (- c 5)]) (+ a b))))", const_prop=False)
    inner_let = p.body[0].body[0]
    assert inner_let.remove_num_envs == [0, 0, 1, 2]
    assert len(inner_let.args.elems) == 3
    assert str(inner_let.args.elems[-3]).startswith('b')

    # can't copy env, because of the mutation
    p = expr_ast("(let ([c 7]) (let ([b (+ c 1)]) (let ([a (b + 1)] [d (- c 5)]) (set! b (+ b 1)) (+ a b))))", const_prop=False)
    inner_let = p.body[0].body[0]
    assert inner_let.remove_num_envs == [0, 0, 0, 0]

    # can't copy env, because of the mutation
    p = expr_ast("(let ([c 7]) (let ([b (+ c 1)]) (set! b (+ b 1)) (let ([a (b + 1)] [d (- c 5)]) (+ a b))))", const_prop=False)
    inner_let = p.body[0].body[0]
    assert inner_let._sequenced_remove_num_envs == [0, 1]

def test_prune_sequenced_body():
    p = expr_ast("""
    (let ([c 7])
      (let ([b (+ c 1)])
        (let ([d (+ b 1)])
          (equal? d 1)
          (equal? b 1)
          (equal? c 1))))
    """, const_prop=False)
    inner_let = p.body[0].body[0]
    assert inner_let._sequenced_remove_num_envs == [0, 1, 2]

def test_reclambda():
    # simple case:
    p = expr_ast("(letrec ([a (lambda () a)]) a)")
    assert isinstance(p, Let)
    assert isinstance(p.rhss[0], CaseLambda)
    assert p.rhss[0].recursive_sym is not None

    # immediate application
    p = expr_ast("(letrec ([a (lambda () a)]) (a))")
    assert isinstance(p, Let)
    assert isinstance(p.rhss[0], CaseLambda)
    assert p.rhss[0].recursive_sym is not None

    # immediate application
    p = expr_ast("(letrec ([a (lambda (b) (a b))]) (a 1))")
    assert isinstance(p, Let)
    assert isinstance(p.rhss[0], CaseLambda)
    assert p.rhss[0].recursive_sym is not None

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
    assert let.rhss[0].rator.sym is g2
    let = let.body[0]
    assert let.rhss[0].rator.sym is g1
    assert let.rhss[1].rator.sym is h2
    let = let.body[0]
    assert let.rhss[0].rator.sym is h1
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
    assert let.rhss[0].rator.sym is car
    assert let.rhss[1].rator.sym is cdr
    let = let.body[0]
    assert let.rhss[0].rator.sym is append
    let = let.body[0]
    assert let.rator.sym is cons


def test_specialized_app_for_simple_prims():
    p = expr_ast("(car 1)")
    assert isinstance(p, SimplePrimApp1)

    p = expr_ast("(cons 1 2)")
    assert isinstance(p, SimplePrimApp2)

def test_simple_prim_calls_are_simple_expressions():
    p = expr_ast("(car (cons 1 2))")
    assert isinstance(p, SimplePrimApp1)

def test_nested_lets():
    p = expr_ast("(let ([x (let ([y (equal? 1 2)]) y)]) (equal? #t x))")
    assert isinstance(p, Let)
    assert isinstance(p.rhss[0], App)
    let_body = p.body[0]
    assert isinstance(let_body, Let)
    let_body = let_body.body[0]
    assert isinstance(let_body, App)

def test_nontrivial_with_continuation_mark():
    p = expr_ast("(with-continuation-mark 'key 'val (equal? (equal? 1 2) 3))")
    assert isinstance(p, WithContinuationMark)
    body = p.body
    assert isinstance(body, Let)
    assert isinstance(body.rhss[0], App)
    assert isinstance(body.body[0], App)

def test_flatten_nested_begins():
    p = expr_ast("(let () (begin (begin (0) (1)) (begin (2) (3) (begin (4) (5) (6) (7)))))")
    assert isinstance(p, Begin)
    assert len(p.body) == 8
    for i, b in enumerate(p.body):
        assert isinstance(b, App)
        b = b.rator
        assert isinstance(b, Quote)
        val = b.w_val
        assert isinstance(val, W_Fixnum) and val.value == i

    p = expr_ast("(let () (begin (begin 0 1) (begin 2 3 (begin 4 5 6 7))))")
    assert isinstance(p, Quote)
    val = p.w_val
    assert isinstance(val, W_Fixnum) and val.value == 7

def test_anf_setbang():
    p = expr_ast("(let ([x 0]) (set! x (+ 1 (+ x 3))))")
    assert isinstance(p, Let)
    p = p.body[0]
    assert isinstance(p, Let)
    p = p.body[0]
    assert isinstance(p, Let)
    p = p.body[0]
    assert isinstance(p, SetBang)
    assert p.rhs.simple

