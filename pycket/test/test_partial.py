#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

from pycket.values import *
from pycket.values_string import W_String
from pycket.vector import *
from pycket.test.testhelper import *
from pycket.env import ToplevelEnv, w_global_config


def test_basic_setup():
    res_lam = partially_eval_app("(+ 1 2)")
    assert res_lam.tostring() == '(lambda () 3)'

#@pytest.mark.m
def test_partial_let_set():
    p = """
    ((lambda (dyn)
       (let-values ([(y) 2]
                    [(vlala) (make-vector dyn)]
                    [(x) 1]
                    [(t) (make-vector dyn)])
         (vector-set! vlala 0 1)
         (vector-set! vlala 1 1)
         (+ (vector-ref vlala 0) (vector-ref vlala 1)))) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 2

#@pytest.mark.m
def test_var_name_resolve_partial_val():
    func_ast = make_ast("(lambda (var) (var 2))")
    func_val = func_ast.interpret_simple_partial([], [], [], ToplevelEnv())[0]
    top_sym = W_Symbol.make("toplevel-func")
    env = new_env_with(top_sym, func_val)
    # trick the sexp_to_ast
    w_global_config.pe_add_toplevel_var_name(top_sym)

    p = """
    ((lambda (dyn)
      (let-values ([(closure) (lambda (l) (+ l dyn))])
        (toplevel-func closure))) 10)
    """
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"], env=env)
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 12

#@pytest.mark.m
def test_closing_over_val_depends_on_dynamic_val():
    func_ast = make_ast("(lambda (vec) (vector-set! vec 0 12))")
    func_val = func_ast.interpret_simple_partial([], [], [], ToplevelEnv())[0]
    top_sym = W_Symbol.make("vector-setter")
    env = new_env_with(top_sym, func_val)
    # trick the sexp_to_ast
    w_global_config.pe_add_toplevel_var_name(top_sym)

    p = """
    ((lambda (dyn)
      (let-values ([(v) (make-vector dyn 3)])
        (vector-setter v)
        (vector-ref v 0))) 10)
    """
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"], env=env)
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 12

def test_var_name_partial_val():
    w_global_config.pe_add_toplevel_var_name(W_Symbol.make("top"))
    p = """
    ((lambda (dyn)
       (let-values ([(v) (lambda (l) (+ dyn l))])
         (v 3))) 10)
    """
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 13

#@pytest.mark.m
def test_different_dynamic_name():
    p = """
    ((lambda (dyn)
       (let-values ([(lexer)
                     (lambda (l) (+ 2 l))])
         (lexer dyn))) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 12


#@pytest.mark.m
def test_different_dynamic_name2():
    p = """
    ((lambda (dyn)
       (let-values ([(lexer)
                     (lambda (l) (+ 2 l))]
                    [(dep) (+ 1 dyn)])
         (lexer dep))) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 13

    # same test with an additional multi-value dummy rhs
    p = """
    ((lambda (dyn)
       (let-values ([(lexer)
                     (lambda (l) (+ 2 l))]
                    [(x y z) (values 1 2 3)]
                    [(dep) (+ 1 dyn)])
         (lexer dep))) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 13

#@pytest.mark.m
def test_closing_over_the_dynamic_var():
    a_top_lvl_func = """
    (define-values (toplevel-func) (lambda (f) (f 2)))
    """
    func_sexp = string_to_sexp(a_top_lvl_func)
    top_sym = W_Symbol.make("toplevel-func")
    env = new_env_with(top_sym, make_ast("(lambda (f) (f 2))"))
    # trick the sexp_to_ast
    w_global_config.pe_add_toplevel_var_name(top_sym)

    # manually creating s-expression of the following
    # because of how string_sexp works
    p = """
    ((lambda (dyn)
       (let-values ([(dyn-depend) (+ dyn 3)])
         (let-values([(closure) (lambda (l) (make-vector dyn-depend))])
           (1/pycket:pe-stop toplevel-func closure)))) 10)
    """
    cons = W_Cons
    null = w_null
    lst = to_list
    from pycket.ast_vs_sexp import lam_sym, let_sym, pe_stop_sym
    dyn_sym = W_Symbol.make("dyn")
    dep_sym = W_Symbol.make("dyn-depend")
    closure_sym = W_Symbol.make("closure")
    t_func_sym = W_Symbol.make("toplevel-func")
    l_sym = W_Symbol.make("l")
    vec_sym = W_Symbol.make("make-vector")
    plus = W_Symbol.make("+")
    ten = W_Fixnum(10)
    three = W_Fixnum(3)


    rhs_1 = to_list([to_list([dep_sym]), to_list([plus, dyn_sym, three])])

    lm = to_list([lam_sym, to_list([l_sym]), to_list([vec_sym, dep_sym])])
    rhs_2 = to_list([to_list([closure_sym]), lm])
    let_body = to_list([pe_stop_sym, t_func_sym, closure_sym])
    inner_let = to_list([let_sym, to_list([rhs_2]), let_body])
    outer_let = to_list([let_sym, to_list([rhs_1]), inner_let])
    outer_lam = to_list([lam_sym, to_list([dyn_sym]), outer_let])
    app_sexp = to_list([outer_lam, ten])
    res_lam = partially_eval_app_sexp(app_sexp, dyn_var_names=["dyn"], env=env)
    kk = run_residual_sexp(res_lam, W_Fixnum(10), additional_funcs=[func_sexp])

    assert isinstance(kk, W_Vector)

#@pytest.mark.m
def test_closing_over_the_dynamic_var_2():
    a_top_lvl_func = """
    (define-values (toplevel-func) (lambda (f) (f 2)))
    """
    func_sexp = string_to_sexp(a_top_lvl_func)
    top_sym = W_Symbol.make("toplevel-func")
    env = new_env_with(top_sym, make_ast("(lambda (f) (f 2))"))
    # trick the sexp_to_ast
    w_global_config.pe_add_toplevel_var_name(top_sym)

    p = """
    ((lambda (dyn)
       (let-values ([(dyn_depend) (+ dyn 3)])
         (let-values([(closure) (lambda (l) (make-vector dyn_depend))])
           (1/pycket:pe-stop toplevel-func closure)))) 10)
    """

    res_lam = partially_eval_app(p, dyn_var_names=["dyn"], env=env, use_racket_read=True)
    kk = run_residual_sexp(res_lam, W_Fixnum(10), additional_funcs=[func_sexp])
    assert isinstance(kk, W_Vector)

#@pytest.mark.m
def test_partial_let_multi_val_rhs_more_than_one():
    p = """
    ((lambda (dyn)
       (let-values ([(a b) (values 1 dyn)])
         (+ a b))) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 11

#@pytest.mark.m
def test_partial_let_multi_val_rhs_more_than_one_2():
    p = """
    ((lambda (dyn)
       (let-values ([(a b) (values 1 dyn)]
                    [(kek) 3])
         (+ a b kek))) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 14

#@pytest.mark.m
def test_partial_let_multi_val_rhs_no_val():
    p = """
    ((lambda (dyn)
       (let-values ([() (values)])
         dyn)) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 10

#@pytest.mark.m
def test_partial_let_multi_val_rhs_no_val_2():
    p = """
    ((lambda (dyn)
       (let-values ([() (begin (+ dyn dyn) (values))])
         dyn)) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 10

#@pytest.mark.m
def test_partial_let_multi_val_rhs_no_val_2_with_cells():
    p = """
    ((lambda (dyn)
       (let-values ([() (begin (set! dyn dyn) (values))])
         dyn)) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 10

#@pytest.mark.m
def test_partial_let_multi_val_rhs_no_val_3():
    p = """
    ((lambda (dyn)
       (let-values ([(a) dyn]
                    [() (values)])
         (+ a 3))) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"], use_racket_read=True)
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 13

#@pytest.mark.m
def test_partial_let_multi_val_rhs_no_val_4():
    p = """
    ((lambda (dyn)
       (let-values (((a) dyn) (() (begin (+ dyn dyn) (values))))
         a)) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"], use_racket_read=True)
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 10

#@pytest.mark.m
def test_partial_let_multi_val_rhs_no_val_5():
    p = """
    ((lambda (dyn)
       (let-values (((or-part) dyn)
                    ((if53) 4)
                    (() (begin (+ dyn dyn) (values))))
        dyn)) 10)
"""
    #((lambda (dyn) (let ([or-part dyn][if53 dyn][ (begin (+ dyn dyn) (values))]) dyn)) 10)
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"], use_racket_read=True)
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 10
    # some weird uses of let may not have any ids for their rhs

#@pytest.mark.m
def test_partial_let_multi_val_rhs_no_val_5_2():
    p = """
    ((lambda (dyn)
       (let-values ([() (let-values ([(or-part) dyn])
                          (let-values ([(if53) 4]) (+ dyn dyn))
                          (values))])
    dyn)) 10)
"""
    #((lambda (dyn) (let ([or-part dyn][if53 dyn][ (begin (+ dyn dyn) (values))]) dyn)) 10)
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"], use_racket_read=True)
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 10
    # some weird uses of let may not have any ids for their rhs

#@pytest.mark.m
def test_partial_let_multi_val_rhs_no_val_6():
    p = """
    ((lambda (dyn)
       (let-values ([() (let-values ([(or-part) dyn])
                          (let-values ([(if53) dyn]) (+ dyn dyn))
                          (values))])
    dyn)) 10)
"""
    #((lambda (dyn) (let ([or-part dyn][if53 dyn][ (begin (+ dyn dyn) (values))]) dyn)) 10)
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"], use_racket_read=True)
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 10
    # some weird uses of let may not have any ids for their rhs

#@pytest.mark.m
def test_partial_let_loop():
    p = """
    ((lambda (dyn)
     (letrec-values ([(loop) (lambda (i)
			   (if (<= i 1)
			    1
			    (* i (pe-test-stopper loop (sub1 i)))))])
      (loop dyn))) 5)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(5))
    assert kk == 120

#@pytest.mark.m
def test_partial_let_loop_dyn_rhs():
    p = """
    ((lambda (dyn)
       (letrec-values ([(loop) (lambda (i)
			   (if (> i dyn)
			    1
			    (* i (pe-test-stopper loop (add1 i)))))])
        (loop 1))) 5)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(5))
    assert kk == 120

#@pytest.mark.m
def test_partial_error_ran_while_pe():
    p = """
    ((lambda (dyn)
       (if (<= dyn 10)
           42
           (error "not dynamic" "at all"))) 10)
"""
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 42

#@pytest.mark.m
def test_partial_literal_letrec():
    p = """
    ((lambda (dyn)
     (letrec-values ([(x) dyn]
                     [(loop) (lambda (i)
			   (if (<= i 0)
			    dyn
			    (* i (pe-test-stopper loop (sub1 i)))))])
      (loop x))) 5)
    """
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"], use_racket_read=True)
    kk = run_residual_sexp(res_lam, W_Fixnum(5))
    assert kk == 600

def test_closure_arg_name_dyn_name():
    p = """
    ((lambda (dyn)
       (let-values ([(closure) (lambda (x dyn) (+ x (string-length dyn) (string-length (string-append dyn dyn))))])
         (closure dyn "caner"))) 10)
    """

    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 25

#@pytest.mark.m
def test_closure_arg_name_dyn_name2():
    p = """
    ((lambda (dyn)
       (let-values ([(closure) (lambda (x dyn) (+ x dyn))])
         (closure 35 (string-length dyn)))) "string")
    """

    # (lambda (in) (+ (string-length in) 5))

    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_String.make("string"))
    assert kk == 41

@pytest.mark.xfail
def test_different_types_in_branches():
    p = """
    ((lambda (dyn)
       (if (number? dyn) (+ dyn dyn) (string-length 3))) 10)
    """

    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    #import pdb;pdb.set_trace()
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 20

#@pytest.mark.m
def test_closure():
    p = """
    ((lambda (dyn)
      (let-values ([(var) (string->symbol "x")])
        (let-values ([(closure-dyn) (lambda (k) (begin (set! var (+ k k)) 3))])
          (let-values ([(a) 4])
            (begin (closure-dyn (+ a dyn)) var))))) 5)
    """
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    #import pdb;pdb.set_trace()
    kk = run_residual_sexp(res_lam, W_Fixnum(5))
    assert kk == 18

#@pytest.mark.m
def test_regular_rhs_set_bang_with_partial_value():
    p = """
    ((lambda (dyn)
      (let-values ([(var) (string->symbol "x")])
        (begin (set! var (+ dyn dyn)) var))) 5)
    """
    res_lam = partially_eval_app(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(5))
    assert kk == 10
