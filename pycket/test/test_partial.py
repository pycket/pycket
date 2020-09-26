#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

from pycket.values import W_Fixnum
from pycket.test.testhelper import partially_eval_app_str, run_residual_sexp

def test_basic_setup():
    res_lam = partially_eval_app_str("(+ 1 2)")
    assert res_lam.tostring() == '(lambda () 3)'

def test_partial_let_set():
    p = """
    ((lambda (dyn) 
       (let-values ([(y) 2]
                    [(v) (make-vector dyn)]
                    [(x) 1]
                    [(t) (make-vector dyn)])
         (vector-set! v 0 1)
         (vector-set! v 1 1)
         (+ (vector-ref v 0) (vector-ref v 1)))) 10)
"""
    res_lam = partially_eval_app_str(p, dyn_var_names=["dyn"])
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 2

def test_different_dynamic_name():
    p = """
    ((lambda (dyn)
       (let-values ([(lexer)
                     (lambda (l) (+ 2 l))]) 
         (lexer dyn))) 10)
"""
    res_lam = partially_eval_app_str(p, dyn_var_names=["dyn"])
    #import pdb;pdb.set_trace()
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
    res_lam = partially_eval_app_str(p, dyn_var_names=["dyn"])
    # import pdb;pdb.set_trace()
    kk = run_residual_sexp(res_lam, W_Fixnum(10))
    assert kk == 13
    
