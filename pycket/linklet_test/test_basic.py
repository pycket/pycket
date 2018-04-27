#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

from pycket.linklet_test.testhelper import run_sexp, run_string, run_sexp_and_string

from pycket.interpreter import App, ModuleVar, Quote
from pycket.values import W_Symbol, W_Fixnum, W_Cons, w_null
from pycket.prims.linklet import sexp_to_ast
from pycket.error import SchemeException
from pycket.prims.primitive_tables import all_prims

def test_constant():
    ov = run_sexp_and_string("1", 1)

def test_read_err ():
    with pytest.raises(SchemeException):
        run_string("(")

def test_arithmetic():
    run_sexp_and_string("(+ )", 0)
    run_sexp_and_string("(+ 1)", 1)
    run_sexp_and_string("(+ 2 3)", 5)
    run_sexp_and_string("(+ 2 3 4)", 9)

    with pytest.raises(SchemeException):
        run_string("(- )", 0)
    run_sexp_and_string("(- 1)", -1)
    run_sexp_and_string("(- 2 3)", -1)
    run_sexp_and_string("(- 2 3 4)", -5)

    run_sexp_and_string("(* )", 1)
    run_sexp_and_string("(* 2)", 2)
    run_sexp_and_string("(* 2 3)", 6)
    run_sexp_and_string("(* 2 3 4)", 24)

    with pytest.raises(SchemeException):
        run_string("(/ )", 0)
    run_sexp_and_string("(/ 2.0)", 0.5)
    # run_string("(/ 2. 3.)", 2. / 3.)
    # run_string("(/ 2. 3. 4.)", 2. / 3. / 4.)

    run_sexp_and_string("(+ 1 2)", 3)
    run_sexp_and_string("(* 1 2)", 2)
    run_sexp_and_string("(- 1 2)", -1)
    run_sexp_and_string("(* -1 2)", -2)

def test_thunk():
    prog = "((lambda () 1))"
    run_sexp_and_string(prog, 1)

@pytest.mark.skip(reason="unhandled lambda form")
def test_thunk2():
    prog = "((lambda () 1 2))"
    run_sexp_and_string(prog, 2)

def test_call():
    prog = "((lambda (x) (+ x 1)) 2)"
    run_sexp_and_string(prog, 3)

def test_curry():
    prog = "(((lambda (y) (lambda (x) (+ x y))) 2) 3)"
    run_sexp_and_string(prog, 5)

# def test_letrec():
#     run_sexp_and_string("(letrec ([x 1]) x)", 1)
#     run_sexp_and_string("(letrec ([x 1] [y 2]) y)", 2)
#     run_sexp_and_string("(letrec ([x 1] [y 2]) (+ x y))", 3)
#     run_sexp_and_string("(let ([x 0]) (letrec ([x 1] [y x]) (+ x y)))", 2)
#     run_sexp_and_string("(letrec ([x (lambda (z) x)]) 2)", 2)
    
# def test_good1(capsys):
#     for i in range(5):
#         print i
#     out, err = capsys.readouterr()
#     open("err.txt", "w").write(err)
#     open("out.txt", "w").write(out)
