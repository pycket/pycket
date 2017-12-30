#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

from pycket.linklet_test.testhelper import run_ast, run_sexp, run_string

from pycket.interpreter import App, ModuleVar, Quote
from pycket.values import W_Symbol, W_Fixnum, W_Cons, w_null
from pycket.prims.linklet import sexp_to_ast

from pycket.prims.primitive_tables import all_prims

# FIXME: automatize the ast and sexp creation from python strings

def test_ast():
    plus_sym = W_Symbol.make("+")
    plus = ModuleVar(plus_sym, "#%kernel", plus_sym, None)
    rands = [Quote(W_Fixnum(1)), Quote(W_Fixnum(2))]
    ast = App.make(plus, rands)

    run_ast(ast, 3)
    run_ast(ast)

def test_sexp():
    run_sexp("(+ 1 2)", 3)
    run_sexp("(car (cons 1 2))", 1)

def test_string():
    run_string("(+ 1 2)", 3)
    
# def test_good1(capsys):
#     for i in range(5):
#         print i
#     out, err = capsys.readouterr()
#     open("err.txt", "w").write(err)
#     open("out.txt", "w").write(out)
