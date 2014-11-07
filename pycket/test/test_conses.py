#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.values import *

from pycket.test.testhelper import execute, run_fix, run


class TestConses(object):

    def test_basics(self):
        _1, _2, _3 = W_Fixnum(1), W_Fixnum(2), W_Fixnum(3)
        c =  W_Cons.make(_1, _2)
        assert c.car().value == _1.value
        assert c.cdr().value == _2.value
        l = to_list([_1, _2, _3])
        assert l.car().value == _1.value
        assert l.cdr().car().value == _2.value
        assert l.cdr().cdr().car().value == _3.value
        assert l.cdr().cdr().cdr() == w_null


    def test_basic_hl(self):
        run_fix("(car (cons 1 2))", 1)
        run_fix("(cdr (cons 1 2))", 2)
        run_fix("(car (list 1))", 1)
        run_fix("(car (cdr (list 1 2)))", 2)
        run("(equal? (cons 1 2) (cons 1 2))", w_true)
        run("(equal? (cons 1 2) (cons 2 2))", w_false)
        run("(equal? (cons 1 2) 'barf)", w_false)
        run("(let ((x (cons 1 2))) (equal? x x))", w_true)
        run("(equal? (cons 1 (cons 2 3)) (cons 1 (cons 2 3)))", w_true)
        run("(equal? (cons 1 (cons 2 3)) (cons 1 (cons 3 3)))", w_false)
        run("(equal? (cons 1 (cons 2 3)) (cons 1 (cons 2 4)))", w_false)

    # white box type spec testing
    def test_intcons(self):
        _1 = W_Fixnum(1)
        _2 = W_Fixnum(2)
        c =  W_Cons.make(_1, _2)
        #here be whitebox
        assert isinstance(c, W_UnwrappedFixnumCons)
        assert isinstance(c.car(), W_Fixnum)
        assert isinstance(c.cdr(), W_Fixnum)

    def test_flocons(self):
        _1 = W_Flonum(1.2)
        _2 = W_Flonum(2.2)
        c =  W_Cons.make(_1, _2)
        #here be whitebox
        assert isinstance(c, W_UnwrappedFlonumCons)
        assert isinstance(c.car(), W_Flonum)
        assert isinstance(c.cdr(), W_Flonum)

    def test_proper(self):
        assert w_null.is_proper_list()
        _1 = W_Flonum(1.2)
        _2 = W_Flonum(2.2)
        c =  W_Cons.make(_1, _2)
        assert not c.is_proper_list()

        c = W_Cons.make(_1, w_null)
        assert c.is_proper_list()

        c = W_Cons.make(c, c)
        assert c.is_proper_list()

        c =  W_Cons.make(W_Fixnum(1), c)
        assert c.is_proper_list()
