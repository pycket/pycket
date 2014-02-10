#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.values import *

from pycket.test.test_basic import execute, run_fix, run


class TestConses(object):

    def test_basics(self):
        c =  W_Cons.make(1, 2)
        assert c.car() == 1
        assert c.cdr() == 2
        l = to_list([1, 2, 3])
        assert l.car() == 1
        assert l.cdr().car() == 2
        assert l.cdr().cdr().car() == 3
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
