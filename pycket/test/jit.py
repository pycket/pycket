#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Test.
#

import sys
from rpython import conftest
class o:
    view = False
    viewloops = True
conftest.option = o
from rpython.jit.metainterp.test.test_ajit import LLJitMixin

import pytest
from pycket.test.test_larger import parse_file
from pycket.expand import expand, to_ast
from pycket.interpreter import *
from pycket.values import *


class TestLLtype(LLJitMixin):

    def test_countdown(self):
        ast = to_ast(expand("""
(letrec ([countdown (lambda (n) (if (< n 0) 1 (countdown (- n 1))))])
 (countdown 1000))
"""))


        def interp_w():
            val = interpret_one(ast)
            assert isinstance(val, W_Fixnum)
            return val.value

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)


    def test_countdown_loop(self):
        ast = to_ast(expand("""
(let countdown ([n 1000]) (if (< n 0) 1 (countdown (- n 1))))
"""))


        def interp_w():
            val = interpret_one(ast)
            assert isinstance(val, W_Fixnum)
            return val.value

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_setbang(self):

        ast = to_ast(expand("(let ([n 1000]) (letrec ([countdown (lambda () (if (< n 0) 1 (begin (set! n (- n 1)) (countdown))))]) (countdown)))"))

        def interp_w():
            val = interpret_one(ast)
            assert isinstance(val, W_Fixnum)
            return val.value

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_puzzle(self):
        fname = "puzzle.sch"
        ast = parse_file(fname)
        def interp_w():
            val = interpret_one(ast)
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_bubble(self):
        fname = "bubble.sch"
        ast = parse_file(fname)
        def interp_w():
            val = interpret([ast])
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_bubble_arg(self):
        fname = "bubble-arg.sch"
        ast = parse_file(fname)
        def interp_w():
            val = interpret([ast])
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_pseudoknot(self):
        fname = "nucleic2.sch"
        ast = parse_file(fname)
        def interp_w():
            val = interpret([ast])
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_minik(self):
        fname = "minikanren.sch"
        ast = parse_file(fname)
        def interp_w():
            val = interpret_one(ast)
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_microk(self):
        fname = "microkanren.sch"
        ast = parse_file(fname)
        def interp_w():
            val = interpret_one(ast)
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

                     

    def test_append(self):
        ast = to_ast(expand("""
(let () (define (append a b)
  (if (null? a) 
      b
      (cons (car a) (append (cdr a) b))))
 (append (list 1 2 3 5 6 6 7 7 8 3 4 5 3 5 4 3 5 3 5 3 3 5 4 3) (list 4 5 6)))
"""
))

        def interp_w():
            val = interpret_one(ast)
            assert isinstance(val, W_Object)
            return 1

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)


    def test_let_append(self):
        ast = to_ast(expand("""
(let ()
(define (let-append a b)
  (let ([tst (null? a)])
      (if tst
          b
          (cons (car a) (let-append (cdr a) b)))))
 (let-append (list 1 2 3 5 6 6 7 7 8 3 4 5 3 5 4 3 5 3 5 3 3 5 4 3) (list 4 5 6)))
"""
))

    def test_anormal_append(self):
        ast = to_ast(expand("""
(let ()
(define (append-anormal a b)
  (if (null? a) 
      b
      (let* ([ca (car a)]
             [cd (cdr a)]
             [ap (append-anormal cd b)])
        (cons ca ap))))
 (append-anormal (list 1 2 3 5 6 6 7 7 8 3 4 5 3 5 4 3 5 3 5 3 3 5 4 3) (list 4 5 6)))
"""
))

        def interp_w():
            val = interpret_one(ast)
            assert isinstance(val, W_Object)
            return 1

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_ycombinator(self):

        Y = """
      (lambda (f)
        ((lambda (x) (x x))
         (lambda (g)
           (f (lambda (z) ((g g) z))))))
    """
        countdown = """
        (lambda (f)
          (lambda (x)
            (if (< x 0)
                1
                (f (- x 1)))))
     """
        ast = to_ast(expand("((%s %s) 1000)"%(Y,countdown)))

        def interp_w():
            val = interpret_one(ast)
            assert isinstance(val, W_Fixnum)
            return val.value

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)


    def test_vector_get_set(self):
        ast = to_ast(expand("""
(letrec ([v (make-vector 1000 0)]
         [fill-square-vector (lambda (n)
                                (if (< n 0)
                                  (vector-ref v 999)
                                  (begin (vector-set! v n (* n n))
                                         (fill-square-vector (- n 1)))))]
         [sum-vector (lambda (n s)
                        (if (< n 0)
                            s
                            (sum-vector (- n 1) (+ s (vector-ref v n)))))])
 (begin (fill-square-vector 999) (sum-vector 999 0)))
"""))


        def interp_w():
            val = interpret_one(ast)
            assert isinstance(val, W_Fixnum)
            return val.value

        assert interp_w() == sum(i ** 2 for i in range(1000))

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

