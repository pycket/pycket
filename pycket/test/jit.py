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
from pycket.test.testhelper import parse_file
from pycket.expand import expand, to_ast
from pycket.interpreter import *
from pycket.values import *
from pycket.test.testhelper import run, run_fix, run_flo, run_top, execute, run_values
from pycket.expand import expand, to_ast, expand_string, parse_module


class TestLLtype(LLJitMixin):

    def test_countdown_x(self):
        ast = parse_module(expand_string("""
#lang pycket
        (letrec ([countdown (lambda (n m) (if (< n 0) m (countdown (- n 1) (+ 1 m))))])
        (countdown 1000 1000))
"""))


        def interp_w():
            val = interpret_module(ast)
            return val

        assert interp_w()

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_countdown_nested(self):
        ast = to_ast(expand("""
(let ([sub 1])
  (define (nested n)
    (let countdown ([n n]) (if (< n 0) 1 (countdown (- n sub))))
    (if (< n 0) 1 (nested (- n sub))))
  (nested 10))
"""))
        def interp_w():
            val = interpret_one(ast)
            ov = check_one_val(val)
            assert isinstance(ov, W_Fixnum)
            return ov.value

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_countdown_loop(self):
        ast = to_ast(expand("""
#lang pycket
(let countdown ([n 1000]) (if (< n 0) 1 (countdown (- n 1))))
"""))


        def interp_w():
            val = interpret_one(ast)
            ov = check_one_val(val)
            assert isinstance(ov, W_Fixnum)
            return ov.value

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_bistable_loop(self):
        ast = parse_module(expand_string("""
#lang pycket
(let ()
    (define (countdown n sub2?)
        (if (< n 0) 1
            (if sub2?
                (countdown (- n 2) #f)
                (countdown (- n 1) #t)
                )))
    (countdown 1000 #f)
)
"""))


        def interp_w():
            val = interpret_module(ast)
            return val

        #     val = interpret_one(ast)
        #     ov = check_one_val(val)
        #     assert isinstance(ov, W_Fixnum)
        #     return ov.value
        # assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_setbang(self):

        ast = to_ast(expand("(let ([n 1000]) (letrec ([countdown (lambda () (if (< n 0) 1 (begin (set! n (- n 1)) (countdown))))]) (countdown)))"))

        def interp_w():
            val = interpret_one(ast)
            ov = check_one_val(val)
            assert isinstance(ov, W_Fixnum)
            return ov.value

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_side_exit(self):
        ast = to_ast(expand("""
(let ()
    (define (count-positive l sum)
        (if (null? l) sum
            (if (> (car l) 0)
                (count-positive (cdr l) (+ (car l) sum))
                (count-positive (cdr l) sum)
                )))
    (count-positive (list -1 1 1 1 1 -1 2 3 -5 1 2 2 -5 6 4 3 -5) 0))

"""))


        def interp_w():
            val = interpret_one(ast)
            ov = check_one_val(val)
            assert isinstance(ov, W_Fixnum)
            return ov.value

        assert interp_w() == 27

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

        
    # needs to be fixed to use modules
    def run_string(self, str):
        ast = to_ast(expand(str))

        def interp_w():
            val = interpret_one(ast)
            return val

        interp_w() # check that it runs

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_imp_vec(self):

        ast = parse_module(expand_string("""#lang pycket
 (let ([v (impersonate-vector (make-vector 1000 5)
                              (lambda (x y z) (unless (integer? z) (error 'fail)) z)
                              (lambda (x y z) z))])
      (let lp ([n 0] [i 0])
        (if (>= i 1000) n (lp (+ n (vector-ref v i)) (+ 1 i)))))
"""))

        def interp_w():
            interpret_module(ast)

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_puzzle(self):
        fname = "puzzle.sch"
        ast = parse_file(fname)
        def interp_w():
            val = interpret_one(ast)
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def run_file(self, fname):
        ast = parse_file(fname)
        def interp_w():
            val = interpret_module(ast)
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)
        


    def test_bubble_safe(self):
        self.run_file("bubble.rkt")


    def test_bubble_unsafe(self):
        self.run_file("bubble-unsafe.rkt")

    def test_bubble_unsafe2(self):
        self.run_file("bubble-unsafe2.rkt")

    def test_bubble_imp(self):
        self.run_file("bubble-imp.rkt")
    def test_bubble_imp_check(self):
        self.run_file("bubble-imp-check.rkt")

    def test_bubble_unsafe(self):
        self.run_file("bubble-unsafe.sch")

    def test_bubble_arg(self):
        self.run_file("bubble-arg.rkt")

    def test_sumloop(self):
        self.run_file("sumloop.rkt")

    def test_treerec(self):
        self.run_file("treerec.rkt")

    def test_nqueens(self):
        self.run_file("nqueens.rkt")

    def test_ack(self):
        self.run_file("ack.rkt")

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

    def test_earley(self):
        fname = "earley.sch"
        ast = parse_file(fname)
        def interp_w():
            val = interpret_one(ast)
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_triangle(self):
        fname = "triangle.sch"
        ast = parse_file(fname)
        def interp_w():
            val = interpret_one(ast)
            return val

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_append(self):
        ast = parse_module(expand_string("""
#lang pycket
(let () (define (append a b)
  (if (null? a) 
      b
      (cons (car a) (append (cdr a) b))))
 (append (list 1 2 3 5 6 6 7 7 8 3 4 5 3 5 4 3 5 3 5 3 3 5 4 3) (list 4 5 6)))
"""
))

        def interp_w():
            interpret_module(ast)

        interp_w()

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
            ov = check_one_val(val)
            assert isinstance(ov, W_Object)
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
        ast = parse_module(expand_string("#lang pycket ((%s %s) 1000)"%(Y,countdown)))

        def interp_w():
            val = interpret_module(ast)
            return

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
            ov = check_one_val(val)
            assert isinstance(ov, W_Fixnum)
            return val.value

        assert interp_w() == sum(i ** 2 for i in range(1000))

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)


    def _cons_map(self):
        from pycket.expand import expand_string
        from pycket.json import loads

        ast = to_ast(loads(expand_string("""
(letrec ([range-it (lambda (n a)
                           (if (<= n 0)
                               a
                               (range-it (- n 1) (cons n a))))]
         [range (lambda (n) (range-it n '()))])
  (foldl + 0 (range 1000)))
""", wrap=True, stdlib=True)))

        def interp_w():
            val = interpret_one(ast)
            ov = check_one_val(val)
            assert isinstance(ov, W_Fixnum)
            return ov.value
        assert interp_w() == 500500

        self.meta_interp(interp_w, [],
                         listcomp=True, listops=True, backendopt=True)

    def test_scons_map(self):
        import pycket.values
        pycket.values._enable_cons_specialization = True
        self._cons_map()

    def test_ucons_map(self):
        import pycket.values
        pycket.values._enable_cons_specialization = False
        self._cons_map()

    def test_countdown_vector_allocation(self):
        ast = to_ast(expand("""
(letrec ([countdown (lambda (n) (if (< n 0) 1 (let ([v (vector n 1)]) (countdown (- (vector-ref v 0) (vector-ref v 1))))))])
 (countdown 1000))"""))

        def interp_w():
            val = interpret_one(ast)
            ov = check_one_val(val)
            assert isinstance(ov, W_Fixnum)
            return ov.value

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_binarytree(self):
        self.run_file("binarytree.rkt")

    def test_mappy(self):
        ast = parse_module(expand_string("""#lang racket/base
        ;(require (only-in '#%kernel map))
        (letrec
            ([inc      (lambda (x) (+ 1 x))]
             [makelist (lambda (a) 
                         (if (zero? a)
                             '()
                             (cons (modulo a 20) (makelist (- a 1)))))]
             [l        (makelist 10000)])
          (map inc l))
        """))

        def interp_w():
            val = interpret_module(ast)
            return val

        # assert interp_w()

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

