#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Test.
#

import pycket.prims
import pycket.entry_point
import sys
from rpython import conftest

class o:
    view = False
    viewloops = True
conftest.option = o

from rpython.rlib.nonconst import NonConstant
from rpython.jit.metainterp.test.test_ajit import LLJitMixin

import pytest
from pycket.test.testhelper import parse_file
from pycket.interpreter import *
from pycket.values import *
from pycket.test.testhelper import run, run_fix, run_flo, run_top, execute, run_values
from pycket.expand import JsonLoader, expand, expand_string, parse_module, finalize_module
from pycket import pycket_json


class TestLLtype(LLJitMixin):

    def test_sieve00(self):
        self.run_string(u"""
#lang pycket

;; Use the partner file "streams.rkt" to implement the Sieve of Eratosthenes.
;; Then compute and print the 10,000th prime number.


;; ;; A stream is a cons of a value and a thunk that computes the next value when applied
(struct stream (first rest))

;;--------------------------------------------------------------------------------------------------

(define (make-stream hd thunk)
  (stream hd thunk))

;; Destruct a stream into its first value and the new stream produced by de-thunking the tail
(define (stream-unfold st)
  (values (stream-first st) ((stream-rest st))))

;; [stream-get st i] Get the [i]-th element from the stream [st]
(define (stream-get st i)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= i 0) hd]
        [else    (stream-get tl (sub1 i))]))

;; [stream-take st n] Collect the first [n] elements of the stream [st].
(define (stream-take st n)
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (stream-unfold st))
              (cons hd (stream-take tl (sub1 n)))]))
;;--------------------------------------------------------------------------------------------------

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define (count-from n)
  (make-stream n (lambda () (count-from (add1 n)))))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new stream.
(define (sift n st)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-stream hd (lambda () (sift n tl)))]))

;; `sieve st` Sieve of Eratosthenes
(define (sieve st)
  (define-values (hd tl) (stream-unfold st))
  (make-stream hd (lambda () (sieve (sift hd tl)))))

;; stream of prime numbers
(define primes (sieve (count-from 2)))

;; Compute the 10,000th prime number
(define N-1 9)

(define (main)
  (printf "The ~a-th prime number is: ~a\n" (add1 N-1) (stream-get primes N-1)))

(time (main))
"""
)


    def test_sieve01(self):
        self.run_file("sieve01.rkt")


    def test_unroll_regression(self):
        self.run_string(u"""
        #lang pycket

        (define N 1000)

        (define loop
          (λ (f n acc N)
            (if (>= n N) acc
              (loop f (+ n 1) (f acc) N))))

        (define f3 (λ (x) (add1 x)))
        (define f4 (λ (x) (add1 x)))

        ;; These are the same loop, but the second one is 10x slower
        (loop f3 0 0 N)
        (loop f4 0 0 N)
        """)

    def test_countdown_x(self):
        self.run_string("""
        #lang pycket
        (letrec ([countdown (lambda (n m) (if (< n 0) m (countdown (- n 1) (+ 1 m))))])
          (countdown 1000 1000))
        """)

    def test_countdown_nested(self):
        self.run_string("""
        #lang pycket
        (let ([sub 1])
          (define (nested n)
            (let countdown ([n n]) (if (< n 0) 1 (countdown (- n sub))))
            (if (< n 0) 1 (nested (- n sub))))
          (nested 10))
        """)


    def test_countdown_loop(self):
        self.run_string("""
        #lang pycket
        (let countdown ([n 1000]) (if (< n 0) 1 (countdown (- n 1))))
        """)

    def test_countdown_many_lets(self):
        self.run_string("""
        #lang pycket
        (define (id x) x)
        (let countdown ([n 1000]) (if (< n 0) 1 (id (id (id (id (countdown (- n 1))))))))
        """)

    def test_bistable_loop(self):
        self.run_string("""
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
        """)


    def test_setbang(self):

        loader = JsonLoader(bytecode_expand=False)
        ast = loader.to_ast(expand("""
        #lang pycket
        (let ([n 1000])
          (letrec ([countdown (lambda () (if (< n 0) 1 (begin (set! n (- n 1)) (countdown))))])
            (countdown)))
        """))

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
        _json = expand_string(str)
        env = ToplevelEnv()

        def interp_w():
            loader = JsonLoader(False)
            ast = parse_module(_json)
            env.globalconfig.load(ast)
            env.commandline_arguments = []
            interpret_module(ast, env)

        interp_w() # check that it runs

        # ast = parse_module(expand_string(str))
        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)

    def test_imp_vec(self):

        self.run_string("""
        #lang pycket
        (let ([v (impersonate-vector (make-vector 1000 5)
                                     (lambda (x y z) (unless (integer? z) (error 'fail)) z)
                                     (lambda (x y z) z))])
             (let lp ([n 0] [i 0])
               (if (>= i 1000) n (lp (+ n (vector-ref v i)) (+ 1 i)))))
        """)


    def test_puzzle(self):
        self.run_file("puzzle.rkt")

    def test_paraffins(self):
        self.run_file("paraffins.rkt", run_untranslated=True)

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
        self.run_file("treerec.rkt", run_untranslated=False)

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
        self.run_file("triangle.rkt", run_untranslated=False)

    def test_append(self):
        self.run_string("""
        #lang pycket
        (let () (define (append a b)
          (if (null? a)
              b
              (cons (car a) (append (cdr a) b))))
         (append (list 1 2 3 5 6 6 7 7 8 3 4 5 3 5 4 3 5 3 5 3 3 5 4 3) (list 4 5 6)))
        """)

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

        self.run_string("#lang pycket ((%s %s) 1000)"%(Y,countdown))


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


    def test_cons_map(self):
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

    def test_church(self):
        self.run_file("church-simple.rkt")

    def test_spectral_norm(self):
        self.run_file("spectral-norm.rkt")

    def test_fannkuch(self):
        self.run_file("fannkuch-redux.rkt")

    def test_mappy(self):
        self.run_string("""#lang racket/base
        ;(require (only-in '#%kernel map))
        (letrec
            ([inc      (lambda (x) (+ 1 x))]
             [makelist (lambda (a)
                         (if (zero? a)
                             '()
                             (cons (modulo a 20) (makelist (- a 1)))))]
             [l        (makelist 10000)])
          (map inc l))
        """)
    def test_shadow_bug(self):
        self.run_file("bugtest2.rkt", run_untranslated=False)

    def test_dot_ex(self):
        self.run_file("dot-jit.rkt", run_untranslated=False)

    def test_ctak(self):
        self.run_file("ctak.rkt", run_untranslated=False)

    def test_tak(self):
        self.run_string("""
        #lang pycket

        (define (tak x y z)
          (if (not (< y x))
              z
              (tak (tak (- x 1) y z)
                   (tak (- y 1) z x)
                   (tak (- z 1) x y))))

        (time (for ([i (in-range 1000)]) (tak 18 12 6)))
        """)

    def test_cons_emulation(self):
        from pycket.test.test_shape import SConf
        with SConf(substitution_threshold=2):
            self.run_file("cons-emulation.rkt", run_untranslated=False)

    def test_cons_emulation_reverse(self):
        from pycket.test.test_shape import SConf
        with SConf(substitution_threshold=2):
            self.run_file("cons-emulation-reverse.rkt", run_untranslated=False)

    def test_cons_emulation_map(self):
        from pycket.test.test_shape import SConf
        with SConf(substitution_threshold=2):
            self.run_file("cons-emulation-map.rkt", run_untranslated=False)

    def test_cons_emulation_filter(self):
        from pycket.test.test_shape import SConf
        with SConf(substitution_threshold=2):
            self.run_file("cons-emulation-filter.rkt", run_untranslated=False)
