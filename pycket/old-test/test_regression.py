#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# A place for regression tests
#

import pytest
from pycket.values import *
from pycket.interpreter import *
from pycket.prims import *
from pycket.test.testhelper import *

skip = pytest.mark.skipif("True")

class TestRegressions(object):

    def test_cell_closure(self, source):
        """
        (define (append a b)
          (if (null? a)
              b
              (cons (car a) (append (cdr a) b))))

        (define foldr
          (lambda (f base lst)

            (define foldr-aux
              (lambda (lst)
                (if (null? lst)
                    base
                    (f (car lst) (foldr-aux (cdr lst))))))

            (foldr-aux lst)))

        (define concat
          (lambda (lists)
            (foldr append '() lists)))

        (concat '((0 1 2 3) (4 5 6) (7 8 9)))
        """
        w_list = to_list([W_Fixnum(i) for i in range(10)])
        ret = run_mod_expr(source, wrap=True)
        assert ret.equal(w_list)

    @pytest.mark.xfail
    def test_regexp_args(self, doctest):
        """
        > (regexp-match #rx"^-$" "/")
        #f
        > (regexp-match #"^-$" "/")
        #f
        > (regexp-match? #rx"^-$" "/")
        #t
        > (regexp-match? #"^-$" "/")
        #t
        """
        assert doctest

    def test_artiy_does_not_break_contracts(self, source):
        """
        #lang racket/base
        (define ((f x) #:y [y 0]) 0)
        ((procedure-rename (f 1) 'x) #:y 0)
        (define result #t)
        """
        m = run_mod(source)
        assert m.defs[W_Symbol.make("result")] == w_true

    def test_rhss_vs_args(self):
        run_file("rhss_vs_args.rkt")

    @skip
    # This can't work consistently because of paths plus use of stdin
    # We could make something work here by dynamically writing out a file
    # that knows the path to wraptest.rkt
    def test_name_shadowing_huh(self):
        run_file("bugtest2.rkt")

    def test_contract_structs(self, source):
        """
        #lang racket/base
        (require racket/contract)

        (define-contract-struct y (c d))
        (define-opt/c (yopt) (y/dc))
        (define result (y-c (contract (yopt) (make-y 1 1) 'pos 'neg)))
        """
        m = run_mod(source)
        result = W_Symbol.make("result")
        assert type(m.defs[result]) is W_Fixnum and m.defs[result].value == 1

    def test_quote_syntax_expansion(self, source):
        """
        #lang typed/racket/base
        (provide (struct-out stream) make-stream)
        (struct: stream ([first : Natural] [rest : (-> stream)]))
        (: make-stream (-> Natural (-> stream) stream))
        (define (make-stream hd thunk)
          (stream hd thunk))
        """
        # TODO It would be nice to have an example of this problem that
        # does not use Typed Racket

        # Really only intereted in whether or not expansion works and
        # parses properly on the Pycket side
        m = run_mod(source)

    def test_constr_arity_check(self):
        with pytest.raises(SchemeException):
            run_mod("""#lang racket/base
            (struct x (a)) (x)""")
        with pytest.raises(SchemeException):
            run_mod("""#lang racket/base
            (struct x ()) (x 2)""")

