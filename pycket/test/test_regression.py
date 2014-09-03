#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# A place for regression tests
#

from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *
from pycket.test.testhelper import *

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
