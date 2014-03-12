#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# A place for testing primitives
#

from pycket.test.testhelper import check_all, check_none, check_equal

def test_equal():
    check_all(
        "(equal? 1 1)",
        "(equal? 1.5 (+ 0.5 1))",
        "(equal? 'foo 'foo)",
        "(equal? '() '())",
        "(equal? #f #f)",
        "(equal? #t #t)",
        "(equal? (cons 1 2) (cons 1 2))",
        "(equal? (vector->list (make-vector 10000 5)) (vector->list (make-vector 10000 5)))",
        "(equal? #() #())",
        "(equal? #(1) #(1))",
        "(equal? #(1 2) #(1 2))",
    )
    check_none(
        "(equal? 1 2)",
        "(equal? 1 2.2)",
        "(equal? 1 1.0)",
        "(equal? 'foo 'bar)",
        "(equal? '() #f)",
        "(equal? #f #t)",
        "(equal? (cons 1 2) (cons 1 4))",
        "(equal? (cons 2 2) (cons 1 2))",
        "(equal? (cons 2 (cons 1 2)) (cons 1 (cons 1 (cons 1 6))))",
        "(equal? #(1) #())",
        "(equal? #(1 2 3 4 5) #(1 2 3 4 6))",
        "(equal? #(1 2 3 4 5) #(1 2 3 4 'a))",
    )

def test_list_vector_conversion():
    check_equal(
        "(vector->list #(1 2 3 4))", "(list 1 2 3 4)",
        "(vector->list #())", "'()",
        "(vector->list #(1.1 a))", "(list 1.1 'a)",
        "#(1 2 3 4)", "(list->vector (list 1 2 3 4))",
        "#()", "(list->vector '())",
        "#(1.1 a)", "(list->vector (list 1.1 'a))",
    )

