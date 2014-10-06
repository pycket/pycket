#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# A place for testing primitives
#

import pytest
from pycket.values import w_true
from pycket.test.testhelper import check_all, check_none, check_equal, run_flo, run_fix, run, run_mod
from pycket.error import SchemeException

def test_equal():
    check_all(
        "(equal? 1 1)",
        "(equal? 1.5 (+ 0.5 1))",
        "(equal? 1+1i 1+1i)",
        "(equal? 'foo 'foo)",
        "(equal? '() '())",
        "(equal? #f #f)",
        "(equal? #t #t)",
        "(equal? (cons 1 2) (cons 1 2))",
        "(equal? (vector->list (make-vector 10000 5)) (vector->list (make-vector 10000 5)))",
        "(equal? #() #())",
        "(equal? #(1) #(1))",
        "(equal? #(1 2) #(1 2))",
        '(equal? "abc" "abc")',
    )
    check_none(
        "(equal? 1 2)",
        "(equal? 1 2.2)",
        "(equal? 1 1.0)",
        "(equal? 1+1i 1.0+0i)",
        "(equal? 1+1i 1)",
        "(equal? 'foo 'bar)",
        "(equal? '() #f)",
        "(equal? #f #t)",
        "(equal? (cons 1 2) (cons 1 4))",
        "(equal? (cons 2 2) (cons 1 2))",
        "(equal? (cons 2 (cons 1 2)) (cons 1 (cons 1 (cons 1 6))))",
        "(equal? #(1) #())",
        "(equal? #(1 2 3 4 5) #(1 2 3 4 6))",
        "(equal? #(1 2 3 4 5) #(1 2 3 4 'a))",
        '(equal? "abc" "def")',
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

def test_substring(doctest):
    """
    > (substring "Apple" 1 3)
    "pp"
    > (substring "Apple" 1)
    "pple"
    """
    assert doctest
    check_equal(
        '(substring "applebee" 5)', '"bee"',
        '(substring "applebee" 0 8)', '"applebee"',
    )


def test_append_single(doctest):
    """
    > (append (list 1 2) (list 3 4))
    '(1 2 3 4)
    """
    assert doctest

def test_append_vararg(doctest):
    """
    > (append (list 1 2) (list 3 4) (list 5 6) (list 7 8))
    '(1 2 3 4 5 6 7 8)
    """
    assert doctest

def test_for_each_single(doctest):
    """
    ! (require (only-in '#%kernel for-each))
    > (let ([x 0])
        (for-each (lambda (y)
                    (set! x (+ x y)))
                  '(1 2 3))
        x)
    6
    """
    assert doctest

@pytest.mark.xfail
def test_for_each_vararg(doctest):
    """
    ! (require (only-in '#%kernel for-each))
    > (let ([x 1])
        (for-each (lambda (a b c)
                    (set! x (+ x (* a b c))))
                  '(1 2 3) '(4 5 6) '(7 8 9))
        x)
    271
    """
    assert doctest

def test_map(doctest):
    """
    ! (require (only-in '#%kernel map))
    > (map (lambda (number)
             (+ 1 number))
           '(1 2 3 4))
    '(2 3 4 5)
    > (map (lambda (number1 number2)
             (+ number1 number2))
           '(1 2 3 4)
           '(10 100 1000 10000))
    '(11 102 1003 10004)
    """
    assert doctest

def test_random():
    for i in range(100):
        x = run_flo("(random)")
        assert 0.0 <= x < 1.0
        x = run_fix("(random %s)" % (5 + i))
        assert 0 <= x < i + 5


def test_random_seed():
    run("(begin (random-seed 142) (let ((x (random))) (random-seed 142) (= (random) x)))", w_true)


def test_flvector(doctest):
    """
    ! (require '#%flfxnum '#%unsafe)
    > (flvector-ref (flvector 0.0) 0)
    0.0
    > (define v (flvector 0.0 1.0))
    > (flvector-ref v 0)
    0.0
    > (flvector-ref v 1)
    1.0
    > (flvector-set! v 0 2.0)
    (void)
    > (flvector-ref v 0)
    2.0
    > (unsafe-flvector-ref v 0)
    2.0
    > (unsafe-flvector-set! v 0 3.0)
    (void)
    > (flvector-ref v 0)
    3.0
    > (define v2 (make-flvector 5))
    > (flvector-ref v2 4)
    0.0
    > (define v3 (make-flvector 5 3.0))
    > (flvector-ref v3 4)
    3.0
    """
    assert doctest

def test_flvector_set_wrong_type():
    with pytest.raises(SchemeException):
        run_mod("""
            #lang pycket
            (require '#%flfxnum '#%unsafe)
            (let [(a (flvector 1.2 1.3))] (flvector-set! a 1 'a))
        """)

def test_byte_huh(doctest):
    """
    > (byte? 65)
    #t
    > (byte? 0)
    #t
    > (byte? 256)
    #f
    > (byte? -1)
    #f
    """

def test_make_bytes(doctest):
    """
    > (make-bytes 5 65)
    #"AAAAA"
    """
