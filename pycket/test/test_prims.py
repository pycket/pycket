#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# A place for testing primitives
#

import pytest
import sys
from pycket.values import w_true
from pycket.test.testhelper import check_all, check_none, check_equal, run_flo, run_fix, run, run_mod, run_mod_expr
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

###############################################################################

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

def test_shorthands(doctest):
    """
    > (caar '((1 2) 3 4))
    1
    > (cadr '((1 2) 3 4))
    3
    > (cdar '((7 6 5 4 3 2 1) 8 9))
    '(6 5 4 3 2 1)
    > (cddr '(2 1))
    '()
    > (caaar '(((6 5 4 3 2 1) 7) 8 9))
    6
    > (caadr '(9 (7 6 5 4 3 2 1) 8))
    7
    > (cadar '((7 6 5 4 3 2 1) 8 9))
    6
    > (caddr '(3 2 1))
    1
    > (cdaar '(((6 5 4 3 2 1) 7) 8 9))
    '(5 4 3 2 1)
    > (cdadr '(9 (7 6 5 4 3 2 1) 8))
    '(6 5 4 3 2 1)
    > (cddar '((7 6 5 4 3 2 1) 8 9))
    '(5 4 3 2 1)
    > (cdddr '(3 2 1))
    '()
    > (caaaar '((((5 4 3 2 1) 6) 7) 8 9))
    5
    > (caaadr '(9 ((6 5 4 3 2 1) 7) 8))
    6
    > (caadar '((7 (5 4 3 2 1) 6) 8 9))
    5
    > (caaddr '(9 8 (6 5 4 3 2 1) 7))
    6
    > (cadaar '(((6 5 4 3 2 1) 7) 8 9))
    5
    > (cadadr '(9 (7 6 5 4 3 2 1) 8))
    6
    > (caddar '((7 6 5 4 3 2 1) 8 9))
    5
    > (cadddr '(4 3 2 1))
    1
    > (cdaaar '((((5 4 3 2 1) 6) 7) 8 9))
    '(4 3 2 1)
    > (cdaadr '(9 ((6 5 4 3 2 1) 7) 8))
    '(5 4 3 2 1)
    > (cdadar '((7 (5 4 3 2 1) 6) 8 9))
    '(4 3 2 1)
    > (cdaddr '(9 8 (6 5 4 3 2 1) 7))
    '(5 4 3 2 1)
    > (cddaar '(((6 5 4 3 2 1) 7) 8 9))
    '(4 3 2 1)
    > (cddadr '(9 (7 6 5 4 3 2 1) 8))
    '(5 4 3 2 1)
    > (cdddar '((7 6 5 4 3 2 1) 8 9))
    '(4 3 2 1)
    > (cddddr '(4 3 2 1))
    '()
"""
###############################################################################
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
#############################################################################
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

def test_make_bytes_create(doctest):
    """
    > (make-bytes 5 65)
    #"AAAAA"
    > (bytes 65 112 112 108 101)
    #"Apple"
    """


def test_list_to_bytes(doctest):
    """
    > (list->bytes (list 65 112 112 108 101))
    #"Apple"
    """

def test_bytes(doctest):
    """
    > (bytes-length #"Apple")
    5
    > (bytes-ref #"Apple" 0)
    65
    > (define s (bytes 65 112 112 108 101))
    > (bytes-set! s 4 121)
    > s
    #"Apply"
    """


def test_unsafe_bytes(doctest):
    """
    ! (require '#%unsafe)
    > (unsafe-bytes-length #"Apple")
    5
    > (unsafe-bytes-ref #"Apple" 0)
    65
    > (define s (bytes 65 112 112 108 101))
    > (unsafe-bytes-set! s 4 121)
    > s
    #"Apply"
    """

def test_subbytes(doctest):
    """
    > (subbytes #"Apple" 1 3)
    #"pp"
    > (subbytes #"Apple" 1)
    #"pple"
    """

def test_bytes_copy_bang(doctest):
    """
    > (define s (bytes 65 112 112 108 101))
    > (bytes-copy! s 4 #"y")
    > (bytes-copy! s 0 s 3 4)
    > s
    #"lpply"
    """

def test_open_input_bytes_and_read_bytes_line(source):
    """
    (let* ([b (string->bytes/locale "ABC\nDEF\n\nGHI\n\nJKL\n\n\nMNOP\n")]
           [expected '(#"MNOP" #"" #"" #"JKL" #"" #"GHI" #"" #"DEF" #"ABC")]
           [inport (open-input-bytes b)])
      (let ([res (let rev ([lines null])
                   (let ([line (read-bytes-line inport)])
                     (if (eof-object? line)
                         lines
                         (rev (cons line lines)))))])
        (equal? res expected)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_string_copy_bang(doctest):
    """
    > (define s (string #\A #\p #\p #\l #\e))
    > (string-copy! s 4 "y")
    > (string-copy! s 0 s 3 4)
    > s
    "lpply"
    """

def test_string_comparison(doctest):
    """
    > (string=? "Apple" "apple")
    #f
    > (string=? "a" "as" "a")
    #f
    > (string<? "Apple" "apple")
    #t
    > (string<? "apple" "Apple")
    #f
    > (string<? "a" "b" "c")
    #t
    > (string<=? "Apple" "apple")
    #t
    > (string<=? "apple" "Apple")
    #f
    > (string<=? "a" "b" "b")
    #t
    > (string>? "Apple" "apple")
    #f
    > (string>? "apple" "Apple")
    #t
    > (string>? "c" "b" "a")
    #t
    > (string>=? "Apple" "apple")
    #f
    > (string>=? "apple" "Apple")
    #t
    > (string>=? "c" "b" "b")
    #t
    > (string-ci=? "Apple" "apple")
    #t
    > (string-ci=? "a" "a" "a")
    #t
    > (string-ci<? "Apple" "apple")
    #f
    > (string-ci<? "apple" "banana")
    #t
    > (string-ci<? "a" "b" "c")
    #t
    > (string-ci<=? "Apple" "apple")
    #t
    > (string-ci<=? "apple" "Apple")
    #t
    > (string-ci<=? "a" "b" "b")
    #t
    > (string-ci>? "Apple" "apple")
    #f
    > (string-ci>? "banana" "Apple")
    #t
    > (string-ci>? "c" "b" "a")
    #t
    > (string-ci>=? "Apple" "apple")
    #t
    > (string-ci>=? "apple" "Apple")
    #t
    > (string-ci>=? "c" "b" "b")
    #t
    """

def test_bytes_comparison(doctest):
    """
    > (bytes=? #"Apple" #"apple")
    #f
    > (bytes=? #"a" #"as" #"a")
    #f
    > (bytes<? #"Apple" #"apple")
    #t
    > (bytes<? #"apple" #"Apple")
    #f
    > (bytes<? #"a" #"b" #"c")
    #t
    > (bytes>? #"Apple" #"apple")
    #f
    > (bytes>? #"apple" #"Apple")
    #t
    > (bytes>? #"c" #"b" #"a")
    #t
    """


def test_bytes_append(doctest):
    """
    > (bytes-append #"Apple" #"Banana")
    #"AppleBanana"
    """
def test_string_append(doctest):
    """
    > (string-append "Apple" "Banana")
    "AppleBanana"
    """
####################
def test_procedure_arity(doctest):
    """
    ! (require racket/private/norm-arity)
    > (procedure-arity cons)
    2
    > (procedure-arity list)
    (arity-at-least 0)
    > (arity-at-least? (procedure-arity list))
    #t
    > (arity-at-least-value (procedure-arity list))
    0
    > (arity-at-least-value (procedure-arity (lambda (x . y) x)))
    1
    > (procedure-arity (case-lambda [(x) 0] [(x y) 1]))
    '(1 2)
    """

def test_procedure_arity_includes(doctest):
    """
    ! (require racket/private/kw)
    > (procedure-arity-includes? cons 2)
    #t
    > (procedure-arity-includes? display 3)
    #f
    ;> (procedure-arity-includes? (lambda (x #:y y) x) 1)
    ;#f
    > (procedure-arity-includes? (lambda (x #:y y) x) 1 #t)
    #t
    """

#############################################################################
def test_system_type_os(source):
    """(cons (system-type) (system-type 'os))"""
    result = run_mod_expr(source, wrap=True)
    assert result.car() == result.cdr()
    sym = result.car().value
    # Sadly, this can never cover all cases.
    if sys.platform == "darwin":
        assert sym == "macosx"
    elif sys.platform in ['win32', 'cygwin']:
        assert sym == "windows"
    else:
        assert sym == "unix"

def test_system_path_convetion_type(source):
    """(system-path-convention-type)"""
    result = run_mod_expr(source, wrap=True)
    sym = result.value
    if sys.platform in ['win32', 'cygwin']:
        assert sym == "windows"
    else:
        assert sym == "unix"

def test_number_to_string(doctest):
    """
    > (number->string 10)
    "10"
    > (number->string -10)
    "-10"
    > (number->string -1.1)
    "-1.1"
    > (number->string -5.5)
    "-5.5"
    > (number->string -17+1i)
    "-17+1i"
    > (number->string -5/6)
    "-5/6"
    > (number->string 1 16)
    "1"
    > (number->string 10 16)
    "a"
    > (number->string 111 16)
    "6f"
    E (number->string 111 -164)
    > (number->string -164 16)
    "-a4"
    E (number->string -164.3 16)
    ;> (number->string -4/5 16)
    ;"-4/5"
    ;> (number->string -4/12311 16)
    ;"-4/3017"
    > (number->string 111111111111111111111111111111111111111111111111111111111111111111111111111111111 16)
    "3bf9304450677dc5f60e4afde2a26b6546f195ed670022bc71c71c71c71c71c71c7"
    """

