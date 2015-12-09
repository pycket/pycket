#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# A place for testing primitives
#

import pytest
import os
import sys
from pycket        import values
from pycket.values import w_true
from pycket.test.testhelper import check_all, check_none, check_equal, run_flo, run_fix, run, run_mod, run_mod_expr
from pycket.error import SchemeException

skip = pytest.mark.skipif("True")

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

def test_equal2(doctest):
    """
    ! (require racket/base)
    > (equal? (string->path "/usr/bin/bash") (string->path "/usr/bin/bash"))
    #t
    > (equal? (string->path "/usr/bin/bash") (string->path "/usr/bin/tcsh"))
    #f
    """

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
    E (make-bytes 5 11111)
    > (bytes 65 112 112 108 101)
    #"Apple"
    > (bytes)
    #""
    """

def test_make_string_create(doctest):
    """
    > (make-string 5 #\A)
    "AAAAA"
    > (string #\A #\p #\p #\l #\e)
    "Apple"
    > (string)
    ""
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
    (let* ([b (string->bytes/utf-8 "ABC\nDEF\n\nGHI\n\nJKL\n\n\nMNOP\n")]
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

def test_read_bytes(source):
    """
    (let ([ip (open-input-bytes (bytes 115 101 99 114 101 116))])
      (read-bytes 6 ip))
    """
    result = run_mod_expr(source, wrap=True)
    assert isinstance(result, values.W_Bytes)
    assert result.value == list("secret")

def test_read_utf8_bytes_chars(source):
    ur"""
    (let* ([b "ÄÖÜ"]
           [inport (open-input-string b)]
           [res1 (read-byte inport)]
           [res2 (read-byte inport)]
           [res3 (read-char inport)]
           )
        (and
            (equal? res1 195)
            (equal? res2 132)
            (equal? res3 #\Ö)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_open_input_and_read_line(source):
    u"""
    (let* ([b "ÄBC\nDEF\n\nGHI\n\nJKL\n\n\nMNOP\n"]
           [expected '("MNOP" "" "" "JKL" "" "GHI" "" "DEF" "ÄBC")]
           [inport (open-input-string b)])
      (let ([res (let rev ([lines null])
                   (let ([line (read-line inport)])
                     (if (eof-object? line)
                         lines
                         (rev (cons line lines)))))])
        (equal? res expected)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_bytes_port(doctest):
    r"""
    ;> (define op1 (open-output-bytes))
    ;> (write '((1 2 3) ("Tom" "Dick") ('a 'b 'c)) op1)
    ;> (get-output-bytes op1)
    ; #"((1 2 3) (\"Tom\" \"Dick\") ((quote a) (quote b) (quote c)))"
    ;> (define op2 (open-output-bytes))
    ;> (write "Hi " op2)
    ;> (write "there" op2)
    ;> (get-output-bytes op2)
    ; #"\"Hi \"\"there\""
    ! (define op3 (open-output-bytes))
    > (write-bytes #"Hi " op3)
    3
    > (display #"there" op3)
    > (get-output-bytes op3)
    #"Hi there"
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
    > (procedure-arity-includes? (lambda (x #:y y) x) 1)
    #f
    > (procedure-arity-includes? (lambda (x #:y y) x) 1 #t)
    #t
    """

#############################################################################
def test_system_type_os(source):
    """(cons (system-type) (system-type 'os))"""
    result = run_mod_expr(source, wrap=True)
    assert result.car() == result.cdr()
    sym = result.car().asciivalue
    # Sadly, this can never cover all cases.
    if sys.platform == "darwin":
        assert sym == "macosx"
    elif sys.platform in ['win32', 'cygwin']:
        assert sym == "windows"
    else:
        assert sym == "unix"

def test_system_path_convention_type(source):
    """(system-path-convention-type)"""
    result = run_mod_expr(source, wrap=True)
    sym = result.asciivalue
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

def test_list_to_string(doctest):
    r"""
    > (list->string (list #\A #\p #\p #\l #\e))
    "Apple"
    """

def test_char_cmp_huh(doctest):
    r"""
    > (char=? #\a #\a)
    #t
    > (char=? #\a #\A #\a)
    #f
    > (char<? #\A #\a)
    #t
    > (char<? #\a #\A)
    #f
    > (char<? #\a #\b #\c)
    #t
    > (char<=? #\A #\a)
    #t
    > (char<=? #\a #\A)
    #f
    > (char<=? #\a #\b #\b)
    #t
    > (char>? #\A #\a)
    #f
    > (char>? #\a #\A)
    #t
    > (char>? #\c #\b #\a)
    #t
    > (char>=? #\A #\a)
    #f
    > (char>=? #\a #\A)
    #t
    > (char>=? #\c #\b #\b)
    #t
    > (char-ci=? #\A #\a)
    #t
    > (char-ci=? #\a #\a #\a)
    #t
    > (char-ci<? #\A #\a)
    #f
    > (char-ci<? #\a #\b)
    #t
    > (char-ci<? #\a #\b #\c)
    #t
    > (char-ci<=? #\A #\a)
    #t
    > (char-ci<=? #\a #\A)
    #t
    > (char-ci<=? #\a #\b #\b)
    #t
    > (char-ci>? #\A #\a)
    #f
    > (char-ci>? #\b #\A)
    #t
    > (char-ci>? #\c #\b #\a)
    #t
    > (char-ci>=? #\A #\a)
    #t
    > (char-ci>=? #\a #\A)
    #t
    > (char-ci>=? #\c #\b #\b)
    #t
    """

def test_char_prop_huh(doctest):
    r"""
    > (char-alphabetic? #\a)
    #t
    > (char-alphabetic? #\=)
    #f
    > (char-numeric? #\0)
    #t
    > (char-numeric? #\=)
    #f
    > (char-whitespace? #\tab)
    #t
    > (char-whitespace? #\=)
    #f
    """

def test_gcd_lcm(doctest):
    """
    > (gcd 10)
    10
    > (gcd 12 81.0)
    3.0
    > (gcd 1/2 1/3)
    1/6
    > (lcm 10)
    10
    > (lcm 3 4.0)
    12.0
    > (lcm 1/2 2/3)
    2
    """

def test_read(doctest):
    """
    ! (define (rs s) (read (open-input-string s)))
    > (rs "1")
    1
    > (rs "#t")
    #t
    > (rs "abc")
    'abc
    > (define s (open-input-string "1 #t abc"))
    > (read s)
    1
    > (read s)
    #t
    > (read s)
    'abc
    > (rs "()")
    '()
    > (rs "(1)")
    '(1)
    > (rs "(1 2 3 a b c)")
    '(1 2 3 a b c)
    > (rs "(1 (2 3) (a (b c)))")
    '(1 (2 3) (a (b c)))
    > (rs "[]")
    '[]
    > (rs "[]")
    '()
    > (rs "[1]")
    '[1]
    > (rs "[1 2 3 a b c]")
    '[1 2 3 a b c]
    > (rs "[1 [2 3] [a [b c]]]")
    '[1 [2 3] [a [b c]]]
    > (rs "(1 . 2)")
    (cons 1 2)
    > (rs "(a . b)")
    (cons 'a 'b)
    > (rs "(a.b . c.d)")
    (cons 'a.b 'c.d)
    > (rs "...")
    '...
    > (rs "'(1)")
    ''(1)
    > (rs "`(1)")
    '`(1)
    > (rs "`(,1)")
    '`(,1)
    > (rs "\\"1\\"")
    "1"
    > (rs "\\"'abc\\"")
    "'abc"
    > (rs "\\"hello jed\\"")
    "hello jed"
    > (rs "\\"abc.123\\"")
    "abc.123"
    > (rs "\\"\\t\\n\\"")
    "\\t\\n"
    > (rs "ab;cd")
    'ab
    > (rs "12;cd\\n")
    12
    > (define s2 (open-input-string "12;\\n34"))
    > (read s2)
    12
    > (read s2)
    34
    > (rs "#'()")
    '(quote-syntax ())
    > (rs "#`()")
    '(quasisyntax ())
    > (rs "#`(#,x)")
    '(quasisyntax ((unsyntax x)))
    > (rs "#`(#,@x)")
    '(quasisyntax ((unsyntax-splicing x)))
    """

def test_close_port(doctest):
    """
    > (define sp (open-input-string "(apples 42 day)"))
    > (port-closed? sp)
    #f
    > (close-input-port sp)
    > (port-closed? sp)
    #t
    > (define op (open-output-string))
    > (port-closed? op)
    #f
    > (close-output-port op)
    > (port-closed? op)
    #t
    """

def test_port_read_peek(doctest):
    r"""
    > (define sp (open-input-string "(apples 42 day)"))
    > (peek-char sp)
    #\(
    > (peek-char sp 5)
    #\e
    > (read-char sp)
    #\(
    > (define bp (open-input-bytes #"(apples 42 day)"))
    > (peek-byte bp)
    40
    > (peek-byte bp 5)
    101
    > (read-byte bp)
    40
    """

def test_peek_bug(tmpdir):
    from pycket.prims.input_output import open_infile
    from pycket import values_string
    s = "abc\ndef\nghi"
    f = tmpdir.join("example.txt")
    f.write(s)
    w_n = values_string.W_String.fromstr_utf8(str(f))
    w_p = open_infile(w_n, "r")
    for c in s:
        c1 = w_p.peek()
        assert c1 == c
        c2 = w_p.read(1)
        assert c == c
    c = w_p.peek()
    assert c == ''
    c = w_p.read(1)
    assert c == ''
    w_p.close()

def test_listp(doctest):
    """
    > (list? '(1 2))
    #t
    > (list? (cons 1 (cons 2 '())))
    #t
    > (list? (cons 1 2))
    #f
    > (list? 1)
    #f
    """

def test_format(doctest):
    r"""
    > (format "a")
    "a"
    E (format "a~a")
    E (format "a" 1)
    > (format "~~~n~%")
    "~\n\n"
    > (format "abc~adef~aghi" 1 2)
    "abc1def2ghi"
    """

def test_procedure_closure_contents_eq(doctest):
    r"""
    ! (define (f x) (lambda () x))
    ! (define a "abc")
    ! (define (g x) (lambda () (g x)))
    > (procedure-closure-contents-eq? (f a) (f a))
    #t
    > (procedure-closure-contents-eq? (f a) (f "abc"))
    #t
    > (procedure-closure-contents-eq? (f 1) (f 1))
    #t
    > (procedure-closure-contents-eq? (f a) (f "c"))
    #f
    > (procedure-closure-contents-eq? (g a) (g a))
    #t
    > (procedure-closure-contents-eq? (g a) ((g a)))
    #t
    > (procedure-closure-contents-eq? (g a) (((g a))))
    #t
    > (procedure-closure-contents-eq? (g a) (((g "c"))))
    #f
    """

def test_list_ref(doctest):
    """
    > (list-ref '(1 2 3) 0)
    1
    > (list-ref '(1 2 3) 1)
    2
    > (list-ref '(1 2 3) 2)
    3
    """

def test_unsafe_undefined(doctest):
    """
    ! (require '#%unsafe)
    ! (struct p (x y) #:mutable #:transparent)
    > (check-not-unsafe-undefined 1 'a)
    1
    E (check-not-unsafe-undefined unsafe-undefined 'a)
    > (check-not-unsafe-undefined/assign 1 'a)
    1
    E (check-not-unsafe-undefined/assign unsafe-undefined 'a)
    > (chaperone-struct-unsafe-undefined 1)
    1
    > (let* ([x (p 1 2)]
             [y (chaperone-struct-unsafe-undefined x)]
             [_ (set-p-y! y 3)]
             [z (p-y y)])
        z)
    3
    """

def test_dynamic_wind(doctest):
    """
    > (dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3))
    2
    """

def test_bytes_conversions():
    m = run_mod(
    """
    #lang pycket
    (define a (real->floating-point-bytes 1 8 #f))
    (define b (integer-bytes->integer a #f))
    """)
    a = values.W_Symbol.make("a")
    b = values.W_Symbol.make("b")

    vb = m.defs[b]
    assert isinstance(vb, values.W_Fixnum) and vb.value == 4607182418800017408

def test_build_path(doctest):
    """
    > (path->string (build-path "/usr/bin" "bash"))
    "/usr/bin/bash"
    > (path->string (build-path "/usr" "bin" 'up "bash"))
    "/usr/bin/../bash"
    > (path->string (build-path "/usr" "bin" 'same "bash"))
    "/usr/bin/./bash"
    """

def test_path_to_complete_path():
    m = run_mod(
    """
    #lang pycket
    (define p (path->complete-path "test.rkt"))
    """)
    p = m.defs[values.W_Symbol.make("p")]
    cwd = os.getcwd()
    assert isinstance(p, values.W_Path)
    full = cwd + "/" + "test.rkt"
    assert full == p.path

def test_explode_path(doctest):
    """
    ! (require racket/base)
    ! (define (unpath p) (if (path? p) (path->string p) p))
    > (map path->string (explode-path "/home/spenser/src/pycket"))
    '("/" "home" "spenser" "src" "pycket")
    > (map unpath (explode-path "/home/spenser/src/pycket/.././."))
    '("/" "home" "spenser" "src" "pycket" up same same)
    > (map unpath (explode-path "home/spenser/src/pycket/.././."))
    '("home" "spenser" "src" "pycket" up same same)
    """
    assert doctest

def test_file_size(doctest):
    """
    > (file-size "./pycket/test/sample_file.txt")
    256
    """
    assert doctest

def test_andmap(doctest):
    """
    ! (require (only-in '#%kernel andmap))
    > (andmap even? '())
    #t
    > (andmap even? '(1))
    #f
    > (andmap even? '(2))
    #t
    > (andmap even? '(1 2 3 4 5 6 7 8 9))
    #f
    > (andmap even? '(2 4 6 8))
    #t
    > (andmap odd? '())
    #t
    > (andmap odd? '(1))
    #t
    > (andmap odd? '(2))
    #f
    > (andmap odd? '(1 2 3 4 5 6 7 8 9))
    #f
    > (andmap odd? '(2 4 6 8))
    #f
    """

def test_ormap(doctest):
    """
    ! (require (only-in '#%kernel ormap))
    > (ormap even? '())
    #f
    > (ormap even? '(1))
    #f
    > (ormap even? '(2))
    #t
    > (ormap even? '(1 2 3 4 5 6 7 8 9))
    #t
    > (ormap even? '(2 4 6 8))
    #t
    > (ormap odd? '())
    #f
    > (ormap odd? '(1))
    #t
    > (ormap odd? '(2))
    #f
    > (ormap odd? '(1 2 3 4 5 6 7 8 9))
    #t
    > (ormap odd? '(2 4 6 8))
    #f
    """

def test_syntax_to_datum(doctest):
    """
    > (syntax->datum #'a)
    'a
    > (syntax->datum #'(x . y))
    '(x . y)
    > (syntax->datum #'#(1 2 (+ 3 4)))
    '#(1 2 (+ 3 4))
    > (syntax->datum #'#&"hello world")
    '#&"hello world"
    ;;;;; XXX: Ordering problem?
    ;> (syntax->datum #'#hash((imperial . "yellow") (festival . "green")))
    ;'#hash((festival . "green") (imperial . "yellow"))
    > (syntax->datum #'#(point 3 4))
    '#(point 3 4)
    > (syntax->datum #'3)
    3
    > (syntax->datum #'"three")
    "three"
    > (syntax->datum #'#t)
    #t
    """

@skip
def test_syntax_e(doctest):
    """
    > (syntax-e #'a)
    'a
    > (let ((s (syntax-e #'(x . y))))
        (and (pair? s) (syntax? (car s)) (syntax? (cdr s))))
    ;'(#<syntax:11:0 x> . #<syntax:11:0 y>)
    #t
    > (let ((s (syntax-e #'#(1 2 (+ 3 4)))))
        (and (list? s) (syntax? (list-ref s 1))))
    ;'#(#<syntax:12:0 1> #<syntax:12:0 2> #<syntax:12:0 (+ 3 4)>)
    #t
    > (let ((s (syntax-e #'#&"hello world")))
        (and (box? s) (syntax? (unbox s))))
    ;'#&#<syntax:13:0 "hello world">
    #t
    ;;;;; XXX: Ordering problem?
    ;> (syntax-e #'#hash((imperial . "yellow") (festival . "green")))
    ;'#hash((festival . #<syntax:14:0 "green">) (imperial . #<syntax:14:0 "yellow">))
    > (let ((s (syntax-e #'#(point 3 4))))
        (and (vector? s) (syntax? (vector-ref s 1))))
    ;'#(#<syntax:15:0 point> #<syntax:15:0 3> #<syntax:15:0 4>)
    #t
    > (syntax-e #'3)
    3
    > (syntax-e #'"three")
    "three"
    > (syntax-e #'#t)
    #t
    """

def test_relative_path(doctest):
    """
    > (relative-path? "/home/spenser")
    #f
    > (relative-path? "~/bin/racket")
    #t
    > (relative-path? "./../bin/racket")
    #t
    > (relative-path? (string->path "/home/spenser"))
    #f
    > (relative-path? (string->path  "~/bin/racket"))
    #t
    > (relative-path? (string->path "./../bin/racket"))
    #t
    """


def test_continuation_prompt_functions(doctest):
    u"""
    ! (define tag (make-continuation-prompt-tag))
    ! (define (escape v) (abort-current-continuation tag (lambda () v)))
    > (call-with-continuation-prompt (λ () (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0)))))))) tag)
    0
    > (+ 1 (call-with-continuation-prompt (lambda () (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0)))))))) tag))
    1
    > (call-with-continuation-prompt (λ () (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0)))))))) tag (λ (x) (+ 10 (x))))
    10
    > (+ 1 (call-with-continuation-prompt (lambda () (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0)))))))) tag (λ (x) (+ (x) 10))))
    11
    """

def test_continuation_prompt_available(doctest):
    u"""
    ! (define tag  (make-continuation-prompt-tag))
    ! (define tag2 (make-continuation-prompt-tag))
    > (call-with-continuation-prompt (λ () (continuation-prompt-available? tag)) tag)
    #t
    > (call-with-continuation-prompt (λ () (continuation-prompt-available? tag)) tag2)
    #f
    """
