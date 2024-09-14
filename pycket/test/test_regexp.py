#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

from pycket.test.testhelper import run
from pycket.test.utils import to_w_list, string_to_sexp
from pycket.values import *
from pycket.values_string import W_String


def test_make_regexp():
    run("(equal? (regexp \"ap*le\") #rx\"ap*le\")", w_true)
    run("(regexp? (regexp \"ap*le\"))", w_true)
    run("(pregexp? (regexp \"ap*le\"))", w_false)
    run("(pregexp? (pregexp \"ap*le\"))", w_true)
    run("(regexp? #px\"ap*le\")", w_true)
    run("(equal? (byte-regexp #\"ap*le\") #rx#\"ap*le\")", w_true)
    run("(byte-regexp \"ap*le\")", expect_to_fail=True)
    run("(byte-pregexp? (byte-pregexp #\"ap*le\"))", w_true)


def test_regexp_match():
    # (regexp-match #rx"[a-zA-Z]+(c)" "abc") => '("abc" "c")
    run("(regexp-match (regexp \"[a-zA-Z]+(c)\") \"abc\")", to_w_list([W_String.fromascii("abc"), W_String.fromascii("c")]))
    # (regexp-match #rx"[a-zA-Z]+(c)" "______________________abc") => '("abc" "c")
    run("(regexp-match (regexp \"[a-zA-Z]+(c)\") \"______________________abc\")", to_w_list([W_String.fromascii("abc"), W_String.fromascii("c")]))
    # (regexp-match #rx"[a-zA-Z]+" "0000") => #f
    run("(regexp-match (regexp \"[a-zA-Z]+\") \"0000\")", w_false)
    # (regexp-match "[a-zA-Z]+" "abc") => '("abc")
    run("(regexp-match \"[a-zA-Z]+\" \"abc\")", to_w_list([W_String.fromascii("abc")]))
    # (regexp-match "[a-zA-Z]+" #"abc") => '(#"abc")
    run("(regexp-match \"[a-zA-Z]+\" #\"abc\")", to_w_list([W_Bytes.from_string("abc")]))
    # (regexp-match #"[a-zA-Z]+" #"") => #f
    run("(regexp-match #\"[a-zA-Z]+\" #\"\")", w_false)
    # (regexp-match #rx"^([^:]*):(.+)$" ":x") => '(":x" "" "x")
    run("(regexp-match (regexp \"^([^:]*):(.+)$\") \":x\")", to_w_list([W_String.fromascii(":x"), W_String.fromascii(""), W_String.fromascii("x")]))

def test_regexp_match_character_negation():
    # > (regexp-match #px"^([^\t]+)" "ZA,LS,SZ")
    # '("ZA,LS,SZ" "ZA,LS,SZ")
    run("(regexp-match #px\"^([^\t]+)\" \"ZA,LS,SZ\")", to_w_list([W_String.fromascii("ZA,LS,SZ"), W_String.fromascii("ZA,LS,SZ")]))
    # > (regexp-match #px"^([^#\t]+)[\t]([^\t]+)[\t]([^\t]+)(?:[\t](.*))?" "ZA,LS,SZ\t-2615+02800\tAfrica/Johannesburg")
    # '("ZA,LS,SZ\t-2615+02800\tAfrica/Johannesburg" "ZA,LS,SZ" "-2615+02800" "Africa/Johannesburg" #f)
    run("(regexp-match #px\"^([^#\t]+)[\t]([^\t]+)[\t]([^\t]+)(?:[\t](.*))?\" \"ZA,LS,SZ\t-2615+02800\tAfrica/Johannesburg\")", to_w_list([W_String.fromascii("ZA,LS,SZ\t-2615+02800\tAfrica/Johannesburg"), W_String.fromascii("ZA,LS,SZ"), W_String.fromascii("-2615+02800"), W_String.fromascii("Africa/Johannesburg"), w_false]))

def test_regexp_match_positions():
    # (regexp-match-positions #rx"a|b" "dog") => #f
    run("(regexp-match-positions #rx\"a|b\" \"dog\")", w_false)
    # (regexp-match-positions #rx"a|b" "cat") => '((1 . 2))
    run("(regexp-match-positions #rx\"a|b\" \"cat\")", to_w_list([W_Cons.make(W_Fixnum(1), W_Fixnum(2))]))
    # (regexp-match-positions #rx"[a-zA-Z]+(c)" "abc") => '((0 . 3) (2 . 3))
    run("(regexp-match-positions #rx\"[a-zA-Z]+(c)\" \"abc\")", to_w_list([W_Cons.make(W_Fixnum(0), W_Fixnum(3)), W_Cons.make(W_Fixnum(2), W_Fixnum(3))]))
    # (regexp-match-positions #rx"[a-zA-Z]+(c)" "______________________abc") => '((22 . 25) (24 . 25))
    run("(regexp-match-positions #rx\"[a-zA-Z]+(c)\" \"______________________abc\")", to_w_list([W_Cons.make(W_Fixnum(22), W_Fixnum(25)), W_Cons.make(W_Fixnum(24), W_Fixnum(25))]))
    # (regexp-match-positions #px"\\s+$" "blue ") => '((4 . 5))
    test_expr = ur"""(regexp-match-positions #px"\\s+$" "blue ")"""
    run(test_expr, to_w_list([W_Cons.make(W_Fixnum(4), W_Fixnum(5))]))
    # (regexp-match-positions #px"β" "λΖχβα") => '((3 . 4))
    test_expr = ur"""(regexp-match-positions #px"β" "λΖχβα")"""
    run(test_expr, to_w_list([W_Cons.make(W_Fixnum(3), W_Fixnum(4))]))

def test_regexp_match_positions_end():
    # (call-with-values (λ () (regexp-match-positions/end #rx"a|b" "dog")) list) => '(#f #f)
    run("(call-with-values (λ () (regexp-match-positions/end #rx\"a|b\" \"dog\")) list)", to_w_list([w_false, w_false]))

    # (call-with-values (λ () (regexp-match-positions/end #rx"a|b" "cat")) list) => '(((1 . 2)) #"a")
    res = to_w_list([to_w_list([W_Cons.make(W_Fixnum(1), W_Fixnum(2))]), W_Bytes.from_string("a")])
    run("(call-with-values (λ () (regexp-match-positions/end #rx\"a|b\" \"cat\")) list)", res)

    # (call-with-values (λ () (regexp-match-positions/end #rx"[a-zA-Z]+(c)" "abc")) list) => '(((0 . 3) (2 . 3)) #"c")
    res = to_w_list([to_w_list([W_Cons.make(W_Fixnum(0), W_Fixnum(3)), W_Cons.make(W_Fixnum(2), W_Fixnum(3))]), W_Bytes.from_string("c")])
    run("(call-with-values (λ () (regexp-match-positions/end #rx\"[a-zA-Z]+(c)\" \"abc\")) list)", res)

    # (call-with-values (λ () (regexp-match-positions/end #rx"[a-zA-Z]+(c)" "abc" 0 #f #f #"" 3)) list) => '(((0 . 3) (2 . 3)) #"abc")
    res = to_w_list([to_w_list([W_Cons.make(W_Fixnum(0), W_Fixnum(3)), W_Cons.make(W_Fixnum(2), W_Fixnum(3))]), W_Bytes.from_string("abc")])
    run("(call-with-values (λ () (regexp-match-positions/end #rx\"[a-zA-Z]+(c)\" \"abc\" 0 #f #f #\"\" 3)) list)", res)

    # (call-with-values (λ () (regexp-match-positions/end #rx"[a-zA-Z]+(c)" "______________________abc")) list) => '(((22 . 25) (24 . 25)) #"c")
    res = to_w_list([to_w_list([W_Cons.make(W_Fixnum(22), W_Fixnum(25)), W_Cons.make(W_Fixnum(24), W_Fixnum(25))]), W_Bytes.from_string("c")])
    run("(call-with-values (λ () (regexp-match-positions/end #rx\"[a-zA-Z]+(c)\" \"______________________abc\")) list)", res)

    # (call-with-values (λ () (regexp-match-positions/end #px"\\s+$" "blue ")) list) => '(((4 . 5)) #" ")
    res = to_w_list([to_w_list([W_Cons.make(W_Fixnum(4), W_Fixnum(5))]), W_Bytes.from_string(" ")])
    test_expr = ur"""(call-with-values (λ () (regexp-match-positions/end #px"\\s+$" "blue ")) list)"""
    run(test_expr, res)

    # (car (call-with-values (λ () (regexp-match-positions/end #px"β" "λΖχβα")) list)) => '((3 . 4))
    res = to_w_list([W_Cons.make(W_Fixnum(3), W_Fixnum(4))])
    test_expr = ur"""(car (call-with-values (λ () (regexp-match-positions/end #px"β" "λΖχβα")) list))"""
    run(test_expr, res)

    # (bytes-ref (cadr (call-with-values (λ () (regexp-match-positions/end #px"β" "λΖχβα")) list)) 0) => 178
    test_expr = ur"""(bytes-ref (cadr (call-with-values (λ () (regexp-match-positions/end #px"β" "λΖχβα")) list)) 0)"""
    run(test_expr, W_Fixnum(178))

    # (call-with-values (λ () (regexp-match-positions/end #rx"a|b" #"dog")) list) => '(#f #f)
    run("(call-with-values (λ () (regexp-match-positions/end #rx\"a|b\" #\"dog\")) list)", to_w_list([w_false, w_false]))

def test_regexp_match_p():
    run("(regexp-match? #rx\"x.\" \"12x4x6\")", w_true)
    run("(regexp-match? #rx\"y.\" \"12x4x6\")", w_false)

def test_bug():
    run("""(regexp-match? #rx\"^([-+][^-+]$|(--|[+][+])[^-+])\" \"--profile\")""", w_true)
    run("(regexp-match #rx\"^([-+][^-+]$|(--|[+][+])[^-+])\" \"--profile\")", to_w_list([W_String.fromascii("--p"), W_String.fromascii("--p"), W_String.fromascii("--")]))

def test_or_parsing():
    run("(regexp-match? \"ab|c\" \"a\")", w_false)
    run("(regexp-match? \"ab|c\" \"ab\")", w_true)
    run("(regexp-match? \"ab|c\" \"c\")", w_true)
    run("(regexp-match? \"ab|c\" \"ac\")", w_true)

@pytest.mark.skipif(pytest.config.load_expander, reason="hard to convert")
def test_regex_match_port(doctest):
    r"""
    ! (define s (open-input-string "bcbcaxcbayc"))
    > (file-position s)
    0
    > (peek-char s)
    #\b
    > (regexp-match #rx"a" s)
    '("a")
    > (file-position s)
    5
    > (peek-char s)
    #\x
    """

def test_regex_result_types():
    run("(regexp-match #rx\"a\" \"bca\")", to_w_list([W_String.fromascii("a")]))
    run("(regexp-match #rx\"a\" #\"bca\")", to_w_list([W_Bytes.from_string("a")]))
    run("(regexp-match #rx#\"a\" \"bca\")", to_w_list([W_Bytes.from_string("a")]))
    run("(regexp-match \"a\" \"bca\")", to_w_list([W_String.fromascii("a")]))
    run("(regexp-match \"a\" #\"bca\")", to_w_list([W_Bytes.from_string("a")]))
    run("(regexp-match #\"a\" \"bca\")", to_w_list([W_Bytes.from_string("a")]))
    run("(regexp-match \"a\" #\"bca\")", to_w_list([W_Bytes.from_string("a")]))

def test_regex_equality():
    run("(equal? (regexp \"ap*le\") #rx\"ap*le\")", w_true)
    run("(eqv? (regexp \"ap*le\") #rx\"ap*le\")", w_false)
    run("(equal? (byte-regexp #\"ap*le\") #rx\"ap*le\")", w_false)

def test_regexp_match_group_fail():
    test_expr = ur"""(regexp-match "(?:^|[^0-9\\(])(\\(([0-9][0-9][0-9])\\)|([0-9][0-9][0-9])) ([0-9][0-9][0-9])[ -]([0-9][0-9][0-9][0-9])(?:[^0-9]|$)" "(375) 729-6365")"""
    res = to_w_list([W_String.fromascii("(375) 729-6365"), W_String.fromascii("(375)"), W_String.fromascii("375"), w_false, W_String.fromascii("729"), W_String.fromascii("6365")])
    run(test_expr, res)

@pytest.mark.skip(reason="solved when tostring is not 'write'")
def test_regexp_match_group_with_brackets(doctest):
    r"""
    ! (require racket/string)
    > (regexp-match #px"[]]" "]")
    '("]")
    > (regexp-match #px"[]]" "d")
    #f
    > (regexp-match #px"[^]]" "]")
    #f
    > (regexp-match #px"[^]]" "d")
    '("d")
    > (regexp-match #px"[][]" "[")
    '("[")
    > (regexp-match #px"[][]" "]")
    '("]")
    > (regexp-match #px"[][]" "a")
    #f
    > (regexp-match* #px"[][.*?+|(){}\\$^]" "][.*?+|(){}\\^]")
    '("]" "[" "." "*" "?" "+" "|" "(" ")" "{" "}" "^" "]")
    """

@pytest.mark.skipif(pytest.config.load_expander, reason="hard to convert")
def test_regexp_replace(doctest):
    r"""
    ! (require (only-in '#%kernel regexp-replace*))
    ! (define sample "hello")
    > (regexp-replace #rx"mi" "mi casa" "su")
    "su casa"
    > (regexp-replace #rx"([Mm])i ([a-zA-Z]*)" "Mi Casa" "\\1y \\2")
    "My Casa"
    > (regexp-replace #rx"([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi"
                  "\\1y \\2")
    "my cerveza Mi Mi Mi"
    > (regexp-replace #rx"x" "12x4x6" "\\\\")
    "12\\4x6"
    > (eq? sample (regexp-replace #rx"z" sample "Z"))
    #t
    """

@pytest.mark.skipif(pytest.config.load_expander, reason="hard to convert")
def test_regexp_replace_star(doctest):
    r"""
    ! (require (only-in '#%kernel regexp-replace*))
    ! (define sample "hello")
    > (regexp-replace* #rx"mi" "mi casa" "su")
    "su casa"
    > (regexp-replace* #rx"([Mm])i ([a-zA-Z]*)" "Mi Casa" "\\1y \\2")
    "My Casa"
    > (regexp-replace* #rx"([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi"
                  "\\1y \\2")
    "my cerveza My Mi Mi"
    > (regexp-replace* #rx"x" "12x4x6" "\\\\")
    "12\\4\\6"
    > (equal? sample (regexp-replace* #rx"z" sample "Z"))
    #t
    """

