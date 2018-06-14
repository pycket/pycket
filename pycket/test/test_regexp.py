#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_make_regexp(doctest):
    """
    > (regexp "ap*le")
    #rx"ap*le"
    > (regexp? (regexp "ap*le"))
    #t
    > (pregexp? (regexp "ap*le"))
    #f
    > (pregexp? (pregexp "ap*le"))
    #t
    > (regexp? #px"ap*le")
    #t
    > (byte-regexp #"ap*le")
    #rx#"ap*le"
    ;E (byte-regexp "ap*le")
    > (byte-pregexp? (byte-pregexp #"ap*le"))
    #t
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regexp_match(doctest):
    r"""
    > (regexp-match #rx"[a-zA-Z]+(c)" "abc")
    '("abc" "c")
    > (regexp-match #rx"[a-zA-Z]+(c)" "______________________abc")
    '("abc" "c")
    > (regexp-match #rx"[a-zA-Z]+" "0000")
    #f
    > (regexp-match "[a-zA-Z]+" "abc")
    '("abc")
    > (regexp-match "[a-zA-Z]+" #"abc")
    '(#"abc")
    > (regexp-match #"[a-zA-Z]+" #"")
    #f
    > (regexp-match #rx"[\\]" "")
    #f
    > (regexp-match #rx"\\" (string (integer->char 0)))
    (quote ("\u0000"))
    > (regexp-match "[\\]" "\\")
    (quote ("\\"))
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regexp_match_character_negation(doctest):
    """
    > (regexp-match #px"^([^\t]+)" "ZA,LS,SZ")
    '("ZA,LS,SZ" "ZA,LS,SZ")
    > (regexp-match #px"^([^#\t]+)[\t]([^\t]+)[\t]([^\t]+)(?:[\t](.*))?" "ZA,LS,SZ\t-2615+02800\tAfrica/Johannesburg")
    '("ZA,LS,SZ\t-2615+02800\tAfrica/Johannesburg" "ZA,LS,SZ" "-2615+02800" "Africa/Johannesburg" #f)
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regexp_match_positions(doctest):
    ur"""
    > (regexp-match-positions #rx"a|b" "dog")
    #f
    > (regexp-match-positions #rx"a|b" "cat")
    '((1 . 2))
    > (regexp-match-positions #rx"[a-zA-Z]+(c)" "abc")
    '((0 . 3) (2 . 3))
    > (regexp-match-positions #rx"[a-zA-Z]+(c)" "______________________abc")
    '((22 . 25) (24 . 25))
    > (regexp-match-positions #px"\\s+$" "blue ")
    '((4 . 5))
    ;> (regexp-match-positions #px"β" "λΖχβα")
    ;'((3 . 4))
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regexp_match_positions_end(doctest):
    ur"""
    > (call-with-values (lambda () (regexp-match-positions/end #rx"a|b" "dog")) list)
    (quote (#f #f))
    > (call-with-values (lambda () (regexp-match-positions/end #rx"a|b" "cat")) list)
    (quote (((1 . 2)) #"a"))
    > (call-with-values (lambda () (regexp-match-positions/end #rx"[a-zA-Z]+(c)" "abc")) list)
    (quote (((0 . 3) (2 . 3)) #"c"))
    > (call-with-values (lambda () (regexp-match-positions/end #rx"[a-zA-Z]+(c)" "abc" 0 #f #f #"" 3)) list)
    (quote (((0 . 3) (2 . 3)) #"abc"))
    > (call-with-values (lambda () (regexp-match-positions/end #rx"[a-zA-Z]+(c)" "______________________abc")) list)
    (quote (((22 . 25) (24 . 25)) #"c"))
    > (call-with-values (lambda () (regexp-match-positions/end #px"\\s+$" "blue ")) list)
    (quote (((4 . 5)) #" "))
    ;> (car (call-with-values (lambda () (regexp-match-positions/end #px"β" "λΖχβα")) list))
    ;'((3 . 4))
    ;> (bytes-ref (cadr (call-with-values (lambda () (regexp-match-positions/end #px"β" "λΖχβα")) list)) 0)
    ;178
    > (call-with-values (lambda () (regexp-match-positions/end #rx"a|b" #"dog")) list)
    (quote (#f #f))
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regexp_match_p(doctest):
    """
    > (regexp-match? #rx"x." "12x4x6")
    #t
    > (regexp-match? #rx"y." "12x4x6")
    #f
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp") ###############################################
def test_bug(doctest):
    """
    ! (define-values (flag) "--profile")
    > (regexp-match? #rx"^([-+][^-+]$|(--|[+][+])[^-+])" flag)
    #t
    > (regexp-match #rx"^([-+][^-+]$|(--|[+][+])[^-+])" "--profile")
    '("--p" "--p" "--")
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_or_parsing(doctest):
    """
    > (regexp-match? "ab|c" "a")
    #f
    > (regexp-match? "ab|c" "ab")
    #t
    > (regexp-match? "ab|c" "c")
    #t
    > (regexp-match? "ab|c" "ac")
    #t
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp") ###################################################
def test_regex_match_port(doctest):
    r"""
    ! (define-values (s) (open-input-string "bcbcaxcbayc"))
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

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regex_result_types(doctest):
    r"""
    > (regexp-match #rx"a" "bca")
    '("a")
    > (regexp-match #rx"a" #"bca")
    '(#"a")
    > (regexp-match #rx#"a" "bca")
    '(#"a")
    > (regexp-match #rx#"a" "bca")
    '(#"a")
    > (regexp-match "a" "bca")
    '("a")
    > (regexp-match "a" #"bca")
    '(#"a")
    > (regexp-match "a" #"bca")
    '(#"a")
    > (regexp-match #"a" "bca")
    '(#"a")
    > (regexp-match "a" #"bca")
    '(#"a")
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regex_equality(doctest):
    r"""
    > (equal? (regexp "ap*le") #rx"ap*le")
    #t
    > (eqv? (regexp "ap*le") #rx"ap*le")
    #f
    > (equal? (byte-regexp #"ap*le") #rx"ap*le")
    #f
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regexp_match_group_fail(doctest):
    r"""
    > (regexp-match "(?:^|[^0-9\\(])(\\(([0-9][0-9][0-9])\\)|([0-9][0-9][0-9])) ([0-9][0-9][0-9])[ -]([0-9][0-9][0-9][0-9])(?:[^0-9]|$)" "(375) 729-6365")
    '("(375) 729-6365" "(375)" "375" #f "729" "6365")
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regexp_match_group_with_brackets(doctest):
    r"""
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
    ;> (regexp-match* #px"[][.*?+|(){}\\$^]" "][.*?+|(){}\\^]")
    ;'("]" "[" "." "*" "?" "+" "|" "(" ")" "{" "}" "^" "]")
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regexp_replace(doctest):
    r"""
    ! (define-values (sample) "hello")
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
    > (regexp-replace #rx"x" "12x4x6" "(&)")
    "12(&)4x6"
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="string->sexp can't parse regexp")
def test_regexp_replace_star(doctest):
    r"""
    ! (define-values (sample) "hello")
    > (regexp-replace* #rx"mi" "mi casa" "su")
    "su casa"
    > (regexp-replace* #rx"([Mm])i ([a-zA-Z]*)" "Mi Casa" "\\1y \\2")
    "My Casa"
    > (regexp-replace* #rx"([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi"
                  "\\1y \\2")
    "my cerveza My Mi Mi"
    > (regexp-replace* #rx"x" "12x4x6" "\\\\")
    "12\\4\\6"
    > (eq? sample (regexp-replace* #rx"z" sample "Z"))
    #t
    > (regexp-replace* #rx"x" "12x4x6" "(&)")
    "12(&)4(&)6"
    """

