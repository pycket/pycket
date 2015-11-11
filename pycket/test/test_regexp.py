#! /usr/bin/env python
# -*- coding: utf-8 -*-

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
    E (byte-regexp "ap*le")
    > (byte-pregexp? (byte-pregexp #"ap*le"))
    #t
    """


def test_regexp_match(doctest):
    """
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
    """

def test_regexp_match_character_negation(doctest):
    """
    > (regexp-match #px"^([^\t]+)" "ZA,LS,SZ")
    '("ZA,LS,SZ" "ZA,LS,SZ")
    > (regexp-match #px"^([^#\t]+)[\t]([^\t]+)[\t]([^\t]+)(?:[\t](.*))?" "ZA,LS,SZ\t-2615+02800\tAfrica/Johannesburg")
    '("ZA,LS,SZ\t-2615+02800\tAfrica/Johannesburg" "ZA,LS,SZ" "-2615+02800" "Africa/Johannesburg" #f)
    """

def test_regexp_match_positions(doctest):
    r"""
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
    """

def test_regexp_match_positions_end(doctest):
    ur"""
    > (call-with-values (λ () (regexp-match-positions/end #rx"a|b" "dog")) list)
    '(#f #f)
    > (call-with-values (λ () (regexp-match-positions/end #rx"a|b" "cat")) list)
    '(((1 . 2)) #"a")
    > (call-with-values (λ () (regexp-match-positions/end #rx"[a-zA-Z]+(c)" "abc")) list)
    '(((0 . 3) (2 . 3)) #"c")
    > (call-with-values (λ () (regexp-match-positions/end #rx"[a-zA-Z]+(c)" "abc" 0 #f #f #"" 3)) list)
    '(((0 . 3) (2 . 3)) #"abc")
    > (call-with-values (λ () (regexp-match-positions/end #rx"[a-zA-Z]+(c)" "______________________abc")) list)
    '(((22 . 25) (24 . 25)) #"c")
    > (call-with-values (λ () (regexp-match-positions/end #px"\\s+$" "blue ")) list)
    '(((4 . 5)) #" ")
    """

def test_regexp_match_p(doctest):
    """
    > (regexp-match? #rx"x." "12x4x6")
    #t
    > (regexp-match? #rx"y." "12x4x6")
    #f
    """

def test_bug(doctest):
    """
    ! (define flag "--profile")
    > (regexp-match? #rx"^([-+][^-+]$|(--|[+][+])[^-+])" flag)
    #t
    > (regexp-match #rx"^([-+][^-+]$|(--|[+][+])[^-+])" "--profile")
    '("--p" "--p" "--")
    """

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

def test_regex_equality(doctest):
    r"""
    > (equal? (regexp "ap*le") #rx"ap*le")
    #t
    > (eqv? (regexp "ap*le") #rx"ap*le")
    #f
    > (equal? (byte-regexp #"ap*le") #rx"ap*le")
    #f
    """

def test_regexp_match_group_fail(doctest):
    r"""
    > (regexp-match "(?:^|[^0-9\\(])(\\(([0-9][0-9][0-9])\\)|([0-9][0-9][0-9])) ([0-9][0-9][0-9])[ -]([0-9][0-9][0-9][0-9])(?:[^0-9]|$)" "(375) 729-6365")
    '("(375) 729-6365" "(375)" "375" #f "729" "6365")
    """

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

def test_regexp_replace_star(doctest):
    r"""
    ! (require (only-in '#%kernel regexp-replace*))
    ! (define sample "hello")
    > (regexp-replace* #rx"mi" "mi casa" "su")
    "su casa"
    > (eq? sample (regexp-replace* #rx"z" sample "Z"))
    #t
    > (regexp-replace #rx"x" "12x4x6" "\\\\")
    "12\\4x6"
    """
