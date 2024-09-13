# coding: utf-8

def test_string_set_bang(doctest):
    """
    ! (define str (substring "hello world" 0 5))
    ! (string-set! str 0 #\\x)
    > str
    "xello"
    > (immutable? str)
    #f
    """

def test_substring(doctest):
    """
    > (substring "Apple" 1 3)
    "pp"
    > (substring "Apple" 1)
    "pple"
    > (substring "applebee" 5)
    "bee"
    > (substring "applebee" 0 8)
    "applebee"
    """


def test_string_copy_bang(doctest):
    r"""
    > (define s (string #\A #\p #\p #\l #\e))
    > (define s2 (make-string 10 #\x))
    > (string-copy! s 4 "y")
    > (string-copy! s 0 s 3 4)
    > s
    "lpply"
    E (let ([s (string #\a #\b #\c)]) (string-copy! s 0 "abde" 0) s)
    E (string-copy! s 1 "hello" 3 6)
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
    u"""
    > (string-append "Apple" "Banana")
    "AppleBanana"
    > (string-append "Apple")
    "Apple"
    > (string-append)
    ""
    > (string-append "Apple" "Βανανα")
    "AppleΒανανα"
    """

def test_bytes_to_string_utf8(doctest):
    """
    > (bytes->string/utf-8 (bytes 65 66 67))
    "ABC"
    > (immutable? (bytes->string/utf-8 (bytes 65 66 67)))
    #f
    > (bytes->string/utf-8 (string->bytes/utf-8 "abc" 0 0 3) #\? 2 3)
    "c"
    """

def test_string_ref(doctest):
    r"""
    > (string-ref "abcdef" 0)
    #\a
    > (string-ref "abcdef" 1)
    #\b
    > (string-ref "abcdef" 2)
    #\c
    > (string-ref "abcdef" 3)
    #\d
    > (string-ref "abcdef" 4)
    #\e
    > (string-ref "abcdef" 5)
    #\f
    E (string-ref "abcdef" 6)
    E (string-ref "abcdef" -1)
    """

def test_output_string(doctest):
    r"""
    ! (define p (open-output-string))
    > (write-string "abccarstaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" p 0 10)
    10
    > (get-output-string p)
    "abccarstaa"
    """

def test_immutable_literals(doctest):
    """
    > (immutable? "abc")
    #t
    """

def test_string_to_immutable(doctest):
    r"""
    ; easy case
    > (string->immutable-string "abc")
    "abc"
    > (string->immutable-string (string #\a #\b))
    "ab"
    > (immutable?  (string #\a #\b))
    #f
    > (immutable? (string->immutable-string (string #\a #\b)))
    #t
    """

def test_string_to_symbol(doctest):
    """
    > (string->symbol "abc")
    'abc
    > (symbol->string 'abc)
    "abc"
    > (symbol->string (string->unreadable-symbol "abc"))
    "abc"
    > (symbol->string (string->uninterned-symbol "abc"))
    "abc"
    > (eq? (string->unreadable-symbol "abc") (string->unreadable-symbol "abc"))
    #t
    > (eq? (string->symbol "abc") (string->symbol "abc"))
    #t
    > (eq? (string->symbol "abc") 'abc)
    #t
    > (eq? (string->unreadable-symbol "abc") 'abc)
    #f
    > (eq? (string->uninterned-symbol "abc") (string->uninterned-symbol "abc"))
    #f
    """

def test_char_integer(doctest):
    """
    > (char->integer (integer->char 65))
    65
    > (char->integer (integer->char 128293))
    128293
    """

def test_char_general_category(doctest):
    """
    > (char-general-category #\ )
    'zs
    > (char-general-category #\c)
    'll
    > (char-general-category #\.)
    'po
    > (char-general-category #\\\\)
    'po
    > (char-general-category #\|)
    'sm
    """

def test_string_change_case(doctest):
    """
    > (string-upcase "abc")
    "ABC"
    > (string-upcase "abc__123")
    "ABC__123"
    """


def test_unicode(doctest):
    u"""
    ! (define str (substring "hello world" 0 5))
    ! (define str1 (substring "hello fuß" 0 5))
    ! (string-set! str 1 #\\ä)
    ! (string-set! str1 1 #\\ä)
    > str
    "hällo"
    > str1
    "hällo"
    > (immutable? str)
    #f
    """

def test_format_unicode(doctest):
    u"""
    > (format "~s.00€" 1)
    "1.00€"
    """

def test_string_upcase_downcase(doctest):
    u"""
    > (string-upcase "über")
    "ÜBER"
    > (string-upcase "uber")
    "UBER"
    > (string-upcase "123aA")
    "123AA"
    > (string-downcase "ÜBER")
    "über"
    > (string-downcase "UBER")
    "uber"
    > (string-downcase "123Aa")
    "123aa"
    > (immutable? (string-upcase "abc"))
    #f
    """

def test_string_cmp(doctest):
    u"""
    > (string<? "a" "a")
    #f
    > (string<=? "a" "a")
    #t
    > (string=? "a" "a")
    #t
    > (string>=? "a" "a")
    #t
    > (string>? "a" "a")
    #f
    > (string<? "a" "b")
    #t
    > (string<=? "a" "b")
    #t
    > (string=? "a" "b")
    #f
    > (string>? "a" "b")
    #f
    > (string>=? "a" "b")
    #f
    > (string<? "a" "aa")
    #t
    > (string<=? "a" "aa")
    #t
    > (string=? "a" "aa")
    #f
    > (string>? "a" "aa")
    #f
    > (string>=? "a" "aa")
    #f
    > (string<? "a" "ä")
    #t
    > (string=? "a" "ä")
    #f
    > (string>? "a" "ä")
    #f
    """


def test_string_cmp_ci(doctest):
    u"""
    > (string-ci<? "A" "a")
    #f
    > (string-ci<=? "A" "a")
    #t
    > (string-ci=? "A" "a")
    #t
    > (string-ci>=? "A" "a")
    #t
    > (string-ci>? "A" "a")
    #f
    > (string-ci<? "A" "b")
    #t
    > (string-ci<=? "A" "b")
    #t
    > (string-ci=? "A" "b")
    #f
    > (string-ci>? "A" "b")
    #f
    > (string-ci>=? "A" "b")
    #f
    > (string-ci<? "A" "aa")
    #t
    > (string-ci<=? "A" "aa")
    #t
    > (string-ci=? "A" "aa")
    #f
    > (string-ci>? "A" "aa")
    #f
    > (string-ci>=? "A" "aa")
    #f
    > (string-ci<? "A" "ä")
    #t
    > (string-ci=? "A" "ä")
    #f
    > (string-ci>? "A" "ä")
    #f
    """

def test_make_string(doctest):
    ur"""
    > (make-string 4 #\a)
    "aaaa"
    > (make-string 4 #\ä)
    "ääää"
    E (make-string -5 #\ä)
    > (make-string 20 #\_)
    "____________________"
    > (make-string 2)
    "\u0000\u0000"
    """

def test_string_unicode(doctest):
    ur"""
    > (string #\A #\p #\p #\l #\e)
    "Apple"
    >  (string #\Ä #\p #\f #\e #\l)
    "Äpfel"
    """

def test_mutable_unicode_to_bytes(doctest):
    ur"""
    ! (define st (substring "hello fuß" 6 9))
    ! (string-set! st 0 #\s)
    ! (string-set! st 1 #\ü)
    > (string->bytes/utf-8 st)
    #"s\303\274\303\237"
    """

def test_list_to_string(doctest):
    u"""
    > (list->string '())
    ""
    > (list->string '(#\\a #\\b #\\c))
    "abc"
    > (list->string '(#\\α #\\β #\\ξ))
    "αβξ"
    > (immutable? (list->string '(#\\z)))
    #f
    """

def test_list_to_bytes(doctest):
    u"""
    > (list->bytes '(0 1 2 3 4))
    (bytes 0 1 2 3 4)
    > (list->bytes '())
    (bytes)
    > (list->bytes '(250 251 252 253 254 255))
    (bytes 250 251 252 253 254 255)
    """

def test_string_to_list(doctest):
    u"""
    > (string->list "")
    '()
    > (string->list "abc")
    '(#\\a #\\b #\\c)
    > (string->list "αβξ")
    '(#\\α #\\β #\\ξ)
    """

def test_string_length(doctest):
    u"""
    > (string-length "")
    0
    > (string-length "abc")
    3
    > (string-length "wasd")
    4
    > (string-length "αβξ")
    3
    > (unsafe-string-length "")
    0
    > (unsafe-string-length "abc")
    3
    > (unsafe-string-length "wasd")
    4
    > (unsafe-string-length "αβξ")
    3
    """

def test_string_to_number(doctest):
    """
    > (string->number "000")
    0
    > (string->number "069")
    69
    > (string->number "777")
    777
    > (string->number "3.14")
    3.14
    > (string->number "1111111112983718926391623986912350912395612093409182368590812")
    1111111112983718926391623986912350912395612093409182368590812
    > (string->number "9e6495a3" 16)
    2657392035
    """

def test_string_utf_8_length(doctest):
    """
    ! (define-values (h) (string-append "caner" (bytes->string/utf-8 (bytes 195 167 195 176 195 182 194 163))))
    > (string-length h)
    9
    > (string-utf-8-length h)
    13
    > (string-utf-8-length h 1 5)
    4
    > (string-utf-8-length h 2 7)
    7
    > (string-utf-8-length (substring h 2 7))
    7
    """

def test_bytes_utf_8_length(doctest):
    """
    > (bytes-utf-8-length (bytes 195 167 195 176 195 182 194 163))
    4
    > (bytes-length (bytes 195 167 195 176 195 182 194 163))
    8
    > (bytes-utf-8-length (make-bytes 5 65))
    5
    """
