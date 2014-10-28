
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
