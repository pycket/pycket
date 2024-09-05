import pytest

def test_equal_huh(doctest):
    """
    > (equal? (integer->char 955) (integer->char 955))
    #t
    """
    assert doctest

def test_equal_always(doctest):
    """
    > (equal-always? #f #f)
    #t
    > (equal-always? 'yes 'yes)
    #t
    > (equal-always? 'yes 'no)
    #f
    > (equal-always? (* 6 7) 42)
    #t
    > (equal-always? (expt 2 100) (expt 2 100))
    #t
    > (equal-always? 2 2.0)
    #f
    > (equal-always? (list 1 2) (list 1 2))
    #t
    > (let ([v (mcons 1 2)]) (equal-always? v v))
    #t
    > (equal-always? (mcons 1 2) (mcons 1 2))
    #f
    > (equal-always? (integer->char 955) (integer->char 955))
    #t
    > (equal-always? "test" "test")
    #t
    > (equal-always? (make-string 3 #\z) (make-string 3 #\z))
    #f
    > (equal-always? (string->immutable-string (make-string 3 #\z)) (string->immutable-string (make-string 3 #\z)))
    #t
    > (equal-always? (hash) (hash))
    #t
    > (equal-always? (make-hash) (make-hash))
    #f
    > (equal-always? (hash 1 2) (hash))
    #f
    > (equal-always? (hash 1 2) (hash 1 2))
    #t
    > (struct color (r g b))
    > (equal-always? color color)
    #t
    > (equal-always? (vector 1 2) (vector))
    #f
    > (equal-always? (vector 1 2) (vector 1 2))
    #f
    > (equal-always? (box 2) (box 2))
    #f
    > (define b (box 2))
    > (equal-always? b b)
    #t
    > (equal-always? (list 1 2) (list 1 2))
    #t
    > (equal-always? (list 1 (box 2)) (list 1 (box 2)))
    #f
    > (equal-always? (list 1 b) (list 1 b))
    #t
    > (equal-always? (box (list 1)) (box (list 1)))
    #f
    """
    assert doctest

