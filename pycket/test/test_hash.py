
def test_hash_simple(doctest):
    """
    ! (define ht (make-hash))
    ! (hash-set! ht "apple" '(red round))
    ! (hash-set! ht "banana" '(yellow long))
    > (hash-ref ht "apple")
    '(red round)
    E (hash-ref ht "coconut")
    > (hash-ref ht "coconut" "not there")
    "not there"
    """

def test_use_equal(doctest):
    """
    ! (define ht (make-hash))
    ! (define key (cons 'a 'b))
    ! (hash-set! ht key 1)
    ! (define hteqv (make-hasheqv))
    ! (hash-set! hteqv key 1)
    > (hash-ref ht key)
    1
    > (hash-ref ht (cons 'a 'b) 2)
    1
    ; now with eqv
    > (hash-ref hteqv key)
    1
    > (hash-ref hteqv (cons 'a 'b) 2)
    2
    """

def test_hash_tableau(doctest):
    """
    ! (define ht #hash((1.0 . 3) (1 . 2)))
    ! (define ht2 '#hash(((a . b) . 1)))
    > (hash-ref ht 1.0)
    3
    > (hash-ref ht 1)
    2
    > (hash-ref ht2 (cons 'a 'b) 2)
    1
    """
