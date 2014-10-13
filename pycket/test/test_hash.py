
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

