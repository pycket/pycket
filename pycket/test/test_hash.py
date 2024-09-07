import operator as op
from pycket                          import values
from pycket.hash.base                import ll_get_dict_item, get_dict_item
from pycket.hash.equal               import MutableByteHashmapStrategy, StringHashmapStrategy
from pycket.hash.persistent_hash_map import make_persistent_hash_type, validate_persistent_hash
from pycket.test.testhelper          import run_mod_expr, run_mod
from rpython.rlib.rarithmetic        import r_uint
from rpython.jit.metainterp.test.support import LLJitMixin, noConst

import pytest

@pytest.mark.skip(reason="FIXME")
def test_hash_weak(doctest):
    """
    > (hash-weak? (impersonate-hash (make-hash) (lambda (x y) 1) (lambda (x y z) 2) (lambda (h a) 3) (lambda (h a) 4)))
    #f
    """

def test_hash_equal(doctest):
    """
    > (hash-equal? (hash))
    #t
    > (hash-equal? (hashalw))
    #f
    > (hash-equal? (hasheq))
    #f
    > (hash-equal? (hasheqv))
    #f
    > (hash-equal? (make-hash))
    #t
    > (hash-equal? (make-hashalw))
    #f
    > (hash-equal? (make-hasheq))
    #f
    > (hash-equal? (make-hasheqv))
    #f
    > (hash-equal? #hash())
    #t
    > (hash-equal? #hashalw())
    #f
    > (hash-equal? #hasheq())
    #f
    > (hash-equal? #hasheqv())
    #f
    > (hash-equal? (impersonate-hash (make-hash) (lambda (x y) 1) (lambda (x y z) 2) (lambda (h a) 3) (lambda (h a) 4)))
    #t
    > (hash-equal? (chaperone-hash (hash) (lambda (x y) 1) (lambda (x y z) 2) (lambda (h a) 3) (lambda (h a) 4)))
    #t
    > (hash-equal? (make-weak-hash))
    #t
    > (hash-equal? (make-weak-hasheq))
    #f
    > (hash-equal? (make-weak-hasheqv))
    #f
    """

def test_hash_equal_always(doctest):
    """
    > (hash-equal-always? (hash))
    #f
    > (hash-equal-always? (hashalw))
    #t
    > (hash-equal-always? (hasheq))
    #f
    > (hash-equal-always? (hasheqv))
    #f
    > (hash-equal-always? (make-hash))
    #f
    > (hash-equal-always? (make-hashalw))
    #t
    > (hash-equal-always? (make-hasheq))
    #f
    > (hash-equal-always? (make-hasheqv))
    #f
    > (hash-equal-always? #hash())
    #f
    > (hash-equal-always? #hashalw())
    #t
    > (hash-equal-always? #hasheq())
    #f
    > (hash-equal-always? #hasheqv())
    #f
    > (hash-equal-always? (impersonate-hash (make-hash) (lambda (x y) 1) (lambda (x y z) 2) (lambda (h a) 3) (lambda (h a) 4)))
    #f
    > (hash-equal-always? (chaperone-hash (hash) (lambda (x y) 1) (lambda (x y z) 2) (lambda (h a) 3) (lambda (h a) 4)))
    #f
    > (hash-equal-always? (make-weak-hash))
    #f
    > (hash-equal-always? (make-weak-hasheq))
    #f
    > (hash-equal-always? (make-weak-hasheqv))
    #f
    """

def test_hash_eq(doctest):
    """
    > (hash-eq? (hash))
    #f
    > (hash-eq? (hashalw))
    #f
    > (hash-eq? (hasheq))
    #t
    > (hash-eq? (hasheqv))
    #f
    > (hash-eq? (make-hash))
    #f
    > (hash-eq? (make-hashalw))
    #f
    > (hash-eq? (make-hasheq))
    #t
    > (hash-eq? (make-hasheqv))
    #f
    > (hash-eq? #hash())
    #f
    > (hash-eq? #hashalw())
    #f
    > (hash-eq? #hasheq())
    #t
    > (hash-eq? #hasheqv())
    #f
    > (hash-eq? (impersonate-hash (make-hasheq) (lambda (x y) 1) (lambda (x y z) 2) (lambda (h a) 3) (lambda (h a) 4)))
    #t
    > (hash-eq? (chaperone-hash (hasheq) (lambda (x y) 1) (lambda (x y z) 2) (lambda (h a) 3) (lambda (h a) 4)))
    #t
    > (hash-eq? (make-weak-hash))
    #f
    > (hash-eq? (make-weak-hasheq))
    #t
    > (hash-eq? (make-weak-hasheqv))
    #f
    """

def test_hash_eqv(doctest):
    """
    > (hash-eqv? (hash))
    #f
    > (hash-eqv? (hashalw))
    #f
    > (hash-eqv? (hasheq))
    #f
    > (hash-eqv? (hasheqv))
    #t
    > (hash-eqv? (make-hash))
    #f
    > (hash-eqv? (make-hashalw))
    #f
    > (hash-eqv? (make-hasheq))
    #f
    > (hash-eqv? (make-hasheqv))
    #t
    > (hash-eqv? #hash())
    #f
    > (hash-eqv? #hashalw())
    #f
    > (hash-eqv? #hasheq())
    #f
    > (hash-eqv? #hasheqv())
    #t
    > (hash-eqv? (impersonate-hash (make-hasheqv) (lambda (x y) 1) (lambda (x y z) 2) (lambda (h a) 3) (lambda (h a) 4)))
    #t
    > (hash-eqv? (chaperone-hash (hasheqv) (lambda (x y) 1) (lambda (x y z) 2) (lambda (h a) 3) (lambda (h a) 4)))
    #t
    > (hash-eqv? (make-weak-hash))
    #f
    > (hash-eqv? (make-weak-hasheq))
    #f
    > (hash-eqv? (make-weak-hasheqv))
    #t
    """

def test_hash_simple(doctest):
    """
    ! (define ht (make-hash))
    ! (hash-set! ht "apple" '(red round))
    ! (hash-set! ht "banana" '(yellow long))
    ! (define (f) #hash())
    > (hash-ref ht "apple")
    '(red round)
    E (hash-ref ht "coconut")
    > (hash-ref ht "coconut" "not there")
    "not there"
    > (hash-count ht)
    2
    > (eq? (f) (f))
    #t
    """

def test_hash_ref_key(doctest):
    """
    ! (define original-key "hello")
    ! (define key-copy (string-copy original-key))
    ! (define table (make-hash))
    ! (hash-set! table original-key 'value)
    > (equal? original-key key-copy)
    #t
    > (eq? original-key key-copy)
    #f
    > (eq? (hash-ref-key table "hello") original-key)
    #t
    > (eq? (hash-ref-key table "hello") key-copy)
    #f
    """

def test_hash_immutable(doctest):
    """
    > (immutable? (hash))
    #t
    > (immutable? (hasheq))
    #t
    > (immutable? (hasheqv))
    #t
    > (immutable? (make-hash))
    #f
    > (immutable? (make-hasheq))
    #f
    > (immutable? (make-hasheqv))
    #f
    > (immutable? #hash())
    #t
    """

def test_hasheqv(doctest):
    """
    ! (define ht (make-hasheqv))
    > (hash-set! ht 1.0 'a)
    > (hash-set! ht 2.0 'a)
    > (hash-ref ht (+ 1.0 1.0))
    'a
    """

def test_immutable_hasheqv(doctest):
    """
    ! (require racket/private/for)
    ! (define h (for/fold ([acc (make-immutable-hasheqv)]) ([i (in-range 0 100)]) (hash-set acc i (+ i 1))))
    ! (define h^ (hash-set h 2.0 'a))
    > (hash-ref h 0)
    1
    > (hash-ref h 50)
    51
    > (hash-ref h  99)
    100
    > (hash-ref h^ (+ 1.0 1.0))
    'a
    """

def test_immutable_hasheq(doctest):
    """
    ! (define h (make-immutable-hasheq '((a . b) (b . c) (c . d))))
    > (hash-ref h 'a)
    'b
    > (hash-ref h 'b)
    'c
    > (hash-ref h 'c)
    'd
    > (hash-ref (hash-remove h 'b) 'b #f)
    #f
    """

def test_hash_symbols(doctest):
    """
    ! (define ht (make-hash))
    ! (hash-set! ht 'a '(red round))
    ! (hash-set! ht 'b '(yellow long))
    > (hash-ref ht 'a)
    '(red round)
    > (hash-ref ht 'b)
    '(yellow long)
    E (hash-ref ht 'c)
    > (hash-ref ht 'c "not there")
    "not there"

    > (hash-set! ht 1 'ohnoes) ; dehomogenize
    > (hash-ref ht 'a)
    '(red round)
    > (hash-ref ht 'b)
    '(yellow long)
    > (hash-ref ht 1)
    'ohnoes
    """

def test_hash_strings(doctest):
    """
    ! (define ht (make-hash))
    ! (hash-set! ht "a" '(red round))
    ! (hash-set! ht "b" '(yellow long))
    > (hash-ref ht "a")
    '(red round)
    > (hash-ref ht "b")
    '(yellow long)
    E (hash-ref ht "c")
    > (hash-ref ht "c" "not there")
    "not there"

    > (hash-set! ht 1 'ohnoes) ; dehomogenize
    > (hash-ref ht "a")
    '(red round)
    > (hash-ref ht "b")
    '(yellow long)
    > (hash-ref ht 1)
    'ohnoes
    """

def test_hash_bytes(doctest):
    """
    ! (define ht (make-hash))
    ! (hash-set! ht #"a" '(red round))
    ! (hash-set! ht #"bc" '(yellow long))
    > (hash-ref ht #"a")
    '(red round)
    > (hash-ref ht #"bc")
    '(yellow long)
    > (hash-ref ht (bytes-append #"b" #"c"))
    '(yellow long)
    E (hash-ref ht #"c")
    > (hash-ref ht #"c" "not there")
    "not there"

    > (hash-set! ht 1 'ohnoes) ; dehomogenize
    > (hash-ref ht #"a")
    '(red round)
    > (hash-ref ht #"bc")
    '(yellow long)
    > (hash-ref ht 1)
    'ohnoes
    """

def test_hash_ints(doctest):
    """
    ! (define ht (make-hash))
    ! (hash-set! ht 1 '(red round))
    ! (hash-set! ht 1099 '(yellow long))
    > (hash-ref ht 1)
    '(red round)
    > (hash-ref ht 1099)
    '(yellow long)
    E (hash-ref ht 28)
    > (hash-ref ht 28 "not there")
    "not there"
    > (hash-ref ht 'foo 'nope)
    'nope
    > (hash-ref ht 1)
    '(red round)
    > (hash-ref ht 1099)
    '(yellow long)
    """

def test_hash_for_each(doctest):
    """
    ! (define x 1)
    ! (define h #hash((1 . 2) (2 . 3) (3 . 4)))
    ! (define (fe c v) (set! x (+ x (* c v))))
    ! (hash-for-each h fe)
    > x
    21
    """

def test_persistent_eqhash_for_each(doctest):
    """
    ! (require racket/private/for)
    ! (define x 1)
    ! (define h (for/fold ([acc (make-immutable-hasheq)]) ([i (in-range 1 4)]) (hash-set acc i (+ i 1))))
    ! (define (fe c v) (set! x (+ x (* c v))))
    ! (hash-for-each h fe)
    > x
    21
    """

def test_hash_map(doctest):
    """
    ! (define h #hash((1 . 2) (2 . 3) (3 . 4)))
    ! (define s (hash-map h (lambda (k v) (+ k v))))
    > s
    > (or (equal? s '(3 5 7)) (equal? s '(3 7 5))
          (equal? s '(5 3 7)) (equal? s '(5 7 3))
          (equal? s '(7 3 5)) (equal? s '(7 5 3)))
    #t
    """

def test_hash_copy(doctest):
    """
    ! (define h #hash((1 . 2) (2 . 3) (3 . 4)))
    ! (define k (hash-copy h))
    > (equal? 2 (hash-ref k 1))
    #t
    > (equal? 3 (hash-ref k 2))
    #t
    > (equal? 4 (hash-ref k 3))
    #t
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
    > (hash-remove! (make-hash) 1)
    (void)
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

def test_default_hash(source):
    """
    (let ()
    (make-weak-hasheq)
    (make-immutable-hash)
    (make-hash)
    (make-hasheq)
    (make-hasheqv)
    #t)
    """
    result = run_mod_expr(source, wrap=True)
    assert result is values.w_true

def test_get_item():
    from rpython.rtyper.test.test_llinterp import interpret, get_interpreter
    def tg(a, b, c, d):
        dct = {str(a): b, str(c): d}
        i = 0
        while 1:
            try:
                x, y = get_dict_item(dct, i)
                assert (x == str(a) and y == b) or (x == str(c) and y == d)
            except KeyError:
                pass
            except IndexError:
                break
            i += 1
        return i
    assert tg("1", 2, "3", 4) == interpret(tg, ["1", 2, "3", 4])
    assert tg(1, 2, 334, 4)   == interpret(tg, [1, 2, 334, 4])
    assert tg(1, 2, 3, 4)     == interpret(tg, [1, 2, 3, 4])

def test_ll_get_dict_item():
    """
    Tests the low-level implementation of get_dict_item.
    """
    from rpython.annotator.annrpython import RPythonAnnotator
    from rpython.annotator.model import SomeTuple, SomeInteger, SomeString

    from rpython.rtyper.rtyper import RPythonTyper
    from rpython.rtyper.rmodel import inputconst
    from rpython.rtyper.annlowlevel import llstr, hlstr

    from rpython.rtyper.lltypesystem import lltype, rffi
    from rpython.rtyper.lltypesystem import rordereddict, rstr

    dummykeyobj = None
    dummyvalueobj = None

    def _get_str_dict():
        # STR -> lltype.Signed
        DICT = rordereddict.get_ll_dict(lltype.Ptr(rstr.STR), lltype.Signed,
                            ll_fasthash_function=rstr.LLHelpers.ll_strhash,
                            ll_hash_function=rstr.LLHelpers.ll_strhash,
                            ll_eq_function=rstr.LLHelpers.ll_streq,
                            dummykeyobj=dummykeyobj,
                            dummyvalueobj=dummyvalueobj)
        return DICT
    s_tuple = SomeTuple([SomeString(), SomeInteger()])
    DICT = _get_str_dict()

    ll_d = rordereddict.ll_newdict(DICT)
    a = RPythonAnnotator()
    rtyper = RPythonTyper(a)
    a.translator.rtyper = rtyper
    r_tuple = rtyper.getrepr(s_tuple)
    cTUPLE = inputconst(lltype.Void, r_tuple.lowleveltype)
    s_tuple = rtyper.annotation(cTUPLE)
    rtyper.call_all_setups()

    for i in range(20):
        rordereddict.ll_dict_setitem(ll_d, llstr(str(i)), i)
    for i in range(20):
        element = ll_get_dict_item(s_tuple.const, ll_d, i)
        assert (str(i), i) == (hlstr(element.item0), element.item1)

def test_whitebox_str(source):
    r"""
    (let ([ht (make-hash)] [st (string #\a #\b)])
        (string-set! st 0 #\x)
        (hash-set! ht "a" '(red round))
        (hash-set! ht "b" '(yellow long))
        (hash-set! ht st 77)
        (hash-ref ht "c" "not there")
        ht)
    """
    result = run_mod_expr(source)
    assert result.strategy is StringHashmapStrategy.singleton

def test_whitebox_str(source):
    r"""
    (let ([ht (make-hash)] [st (string #\a #\b)])
        (string-set! st 0 #\x)
        (hash-set! ht "a" '(red round))
        (hash-set! ht "b" '(yellow long))
        (hash-set! ht st 77)
        (hash-ref ht "c" "not there")
        ht)
    """
    result = run_mod_expr(source)
    assert result.strategy is StringHashmapStrategy.singleton

def test_whitebox_bytes(source):
    r"""
    (let ([ht (make-hash)] [st (bytes 65 66)])
        (bytes-set! st 0 67)
        (hash-set! ht (bytes 1 2 3) '(red round))
        (hash-set! ht (bytes 4 5 6) '(yellow long))
        (hash-set! ht st 77)
        (hash-ref ht (bytes 7 8 9) "not there")
        ht)
    """
    result = run_mod_expr(source)
    assert result.strategy is MutableByteHashmapStrategy.singleton

def test_hash_for(doctest):
    """
    ! (require racket/private/for)
    > (define ht (make-hash))
    > (hash-set! ht 'a 1)
    > (hash-set! ht 'b 2)
    > (hash-set! ht 'c 3)
    > (for/sum ([(k v) (in-hash ht)]) v)
    6
    > (for/sum ([(k v) ht]) v)
    6
    """

def test_persistent_hash():
    HashTable = make_persistent_hash_type()
    acc = HashTable.EMPTY()

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i)

    assert len(acc) == 10
    assert len(list(acc.iteritems())) == 10
    for k, v in acc.iteritems():
        assert k <= 10
        assert v >= 990
        assert v % 10 == k
        assert acc.val_at(k, None) is v

def test_persistent_hash2():
    HashTable = make_persistent_hash_type()
    acc = HashTable.EMPTY()

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i)

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i)

    assert len(acc) == 10
    assert len(list(acc.iteritems())) == 10
    for k, v in acc.iteritems():
        assert k <= 10
        assert v >= 990
        assert v % 10 == k
        assert acc.val_at(k, None) is v

def test_persistent_hash_collisions():
    HashTable = make_persistent_hash_type(hashfun=lambda x: r_uint(42))
    acc = HashTable.EMPTY()

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i)

    assert len(acc) == 10
    assert len(list(acc.iteritems())) == 10
    for k, v in acc.iteritems():
        assert k <= 10
        assert v >= 990
        assert v % 10 == k
        assert acc.val_at(k, None) is v

def test_persistent_hash_collisions2():
    HashTable = make_persistent_hash_type(hashfun=lambda x: r_uint(hash(x)) % 8)
    acc = HashTable.EMPTY()

    for i in range(2048):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 128, i)

    assert len(acc) == 128
    assert len(list(acc.iteritems())) == 128
    for k, v in acc.iteritems():
        assert acc.val_at(k, None) is v

def test_persistent_hash_removal():
    HashTable = make_persistent_hash_type()
    acc = HashTable.EMPTY()

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i).without(i % 10)

    assert len(acc) == 0
    assert list(acc.iteritems()) == []

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i).without(i % 10 + 1)

    assert len(acc) == 10
    assert len(list(acc.iteritems())) == 10
    for k, v in acc.iteritems():
        assert k <= 10
        assert v >= 990
        assert v % 10 == k
        assert acc.val_at(k, None) is v

def test_persistent_hash__collisions_removal():
    HashTable = make_persistent_hash_type(hashfun=lambda x: r_uint(42))
    acc = HashTable.EMPTY()

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i).without(i % 10)

    assert len(acc) == 0
    assert list(acc.iteritems()) == []

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i).without(i % 10 + 1)

    assert len(acc) == 10
    assert len(list(acc.iteritems())) == 10
    for k, v in acc.iteritems():
        assert k <= 10
        assert v >= 990
        assert v % 10 == k
        assert acc.val_at(k, None) is v

def test_persistent_hash__collisions_removal2():
    HashTable = make_persistent_hash_type(hashfun=lambda x: r_uint(hash(x) % 8))
    acc = HashTable.EMPTY()

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i).without(i % 10)

    assert len(acc) == 0
    assert list(acc.iteritems()) == []

    for i in range(1000):
        validate_persistent_hash(acc)
        acc = acc.assoc(i % 10, i).without(i % 10 + 1)

    assert len(acc) == 10
    assert len(list(acc.iteritems())) == 10
    for k, v in acc.iteritems():
        assert k <= 10
        assert v >= 990
        assert v % 10 == k
        assert acc.val_at(k, None) is v

def test_persistent_hash_union():
    HashTable = make_persistent_hash_type()
    acc1 = HashTable.EMPTY()
    acc2 = HashTable.EMPTY()

    for i in range(128):
        acc1 = acc1.assoc(i, i)

    for i in range(128, 256):
        acc2 = acc2.assoc(i, i)

    assert len(acc1) == 128
    assert len(acc2) == 128

    acc3 = acc1 + acc2
    assert len(acc3) == 256
    for i in range(256):
        assert i in acc3

def test_without_many():
    HashTable = make_persistent_hash_type()
    acc = HashTable.EMPTY()

    for i in range(256):
        acc = acc.assoc(i, i)

    acc = acc.without_many(range(256))
    assert len(acc) == 0

def test_hash_iterate_functions(doctest):
    """
    ! (define eq-table (hasheq (cons 1 2) 3))
    ! (define equal-table1 (hash (cons 1 2) 3))
    ! (define equal-table2 (hash 'hello 'bye))
    ! (define equal-table3 (hash "hello" "bye"))
    > (hash-iterate-next eq-table (hash-iterate-first eq-table))
    #f
    > (hash-iterate-next equal-table1 (hash-iterate-first equal-table1))
    #f
    > (hash-iterate-next equal-table2 (hash-iterate-first equal-table2))
    #f
    > (hash-iterate-next equal-table3 (hash-iterate-first equal-table3))
    #f
    """

def test_hash_keys_subset(doctest):
    """
    ! (struct a (n m) #:property prop:equal+hash (list (lambda (a b eql) (and (= (a-n a) (a-n b)) (= (a-m a) (a-m b)))) (lambda (a hc) (a-n a)) (lambda (a hc) (a-n a))))
    ! (define ht0 (hash (a 1 0) #t))
    ! (define ht1 (hash (a 1 0) #t (a 1 2) #t))
    ! (define ht2 (hash (a 1 0) #t (a 1 2) #t (a 33 0) #t))
    ! (define ht3 (hash (a 1 0) #t (a 33 0) #t))
    > (hash-keys-subset? ht1 ht2)
    #t
    > (hash-keys-subset? ht3 ht2)
    #t
    > (hash-keys-subset? ht0 ht3)
    #t
    > (hash-keys-subset? ht0 ht2)
    #t
    > (hash-keys-subset? ht0 ht1)
    #t
    > (hash-keys-subset? ht2 ht1)
    #f
    > (hash-keys-subset? ht2 ht0)
    #f
    > (hash-keys-subset? ht1 ht0)
    #f
    > (hash-keys-subset? ht1 ht3)
    #f
    E (hash-keys-subset? ht0 (hasheq (a 1 0) #t))
    """

def test_hash_equal_hash_codes(doctest):
    """
    > (equal? (hash (hash) 1) (hash (hash) 1))
    #t
    > (hash-ref (make-hash '((#hasheq() . 1))) #hasheq() #f)
    1
    > (hash-ref (make-hash '((#hasheq((a . 3)) . 1))) #hasheq((a . 3)) #f)
    1
    > (hash-ref (make-hash (list (cons (hasheq) 1))) (hasheq) #f)
    1
    > (hash-ref (make-hash (list (cons (hash) 1))) (hash) #f)
    1
    > (hash-ref (hash (hash) 1) (hash) #f)
    1
    """
