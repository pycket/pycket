import pytest
from pycket.expand import expand
from pycket.interpreter import *
from pycket.values import *
from pycket.impersonators import *
from pycket.vector import *
from pycket.prims import *
from pycket.test.testhelper import run_fix, run, run_mod, execute, check_equal
import sys

def test_vec():
    assert isinstance(run('(vector 1)'), W_Vector)
    #run('(vector? (quote #(0 (2 2 2 2)) "Anna"))', w_true)
    #run("(vector? (quote #())", w_true)
    run_fix("(let-values ([(v) (vector 1 2 3)]) (vector-length v))", 3)
    run("(let-values ([(v) (vector 1 2 3)]) (vector-set! v 0 0))", w_void)
    run_fix("(let-values ([(v) (vector 1 2 3)]) (vector-set! v 0 0) (vector-length v))", 3)
    run_fix("(let-values ([(v) (vector 1 2 3)]) (vector-set! v 0 0) (vector-ref v 0))", 0)

def test_vec_equal():
    run("(equal? (vector 1 2 3) (vector 1 2 3))", w_true)
    run("(equal? (vector 1 2 3) (vector 1 2))", w_false)
    run("(equal? (vector 1 2 3) (vector 1 2 5))", w_false)

def test_make_vector():
    run_fix("(let-values ([(v) (vector)]) (vector-length v))", 0)
    run_fix("(let-values ([(v) (make-vector 5)]) (vector-length v))", 5)
    vec = run('(make-vector 5)')
    for i in range(vec.length()):
        assert vec.ref(i).value == 0

def test_vec_strategies_empty():
    vec = run("(vector)")
    print "First size: %s" % vec.length()
    assert isinstance(vec.strategy, ObjectVectorStrategy)
    vec = run("(make-vector 0)")
    print "Second size: %s" % vec.length()
    assert isinstance(vec.strategy, ObjectVectorStrategy)
    vec = run("(vector-immutable)")
    assert isinstance(vec.strategy, ObjectImmutableVectorStrategy)
    vec = run("(vector-immutable (quote hello))")
    assert isinstance(vec.strategy, ObjectImmutableVectorStrategy)

def test_vec_strategies_fixnum():
    vec = run("(vector 1 2 3)")
    assert isinstance(vec.strategy, FixnumVectorStrategy)
    vec = run("(make-vector 2)")
    assert isinstance(vec.strategy, ConstantVectorStrategy)

def test_vec_strategies_flonum():
    vec = run("(vector 1.0 2.1 3.2)")
    assert isinstance(vec.strategy, FlonumVectorStrategy)
    vec = run("(make-vector 2 1.2)")
    assert isinstance(vec.strategy, ConstantVectorStrategy)

def test_vec_strategies_fixnum_singleton():
    vec1 = run("(vector 1 2 3)")
    vec2 = run("(vector 3 2 1)")
    assert vec1.strategy is vec2.strategy

def test_vec_strategies_object():
    vec = run("(vector (cons 1 2) 2 3)")
    assert isinstance(vec.strategy, ObjectVectorStrategy)
    vec = run("(vector-immutable (cons 1 2) 2 3)")
    assert isinstance(vec.strategy, ObjectImmutableVectorStrategy)

def test_vec_strategies_stays_fixnum():
    vec = run("(let-values ([(vec) (vector 0 0 0)]) (vector-set! vec 1 5) vec)")
    assert isinstance(vec.strategy, FixnumVectorStrategy)

def test_vec_strategies_stays_flonum():
    vec = run("(let-values ([(vec) (vector 1.2 1.2 1.2)]) (vector-set! vec 1 5.5) vec)")
    assert isinstance(vec.strategy, FlonumVectorStrategy)
    vec = run("(let-values ([(vec) (vector 1.2 1.2 1.2)]) (vector-set! vec 1 0) vec)")

    # Test that we can encode the full range of signed 32-bit values in the tagged
    # flonum strategy
    assert isinstance(vec.strategy, FlonumTaggedVectorStrategy)
    vec = run("(let-values ([(vec) (vector 1.2 1.2 1.2)]) (vector-set! vec 1 %d) vec)" % (2 ** 31 - 1))
    assert isinstance(vec.strategy, FlonumTaggedVectorStrategy)
    vec = run("(let-values ([(vec) (vector 1.2 1.2 1.2)]) (vector-set! vec 1 %d) vec)" % (-(2 ** 31)))
    assert isinstance(vec.strategy, FlonumTaggedVectorStrategy)

    # Test transitions from the constant vector strategy to the tagged flonum strategy
    vec = run("(let-values ([(vec) (make-vector 10 0)]) (vector-set! vec 1 1.1) vec)")
    assert isinstance(vec.strategy, FlonumTaggedVectorStrategy)
    vec = run("(let-values ([(vec) (make-vector 10 %d)]) (vector-set! vec 1 1.1) vec)" % (2 ** 31 - 1))
    assert isinstance(vec.strategy, FlonumTaggedVectorStrategy)
    vec = run("(let-values ([(vec) (make-vector 10 %d)]) (vector-set! vec 1 1.1) vec)" % (-(2 ** 31)))
    assert isinstance(vec.strategy, FlonumTaggedVectorStrategy)

def test_vec_strategies_dehomogenize():
    vec = run('(let-values ([(vec) (vector 1 2 3)]) (vector-set! vec 1 "Anna") vec)')
    assert isinstance(vec.strategy, ObjectVectorStrategy)
    vec = run('(let-values ([(vec) (make-vector 3 1)]) (vector-set! vec 1 "Anna") vec)')
    assert isinstance(vec.strategy, ObjectVectorStrategy)
    vec = run('(let-values ([(vec) (make-vector 3 1)]) (vector-set! vec 1 2) vec)')
    assert isinstance(vec.strategy, FixnumVectorStrategy)

def test_vec_strategies_character():
    vec1 = run(r"(vector #\A #\B #\C)")
    assert isinstance(vec1.strategy, CharacterVectorStrategy)
    vec2 = run(r"(vector #\a)")
    assert isinstance(vec2.strategy, CharacterVectorStrategy)

def test_vec_strategies_stays_character():
    vec = run(r"(let-values ([(vec) (vector #\A #\A #\A)]) (vector-set! vec 1 #\D) vec)")
    assert isinstance(vec.strategy, CharacterVectorStrategy)

def test_vec_strategies_character_singleton():
    vec1 = run(r"(vector #\A #\A #\A)")
    vec2 = run(r"(vector #\B #\B)")
    assert vec1.strategy is vec2.strategy

def test_vec_strategies_character_ref(doctest):
    r"""
    > (define v (vector #\a #\b #\c))
    > (vector-ref v 0)
    #\a
    > (vector-ref v 1)
    #\b
    > (vector-ref v 2)
    #\c
    """

def run_unsafe_expander(e,v):
    run("(begin (#%%require (quote #%%unsafe)) %s)" % e,v,extra="")
def run_fix_unsafe_expander(e,v):
    run_fix("(begin (#%%require (quote #%%unsafe)) %s)" % e,v,extra="")

def run_unsafe(e,v):
    run(e,v,extra="")
def run_fix_unsafe(e,v):
    run_fix(e,v,extra="")

def test_unsafe_impersonators():
    ru = run_unsafe
    ru_fix = run_fix_unsafe
    if pytest.config.load_expander:
        ru = run_unsafe_expander
        ru_fix = run_fix_unsafe_expander
    ru("(equal? 3 (unsafe-vector-length (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))))", w_true)
    ru("(equal? 3 (unsafe-vector-ref (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) 2))", w_true)
    ru_fix("(let-values ([(v) (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (unsafe-vector-set! v 0 0) (unsafe-vector-ref v 0))", 0)

def test_unsafe():
    ru = run_unsafe
    ru_fix = run_fix_unsafe
    if pytest.config.load_expander:
        ru = run_unsafe_expander
        ru_fix = run_fix_unsafe_expander

    ru("(equal? 3 (unsafe-vector-length (vector 1 2 3)))", w_true)
    ru("(equal? 3 (unsafe-vector*-length (vector 1 2 3)))", w_true)
    ru("(equal? 3 (unsafe-vector-ref (vector 1 2 3) 2))", w_true)
    ru("(equal? 3 (unsafe-vector*-ref (vector 1 2 3) 2))", w_true)
    ru_fix("(let-values ([(v) (vector 1 2 3)]) (unsafe-vector-set! v 0 0) (unsafe-vector-ref v 0))", 0)
    ru_fix("(let-values ([(v) (vector 1 2 3)]) (unsafe-vector*-set! v 0 0) (unsafe-vector*-ref v 0))", 0)

def test_vec_imp():
    assert isinstance(run('(impersonate-vector (vector 1) values values)'), W_ImpVector)
    #run('(vector? (chaperone-vector \'#(0 (2 2 2 2) "Anna") values values))', w_true)
    run_fix("(let-values ([(v) (impersonate-vector (vector 1 2 3) values values)]) (vector-length v))", 3)
    run("(let-values ([(v) (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (vector-set! v 0 0))", w_void)
    run_fix("(let-values ([(v) (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (vector-set! v 0 0) (vector-length v))", 3)
    run_fix("(let-values ([(v) (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (vector-set! v 0 0) (vector-ref v 0))", 0)

def test_vec_equal_imp():
    run("(equal? (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) (vector 1 2 3))", w_true)
    run("(equal? (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) (vector 1 2))", w_false)
    run("(equal? (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) (vector 1 2 5))", w_false)

def test_make_vector_imp():
    run_fix("(let-values ([(v) (impersonate-vector (vector) (lambda (x y z) z) (lambda (x y z) z))]) (vector-length v))", 0)
    run_fix("(let-values ([(v) (impersonate-vector (make-vector 5) (lambda (x y z) z) (lambda (x y z) z))]) (vector-length v))", 5)

def test_bug_symbol_in_vector():
    # FIXME somebody who knows expand
    run("#('a)")

def test_vec_values():
    run_fix("(let-values ([(a b c) (vector->values (vector 1 2 3))]) (+ a b c))", 6)
    run_fix("(let-values ([(b c) (vector->values (vector 1 2 3) 1)]) (+ b c))", 5)
    run_fix("(let-values ([(b) (vector->values (vector 1 2 3) 1 2)]) (+ b))", 2)


def test_flvector(doctest):
    """
    ! (require '#%flfxnum '#%unsafe)
    > (flvector-ref (flvector 0.0) 0)
    0.0
    > (define v (flvector 0.0 1.0))
    > (flvector-ref v 0)
    0.0
    > (flvector-ref v 1)
    1.0
    > (flvector-set! v 0 2.0)
    (void)
    > (flvector-ref v 0)
    2.0
    > (unsafe-flvector-ref v 0)
    2.0
    > (unsafe-flvector-set! v 0 3.0)
    (void)
    > (flvector-ref v 0)
    3.0
    > (define v2 (make-flvector 5))
    > (flvector-ref v2 4)
    0.0
    > (define v3 (make-flvector 5 3.0))
    > (flvector-ref v3 4)
    3.0
    """
    assert doctest

def test_flvector_set_wrong_type():
    with pytest.raises(SchemeException):
        run_mod("""
            #lang pycket
            (require '#%flfxnum '#%unsafe)
            (let [(a (flvector 1.2 1.3))] (flvector-set! a 1 'a))
        """)

def test_vector_copy_bang(doctest):
    """
    > (define v (vector 'A 'p 'p 'l 'e))
    > (define src (vector))
    > (define dest (vector 1))
    > (vector-copy! v 4 #(y))
    > (vector-copy! v 0 v 3 4)
    > v
    '#(l p p l y)
    > (vector-copy! v 0 #() 0 0)
    > (vector-copy! dest 1 src 0)
    > dest
    '#(1)
    """

def test_list_vector_conversion():
    check_equal(
        "(vector->list #(1 2 3 4))", "(list 1 2 3 4)",
        "(vector->list #())", "'()",
        "(vector->list #(1.1 a))", "(list 1.1 'a)",
        "#(1 2 3 4)", "(list->vector (list 1 2 3 4))",
        "#()", "(list->vector '())",
        "#(1.1 a)", "(list->vector (list 1.1 'a))",
    )

def test_eq_and_vectors_agree(doctest):
    """
    > (eq? 3.5 (vector-ref (vector 3.5) 0))
    #t
    """

def test_vector_to_immutable_vector(doctest):
    r"""
    ! (define fl-vector^ (vector-immutable 1.0 2.0 3.0))
    ! (define fl-vector (vector 1.0 2.0 3.0))
    ! (define fx-vector (vector 1 2 3))
    ! (define ch-vector (vector #\a #\b #\c))
    > (eq? fl-vector^ (vector->immutable-vector fl-vector^))
    #t
    > (eq? fl-vector (vector->immutable-vector fl-vector))
    #f
    > (eq? fx-vector (vector->immutable-vector fx-vector))
    #f
    > (eq? ch-vector (vector->immutable-vector ch-vector))
    #f
    """

def test_copy_vector_strategy_preserve():
    vec = run("(vector->immutable-vector (vector 1.0 2.0 3.0))")
    assert vec.strategy is FlonumImmutableVectorStrategy.singleton
    vec = run("(vector->immutable-vector (vector 1 2 3))")
    assert vec.strategy is FixnumImmutableVectorStrategy.singleton
    vec = run(r"(vector->immutable-vector (vector #\a #\b #\c))")
    assert vec.strategy is CharacterImmutableVectorStrategy.singleton
    vec = run(r"(vector->immutable-vector (vector #\a #\b #\c 1 1.0))")
    assert vec.strategy is ObjectImmutableVectorStrategy.singleton

def test_constant_strategy():
    vec = run("(make-vector 10 #f)")
    assert vec.strategy is ConstantVectorStrategy.singleton
    vec = run("(vector->immutable-vector (make-vector 10 #t))")
    assert vec.strategy is ConstantImmutableVectorStrategy.singleton
