import pytest
from pycket.expand import expand
from pycket.interpreter import *
from pycket.values import *
from pycket.impersonators import *
from pycket.vector import *
from pycket.prims import *
from pycket.test.testhelper import run_fix, run, run_mod, execute, check_equal

def test_vec():
    assert isinstance(run('(vector 1)'), W_Vector)
    run('(vector? \'#(0 (2 2 2 2) "Anna"))', w_true)
    run("(vector? '#())", w_true)
    run_fix("(let ([v (vector 1 2 3)]) (vector-length v))", 3)
    run("(let ([v (vector 1 2 3)]) (vector-set! v 0 0))", w_void)
    run_fix("(let ([v (vector 1 2 3)]) (vector-set! v 0 0) (vector-length v))", 3)
    run_fix("(let ([v (vector 1 2 3)]) (vector-set! v 0 0) (vector-ref v 0))", 0)

def test_vec_equal():
    run("(equal? (vector 1 2 3) (vector 1 2 3))", w_true)
    run("(equal? (vector 1 2 3) (vector 1 2))", w_false)
    run("(equal? (vector 1 2 3) (vector 1 2 5))", w_false)

def test_make_vector():
    run_fix("(let ([v (vector)]) (vector-length v))", 0)
    run_fix("(let ([v (make-vector 5)]) (vector-length v))", 5)
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

def test_vec_strategies_fixnum():
    vec = run("(vector 1 2 3)")
    assert isinstance(vec.strategy, FixnumVectorStrategy)
    vec = run("(make-vector 2)")
    assert isinstance(vec.strategy, FixnumVectorStrategy)

def test_vec_strategies_flonum():
    vec = run("(vector 1.0 2.1 3.2)")
    assert isinstance(vec.strategy, FlonumVectorStrategy)
    vec = run("(make-vector 2 1.2)")
    assert isinstance(vec.strategy, FlonumVectorStrategy)

def test_vec_strategies_fixnum_singleton():
    vec1 = run("(vector 1 2 3)")
    vec2 = run("(make-vector 2)")
    assert vec1.strategy is vec2.strategy

def test_vec_strategies_object():
    vec = run("(vector (cons 1 2) 2 3)")
    assert isinstance(vec.strategy, ObjectVectorStrategy)

def test_vec_strategies_stays_fixnum():
    vec = run("(let ([vec (make-vector 3)]) (vector-set! vec 1 5) vec)")
    assert isinstance(vec.strategy, FixnumVectorStrategy)

def test_vec_strategies_stays_flonum():
    vec = run("(let ([vec (make-vector 3 1.2)]) (vector-set! vec 1 5.5) vec)")
    assert isinstance(vec.strategy, FlonumVectorStrategy)

def test_vec_strategies_dehomogenize():
    vec = run('(let ([vec (vector 1 2 3)]) (vector-set! vec 1 "Anna") vec)')
    assert isinstance(vec.strategy, ObjectVectorStrategy)

def run_unsafe(e,v):
    run(e,v,extra="")
def run_fix_unsafe(e,v):
    run_fix(e,v,extra="")

def test_unsafe_impersonators():
    run_unsafe("(equal? 3 (unsafe-vector-length (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))))", w_true)
    run_unsafe("(equal? 3 (unsafe-vector-ref (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) 2))", w_true)
    run_fix_unsafe("(let ([v (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (unsafe-vector-set! v 0 0) (unsafe-vector-ref v 0))", 0)

def test_unsafe():
    run_unsafe("(equal? 3 (unsafe-vector-length (vector 1 2 3)))", w_true)
    run_unsafe("(equal? 3 (unsafe-vector*-length (vector 1 2 3)))", w_true)
    run_unsafe("(equal? 3 (unsafe-vector-ref (vector 1 2 3) 2))", w_true)
    run_unsafe("(equal? 3 (unsafe-vector*-ref (vector 1 2 3) 2))", w_true)
    run_fix_unsafe("(let ([v (vector 1 2 3)]) (unsafe-vector-set! v 0 0) (unsafe-vector-ref v 0))", 0)
    run_fix_unsafe("(let ([v (vector 1 2 3)]) (unsafe-vector*-set! v 0 0) (unsafe-vector*-ref v 0))", 0)


def test_vec_imp():
    assert isinstance(run('(impersonate-vector (vector 1) values values)'), W_ImpVector)
    run('(vector? (chaperone-vector \'#(0 (2 2 2 2) "Anna") values values))', w_true)
    run_fix("(let ([v (impersonate-vector (vector 1 2 3) values values)]) (vector-length v))", 3)
    run("(let ([v (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (vector-set! v 0 0))", w_void)
    run_fix("(let ([v (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (vector-set! v 0 0) (vector-length v))", 3)
    run_fix("(let ([v (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (vector-set! v 0 0) (vector-ref v 0))", 0)

def test_vec_equal_imp():
    run("(equal? (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) (vector 1 2 3))", w_true)
    run("(equal? (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) (vector 1 2))", w_false)
    run("(equal? (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) (vector 1 2 5))", w_false)

def test_make_vector_imp():
    run_fix("(let ([v (impersonate-vector (vector) (lambda (x y z) z) (lambda (x y z) z))]) (vector-length v))", 0)
    run_fix("(let ([v (impersonate-vector (make-vector 5) (lambda (x y z) z) (lambda (x y z) z))]) (vector-length v))", 5)

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
    > (vector-copy! v 4 #(y))
    > (vector-copy! v 0 v 3 4)
    > v
    '#(l p p l y)
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

