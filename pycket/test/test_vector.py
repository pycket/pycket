import pytest
from pycket.expand import expand
from pycket.interpreter import *
from pycket.values import *
from pycket.impersonators import *
from pycket.vector import *
from pycket.prims import *
from pycket.test.testhelper import run_fix, run, execute

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

def test_unsafe():
    run_unsafe("(equal? 3 (unsafe-vector-length (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))))", w_true)
    run_unsafe("(equal? 3 (unsafe-vector-ref (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) 2))", w_true)
    run_fix_unsafe("(let ([v (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (unsafe-vector-set! v 0 0) (unsafe-vector-ref v 0))", 0)

def test_unsafe_impersonators():
    run_unsafe("(equal? 3 (unsafe-vector-length (vector 1 2 3)))", w_true)
    run_unsafe("(equal? 3 (unsafe-vector-ref (vector 1 2 3) 2))", w_true)
    run_fix_unsafe("(let ([v (vector 1 2 3)]) (unsafe-vector-set! v 0 0) (unsafe-vector-ref v 0))", 0)


def test_vec_imp():
    assert isinstance(run('(impersonate-vector (vector 1) values values)'), W_ImpVector)
    run('(vector? (impersonate-vector \'#(0 (2 2 2 2) "Anna") values values))', w_true)
    run_fix("(let ([v (impersonate-vector (vector 1 2 3) values values)]) (vector-length v))", 3)
    run("(let ([v (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (vector-set! v 0 0))", w_void)
    run_fix("(let ([v (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (vector-set! v 0 0) (vector-length v))", 3)
    run_fix("(let ([v (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z))]) (vector-set! v 0 0) (vector-ref v 0))", 0)

def test_vec_equal_imp():
# FIXME: can't work yet
#    run("(equal? (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) (vector 1 2 3))", w_true)
    run("(equal? (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) (vector 1 2))", w_false)
    run("(equal? (impersonate-vector (vector 1 2 3) (lambda (x y z) z) (lambda (x y z) z)) (vector 1 2 5))", w_false)

def test_make_vector_imp():
    run_fix("(let ([v (impersonate-vector (vector) (lambda (x y z) z) (lambda (x y z) z))]) (vector-length v))", 0)
    run_fix("(let ([v (impersonate-vector (make-vector 5) (lambda (x y z) z) (lambda (x y z) z))]) (vector-length v))", 5)

def test_bug_symbol_in_vector():
    # FIXME somebody who knows expand
    run("#('a)")
