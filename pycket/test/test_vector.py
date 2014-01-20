import pytest
from pycket.expand import expand
from pycket.interpreter import *
from pycket.values import *
from pycket.vector import *
from pycket.prims import *
from pycket.test.test_basic import run_fix, run, execute

def test_vec():
    assert isinstance(execute('(vector 1)'), W_Vector)
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
    vec = execute('(make-vector 5)')
    for i in range(vec.length()):
        assert vec.ref(i).value == 0

def test_vec_strategies_empty():
    vec = execute("(vector)")
    print "First size: %s" % vec.length()
    assert isinstance(vec.strategy, ObjectVectorStrategy)
    vec = execute("(make-vector 0)")
    print "Second size: %s" % vec.length()
    assert isinstance(vec.strategy, ObjectVectorStrategy)

def test_vec_strategies_fixnum():
    vec = execute("(vector 1 2 3)")
    assert isinstance(vec.strategy, FixnumVectorStrategy)
    vec = execute("(make-vector 2)")
    assert isinstance(vec.strategy, FixnumVectorStrategy)

def test_vec_strategies_object():
    vec = execute("(vector (cons 1 2) 2 3)")
    assert isinstance(vec.strategy, ObjectVectorStrategy)

def test_vec_strategies_stays_fixnum():
    vec = execute("(let ([vec (make-vector 3)]) (vector-set! vec 1 5) vec)")
    assert isinstance(vec.strategy, FixnumVectorStrategy)

def test_vec_strategies_dehomogenize():
    vec = execute('(let ([vec (vector 1 2 3)]) (vector-set! vec 1 "Anna") vec)')
    assert isinstance(vec.strategy, ObjectVectorStrategy)
