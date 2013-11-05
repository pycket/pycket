import pytest
from pycket.expand import expand
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *
from pycket.test.test_basic import run_fix, run

def test_vec():
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
