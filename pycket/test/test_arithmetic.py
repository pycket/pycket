import pytest
from pycket.expand import expand
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *
from pycket.test.test_basic import run_fix, run, run_top

def run_std(c, v):
    return run_top(c, v, stdlib=True)

def test_mul_zero():
    run_fix("(* 0 1.2)", 0)
    run_fix("(* 1.2 0)", 0)

def test_lt():
    run("(< 0 1)", w_true)
    run("(< 0 1000000000000000000000000000)", w_true)
    run("(< 10000000000000000000000000001000000000000000000000000000 0 )", w_false)

def test_lt_fixnum_flonum():
    run("(< 0 1.0)", w_true)
    run("(< 0 1000000000000000000000000000.0)", w_true)
    run("(< 10000000000000000000000000001000000000000000000000000000 0.0 )", w_false)
    run("(< 0.0 1)", w_true)
    run("(< 0.0 1000000000000000000000000000)", w_true)
    run("(< 10000000000000000000000000001000000000000000000000000000.0 0 )", w_false)

def test_lt_fixnum_bignum():
    run("(< (expt 10 100) 1)", w_false)
    run("(< 1 (expt 10 100))", w_true)

def test_lt_flonum_bignum():
    run("(< (expt 10 100) 1.0)", w_false)
    run("(< 1.0 (expt 10 100))", w_true)

def test_neg_pos():
    run_std("(negative? -1)", w_true)
    run_std("(negative?  0)", w_false)
    run_std("(negative?  1)", w_false)
    run_std("(negative? -1.0)", w_true)
    run_std("(negative?  0.0)", w_false)
    run_std("(negative?  1.0)", w_false)
    run_std("(negative?  -10000000000000000000000000001000000000000000000000000000)", w_true)
    run_std("(negative?   10000000000000000000000000001000000000000000000000000000)", w_false)
    run_std("(positive? -1)", w_false)
    run_std("(positive?  0)", w_false)
    run_std("(positive?  1)", w_true)
    run_std("(positive? -1.0)", w_false)
    run_std("(positive?  0.0)", w_false)
    run_std("(positive?  1.0)", w_true)
    run_std("(positive?  -10000000000000000000000000001000000000000000000000000000)", w_false)
    run_std("(positive?   10000000000000000000000000001000000000000000000000000000)", w_true)
