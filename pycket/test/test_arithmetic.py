import pytest
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *
from pycket.test.testhelper import run_fix, run, run_top, run_std, run_flo
from pycket.error import SchemeException

skip = pytest.mark.skipif("True")

def test_mul_zero():
    run_fix("(* 0 1.2)", 0)
    run_fix("(* 1.2 0)", 0)

def test_quotient():
    run_fix("(quotient 0 1)", 0)
    run_fix("(quotient 0 -1)", 0)
    run_fix("(quotient 0 2)", 0)
    run_fix("(quotient 0 -2)", 0)
    run_fix("(quotient 0 3)", 0)
    run_fix("(quotient 1 1)", 1)
    run_fix("(quotient -1 1)", -1)
    run_fix("(quotient 1 -1)", -1)
    run_fix("(quotient -1 -1)", 1)
    run_fix("(quotient 1 2)", 0)
    run_fix("(quotient -1 2)", 0)
    run_fix("(quotient 1 -2)", 0)
    run_fix("(quotient -1 -2)", 0)
    run_fix("(quotient -1234 -10)", 123)
    run_fix("(quotient 1234 1234)", 1)
    big = 2 ** 70
    run_fix("(quotient %s %s)" % (big, big), 1)
    run_fix("(quotient %s %s)" % (-big, big), -1)
    run_fix("(quotient %s %s)" % (big, -big), -1)
    run_fix("(quotient %s %s)" % (-big, -big), 1)
    run_fix("(quotient %s %s)" % (big+1, big), 1)
    run_fix("(quotient %s %s)" % (-(big+1), big), -1)
    res = run(str(big / 2))
    run("(quotient %s 2)" % (big, ), res)

def test_div_fix():
    run_fix("(/ 6 3)", 2)
    with pytest.raises(SchemeException):
        run("(/ 1 2)", None) # XXX for now

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
    run("(negative? -1)", w_true)
    run("(negative?  0)", w_false)
    run("(negative?  1)", w_false)
    run("(negative? -1.0)", w_true)
    run("(negative?  0.0)", w_false)
    run("(negative?  1.0)", w_false)
    run("(negative?  -10000000000000000000000000001000000000000000000000000000)", w_true)
    run("(negative?   10000000000000000000000000001000000000000000000000000000)", w_false)
    run("(positive? -1)", w_false)
    run("(positive?  0)", w_false)
    run("(positive?  1)", w_true)
    run("(positive? -1.0)", w_false)
    run("(positive?  0.0)", w_false)
    run("(positive?  1.0)", w_true)
    run("(positive?  -10000000000000000000000000001000000000000000000000000000)", w_false)
    run("(positive?   10000000000000000000000000001000000000000000000000000000)", w_true)


def test_even_odd():
    run("(even? -1)", w_false)
    run("(even?  0)", w_true)
    run("(even?  1)", w_false)
    run("(even? -1.0)", w_false)
    run("(even?  0.0)", w_true)
    run("(even?  1.0)", w_false)
    run("(even?  -10000000000000000000000000001000000000000000000000000000)", w_true)
    run("(even?   10000000000000000000000000001000000000000000000000000000)", w_true)
    run("(even?  -10000000000000000000000000001000000000000000000000000001)", w_false)
    run("(even?   10000000000000000000000000001000000000000000000000000001)", w_false)
    run("(odd? -1)", w_true)
    run("(odd?  0)", w_false)
    run("(odd?  1)", w_true)
    run("(odd? -1.0)", w_true)
    run("(odd?  0.0)", w_false)
    run("(odd?  1.0)", w_true)
    run("(odd?  -10000000000000000000000000001000000000000000000000000000)", w_false)
    run("(odd?   10000000000000000000000000001000000000000000000000000000)", w_false)
    run("(odd?  -10000000000000000000000000001000000000000000000000000001)", w_true)
    run("(odd?   10000000000000000000000000001000000000000000000000000001)", w_true)

def test_zero():
    run("(zero? -1)", w_false)
    run("(zero?  0)", w_true)
    run("(zero?  1)", w_false)
    run("(zero? -1.0)", w_false)
    run("(zero?  0.0)", w_true)
    run("(zero?  1.0)", w_false)

def test_string_to_number(doctest):
    """
    ; not yet supported
    ;> (string->number "3.0+2.5i")
    ;3.0+2.5i
    > (string->number "hello")
    #f
    ;> (string->number "111" 7)
    ;57
    ;> (string->number "#b111" 7)
    ;7
    > (string->number "13")
    13
    > (string->number "-13")
    -13
    > (string->number "-1.3")
    -1.3
    > (string->number "1.3")
    1.3
    > (string->number "-10000000000000000000000000001000000000000000000000000000")
    -10000000000000000000000000001000000000000000000000000000
    > (string->number "10000000000000000000000000001000000000000000000000000000")
    10000000000000000000000000001000000000000000000000000000
    """
    assert doctest

@skip
def test_atan(doctest):
    """
    > (atan 0.5)
    0.4636476090008061
    > (atan 2 1)
    1.1071487177940904
    > (atan -2 -1)
    -2.0344439357957027
    ;> (atan 1.0+5.0i)
    ;1.530881333938778+0.19442614214700213i
    ;> (atan +inf.0 -inf.0)
    ;2.356194490192345
    """

def test_flonum_special(doctest):
    """
    ! (require '#%flfxnum)
    > (fl+ 1.0 2.0)
    3.0
    > (fl- 2.0 1.0)
    1.0
    > (fl* 2.0 0.5)
    1.0
    > (fl/ 2.0 0.5)
    4.0
    """

def test_fixnum_special(doctest):
    """
    ! (require '#%flfxnum)
    > (fx+ 1 2)
    3
    > (fx- 2 1)
    1
    > (fx* 2 5)
    10
    """
