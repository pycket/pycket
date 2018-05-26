import math
import pytest
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *
from pycket.test.testhelper import run_expr, run_expr_result #run_fix, run, run_top, run_std, run_flo
from pycket.error import SchemeException

def test_flonum_tostring():
    from rpython.rtyper.test.test_llinterp import interpret
    import math
    def float_tostring(x):
        print W_Flonum(x).tostring()
        return W_Flonum(x).tostring() == s
    s = str(math.pi)
    res = interpret(float_tostring, [math.pi])
    assert res

def test_mul_zero():
    run_expr("(* 0 1.2)", 0)
    run_expr("(* 1.2 0)", 0)

def test_quotient():
    run_expr("(quotient 0 1)", 0)
    run_expr("(quotient 0 -1)", 0)
    run_expr("(quotient 0 2)", 0)
    run_expr("(quotient 0 -2)", 0)
    run_expr("(quotient 0 3)", 0)
    run_expr("(quotient 1 1)", 1)
    run_expr("(quotient -1 1)", -1)
    run_expr("(quotient 1 -1)", -1)
    run_expr("(quotient -1 -1)", 1)
    run_expr("(quotient 1 2)", 0)
    run_expr("(quotient -1 2)", 0)
    run_expr("(quotient 1 -2)", 0)
    run_expr("(quotient -1 -2)", 0)
    run_expr("(quotient -1234 -10)", 123)
    run_expr("(quotient 1234 1234)", 1)
    big = 2 ** 70
    run_expr("(quotient %s %s)" % (big, big), 1)
    run_expr("(quotient %s %s)" % (-big, big), -1)
    run_expr("(quotient %s %s)" % (big, -big), -1)
    run_expr("(quotient %s %s)" % (-big, -big), 1)
    run_expr("(quotient %s %s)" % (big+1, big), 1)
    run_expr("(quotient %s %s)" % (-(big+1), big), -1)
    res = run_expr_result(str(big / 2))
    run_expr("(quotient %s 2)" % (big, ), res, equal_huh=True)

    res = run_expr_result("(quotient 8.0 2.0)")
    assert isinstance(res, W_Flonum) and res.value == 4.0
    res = run_expr_result("(quotient 1.0 2.0)")
    assert isinstance(res, W_Flonum) and res.value == 0.0

def test_remainder(doctest):
    """
    > (remainder 0 1)
    0
    > (remainder 0 -1)
    0
    > (remainder 0 2)
    0
    > (remainder 0 -2)
    0
    > (remainder 1 1)
    0
    > (remainder -1 1)
    0
    > (remainder 1 -1)
    0
    > (remainder 2 1)
    0
    > (remainder 2 -1)
    0
    > (remainder 4 3)
    1
    > (remainder 4 -3)
    1
    > (remainder -4 3)
    -1
    > (remainder 10 3)
    1
    > (remainder -10.0 3)
    -1.0
    > (remainder 10.0 -3)
    1.0
    > (remainder -10 -3)
    -1
    > (remainder 11111111111111111111111111111111111111 3333333333333333333333333333333)
    1111111111111111111111112222222
    > (remainder 11111111111111111111111111111111111111 -3333333333333333333333333333333)
    1111111111111111111111112222222
    > (remainder -11111111111111111111111111111111111111 3333333333333333333333333333333)
    -1111111111111111111111112222222
    > (remainder -11111111111111111111111111111111111111 -3333333333333333333333333333333)
    -1111111111111111111111112222222
    """
