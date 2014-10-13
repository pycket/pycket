import pytest
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *
from pycket.test.testhelper import run_fix, run, run_top, run_std, run_flo
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

@pytest.mark.xfail
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
    """

def test_modulo(doctest):
    """
    > (modulo 10 3)
    1
    > (modulo -10.0 3)
    2.0
    > (modulo 10.0 -3)
    -2.0
    > (modulo -10 -3)
    -1
    > (modulo 1111111111111111111111111111111111111111111111111111111111111111111111111111 -2222222222222222222222222222222222222222222)
    -2222222222111111111111111111111111111111111
    > (modulo -1111111111111111111111111111111111111111111111111111111111111111111111111111 2222222222222222222222222222222222222222222)
    2222222222111111111111111111111111111111111
    > (modulo -1111111111111111111111111111111111111111111111111111111111111111111111111111 -2222222222222222222222222222222222222222222)
    -111111111111111111111111111111111
    """

def test_div_fix():
    run_fix("(/ 6 3)", 2)
    x = run("(/ 1 2)")
    assert x.tostring() == "1/2"

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

def test_comparison_doctest(doctest):
    """
    > (= 5+5i 5+5.0i)
    #t
    > (= 5+5i 5+5.0i 5.0+5i 5.0+5.0i)
    #t
    > (= 2/3 2/3)
    #t
    > (= 5+5i 5+3i)
    #f
    > (= 2/3 2/7)
    #f
    > (< -1/2 -1/3 2/3 11/10)
    #t
    > (< -2/3 1)
    #t
    > (< 2/3 1/3)
    #f
    E (< 1)
    E (< )
    E (< #f #t)
    """

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

@pytest.mark.xfail
def test_atan(doctest):
    """
    > (atan 0.5)
    0.4636476090008061
    > (atan 2 1)
    1.1071487177940904
    > (atan -2 -1)
    -2.0344439357957027
    > (atan 1.0+5.0i)
    1.530881333938778+0.19442614214700213i
    > (atan +inf.0 -inf.0)
    2.356194490192345
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
    > (flmin 1.0 2.0)
    1.0
    > (flmin 2.0 1.0)
    1.0
    > (flmax 1.0 2.0)
    2.0
    > (flmax 2.0 1.0)
    2.0
    > (fl> 2.5 1.5)
    #t
    > (fl>= 2.5 1.5)
    #t
    > (fl>= 1.5 1.5)
    #t
    > (fl>= -1.5 1.5)
    #f
    > (fl<= -1.5 1.5)
    #t
    > (fl<= -10.5 -10.5)
    #t
    > (fl< -10.0 -10.0)
    #f
    > (fl= -10.0 -10.0)
    #t
    E (fl= -10 -10.0)
    """

def test_fixnum_special(doctest):
    """
    ! (require '#%flfxnum)
    > (fx+ 1 2)
    3
    E (fx+ 1 1.2)
    > (fx- 2 1)
    1
    > (fx* 2 5)
    10
    > (fxmin 1 2)
    1
    > (fxmin 2 1)
    1
    > (fxmax 1 2)
    2
    > (fxmax 2 1)
    2
    > (fx> 2 1)
    #t
    > (fx>= 2 1)
    #t
    > (fx>= 1 1)
    #t
    > (fx>= -1 1)
    #f
    > (fx<= -1 1)
    #t
    > (fx<= -10 -10)
    #t
    > (fx< -10 -10)
    #f
    > (fx= -10 -10)
    #t
    E (fx= -10 -10.0)
    > (fxand 2 3)
    2
    > (fxlshift 10 10)
    10240
    E (fxlshift 10 63)
    E (fxlshift 10 100)
    > (fxrshift 1 20)
    0
    > (fxrshift 20 1)
    10
    > (fxrshift -20 1)
    -10
    """

def test_all_comparators(doctest):
    """
    ; http://docs.racket-lang.org/reference/generic-numbers.html
    > (= 1 1.0)
    #t
    > (= 1 2)
    #f
    > (= 2+3i 2+3i 2+3i)
    #t
    > (< 1 1)
    #f
    > (< 1 2 3)
    #t
    > (< 1 +inf.0)
    #t
    > (< 1 +nan.0)
    #f
    > (<= 1 1)
    #t
    > (<= 1 2 1)
    #f
    > (> 1 1)
    #f
    > (> 3 2 1)
    #t
    > (> +inf.0 1)
    #t
    > (> +nan.0 1)
    #f
    > (>= 1 1)
    #t
    > (>= 1 2 1)
    #f
    """

@pytest.mark.xfail
def test_edge_cases(doctest):
    """
    > (* 0.0 1)
    0.0
    > (* 0 0.1)
    0
    > (* 0.0 0)
    0
    > (+ -0.1 0.1)
    0.0
    > (complex? (+ 1+1i 1-1i))
    #t
    > (complex? 2)
    #t
    > (+ 1+0.5i 1-0.5i)
    2.0+0.0i
    > (real? (+ 1+0.5i 1-0.5i))
    #f
    > (integer? (+ 1+1i 1-1i))
    #t
    > (+ 1+1i 1-1i)
    2
    """

def test_rational(doctest):
    """
    > (/ 1 2)
    1/2
    > (+ 1/2 1/3)
    5/6
    > (+ 1/2 1)
    3/2
    > (+ 1/2 0.5)
    1.0
    > (- 4/5 -7/9)
    71/45
    > (- 1/2 2)
    -3/2
    > (- 1/2 0.0)
    0.5
    > (/ 2/3 3/2)
    4/9
    > (/ -2/3 -5)
    2/15
    > (/ 0.5 -1/4)
    -2.0
    > (* 2/3 3/2)
    1
    > (* 2 3/2)
    3
    > (* 3/2 5)
    15/2
    > (* 1/2 2.0)
    1.0
    > (+ 1/4 1/4)
    1/2
    > (sub1 5/3)
    2/3
    ; bignum to rational
    > (/ 12323111111111111111111111111111111111111112222222222222 232321122)
    2053851851851851851851851851851851851851852037037037037/38720187
    """

def test_gcd():
    from pycket.arithmetic import gcd
    from rpython.rlib.rbigint import rbigint
    def gcd_long(a, b):
        return gcd(rbigint.fromlong(a), rbigint.fromlong(b)).tolong()

    for a, b, r in [(5, 0, 5),
                    (2**1000, 0, 2**1000),
                    (4, 2, 2),
                    (3*3*5*7*11*2**10, 2**7*3*7*11*13, 2**7*3*7*11)]:
        assert gcd_long(a, b) == r
        assert gcd_long(b, a) == r
        if b:
            assert gcd_long(a, -b) == -r
            assert gcd_long(-a, -b) == -r
            assert gcd_long(-a, b) == r
        else:
            assert gcd_long(-a, b) == -r
        if a:
            assert gcd_long(b, -a) == -r
            assert gcd_long(-b, -a) == -r
            assert gcd_long(-b, a) == r
        else:
            assert gcd_long(-b, a) == -r

def test_sub1(doctest):
    """
    > (sub1 1)
    0
    > (sub1 -11111111111111111111111111111111112)
    -11111111111111111111111111111111113
    > (sub1 1.4)
    0.3999999999999999
    > (sub1 1.5)
    0.5
    > (sub1 1+1i)
    0+1i
    > (sub1 1/2)
    -1/2
    """
    w_x = W_Fixnum(-sys.maxint-1).arith_sub1()
    assert isinstance(w_x, W_Bignum)

def test_round(doctest):
    """
    > (round 0.1)
    0.0
    > (round 0.0)
    0.0
    > (round 0.5)
    0.0
    > (round 0.51)
    1.0
    > (round -0.5)
    -0.0
    > (round -0.5001)
    -1.0
    > (round 1)
    1
    > (round 111111111111111111111111111111111111111111111111)
    111111111111111111111111111111111111111111111111
    > (round 1/2)
    0
    > (round 3/2)
    2
    > (round 11/20)
    1
    > (round -11/20)
    -1
    > (round -1/2)
    0
    > (round -5/4)
    -1
    > (round 5/4)
    1
    > (round 111111111111111111111111111111111111111/2)
    55555555555555555555555555555555555556
    """

def test_max(doctest):
    """
    > (max 1 1.1)
    1.1
    > (max 1 0.2)
    1.0
    > (max 111111111111111111111111111111111111111 5)
    111111111111111111111111111111111111111
    > (max 111111111111111111111111111111111111111111111111111111111111 0.2)
    1.1111111111111112e+59
    > (max 1 3 2)
    3
    > (max 1 3 2.0)
    3.0
    > (max 1 1.1 0)
    1.1
    > (max 1 0.2 -5)
    1.0
    > (max 111111111111111111111111111111111111111 5 6)
    111111111111111111111111111111111111111
    > (max 111111111111111111111111111111111111111111111111111111111111 0.2 1023)
    1.1111111111111112e+59
    > (max 1 3 2 -6)
    3
    > (max 1 3 -17 2.0)
    3.0
    """

def test_bitwise(doctest):
    """
    > (bitwise-ior 1 2)
    3
    > (bitwise-ior -32 1)
    -31
    > (bitwise-ior)
    0
    > (bitwise-and 1 2)
    0
    > (bitwise-and -32 -1)
    -32
    > (bitwise-and)
    -1
    > (bitwise-xor 1 5)
    4
    > (bitwise-xor -32 -1)
    31
    > (bitwise-xor)
    0
    > (bitwise-not 1)
    -2
    > (bitwise-not -1111111111111111111111111111111111111111111114243232)
    1111111111111111111111111111111111111111111114243231
    > (bitwise-bit-set? 5 0)
    #t
    > (bitwise-bit-set? 5 2)
    #t
    > (bitwise-bit-set? -5 (expt 2 700))
    #t
    > (bitwise-bit-set? 5 (expt 2 700))
    #f
    > (bitwise-bit-set? (expt 2 100) 100)
    #t
    > (bitwise-bit-set? (expt 2 100) 101)
    #f
    E (bitwise-bit-set? 2 -5)
    """

def test_exact_to_inexact(doctest):
    """
    > (exact->inexact 1)
    1.0
    > (exact->inexact 1/2)
    0.5
    > (exact->inexact 0.5)
    0.5
    > (exact->inexact 1+2i)
    1.0+2.0i
    > (exact->inexact 102222222222222222222222222222222222222222222222123123)
    1.0222222222222222e+53
    """

def test_inexact_to_exact(doctest):
    """
    > (inexact->exact 1.0)
    1
    > (inexact->exact 0.5)
    1/2
    > (inexact->exact 1/2)
    1/2
    > (inexact->exact 1.0+2.0i)
    1+2i
    > (inexact->exact 1.0222222222222222e+53)
    102222222222222223892324523663483522756187192341561344
    """


def test_flonum_unsafe(doctest):
    """
    ! (require '#%flfxnum '#%unsafe)
    > (unsafe-fl+ 1.0 2.0)
    3.0
    > (unsafe-fl- 2.0 1.0)
    1.0
    > (unsafe-fl* 2.0 0.5)
    1.0
    > (unsafe-fl/ 2.0 0.5)
    4.0
    """

def test_fixnum_unsafe(doctest):
    """
    ! (require '#%flfxnum '#%unsafe)
    > (unsafe-fx+ 10 20)
    30
    > (unsafe-fx- 20 10)
    10
    > (unsafe-fx* 20 5)
    100
    > (unsafe-fxmin 10 20)
    10
    > (unsafe-fxmin 20 10)
    10
    > (unsafe-fxmax 10 20)
    20
    > (unsafe-fxmax 20 10)
    20
    > (unsafe-fxmodulo -100 30)
    20
    > (unsafe-fxmodulo 100 -30)
    -20
    > (unsafe-fxlshift 1 10)
    1024
    ; implementation defined, but should terminate quickly
    > (unsafe-fxlshift 1 10000000)
    """

def test_exp(doctest):
    """
    > (exp 1)
    2.718281828459045
    > (exp 0)
    1
    > (exp 2+3i)
    -7.315110094901103+1.0427436562359045i
    > (exp 2.0+3i)
    -7.315110094901103+1.0427436562359045i
    """

def test_shift(doctest):
    """
    > (arithmetic-shift 1 10)
    1024
    > (arithmetic-shift 255 -3)
    31
    > (arithmetic-shift 10 1000)
    107150860718626732094842504906000181056140481170553360744375038837035105112493612249319837881569585812759467291755314682518714528569231404359845775746985748039345677748242309854210746050623711418779541821530464749835819412673987675591655439460770629145711964776865421676604298316526243868372056680693760
    > (arithmetic-shift 107150860718626732094842504906000181056140481170553360744375038837035105112493612249319837881569585812759467291755314682518714528569231404359845775746985748039345677748242309854210746050623711418779541821530464749835819412673987675591655439460770629145711964776865421676604298316526243868372056680693760 -1000)
    10
    """

def test_ceiling(doctest):
    """
    > (ceiling 17/4)
    5
    > (ceiling -17/4)
    -4
    > (ceiling 2.5)
    3.0
    > (ceiling -2.5)
    -2.0
    """

def test_floor(doctest):
    """
    > (floor 17/4)
    4
    > (floor -17/4)
    -5
    > (floor 2.5)
    2.0
    > (floor -2.5)
    -3.0
    """


def test_truncate(doctest):
    """
    > (truncate 17/4)
    4
    > (truncate -17/4)
    -4
    > (truncate 2.5)
    2.0
    > (truncate -2.5)
    -2.0
    > (truncate +inf.0)
    +inf.0
    """

@pytest.mark.xfail
def test_expt(doctest):
    """
    > (expt 2 3)
    8
    > (expt 4 0.5)
    2.0
    > (expt 2 -5)
    1/32
    > (expt 2. -5)
    0.03125
    """
