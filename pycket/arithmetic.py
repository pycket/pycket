from pycket                    import values
from pycket.error              import SchemeException
from rpython.rlib              import rarithmetic, jit, objectmodel
from rpython.rlib.rarithmetic  import r_int, r_uint, intmask, int_c_div
from rpython.rlib.objectmodel  import specialize
from rpython.rlib.rbigint      import rbigint, NULLRBIGINT, ONERBIGINT

import math
import sys

def int_floordiv_ovf(x, y):
    # JIT: intentionally not short-circuited to produce only one guard
    # and to remove the check fully if one of the arguments is known
    if (x == -sys.maxint - 1) & (y == -1):
        raise OverflowError("integer division")
    return int_c_div(x, y)

@jit.elidable
def count_trailing_zeros(u):
    """
    Computes the number of lower order zero bits in the given rbigint.
    This information is used in the gcd algorithm which only works on odd integers.
    This is a nasty bit of hackery, as we inspect the internal representation of the
    rbigint (rather than using their nice interface). This method is much faster
    than performing iterated shifting, however.
    """
    from rpython.rlib.rbigint import UDIGIT_TYPE, SHIFT

    shift = 0
    bit = 1
    digit = 0
    mask = UDIGIT_TYPE(0x1)

    while digit < u.numdigits() and (u.udigit(digit) & mask) == 0:
        shift += 1
        if bit == SHIFT:
            bit = 1
            digit += 1
            mask = UDIGIT_TYPE(0x1)
        else:
            bit += 1
            mask = mask << 1

    return shift

@jit.elidable
def gcd1(u, v):
    """
    Single word variant of the gcd function. Expects u and v to be positive.
    """

    assert u > 0
    assert v > 0

    shift = 0
    while not (u & 1) and not (v & 1):
        shift += 1
        u >>= 1
        v >>= 1
    while not (u & 1):
        u >>= 1

    while True:
        while not (v & 0x1):
            v >>= 1

        if u > v:
            u, v = v, u

        v -= u
        if not v:
            break

    result = u << shift
    return result

if sys.maxint > 2147483647:
    SQRT_BIT_MAX = 31
else:
    SQRT_BIT_MAX = 15

@jit.elidable
def fixnum_sqrt(_n):
    # Taken from Racket implementation:
    # https://github.com/racket/racket/blob/2a88662d01599d9c284d2fbd1f7b987d57658797/racket/src/racket/src/bignum.c#L1652
    n      = r_uint(_n)
    root   = r_uint(0)
    square = r_uint(0)

    for i in range(SQRT_BIT_MAX, -1, -1):
        try_root   = root | (r_uint(0x1) << i)
        try_square = try_root * try_root
        if try_square <= n:
            root = try_root
            square = try_square

    return intmask(root), intmask(n - square)

def imaginary(val):
    return values.W_Complex(values.W_Fixnum.ZERO, val)

class __extend__(values.W_Object):
    # default implementations that all raise
    def arith_unaryadd(self):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_add(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_sub(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_sub1(self):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_mul(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_div(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_mod(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_quotient(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_remainder(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_pow(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_shl(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_shr(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_min(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_max(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_or(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_and(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_xor(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_eq(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_ne(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_lt(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_le(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_gt(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_ge(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_gcd(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_lcm(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

    def arith_exp(self):
        raise SchemeException("not a number: %s" % self.tostring())

    def same_numeric_class_reversed(self, other):
        raise SchemeException("not a number: %s" % self.tostring())

class __extend__(values.W_Number):
    def arith_unaryadd(self):
        return self

    def arith_add(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_add_same(other)

    def arith_sub(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_sub_same(other)

    def arith_sub1(self):
        return self.arith_sub(values.W_Fixnum.ONE)

    def arith_mul(self, other):
        if isinstance(self, values.W_Fixnum) and not self.value:
            return self
        if isinstance(other, values.W_Fixnum) and not other.value:
            return other
        self, other = self.same_numeric_class(other)
        return self.arith_mul_same(other)

    def arith_div(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_div_same(other)

    def arith_mod(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_mod_same(other)

    def arith_quotient(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_quotient_same(other)

    def arith_remainder(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_remainder_same(other)

    def arith_pow(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_pow_same(other)

    def arith_shl(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_shl_same(other)

    def arith_shr(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_shr_same(other)

    def arith_min(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_min_same(other)

    def arith_max(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_max_same(other)

    def arith_or(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_or_same(other)

    def arith_and(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_and_same(other)

    def arith_xor(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_xor_same(other)

    def arith_eq(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_eq_same(other)

    def arith_ne(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_ne_same(other)

    def arith_lt(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_lt_same(other)

    def arith_le(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_le_same(other)

    def arith_gt(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_gt_same(other)

    def arith_ge(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_ge_same(other)

    def arith_gcd(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_gcd_same(other)

    def arith_lcm(self, other):
        return self.arith_mul(other).arith_div(self.arith_gcd(other))

    # default implementations

    def arith_ne_same(self, other):
        return not self.arith_eq_same(other)

    def arith_le_same(self, other):
        return not other.arith_lt_same(self)

    def arith_gt_same(self, other):
        return other.arith_lt_same(self)

    def arith_ge_same(self, other):
        return other.arith_le_same(self)

    def arith_min_same(self, other):
        return self if self.arith_lt_same(other) else other

    def arith_max_same(self, other):
        return other if self.arith_lt_same(other) else self

    def arith_exp(self):
        self = self.arith_exact_inexact()
        assert isinstance(self, values.W_Flonum)
        return self.arith_exp()

    def same_numeric_class_reversed(self, other):
        self, other = self.same_numeric_class(other)
        return other, self

    def isinteger(self):
        return False

class __extend__(values.W_Integer):

    def arith_round(self):
        return self

    def arith_truncate(self):
        return self

    def arith_floor(self):
        return self

    def arith_ceiling(self):
        return self

    def arith_inexact_exact(self):
        return self

    def isinteger(self):
        return True

class __extend__(values.W_Fixnum):

    def same_numeric_class(self, other):
        if isinstance(other, values.W_Fixnum):
            return self, other
        return other.same_numeric_class_reversed(self)

    def arith_add_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        try:
            res = rarithmetic.ovfcheck(self.value + other.value)
        except OverflowError:
            return values.W_Bignum(rbigint.fromint(self.value)).arith_add(other)
        return values.W_Fixnum(res)

    def arith_sub_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        try:
            res = rarithmetic.ovfcheck(self.value - other.value)
        except OverflowError:
            return values.W_Bignum(rbigint.fromint(self.value)).arith_sub(other)
        return values.W_Fixnum(res)

    def arith_unarysub(self):
        try:
            res = rarithmetic.ovfcheck(-self.value)
        except OverflowError:
            return values.W_Bignum(rbigint.fromint(self.value).neg())
        return values.W_Fixnum(res)

    def arith_mul_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        if not self.value: return self
        if not other.value: return other
        try:
            res = rarithmetic.ovfcheck(self.value * other.value)
        except OverflowError:
            return self.arith_mul(values.W_Bignum(rbigint.fromint(other.value)))
        return values.W_Fixnum(res)

    def arith_div_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        if other.value == 0:
            raise SchemeException("zero_divisor")
        try:
            res = rarithmetic.ovfcheck(self.value / other.value)
        except OverflowError:
            return self.arith_div(values.W_Bignum(rbigint.fromint(other.value)))
        if res * other.value == self.value:
            return values.W_Fixnum(res)
        return values.W_Rational.fromint(
            self.value, other.value, need_to_check=False)

    def arith_mod_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        if other.value == 0:
            raise Exception("zero_divisor")
        try:
            res = rarithmetic.ovfcheck(self.value % other.value)
        except OverflowError:
            return self.arith_mod(values.W_Bignum(rbigint.fromint(other.value)))
        return values.W_Fixnum(res)

    def arith_remainder_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        if other.value == 0:
            raise Exception("zero_divisor")
        a = abs(self.value)
        b = abs(other.value)
        try:
            res = rarithmetic.ovfcheck(a % b)
            if self.value < 0:
                res = rarithmetic.ovfcheck(-res)
        except OverflowError:
            res = a % b
            res1 = -res if self.value < 0 else res
            return self.arith_mod(values.W_Bignum(rbigint.fromint(res1)))
        return values.W_Fixnum(res)

    def arith_quotient_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        x = self.value
        y = other.value
        if y:
            try:
                res = int_floordiv_ovf(x, y) # misnomer, should be int_truncdiv or so
            except OverflowError:
                return self.arith_quotient(values.W_Bignum(rbigint.fromint(other.value)))
        else:
            raise SchemeException("zero_divisor")
        return values.W_Fixnum(res)

    def arith_pow_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        # XXX nonsense
        try:
            if other.value < 0:
                return values.W_Rational.fromfloat(math.pow(self.value, other.value))
            res = rarithmetic.ovfcheck_float_to_int(math.pow(self.value, other.value))
            return values.W_Fixnum(res)
        except OverflowError:
            return self.arith_pow(values.W_Bignum(rbigint.fromint(other.value)))


    def arith_shr_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return values.W_Fixnum(self.value >> other.value)

    def arith_shl_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        if other.value >= r_int.BITS:
            if not self.value:
                return values.W_Fixnum.ZERO
            val = rbigint.fromint(self.value).lshift(other.value)
            return values.W_Integer.frombigint(val)
        try:
            res = rarithmetic.ovfcheck(self.value << other.value)
        except OverflowError:
            return self.arith_shl(values.W_Bignum(rbigint.fromint(other.value)))
        return values.W_Fixnum(res)

    def arith_or_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return values.W_Fixnum(self.value | other.value)

    def arith_and_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return values.W_Fixnum(self.value & other.value)

    def arith_xor_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return values.W_Fixnum(self.value ^ other.value)

    def arith_not(self):
        return values.W_Fixnum(~self.value)

    def arith_gcd_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        u = self.value
        v = other.value

        if not u:
            return other
        if not v:
            return self

        if u < 0 and v < 0:
            sign = -1
        else:
            sign = 1
        u = abs(u)
        v = abs(v)

        result = gcd1(u, v)
        return values.W_Fixnum(sign * result)

    # ------------------ abs ------------------
    def arith_abs(self):
        if self.value >= 0:
            return self
        return values.W_Fixnum.ZERO.arith_sub(self)

    # ------------------ trigonometry ------------------

    def arith_sqrt(self):
        n = abs(self.value)
        root, rem = fixnum_sqrt(n)
        if rem == 0:
            result = values.W_Fixnum(root)
        else:
            result = values.W_Flonum(math.sqrt(float(n)))

        if self.value < 0:
            return imaginary(result)

        return result

    def arith_log(self):
        return values.W_Flonum(math.log(self.value))
    def arith_sin(self):
        return values.W_Flonum(math.sin(self.value))
    def arith_cos(self):
        return values.W_Flonum(math.cos(self.value))
    def arith_tan(self):
        return values.W_Flonum(math.tan(self.value))
    def arith_sinh(self):
        return values.W_Flonum(math.sinh(self.value))
    def arith_cosh(self):
        return values.W_Flonum(math.cosh(self.value))
    def arith_tanh(self):
        return values.W_Flonum(math.tanh(self.value))
    def arith_asin(self):
        return values.W_Flonum(math.asin(self.value))
    def arith_acos(self):
        return values.W_Flonum(math.acos(self.value))
    def arith_atan(self):
        return values.W_Flonum(math.atan(self.value))

    # ------------------ miscellanous ------------------

    def arith_float_fractional_part(self):
        return values.W_Fixnum.ZERO

    def arith_float_integer_part(self):
        return self

    def arith_exact_inexact(self):
        return values.W_Flonum(float(self.value))

    def arith_zerop(self):
        return values.W_Bool.make(self.value == 0)

    def arith_negativep(self):
        return values.W_Bool.make(self.value < 0)

    def arith_positivep(self):
        return values.W_Bool.make(self.value > 0)

    def arith_evenp(self):
        return values.W_Bool.make((self.value % 2) == 0)

    def arith_oddp(self):
        return values.W_Bool.make((self.value % 2) != 0)

    def arith_exp(self):
        if self.value == 0:
            return values.W_Fixnum.ONE
        return values.W_Flonum(math.exp(self.value))

    # ------------------ comparisons ------------------

    def arith_eq_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return self.value == other.value

    def arith_ne_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return self.value != other.value

    def arith_lt_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return self.value < other.value

    def arith_le_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return self.value <= other.value

    def arith_gt_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return self.value > other.value

    def arith_ge_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return self.value >= other.value

class __extend__(values.W_Flonum):
    def same_numeric_class(self, other):
        if isinstance(other, values.W_Fixnum):
            return self, values.W_Flonum(float(other.value))
        if isinstance(other, values.W_Flonum):
            return self, other
        if isinstance(other, values.W_Bignum):
            return self, other.toflonum()
        if isinstance(other, values.W_Rational):
            return self, other.arith_exact_inexact()
        return other.same_numeric_class_reversed(self)

    def arith_add_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return values.W_Flonum(self.value + other.value)

    def arith_sub_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return values.W_Flonum(self.value - other.value)

    def arith_unarysub(self):
        return values.W_Flonum(-self.value)

    def arith_mul_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return values.W_Flonum(self.value * other.value)

    def arith_div_same(self, other):
        assert isinstance(other, values.W_Flonum)
        if other.value == 0.0:
            raise SchemeException("zero_divisor")
        return values.W_Flonum(self.value / other.value)

    def arith_quotient_same(self, other):
        assert isinstance(other, values.W_Flonum)
        v1 = self.value
        v2 = other.value
        if math.floor(v1) != v1 or math.floor(v2) != v2:
            raise SchemeException("quotient: expected integer")
        val = math.floor(v1 / v2)
        return values.W_Flonum(val)

    def arith_mod_same(self, other):
        assert isinstance(other, values.W_Flonum)
        if other.value == 0.0:
            raise Exception("zero_divisor")
        x, y = self.value, other.value
        res = math.fmod(x, y)
        # ensure the remainder has the same sign as the denominator
        if (y < 0.0) != (res < 0.0):
            res += y
        return values.W_Flonum(res)

    def arith_remainder_same(self, other):
        assert isinstance(other, values.W_Flonum)
        if other.value == 0.0:
            raise Exception("zero_divisor")
        x, y = self.value, other.value
        res = math.fmod(x, y)
        return values.W_Flonum(res)

    def arith_pow_same(self, other):
        assert isinstance(other, values.W_Flonum)
        val = math.pow(self.value, other.value)
        return values.W_Flonum(val)

    def arith_abs(self):
        return values.W_Flonum(abs(self.value))

    def arith_max_same(self, other):
        assert isinstance(other, values.W_Flonum)
        if math.isnan(self.value):
            return self
        if math.isnan(other.value):
            return other
        return values.W_Number.arith_max_same(self, other)

    def arith_min_same(self, other):
        assert isinstance(other, values.W_Flonum)
        if math.isnan(self.value):
            return self
        if math.isnan(other.value):
            return other
        return values.W_Number.arith_min_same(self, other)
    # ------------------ trigonometry ------------------

    def arith_sqrt(self):
        n = self.value
        if n < 0.0:
            return imaginary(values.W_Flonum(math.sqrt(-n)))
        return values.W_Flonum(math.sqrt(n))

    def arith_log(self):
        return values.W_Flonum(math.log(self.value))
    def arith_sin(self):
        return values.W_Flonum(math.sin(self.value))
    def arith_cos(self):
        return values.W_Flonum(math.cos(self.value))
    def arith_tan(self):
        return values.W_Flonum(math.tan(self.value))
    def arith_sinh(self):
        return values.W_Flonum(math.sinh(self.value))
    def arith_cosh(self):
        return values.W_Flonum(math.cosh(self.value))
    def arith_tanh(self):
        return values.W_Flonum(math.tanh(self.value))
    def arith_asin(self):
        return values.W_Flonum(math.asin(self.value))
    def arith_acos(self):
        return values.W_Flonum(math.acos(self.value))
    def arith_atan(self):
        return values.W_Flonum(math.atan(self.value))


    # ------------------ miscellanous ------------------
    def arith_round(self):
        from rpython.rlib.rfloat import round_double
        return values.W_Flonum(round_double(self.value, 0, half_even=True))

    def arith_truncate(self):
        from math import isinf
        if isinf(self.value):
            return self
        elif self.value < 0:
            return self.arith_ceiling()
        else:
            return self.arith_floor()

    def arith_floor(self):
        from math import isinf
        if isinf(self.value):
            return self
        return values.W_Flonum(float(math.floor(self.value)))

    def arith_ceiling(self):
        from math import isinf
        if isinf(self.value):
            return self
        return values.W_Flonum(float(math.ceil(self.value)))

    def _arith_float_fractional_part(self):
        try:
            val = rarithmetic.ovfcheck_float_to_int(self.value)
        except OverflowError:
            val = rbigint.fromfloat(self.value).tofloat()
        return float(self.value - val)

    def arith_float_fractional_part(self):
        return values.W_Flonum(self._arith_float_fractional_part())

    def arith_float_integer_part(self):
        return values.W_Integer.fromfloat(self.value)

    def arith_inexact_exact(self):
        fractional_part = self.arith_float_fractional_part()
        if fractional_part.value == 0:
            return values.W_Integer.fromfloat(self.value)
        else:
            return values.W_Rational.fromfloat(self.value)

    def arith_exact_inexact(self):
        return self

    def arith_zerop(self):
        return values.W_Bool.make(self.value == 0.0)

    def arith_negativep(self):
        return values.W_Bool.make(self.value < 0.0)

    def arith_positivep(self):
        return values.W_Bool.make(self.value > 0.0)

    def arith_evenp(self):
        return values.W_Bool.make(math.fmod(self.value, 2.0) == 0.0)

    def arith_oddp(self):
        value = self.value
        if math.isnan(value) or math.isinf(value):
            return values.w_false
        return values.W_Bool.make(math.fmod(value, 2.0) != 0.0)

    def arith_exp(self):
        if self.value == 0:
            return values.W_Fixnum.ONE
        return values.W_Flonum(math.exp(self.value))

    def arith_gcd_same(self, other):
        assert isinstance(other, values.W_Flonum)
        if not other.value:
            return self
        res = math.fmod(self.value, other.value)
        return other.arith_gcd(values.W_Flonum(res))

    # ------------------ comparisons ------------------

    def arith_eq_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return self.value == other.value

    def arith_ne_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return self.value != other.value

    def arith_lt_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return self.value < other.value

    def arith_le_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return self.value <= other.value

    def arith_gt_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return self.value > other.value

    def arith_ge_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return self.value >= other.value

    def isinteger(self):
        val = self.value
        if math.isnan(val) or math.isinf(val):
            return False
        return math.floor(val) == val

class __extend__(values.W_Bignum):
    def same_numeric_class(self, other):
        if isinstance(other, values.W_Fixnum):
            return self, values.W_Bignum(rbigint.fromint(other.value))
        if isinstance(other, values.W_Bignum):
            return self, other
        return other.same_numeric_class_reversed(self)
    # ------------------ addition ------------------
    def arith_add_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return values.W_Integer.frombigint(self.value.add(other.value))

    def arith_sub_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return values.W_Integer.frombigint(self.value.sub(other.value))

    def arith_unarysub(self):
        # XXX fix the sys.maxint + 1 case
        return values.W_Bignum(self.value.neg())

    def arith_mul_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return values.W_Integer.frombigint(self.value.mul(other.value))

    def arith_div_same(self, other):
        assert isinstance(other, values.W_Bignum)
        try:
            res, mod = self.value.divmod(other.value)
        except ZeroDivisionError:
            raise SchemeException("zero_divisor")
        if mod.tobool():
            return values.W_Rational.frombigint(
                    self.value, other.value, need_to_check=False)
        return values.W_Integer.frombigint(res)

    def arith_mod_same(self, other):
        assert isinstance(other, values.W_Bignum)
        try:
            return values.W_Integer.frombigint(self.value.mod(other.value))
        except ZeroDivisionError:
            raise Exception("zero_divisor")

    def arith_remainder_same(self, other):
        from rpython.rlib.rbigint import _divrem
        assert isinstance(other, values.W_Bignum)
        try:
            div, rem = _divrem(self.value, other.value)
            return values.W_Integer.frombigint(rem)
        except ZeroDivisionError:
            raise Exception("zero_divisor")

    def arith_quotient_same(self, other):
        from rpython.rlib.rbigint import _divrem # XXX make nice interface
        assert isinstance(other, values.W_Bignum)
        x = self.value
        y = other.value
        try:
            div, rem = _divrem(x, y)
        except ZeroDivisionError:
            raise SchemeException("zero_divisor")
        return values.W_Integer.frombigint(div)

    def arith_pow_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return values.W_Integer.frombigint(self.value.pow(other.value))

    def arith_shr_same(self, other):
        assert isinstance(other, values.W_Bignum)
        try:
            num = other.value.toint()
        except OverflowError:
            # XXX raise a Racket-level error!
            raise ValueError('Right operand too big')
        return values.W_Integer.frombigint(self.value.rshift(num))

    def arith_shl_same(self, other):
        assert isinstance(other, values.W_Bignum)
        try:
            num = other.value.toint()
        except OverflowError:
            # XXX raise a Racket-level error!
            raise ValueError('Right operand too big')
        return values.W_Integer.frombigint(self.value.lshift(num))

    def arith_or_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return values.W_Integer.frombigint(self.value.or_(other.value))

    def arith_and_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return values.W_Integer.frombigint(self.value.and_(other.value))

    def arith_xor_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return values.W_Integer.frombigint(self.value.xor(other.value))

    def arith_not(self):
        return values.W_Integer.frombigint(self.value.invert())


    def arith_abs(self):
        return values.W_Integer.frombigint(self.value.abs())


    # ------------------ max ------------------

    def arith_gcd_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return values.W_Integer.frombigint(self.value.gcd(other.value))

    # ------------------ miscellanous ------------------

    def arith_arith_fractional_part(self):
        return values.W_Fixnum.ZERO

    def arith_arith_integer_part(self):
        return self

    def arith_exact_inexact(self):
        return values.W_Flonum(self.value.tofloat())

    def arith_zerop(self):
        return values.W_Bool.make(not self.value.tobool())

    def arith_negativep(self):
        return values.W_Bool.make(self.value.get_sign() == -1)

    def arith_positivep(self):
        return values.W_Bool.make(self.value.get_sign() == 1)

    def arith_evenp(self):
        return values.W_Bool.make(
            not self.value.mod(rbigint.fromint(2)).tobool())

    def arith_oddp(self):
        return values.W_Bool.make(
            self.value.mod(rbigint.fromint(2)).tobool())

    # ------------------ comparisons ------------------

    def arith_eq_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return self.value.eq(other.value)

    def arith_lt_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return self.value.lt(other.value)

class __extend__(values.W_Rational):
    def same_numeric_class(self, other):
        # nb: intentionally use the direct constructor
        if isinstance(other, values.W_Fixnum):
            return self, values.W_Rational(rbigint.fromint(other.value), ONERBIGINT)
        if isinstance(other, values.W_Bignum):
            return self, values.W_Rational(other.value, ONERBIGINT)
        if isinstance(other, values.W_Rational):
            return self, other
        return other.same_numeric_class_reversed(self)

    def arith_add_same(self, other):
        assert isinstance(other, values.W_Rational)
        return values.W_Rational.frombigint(
                self._numerator.mul(other._denominator).add(other._numerator.mul(self._denominator)),
                self._denominator.mul(other._denominator))

    def arith_sub_same(self, other):
        assert isinstance(other, values.W_Rational)
        return values.W_Rational.frombigint(
                self._numerator.mul(other._denominator).sub(other._numerator.mul(self._denominator)),
                self._denominator.mul(other._denominator))

    def arith_mul_same(self, other):
        assert isinstance(other, values.W_Rational)
        return values.W_Rational.frombigint(
            self._numerator.mul(other._numerator),
            self._denominator.mul(other._denominator))

    def arith_div_same(self, other):
        assert isinstance(other, values.W_Rational)
        return values.W_Rational.frombigint(
            self._numerator.mul(other._denominator),
            self._denominator.mul(other._numerator))

    def arith_pow_same(self, other):
        assert isinstance(other, values.W_Rational)
        # XXX Not precise
        v = self._numerator.tofloat()  / self._denominator.tofloat()
        p = other._numerator.tofloat() / other._denominator.tofloat()
        return values.W_Flonum(math.pow(v, p))

    def arith_gcd_same(self, other):
        assert isinstance(other, values.W_Rational)
        num = self._numerator.mul(other._denominator).gcd(other._numerator.mul(self._denominator))
        den = self._denominator.mul(other._denominator)
        return values.W_Rational.frombigint(num, den)

    def arith_abs(self):
        num = self._numerator.abs()
        return values.W_Rational(num, self._denominator)

    def arith_negativep(self):
        return values.W_Bool.make(self._numerator.get_sign() == -1)

    def arith_positivep(self):
        return values.W_Bool.make(self._numerator.get_sign() == 1)

    def arith_zerop(self):
        return values.W_Bool.make(not self._numerator.tobool())

    def arith_round(self):
        res1 = self._numerator.floordiv(self._denominator)
        diff1 = res1.mul(self._denominator).sub(self._numerator)
        diff2 = diff1.add(self._denominator).abs()
        diff1 = diff1.abs()
        if diff1.gt(diff2):
            res2 = res1.int_add(1)
            return values.W_Integer.frombigint(res2)
        elif diff1.eq(diff2):
            if res1.int_and_(1).tobool():
                res2 = res1.int_add(1)
                return values.W_Integer.frombigint(res2)
            else:
                return values.W_Integer.frombigint(res1)
        else:
            return values.W_Integer.frombigint(res1)

    def arith_ceiling(self):
        res1 = self._numerator.floordiv(self._denominator)
        res = res1.int_add(1)
        return values.W_Integer.frombigint(res)

    def arith_floor(self):
        res = self._numerator.floordiv(self._denominator)
        return values.W_Integer.frombigint(res)

    def arith_truncate(self):
        assert self._numerator.ne(NULLRBIGINT)
        if self._numerator.get_sign() == self._denominator.get_sign():
            return self.arith_floor()
        else:
            return self.arith_ceiling()

    def arith_inexact_exact(self):
        return self

    def arith_exact_inexact(self):
        num = self._numerator
        den = self._denominator
        try:
            return values.W_Flonum(num.truediv(den))
        except OverflowError:
            if num.get_sign() == den.get_sign():
                return values.W_Flonum.INF
            return values.W_Flonum.NEGINF

    # ------------------ comparisons ------------------

    def arith_eq_same(self, other):
        assert isinstance(other, values.W_Rational)
        return self._numerator.eq(other._numerator) and self._denominator.eq(other._denominator)

    def arith_lt_same(self, other):
        assert isinstance(other, values.W_Rational)
        ad = self._numerator.mul(other._denominator)
        cb = other._numerator.mul(self._denominator)
        return ad.lt(cb)

class __extend__(values.W_Complex):
    def same_numeric_class(self, other):
        if isinstance(other, values.W_Complex):
            return self, other
        return self, values.W_Complex(other, values.W_Fixnum.ZERO)

    def arith_add_same(self, other):
        assert isinstance(other, values.W_Complex)
        return values.W_Complex(self.real.arith_add(other.real),
                                self.imag.arith_add(other.imag))

    def arith_sub_same(self, other):
        assert isinstance(other, values.W_Complex)
        return values.W_Complex(self.real.arith_sub(other.real),
                                self.imag.arith_sub(other.imag))

    def arith_mul_same(self, other):
        assert isinstance(other, values.W_Complex)
        re1 = self.real.arith_mul(other.real)
        re2 = self.imag.arith_mul(other.imag)
        im1 = self.real.arith_mul(other.imag)
        im2 = self.imag.arith_mul(other.real)
        return values.W_Complex(re1.arith_sub(re2), im1.arith_add(im2))

    def arith_div_same(self, other):
        assert isinstance(other, values.W_Complex)
        if other.imag.arith_eq(values.W_Fixnum.ZERO):
            divisor = other.real
            r = self.real.arith_div(divisor)
            i = self.imag.arith_div(divisor)
            return values.W_Complex(r, i)
        factor = other.reciprocal()
        return self.arith_mul(factor)

    def arith_zerop(self):
        real = self.real.arith_zerop() is not values.w_false
        imag = self.imag.arith_zerop() is not values.w_false
        return values.W_Bool.make(real and imag)

    # Useful complex number operations
    def complex_conjugate(self):
        return values.W_Complex(self.real, self.imag.arith_unarysub())

    def reciprocal(self):
        re2 = self.real.arith_mul(self.real)
        im2 = self.imag.arith_mul(self.imag)
        denom = re2.arith_add(im2)
        return self.complex_conjugate().arith_div(denom)

    def arith_inexact_exact(self):
        return values.W_Complex(
                self.real.arith_inexact_exact(),
                self.imag.arith_inexact_exact())

    def arith_exact_inexact(self):
        return values.W_Complex(
                self.real.arith_exact_inexact(),
                self.imag.arith_exact_inexact())

    def arith_exp(self):
        r = self.real.arith_exp()
        cos = self.imag.arith_cos()
        sin = self.imag.arith_sin()
        return values.W_Complex(cos, sin).arith_mul(r)

    def arith_sin(self):
        "sin(a+bi)=sin a cosh b + i cos a sinh b"
        r = self.real.arith_sin().arith_mul(self.imag.arith_cosh())
        i = self.real.arith_cos().arith_mul(self.imag.arith_sinh())
        return values.W_Complex(r, i)

    def arith_cos(self):
        "cos(a+bi)=cos a cosh b - i sin a sinh b"
        r = self.real.arith_cos().arith_mul(self.imag.arith_cosh())
        i = self.real.arith_sin().arith_mul(self.imag.arith_sinh())
        return values.W_Complex(r, i).complex_conjugate()

    def arith_tan(self):
        return self.arith_sin().arith_div_same(self.arith_cos())

    def arith_sinh(self):
        "sinh(a+bi)=sinh a cos b + i cosh a sinh b"
        r = self.real.arith_sinh().arith_mul(self.imag.arith_cos())
        i = self.real.arith_cosh().arith_mul(self.imag.arith_sin())
        return values.W_Complex(r, i)

    def arith_cosh(self):
        "cosh(a+bi)=cosh a cos b + i sinh a sin b"
        r = self.real.arith_cosh().arith_mul(self.imag.arith_cos())
        i = self.real.arith_sinh().arith_mul(self.imag.arith_sin())
        return values.W_Complex(r, i)

    def arith_tanh(self):
        return self.arith_sinh().arith_div(self.arith_cosh())

    def arith_asin(self):
        raise NotImplementedError("to be done")

    def arith_acos(self):
        raise NotImplementedError("to be done")

    def arith_atan(self):
        raise NotImplementedError("to be done")

    # ------------------ comparisons ------------------

    def arith_eq_same(self, other):
        assert isinstance(other, values.W_Complex)
        return self.real.arith_eq(other.real) and self.imag.arith_eq(other.imag)

    def arith_lt_same(self, other):
        raise SchemeException("can't compare complex numbers")
