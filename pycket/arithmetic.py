from rpython.rlib.rbigint import rbigint, NULLRBIGINT
from rpython.rlib import rarithmetic
from rpython.rtyper.raisingops import int_floordiv_ovf
from pycket import values, error
from pycket.error import SchemeException
import math

def make_int(w_value):
    # XXX is this ever called with a non-bignum?
    if isinstance(w_value, values.W_Bignum):
        try:
            num = w_value.value.toint()
        except OverflowError:
            pass
        else:
            return values.W_Fixnum(num)
    return w_value

class __extend__(values.W_Number):
    def arith_unaryadd(self):
        return self

    def arith_add(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_add_same(other)

    def arith_sub(self, other):
        self, other = self.same_numeric_class(other)
        return self.arith_sub_same(other)

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

    def same_numeric_class_reversed(self, other):
        self, other = self.same_numeric_class(other)
        return other, self

class __extend__(values.W_Complex):
    def same_numeric_class(self, other):
        if isinstance(other, values.W_Complex):
            return self, other
        return self, values.W_Complex(other, values.W_Fixnum(0))

    def arith_add_same(self, other):
        assert isinstance(other, values.W_Complex)
        return values.W_Complex(self.real.arith_add(other.real),
                                self.imag.arith_add(other.imag))

    def arith_sub_same(self, other):
        assert isinstance(other, values.W_Complex)
        return values.W_Complex(self.real.arith_sub(other.real),
                                self.imag.arith_sub(other.imag))

    def arith_sub1(self):
        return values.W_Complex(self.real.arith_sub1(), self.imag)

    def arith_mul_same(self, other):
        assert isinstance(other, values.W_Complex)
        re1 = self.real.arith_mul(other.real)
        re2 = self.imag.arith_mul(other.imag)
        im1 = self.real.arith_mul(other.imag)
        im2 = self.imag.arith_mul(other.real)
        return values.W_Complex(re1.arith_sub(re2), im1.arith_add(im2))

    def arith_div_same(self, other):
        assert isinstance(other, values.W_Complex)
        factor = other.reciprocal()
        return self.arith_mul(factor)

    # Useful complex number operations
    def complex_conjugate(self):
        # XXX
        return values.W_Complex(self.real, self.imag.arith_mul(values.W_Fixnum(-1)))

    def reciprocal(self):
        re2 = self.real.arith_mul(self.real)
        im2 = self.imag.arith_mul(self.imag)
        denom = re2.arith_add(im2)
        return self.complex_conjugate().arith_div(denom)


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

    def arith_sub1(self):
        # XXX ovf check
        return values.W_Fixnum(self.value - 1)

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
        # XXX
        raise SchemeException("rationals not implemented")

    def arith_mod_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        if other.value == 0:
            raise Exception("zero_divisor")
        try:
            res = rarithmetic.ovfcheck(self.value % other.value)
        except OverflowError:
            return self.arith_mod(values.W_Bignum(rbigint.fromint(other.value)))
        return values.W_Fixnum(res)

    def arith_quotient_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        x = self.value
        y = other.value
        if y == 0:
            raise SchemeException("zero_divisor")
        try:
            res = int_floordiv_ovf(x, y) # misnomer, should be int_truncdiv or so
        except OverflowError:
            return self.arith_quotient(values.W_Bignum(rbigint.fromint(other.value)))
        return values.W_Fixnum(res)

    def arith_pow_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        try:
            res = rarithmetic.ovfcheck_float_to_int(math.pow(self.value, other.value))
        except OverflowError:
            return self.arith_pow(values.W_Bignum(rbigint.fromint(other.value)))
        return values.W_Fixnum(res)

    def arith_shr_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return values.W_Fixnum(self.value >> other.value)

    def arith_shl_same(self, other_num):
        assert isinstance(other, values.W_Fixnum)
        return values.W_Fixnum(intmask(self.value << other.value))

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


    # ------------------ abs ------------------
    def arith_abs(self):
        if self.value >= 0:
            return self
        return values.W_Fixnum(0).arith_sub(self)

    def arith_max_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return values.W_Fixnum(max(self.value, other.value))

    def arith_min_same(self, other):
        assert isinstance(other, values.W_Fixnum)
        return values.W_Fixnum(min(self.value, other.value))

    # ------------------ trigonometry ------------------

    def arith_sin(self):
        return values.W_Flonum(math.sin(self.value))
    def arith_sqrt(self):
        assert 0
    def arith_log(self):
        return values.W_Flonum(math.log(self.value))
    def arith_cos(self):
        return values.W_Flonum(math.cos(self.value))
    def arith_atan(self):
        return values.W_Flonum(math.atan(self.value))

    # ------------------ miscellanous ------------------
    def arith_round(self):
        return self

    def arith_floor(self):
        return self

    def arith_ceiling(self):
        return self

    def arith_float_fractional_part(self):
        return values.W_Fixnum(0)

    def arith_float_integer_part(self):
        return self

    def arith_inexact_exact(self):
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


class __extend__(values.W_Flonum):
    def same_numeric_class(self, other):
        if isinstance(other, values.W_Fixnum):
            return self, values.W_Flonum(float(other.value))
        if isinstance(other, values.W_Flonum):
            return self, other
        if isinstance(other, values.W_Bignum):
            return self, values.W_Flonum(other.value.tofloat())
        if isinstance(other, values.W_Rational):
            raise NotImplementedError
        return other.same_numeric_class_reversed(self)

    def arith_add_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return values.W_Flonum(self.value + other.value)

    def arith_sub_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return values.W_Flonum(self.value - other.value)

    def arith_unarysub(self):
        return values.W_Flonum(-self.value)

    def arith_sub1(self):
        return values.W_Flonum(self.value - 1)

    def arith_mul_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return values.W_Flonum(self.value * other.value)

    def arith_div_same(self, other):
        assert isinstance(other, values.W_Flonum)
        if other.value == 0.0:
            raise SchemeException("zero_divisor")
        return values.W_Flonum(self.value / other.value)

    def arith_mod_same(self, other):
        assert isinstance(other, values.W_Flonum)
        if other.value == 0.0:
            raise Exception("zero_divisor")
        return values.W_Flonum(math.fmod(self.value, other.value))

    def arith_pow_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return values.W_Flonum(math.pow(self.value, other.value))

    def arith_abs(self):
        return values.W_Flonum(abs(self.value))

    def arith_max_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return values.W_Flonum(max(self.value, other.value))

    def arith_min_same(self, other):
        assert isinstance(other, values.W_Flonum)
        return values.W_Flonum(min(self.value, other.value))


    # ------------------ trigonometry ------------------

    def arith_sin(self):
        return values.W_Flonum(math.sin(self.value))
    def arith_sqrt(self):
        return values.W_Flonum(math.sqrt(self.value))
    def arith_log(self):
        return values.W_Flonum(math.log(self.value))
    def arith_cos(self):
        return values.W_Flonum(math.cos(self.value))
    def arith_atan(self):
        return values.W_Flonum(math.atan(self.value))


    # ------------------ miscellanous ------------------
    def arith_round(self):
        fval = self.value
        if fval >= 0:
            factor = 1
        else:
            factor = -1

        fval = fval * factor
        try:
            val = rarithmetic.ovfcheck_float_to_int(math.floor(fval + 0.5) * factor)
        except OverflowError:
            # XXX is this correct?
            return values.W_Bignum(rbigint.fromfloat(math.floor(self.value + 0.5) * factor))
        return values.W_Fixnum(val)

    def arith_floor(self):
        # XXX factor out conversion to fix or bignum
        try:
            val = rarithmetic.ovfcheck_float_to_int(math.floor(self.value))
        except OverflowError:
            return values.W_Bignum(rbigint.fromfloat(math.floor(self.value)))
        return values.W_Fixnum(val)

    def arith_ceiling(self):
        try:
            val = rarithmetic.ovfcheck_float_to_int(math.ceil(self.value))
        except OverflowError:
            return values.W_Bignum(rbigint.fromfloat(math.ceil(self.value)))
        return values.W_Fixnum(val)

    def arith_float_fractional_part(self):
        try:
            val = rarithmetic.ovfcheck_float_to_int(self.value)
        except OverflowError:
            val = rbigint.fromfloat(self.value).tofloat()
        return values.W_Flonum(float(self.value - val))

    def arith_float_integer_part(self):
        try:
            val = rarithmetic.ovfcheck_float_to_int(self.value)
        except OverflowError:
            return values.W_Bignum(rbigint.fromfloat(self.value))
        return values.W_Fixnum(val)

    def arith_inexact_exact(self):
        fractional_part = self.arith_float_fractional_part()
        if fractional_part.value == 0:
            return values.W_Fixnum(int(self.value))
        raise SchemeException("rationals not implemented")
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
        return values.W_Bool.make(math.fmod(self.value, 2.0) != 0.0)


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
        return make_int(values.W_Bignum(self.value.add(other.value)))

    def arith_sub_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return make_int(values.W_Bignum(self.value.sub(other.value)))

    def arith_unarysub(self):
        # XXX fix the sys.maxint + 1 case
        return values.W_Bignum(self.value.neg())

    def arith_mul_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return make_int(values.W_Bignum(self.value.mul(other.value)))

    def arith_div_same(self, other):
        assert isinstance(other, values.W_Bignum)
        try:
            res, mod = self.value.divmod(other.value)
        except ZeroDivisionError:
            raise SchemeException("zero_divisor")
        if mod.tobool():
            raise SchemeException("rationals not implemented")
        return make_int(values.W_Bignum(res))

    def arith_mod_same(self, other):
        assert isinstance(other, values.W_Bignum)
        try:
            return make_int(values.W_Bignum(self.value.mod(other.value)))
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
        return make_int(values.W_Bignum(div))


    def arith_pow_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return make_int(values.W_Bignum(self.value.pow(other.value)))

    def arith_shr_same(self, other):
        assert isinstance(other, values.W_Bignum)
        try:
            num = other.value.toint()
        except OverflowError:
            # XXX raise a Racket-level error!
            raise ValueError('Right operand too big')
        return make_int(values.W_Bignum(self.value.rshift(num)))

    def arith_shl_same(self, other):
        assert isinstance(other, values.W_Bignum)
        try:
            num = other.value.toint()
        except OverflowError:
            # XXX raise a Racket-level error!
            raise ValueError('Right operand too big')
        return make_int(values.W_Bignum(self.value.lshift(num)))

    def arith_or_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return make_int(values.W_Bignum(self.value.or_(other.value)))

    def arith_and_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return make_int(values.W_Bignum(self.value.and_(other.value)))

    def arith_xor_same(self, other):
        assert isinstance(other, values.W_Bignum)
        return make_int(values.W_Bignum(self.value.xor(other.value)))

    def arith_not(self):
        return make_int(values.W_Bignum(self.value.invert()))


    def arith_abs(self):
        return make_int(values.W_Bignum(self.value.abs()))


    # ------------------ max ------------------
    def arith_max_same(self, other):
        assert isinstance(other, values.W_Bignum)
        # XXX is this tested?
        if self.value.lt(other.value):
            return make_int(other)
        return make_int(self)

    def arith_min_same(self, other):
        assert isinstance(other, values.W_Bignum)
        if self.value.lt(other.value):
            return make_int(self)
        return make_int(other)


    # ------------------ miscellanous ------------------
    def arith_round(self):
        return make_int(self)

    def arith_floor(self):
        return make_int(self)

    def arith_ceiling(self):
        return make_int(self)

    def arith_arith_fractional_part(self):
        return values.W_Fixnum(0)

    def arith_arith_integer_part(self):
        return make_int(self)

    def arith_inexact_exact(self):
        return make_int(self)
    def arith_exact_inexact(self):
        return values.W_Flonum(self.value.tofloat())

    def arith_zerop(self):
        values.W_Bool.make(not self.value.tobool())

    def arith_negativep(self):
        return values.W_Bool.make(
            self.value.lt(NULLRBIGINT))

    def arith_positivep(self):
        return values.W_Bool.make(
            self.value.gt(NULLRBIGINT))

    def arith_evenp(self):
        return values.W_Bool.make(
            not self.value.mod(rbigint.fromint(2)).tobool())

    def arith_oddp(self):
        return values.W_Bool.make(
            self.value.mod(rbigint.fromint(2)).tobool())
