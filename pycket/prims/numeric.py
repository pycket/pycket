#! /usr/bin/env python
# -*- coding: utf-8 -*-
import math
import operator
from pycket import values
from pycket.error import SchemeException
from pycket.prims.expose import expose, default, unsafe
from rpython.rlib.rbigint import rbigint
from rpython.rlib         import jit
from rpython.rtyper.lltypesystem.lloperation import llop
from rpython.rtyper.lltypesystem.lltype import Signed, SignedLongLong, \
                                        UnsignedLongLong

# imported for side effects
from pycket import arithmetic

def make_cmp(name, op, con):
    from pycket.values import W_Number, W_Fixnum, W_Flonum, W_Bignum
    from rpython.rlib.rbigint import rbigint

    @expose(name, simple=True)
    @jit.unroll_safe
    def do(args):
        assert len(args) >= 2
        idx = 2
        truth = True
        while idx <= len(args):
            start = idx - 2
            assert start >= 0
            w_a, w_b = args[start:idx]
            assert isinstance(w_a, W_Number)
            assert isinstance(w_b, W_Number)
            idx += 1

            if isinstance(w_a, W_Fixnum) and isinstance(w_b, W_Fixnum):
                truth = truth and (getattr(operator, op)(w_a.value, w_b.value))
                continue
            if isinstance(w_a, W_Bignum) and isinstance(w_b, W_Bignum):
                truth = truth and (getattr(w_a.value, op)(w_b.value))
                continue
            if isinstance(w_a, W_Flonum) and isinstance(w_b, W_Flonum):
                truth = truth and (getattr(operator, op)(w_a.value, w_b.value))
                continue

            # Upcast float
            if isinstance(w_a, W_Fixnum) and isinstance(w_b, W_Flonum):
                a = float(w_a.value)
                truth = truth and (getattr(operator, op)(a, w_b.value))
                continue
            if isinstance(w_a, W_Flonum) and isinstance(w_b, W_Fixnum):
                b = float(w_b.value)
                truth = truth and (getattr(operator, op)(w_a.value, b))
                continue

            # Upcast bignum
            if isinstance(w_a, W_Bignum) and isinstance(w_b, W_Fixnum):
                b = rbigint.fromint(w_b.value)
                truth = truth and (getattr(w_a.value, op)(b))
                continue
            if isinstance(w_a, W_Fixnum) and isinstance(w_b, W_Bignum):
                a = rbigint.fromint(w_a.value)
                truth = truth and (getattr(a, op)(w_b.value))
                continue

            # Upcast bignum/float
            if isinstance(w_a, W_Bignum) and isinstance(w_b, W_Flonum):
                b = rbigint.fromfloat(w_b.value)
                truth = truth and (getattr(w_a.value, op)(b))
                continue
            if isinstance(w_a, W_Flonum) and isinstance(w_b, W_Bignum):
                a = rbigint.fromfloat(w_a.value)
                truth = truth and (getattr(a, op)(w_b.value))
                continue

            #FIXME: Complex. Rationals

            raise SchemeException("unsupported operation %s on %s %s" % (
                name, w_a.tostring(), w_b.tostring()))
        return con(truth)
    do.__name__ = op

for args in [
        ("=", "eq", values.W_Bool.make),
        ("<", "lt", values.W_Bool.make),
        (">", "gt", values.W_Bool.make),
        ("<=", "le", values.W_Bool.make),
        (">=", "ge", values.W_Bool.make),
        ]:
    make_cmp(*args)

@expose("integer?", [values.W_Object])
def integerp(n):
    return values.W_Bool.make(isinstance(n, values.W_Fixnum) or
                              isinstance(n, values.W_Bignum) or
                              isinstance(n, values.W_Flonum) and
                              math.floor(n.value) == n.value)


@expose("exact-integer?", [values.W_Object])
def exact_integerp(n):
    return values.W_Bool.make(isinstance(n, values.W_Fixnum) or
                              isinstance(n, values.W_Bignum))

@expose("exact-nonnegative-integer?", [values.W_Object])
def exact_nonneg_integerp(n):
    from rpython.rlib.rbigint import rbigint
    if isinstance(n, values.W_Fixnum):
        return values.W_Bool.make(n.value >= 0)
    if isinstance(n, values.W_Bignum):
        return values.W_Bool.make(n.value.ge(rbigint.fromint(0)))
    return values.w_false

@expose("exact-positive-integer?", [values.W_Object])
def exact_nonneg_integerp(n):
    from rpython.rlib.rbigint import rbigint
    if isinstance(n, values.W_Fixnum):
        return values.W_Bool.make(n.value > 0)
    if isinstance(n, values.W_Bignum):
        return values.W_Bool.make(n.value.gt(rbigint.fromint(0)))
    return values.w_false

@expose("real?", [values.W_Object])
def realp(n):
    return values.W_Bool.make(isinstance(n, values.W_Fixnum) or
                              isinstance(n, values.W_Bignum) or
                              isinstance(n, values.W_Flonum))

@expose("inexact-real?", [values.W_Object])
def inexact_real(n):
    return values.W_Bool.make(isinstance(n, values.W_Flonum))

@expose("single-flonum?", [values.W_Object])
def single_flonum(n):
    return values.w_false

@expose("double-flonum?", [values.W_Object])
def double_flonum(n):
    return values.W_Bool.make(isinstance(n, values.W_Flonum))

@expose("rational?", [values.W_Object])
def rationalp(n):
    if isinstance(n, values.W_Fixnum) or isinstance(n, values.W_Bignum):
        return values.w_true
    if isinstance(n, values.W_Flonum):
        v = n.value
        return values.W_Bool.make(not (math.isnan(v) or math.isinf(v)))

@expose("exact?", [values.W_Object])
def exactp(n):
    return values.W_Bool.make(isinstance(n, values.W_Fixnum) or
                              isinstance(n, values.W_Bignum))

@expose("inexact?", [values.W_Object])
def inexactp(n):
    return values.W_Bool.make(isinstance(n, values.W_Flonum))

@expose("quotient/remainder", [values.W_Integer, values.W_Integer])
def quotient_remainder(a, b):
    return values.Values.make([a.arith_quotient(b), values.W_Fixnum(0)]) #FIXME

def make_binary_arith(name, methname):
    @expose(name, [values.W_Number, values.W_Number], simple=True)
    def do(a, b):
        return getattr(a, methname)(b)
    do.__name__ = methname

for args in [
        ("quotient", "arith_quotient"),
        ("remainder", "arith_mod"), # FIXME
        ("modulo",   "arith_mod"),
        ("expt",     "arith_pow"),
        ("max",      "arith_max"),
        ("min",      "arith_min"),
        ]:
    make_binary_arith(*args)


def make_arith(name, neutral_element, methname, supports_zero_args):
    @expose(name, simple=True)
    @jit.unroll_safe
    def do(args):
        if not args:
            if not supports_zero_args:
                raise SchemeException("expected at least 1 argument to %s" % name)
            return neutral_element
        if len(args) == 1:
            return getattr(neutral_element, methname)(args[0])
        else:
            init = args[0]
            for i in range(1, jit.promote(len(args))):
                init = getattr(init, methname)(args[i])
            return init
    do.__name__ = methname

for args in [
        ("+", values.W_Fixnum(0), "arith_add", True),
        ("-", values.W_Fixnum(0), "arith_sub", False),
        ("*", values.W_Fixnum(1), "arith_mul", True),
        ("/", values.W_Fixnum(1), "arith_div", False),
        ("bitwise-and", values.W_Fixnum(-1), "arith_and", True),
        ("bitwise-ior", values.W_Fixnum(0), "arith_or", True),
        ("bitwise-xor", values.W_Fixnum(0), "arith_xor", True),
        ]:
    make_arith(*args)

def make_fixedtype_arith(name, methname, intversion=True):
    @expose("fl" + name, [values.W_Flonum] * 2, simple=True)
    def do(a, b):
        return getattr(a, methname)(b)
    do.__name__ = "fl_" + methname

    if intversion:
        @expose("fx" + name, [values.W_Fixnum] * 2, simple=True)
        def do(a, b):
            return getattr(a, methname)(b)
        do.__name__ = "fx_" + methname


for args in [
        ("+", "arith_add"),
        ("-", "arith_sub"),
        ("*", "arith_mul"),
        ("/", "arith_div", False),
        ("max", "arith_max"),
        ("min", "arith_min"),
        ]:
    make_fixedtype_arith(*args)

@expose("flsqrt", [values.W_Flonum])
def flsqrt(f):
    return f.arith_sqrt()

@expose("unsafe-flsqrt", [unsafe(values.W_Flonum)])
def flsqrt(f):
    return f.arith_sqrt()

@expose("add1", [values.W_Number])
def add1(v):
    return v.arith_add(values.W_Fixnum(1))

@expose("atan", [values.W_Number, default(values.W_Number, None)])
def atan(y, x):
    if isinstance(x, values.W_Number):
        # FIXME: signs determine the quadrant of the result
        # and care about NaNs and precision
        if getattr(x, "arith_zerop")() is values.w_false:
            z = getattr(y, "arith_div")(x)
        else:
            # we should raise exn_fail_contract_divide_by_zero
            raise SchemeException("zero_divisor")
    else:
        z = y
    return getattr(z, "arith_atan")()


def make_unary_arith(name, methname):
    @expose(name, [values.W_Number], simple=True)
    def do(a):
        return getattr(a, methname)()
    do.__name__ = methname

for args in [
        ("sin", "arith_sin"),
        ("cos", "arith_cos"),
        ("sqrt", "arith_sqrt"),
        ("log", "arith_log"),
        ("sub1", "arith_sub1"),
        ("inexact->exact", "arith_inexact_exact"),
        ("exact->inexact", "arith_exact_inexact"),
        ("zero?", "arith_zerop"),
        ("negative?", "arith_negativep"),
        ("positive?", "arith_positivep"),
        ("even?", "arith_evenp"),
        ("odd?", "arith_oddp"),
        ("abs", "arith_abs"),
        ("round", "arith_round")
        ]:
    make_unary_arith(*args)

## Unsafe Fixnum ops
@expose("unsafe-fx+", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxplus(a, b):
    return values.W_Fixnum(a.value + b.value)

@expose("unsafe-fx-", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxminus(a, b):
    return values.W_Fixnum(a.value - b.value)

@expose("unsafe-fx*", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxtimes(a, b):
    return values.W_Fixnum(a.value * b.value)

@expose("unsafe-fxmin", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxmin(a, b):
    return values.W_Fixnum(min(a.value, b.value))

@expose("unsafe-fx<", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxlt(a, b):
    return values.W_Bool.make(a.value < b.value)

@expose("unsafe-fx>", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxgt(a, b):
    return values.W_Bool.make(a.value > b.value)

@expose("unsafe-fx=", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxeq(a, b):
    return values.W_Bool.make(a.value == b.value)

@expose("unsafe-fxquotient", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxquotient(a, b):
    return values.W_Fixnum.make(llop.int_floordiv(Signed, a.value, b.value))

@expose("unsafe-fx->fl", [unsafe(values.W_Fixnum)])
def unsafe_fxfl(a):
    return values.W_Flonum(float(a.value))

## Unsafe Flonum ops
@expose("unsafe-fl+", [unsafe(values.W_Flonum)] * 2)
def unsafe_flplus(a, b):
    return values.W_Flonum(a.value + b.value)

@expose("unsafe-fl-", [unsafe(values.W_Flonum)] * 2)
def unsafe_flminus(a, b):
    return values.W_Flonum(a.value - b.value)

@expose("unsafe-fl*", [unsafe(values.W_Flonum)] * 2)
def unsafe_fltimes(a, b):
    return values.W_Flonum(a.value * b.value)

@expose("unsafe-fl/", [unsafe(values.W_Flonum)] * 2)
def unsafe_fldiv(a, b):
    return values.W_Flonum(a.value / b.value)

@expose("unsafe-fl<", [unsafe(values.W_Flonum)] * 2)
def unsafe_fllt(a, b):
    return values.W_Bool.make(a.value < b.value)

@expose("unsafe-fl<=", [unsafe(values.W_Flonum)] * 2)
def unsafe_fllte(a, b):
    return values.W_Bool.make(a.value <= b.value)

@expose("unsafe-fl>", [unsafe(values.W_Flonum)] * 2)
def unsafe_flgt(a, b):
    return values.W_Bool.make(a.value > b.value)

@expose("unsafe-fl>=", [unsafe(values.W_Flonum)] * 2)
def unsafe_flgte(a, b):
    return values.W_Bool.make(a.value >= b.value)

@expose("unsafe-fl=", [unsafe(values.W_Flonum)] * 2)
def unsafe_fleq(a, b):
    return values.W_Bool.make(a.value == b.value)



