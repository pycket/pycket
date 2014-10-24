#! /usr/bin/env python
# -*- coding: utf-8 -*-
import math
import operator
from pycket import values
from pycket import vector as values_vector
from pycket.error import SchemeException
from pycket.prims.expose import expose, default, unsafe
from rpython.rlib.rbigint import rbigint
from rpython.rlib         import jit, rarithmetic
from rpython.rtyper.lltypesystem.lloperation import llop
from rpython.rtyper.lltypesystem.lltype import Signed, SignedLongLong, \
                                        UnsignedLongLong

# imported for side effects
from pycket import arithmetic

def make_cmp(name, op, con):

    @expose(name, simple=True)
    @jit.unroll_safe
    def do(args):
        if len(args) < 2:
            raise SchemeException("number of arguments to %s too small" % name)
        idx = 2
        truth = True
        while idx <= len(args):
            start = idx - 2
            assert start >= 0
            w_a, w_b = args[start], args[start + 1]
            if not isinstance(w_a, values.W_Number):
                raise SchemeException("expected number")
            if not isinstance(w_b, values.W_Number):
                raise SchemeException("expected number")
            idx += 1
            truth = truth and getattr(w_a, "arith_" + op)(w_b)

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
    return values.Values.make([a.arith_quotient(b), a.arith_mod(b)]) #FIXME

def make_binary_arith(name, methname):
    @expose(name, [values.W_Number, values.W_Number], simple=True)
    def do(a, b):
        return getattr(a, methname)(b)
    do.__name__ = methname

for args in [
        ("quotient", "arith_quotient"),
        ("remainder", "arith_remainder"),
        ("modulo",   "arith_mod"),
        ("expt",     "arith_pow"),
        ]:
    make_binary_arith(*args)


def make_arith(name, neutral_element, methname, supports_zero_args):
    @expose(name, simple=True)
    @jit.unroll_safe
    def do(args):
        # XXX so far (+ '()) returns '(). need better type checking here
        if not args:
            if not supports_zero_args:
                raise SchemeException("expected at least 1 argument to %s" % name)
            return neutral_element
        if len(args) == 1:
            if neutral_element is not None:
                return getattr(neutral_element, methname)(args[0])
            return args[0]
        else:
            init = args[0]
            for i in range(1, jit.promote(len(args))):
                init = getattr(init, methname)(args[i])
            return init
    do.__name__ = methname

for args in [
        ("+", values.W_Fixnum.make(0), "arith_add", True),
        ("-", values.W_Fixnum.make(0), "arith_sub", False),
        ("*", values.W_Fixnum.make(1), "arith_mul", True),
        ("/", values.W_Fixnum.make(1), "arith_div", False),
        ("max", None, "arith_max", False),
        ("min", None, "arith_min", False),
        ("gcd", values.W_Fixnum.make(0), "arith_gcd", True),
        ("lcm", values.W_Fixnum.make(1), "arith_lcm", True),
        ("bitwise-and", values.W_Fixnum.make(-1), "arith_and", True),
        ("bitwise-ior", values.W_Fixnum.make(0), "arith_or", True),
        ("bitwise-xor", values.W_Fixnum.make(0), "arith_xor", True),
        ]:
    make_arith(*args)

def make_fixedtype_binary_arith(
        name, methname, intversion=True, floatversion=True):
    methname += "_same"
    if floatversion:
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
        ("quotient", "arith_quotient", True, False),
        ("-", "arith_sub"),
        ("*", "arith_mul"),
        ("/", "arith_div", False),
        ("and", "arith_and", True, False),
        ("max", "arith_max"),
        ("min", "arith_min"),
]:
    make_fixedtype_binary_arith(*args)

def make_fixedtype_cmps(name, methname):
    methname = "arith_%s_same" % methname
    def do(a, b):
        return values.W_Bool.make(getattr(a, methname)(b))
    do.__name__ = "fl_" + methname
    expose("fl" + name, [values.W_Flonum] * 2, simple=True)(do)
    expose("unsafe-fl" + name, [unsafe(values.W_Flonum)] * 2, simple=True)(do)

    def do(a, b):
        return values.W_Bool.make(getattr(a, methname)(b))
    do.__name__ = "fx_" + methname
    expose("fx" + name, [values.W_Fixnum] * 2, simple=True)(do)
    expose("unsafe-fx" + name, [unsafe(values.W_Fixnum)] * 2, simple=True)(do)

for args in [
    ("<",  "lt"),
    ("<=", "le"),
    (">",  "gt"),
    (">=", "ge"),
    ("=",  "eq"),
    ]:
    make_fixedtype_cmps(*args)

@expose("unsafe-flsqrt", [unsafe(values.W_Flonum)])
def flsqrt(f):
    return f.arith_sqrt()

@expose("add1", [values.W_Number])
def add1(v):
    return v.arith_add(values.W_Fixnum(1))

@expose("atan", [values.W_Number, default(values.W_Number, None)])
def atan(y, x):
    if x is not None:
        # FIXME: signs determine the quadrant of the result
        # and care about NaNs and precision
        if x.arith_zerop() is values.w_false:
            z = y.arith_div(x)
        else:
            # we should raise exn_fail_contract_divide_by_zero
            raise SchemeException("zero_divisor")
    else:
        z = y
    return getattr(z, "arith_atan")()


def make_unary_arith(name, methname, flversion=False, fxversion=False, unwrap_type=values.W_Number):
    def do(a):
        return getattr(a, methname)()
    do.__name__ = methname
    expose(name, [unwrap_type], simple=True)(do)
    if flversion:
        def dofl(a):
            return getattr(a, methname)()
        dofl.__name__ = methname
        expose("fl" + name, [values.W_Flonum], simple=True)(dofl)
    if fxversion:
        def dofx(a):
            return getattr(a, methname)()
        dofx.__name__ = methname
        expose("fx" + name, [values.W_Fixnum], simple=True)(dofx)

for args in [
        ("sin", "arith_sin", True),
        ("cos", "arith_cos", True),
        ("tan", "arith_tan", True),
        ("sinh", "arith_sinh", True),
        ("cosh", "arith_cosh", True),
        ("tanh", "arith_tanh", True),
        ("sqrt", "arith_sqrt", True),
        ("asin", "arith_asin", True),
        ("acos", "arith_acos", True),
        # ("tan", "arith_tan", True), down below
        ("log", "arith_log", True),
        ("sub1", "arith_sub1"),
        ("inexact->exact", "arith_inexact_exact"),
        ("exact->inexact", "arith_exact_inexact"),
        ("zero?", "arith_zerop"),
        ("negative?", "arith_negativep"),
        ("positive?", "arith_positivep"),
        ("even?", "arith_evenp"),
        ("odd?", "arith_oddp"),
        ("abs", "arith_abs", True),
        ("round", "arith_round", True),
        ("truncate", "arith_truncate", True),
        ("floor", "arith_floor", True),
        ("ceiling", "arith_ceiling", True),
        ("bitwise-not", "arith_not", False, False, values.W_Integer),
        ("exp",     "arith_exp", True),
        ]:
    make_unary_arith(*args)


@expose("bitwise-bit-set?", [values.W_Integer, values.W_Integer])
def bitwise_bit_setp(w_n, w_m):
    if w_m.arith_negativep() is values.w_true:
        raise SchemeException("bitwise-bit-set?: second argument must be non-negative")
    if not isinstance(w_m, values.W_Fixnum):
        # a bignum that has such a big bit set does not fit in memory
        return w_n.arith_negativep()
    v = w_n.arith_and(arith_shift(values.W_Fixnum(1), w_m))
    if isinstance(v, values.W_Fixnum) and 0 == v.value:
        return values.w_false
    else:
        return values.w_true

def arith_shift(w_a, w_b):
    # XXX support biginteger as second argument (returning 0 and out of memory)
    b = w_b.value
    if b >= 0:
        return w_a.arith_shl(w_b)
    else:
        return w_a.arith_shr(values.W_Fixnum(-b))
# don't use the decorator to make the function usable in this file
expose("arithmetic-shift", [values.W_Integer, values.W_Fixnum])(arith_shift)

@expose("fxlshift", [values.W_Fixnum, values.W_Fixnum])
def fxlshift(w_a, w_b):
    b = w_b.value
    if 0 <= b <= 64:
        try:
            res = rarithmetic.ovfcheck(w_a.value << b)
        except OverflowError:
            raise SchemeException(
                "fxlshift: result is not a fixnum")
        return values.W_Fixnum(res)
    else:
        raise SchemeException(
            "fxlshift: expected integer >= 0 and <= 64, got %s" % w_b.tostring())

@expose("fxrshift", [values.W_Fixnum, values.W_Fixnum])
def fxrshift(w_a, w_b):
    b = w_b.value
    if b >= 0:
        return w_a.arith_shr(w_b)
    else:
        raise SchemeException("fxrshift: expected positive argument, got %s"%w_b)


## Unsafe Fixnum ops
@expose("unsafe-fxlshift", [unsafe(values.W_Fixnum), unsafe(values.W_Fixnum)])
def unsafe_fxlshift(w_a, w_b):
    res = rarithmetic.intmask(w_a.value << w_b.value)
    return values.W_Fixnum(res)

@expose("unsafe-fxrshift", [unsafe(values.W_Fixnum), unsafe(values.W_Fixnum)])
def unsafe_fxrshift(w_a, w_b):
    res = w_a.value >> w_b.value
    return values.W_Fixnum(res)

@expose("unsafe-fxand", [unsafe(values.W_Fixnum), unsafe(values.W_Fixnum)])
def unsafe_fxand(w_a, w_b):
    return w_a.arith_and(w_b)

@expose("unsafe-fx+", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxplus(a, b):
    return values.W_Fixnum(a.value + b.value)

@expose("unsafe-fx-", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxminus(a, b):
    return values.W_Fixnum(a.value - b.value)

@expose("unsafe-fx*", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxtimes(a, b):
    return values.W_Fixnum(a.value * b.value)

@expose("unsafe-fxmodulo", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxtimes(a, b):
    return values.W_Fixnum(a.value % b.value)

@expose("unsafe-fxmin", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxmin(a, b):
    return values.W_Fixnum(min(a.value, b.value))

@expose("unsafe-fxmax", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxmax(a, b):
    return values.W_Fixnum(max(a.value, b.value))


@expose("fx->fl", [values.W_Fixnum])
def fxfl(a):
    return values.W_Flonum(float(a.value))

@expose("unsafe-fxquotient", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxquotient(a, b):
    return values.W_Fixnum.make(llop.int_floordiv(Signed, a.value, b.value))

@expose("unsafe-fx->fl", [unsafe(values.W_Fixnum)])
def unsafe_fxfl(a):
    return values.W_Flonum(float(a.value))

# FIXME: implementation
@expose("fxvector?", [values.W_Object])
def is_fxvector(v):
    return values.w_false

@expose("flvector?", [values.W_Object])
def is_flvector(v):
    return values.W_Bool.make(isinstance(v, values_vector.W_FlVector))

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
