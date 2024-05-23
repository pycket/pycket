#! /usr/bin/env python
# -*- coding: utf-8 -*-
import math
import operator

from pycket                                  import values
from pycket                                  import vector as values_vector
from pycket.arity                            import Arity
from pycket.error                            import SchemeException
from pycket.prims.expose                     import expose, default, unsafe

from rpython.rlib                            import jit, longlong2float, rarithmetic, unroll, objectmodel
from rpython.rlib.rarithmetic                import r_uint
from rpython.rlib.objectmodel                import always_inline, specialize
from rpython.rlib.rbigint                    import rbigint
from rpython.rtyper.lltypesystem.lloperation import llop
from rpython.rtyper.lltypesystem.lltype      import Signed

# imported for side effects
from pycket import arithmetic

def make_cmp(name, op, con):

    @expose(name, simple=True, arity=Arity.geq(2))
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
            nan_a = not isinstance(w_a, values.W_Number)
            nan_b = not isinstance(w_b, values.W_Number)
            if nan_a or nan_b:
                pf = ["st", "nd", "rd"][idx-1] if idx <= 3 else "th"
                w = w_a if nan_a else w_b
                raise SchemeException("%s expected number as %s%s argument, got : %s" % (name, idx, pf, w.tostring()))
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
    return values.W_Bool.make(isinstance(n, values.W_Number) and n.isinteger())

@expose("exact-integer?", [values.W_Object])
def exact_integerp(n):
    return values.W_Bool.make(isinstance(n, values.W_Integer))

@expose("exact-nonnegative-integer?", [values.W_Object])
def exact_nonneg_integerp(n):
    from rpython.rlib.rbigint import NULLRBIGINT
    if isinstance(n, values.W_Fixnum):
        return values.W_Bool.make(n.value >= 0)
    if isinstance(n, values.W_Bignum):
        return values.W_Bool.make(n.value.ge(NULLRBIGINT))
    return values.w_false

@expose("exact-positive-integer?", [values.W_Object])
def exact_nonneg_integerp(n):
    from rpython.rlib.rbigint import NULLRBIGINT
    if isinstance(n, values.W_Fixnum):
        return values.W_Bool.make(n.value > 0)
    if isinstance(n, values.W_Bignum):
        return values.W_Bool.make(n.value.gt(NULLRBIGINT))
    return values.w_false

@expose("most-positive-fixnum", [])
def most_positive_fixnum():
    from sys import maxint
    return values.W_Fixnum(maxint)

@expose("most-negative-fixnum", [])
def most_negative_fixnum():
    from sys import maxint
    # there's one more negative than positive
    return values.W_Fixnum(-maxint-1)

@always_inline
def is_real(obj):
    return isinstance(obj, values.W_Real)

@expose("real?", [values.W_Object])
def realp(n):
    return values.W_Bool.make(is_real(n))

@expose("inexact-real?", [values.W_Object])
def inexact_real(n):
    return values.W_Bool.make(isinstance(n, values.W_Flonum))

@expose("single-flonum?", [values.W_Object])
def single_flonum(n):
    return values.w_false

@expose("single-flonum-available?", [])
def single_flonum_available():
    return values.w_false


@expose("double-flonum?", [values.W_Object])
def double_flonum(n):
    return values.W_Bool.make(isinstance(n, values.W_Flonum))

@expose(["real->double-flonum", "real->single-flonum"], [values.W_Number])
def real_to_double_flonum(num):
    if is_real(num):
        return num.arith_exact_inexact()
    raise SchemeException("real->double-flonum: %s is not real" % num.tostring())

@expose("rational?", [values.W_Object])
def rationalp(n):
    if isinstance(n, values.W_Fixnum) or isinstance(n, values.W_Bignum):
        return values.w_true
    if isinstance(n, values.W_Flonum):
        v = n.value
        return values.W_Bool.make(not (math.isnan(v) or math.isinf(v)))
    return values.W_Bool.make(isinstance(n, values.W_Rational))

def is_exact(n):
    if isinstance(n, values.W_Complex):
        return is_exact(n.real) and is_exact(n.imag)
    return (isinstance(n, values.W_Fixnum) or
            isinstance(n, values.W_Bignum) or
            isinstance(n, values.W_Rational))

@expose("numerator", [values.W_Real])
def numerator(r):
    if isinstance(r, values.W_Flonum):
        # FIXME : Racket seems to be computing this differently,
        # see test_arithmetic : (numerator 2.3)
        r = values.W_Rational.fromfloat(r.value)

    assert isinstance(r, values.W_Rational)

    return values.W_Integer.frombigint(r.get_numerator())

@expose("denominator", [values.W_Real])
def denominator(r):
    if isinstance(r, values.W_Flonum):
        r = values.W_Rational.fromfloat(r.value)

    assert isinstance(r, values.W_Rational)

    return values.W_Integer.frombigint(r.get_denominator())

def is_inexact(n):
    if isinstance(n, values.W_Complex):
        return is_inexact(n.real) or is_inexact(n.imag)
    return isinstance(n, values.W_Flonum)

@expose("exact?", [values.W_Object])
def exactp(n):
    return values.W_Bool.make(is_exact(n))

@expose("inexact?", [values.W_Object])
def inexactp(n):
    return values.W_Bool.make(is_inexact(n))

@expose("quotient/remainder", [values.W_Integer, values.W_Integer])
def quotient_remainder(a, b):
    return values.Values._make2(a.arith_quotient(b), a.arith_mod(b)) #FIXME

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

@expose("flexpt", [values.W_Flonum] * 2)
def flexpt(n, m):
    return n.arith_pow_same(m)

def make_arith(name, neutral_element, methname, supports_zero_args):
    art = Arity.geq(0) if supports_zero_args else Arity.geq(1)
    @expose(name, simple=True, arity=art)
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
        ("+"           , values.W_Fixnum.ZERO     , "arith_add" , True  ) ,
        ("-"           , values.W_Fixnum.ZERO     , "arith_sub" , False ) ,
        ("*"           , values.W_Fixnum.ONE      , "arith_mul" , True  ) ,
        ("/"           , values.W_Fixnum.ONE      , "arith_div" , False ) ,
        ("max"         , None                     , "arith_max" , False ) ,
        ("min"         , None                     , "arith_min" , False ) ,
        ("gcd"         , values.W_Fixnum.ZERO     , "arith_gcd" , True  ) ,
        ("lcm"         , values.W_Fixnum.ONE      , "arith_lcm" , True  ) ,
        ("bitwise-and" , values.W_Fixnum.make(-1) , "arith_and" , True  ) ,
        ("bitwise-ior" , values.W_Fixnum.ZERO     , "arith_or"  , True  ) ,
        ("bitwise-xor" , values.W_Fixnum.ZERO     , "arith_xor" , True  ) ,
        ]:
    make_arith(*args)

def make_fixedtype_nary_arith(
        name, methname, int_neutral, fl_neutral, intversion=True, floatversion=True,
        int_supports_zero_args=False, fl_supports_zero_args=False):
    methname += "_same"
    if floatversion:
        @expose("fl" + name, simple=True) # [values.W_Flonum] * 2
        def do(args):
            if not args:
                if not fl_supports_zero_args:
                    raise SchemeException("expected at least 1 argument to %s" % name)
                return fl_neutral
            init = args[0]
            for i in range(1, jit.promote(len(args))):
                init = getattr(init, methname)(args[i])
            return init
        do.__name__ = "fl_" + methname

    if intversion:
        @expose("fx" + name, simple=True) # [values.W_Fixnum] * 2
        def do(args):
            if not args:
                if not int_supports_zero_args:
                    raise SchemeException("expected at least 1 argument to %s" % name)
                return int_neutral
            init = args[0]
            for i in range(1, jit.promote(len(args))):
                init = getattr(init, methname)(args[i])
            return init
        do.__name__ = "fx_" + methname

for args in [
        ("+"         , "arith_add"       , values.W_Fixnum.ZERO , values.W_Flonum.ZERO , True  , True         ) ,
        ("-"         , "arith_sub"       , values.W_Fixnum.ZERO , values.W_Flonum.ZERO                        ) ,
        ("*"         , "arith_mul"       , values.W_Fixnum.ONE  , values.W_Flonum.ONE  , True  , True         ) ,
        ("/"         , "arith_div"       , None                 , values.W_Flonum.ONE  , False                ) ,
        ("and"       , "arith_and"       , None                 , None                 , True  , False , True ) ,
        ("max"       , "arith_max"       , None                 , values.W_Flonum.ZERO                        ) ,
        ("min"       , "arith_min"       , None                 , values.W_Flonum.ZERO                        ) ,
        ("quotient"  , "arith_quotient"  , values.W_Fixnum.ZERO , None                 , True   , False       ) ,
        ("remainder" , "arith_remainder" , values.W_Fixnum.ZERO , None                 , True   , False       ) ,
        ("modulo"    , "arith_mod"       , values.W_Fixnum.ZERO , None                 , True   , False       ) ,
]:
    make_fixedtype_nary_arith(*args)

def make_fixedtype_cmps(name, methname):
    methname = "arith_%s_same" % methname

    @jit.unroll_safe
    def do(args):
        idx = 2
        truth = True
        while idx <= len(args):
            start = idx - 2
            assert start >= 0
            w_a, w_b = args[start], args[start + 1]
            nan_a = not isinstance(w_a, values.W_Number)
            nan_b = not isinstance(w_b, values.W_Number)
            if nan_a or nan_b:
                pf = ["st", "nd", "rd"][idx-1] if idx <= 3 else "th"
                w = w_a if nan_a else w_b
                raise SchemeException("%s expected number as %s%s argument, got : %s" % (name, idx, pf, w.tostring()))
            idx += 1
            truth = truth and getattr(w_a, methname)(w_b)

        return values.W_Bool.make(truth)

    do.__name__ = "fl_" + methname
    expose("fl" + name, simple=True, arity=Arity.geq(2))(do)
    expose("unsafe-fl" + name, simple=True, arity=Arity.geq(2))(do)

    # FIXME: get rid of this code duplication

    @jit.unroll_safe
    def do(args):
        idx = 2
        truth = True
        while idx <= len(args):
            start = idx - 2
            assert start >= 0
            w_a, w_b = args[start], args[start + 1]
            nan_a = not isinstance(w_a, values.W_Number)
            nan_b = not isinstance(w_b, values.W_Number)
            if nan_a or nan_b:
                pf = ["st", "nd", "rd"][idx-1] if idx <= 3 else "th"
                w = w_a if nan_a else w_b
                raise SchemeException("%s expected number as %s%s argument, got : %s" % (name, idx, pf, w.tostring()))
            idx += 1
            truth = truth and getattr(w_a, methname)(w_b)

        return values.W_Bool.make(truth)

    do.__name__ = "fx_" + methname
    expose("fx" + name, simple=True, arity=Arity.geq(2))(do)
    expose("unsafe-fx" + name, simple=True, arity=Arity.geq(2))(do)

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
    return v.arith_add(values.W_Fixnum.ONE)

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


def make_unary_arith(name, methname, flversion=False, fxversion=False,
                     unwrap_type=values.W_Number):
    def do(a):
        return getattr(a, methname)()
    do.__name__ = methname
    expose(name, [unwrap_type], simple=True)(do)
    if flversion:
        @expose("fl" + name, [values.W_Flonum], simple=True)
        def dofl(a):
            return getattr(a, methname)()
        dofl.__name__ = methname

    if fxversion:
        @expose("fx" + name, [values.W_Fixnum], simple=True)
        def dofx(a):
            return getattr(a, methname)()
        dofx.__name__ = methname

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
        ("abs", "arith_abs", True),
        ("round", "arith_round", True),
        ("truncate", "arith_truncate", True),
        ("floor", "arith_floor", True),
        ("ceiling", "arith_ceiling", True),
        ("bitwise-not", "arith_not", False, False, values.W_Integer),
        ("exp",     "arith_exp", True),
        ]:
    make_unary_arith(*args)

@expose("odd?", [values.W_Number])
def oddp(n):
    if not n.isinteger():
        raise SchemeException("odd?: expected integer got %s" % n.tostring())
    return n.arith_oddp()

@expose("even?", [values.W_Number])
def evenp(n):
    if not n.isinteger():
        raise SchemeException("even?: expected integer got %s" % n.tostring())
    return n.arith_evenp()

@expose("negative?", [values.W_Number])
def negative_predicate(n):
    if not is_real(n):
        raise SchemeException("negative?: expected real? in argument 0")
    return n.arith_negativep()

@expose("positive?", [values.W_Number])
def positive_predicate(n):
    if not is_real(n):
        raise SchemeException("positive?: expected real? in argument 0")
    return n.arith_positivep()

@expose("bitwise-bit-field", [values.W_Integer, values.W_Integer, values.W_Integer])
def bitwise_bit_field(w_n, w_start, w_end):
    if w_start.arith_negativep() is values.w_true:
        raise SchemeException("bitwise-bit-field: second argument must be non-negative")
    if w_end.arith_negativep() is values.w_true:
        raise SchemeException("bitwise-bit-field: third argument must be non-negative")
    diff = w_end.arith_sub(w_start)
    assert isinstance(diff, values.W_Fixnum)
    v0 = arith_shift(values.W_Fixnum.ONE, diff)
    assert isinstance(v0, values.W_Fixnum)
    mw_start = values.W_Fixnum.ZERO.arith_sub(w_start)
    assert isinstance(mw_start, values.W_Fixnum)
    rhs = arith_shift(w_n, mw_start)
    assert isinstance(rhs, values.W_Fixnum)
    v = v0.arith_sub1().arith_and(rhs)
    return v

@expose("bitwise-bit-set?", [values.W_Integer, values.W_Integer])
def bitwise_bit_setp(w_n, w_m):
    if w_m.arith_negativep() is values.w_true:
        raise SchemeException("bitwise-bit-set?: second argument must be non-negative")
    if not isinstance(w_m, values.W_Fixnum):
        # a bignum that has such a big bit set does not fit in memory
        return w_n.arith_negativep()
    v = w_n.arith_and(arith_shift(values.W_Fixnum.ONE, w_m))
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

@expose("make-rectangular", [values.W_Number, values.W_Number])
def make_rectangular(x, y):
    if not is_real(x) or not is_real(y):
        raise SchemeException("make-rectangular: expected real inputs")
    return values.W_Complex.from_real_pair(x, y)

## Unsafe Fixnum ops
@expose("unsafe-fxlshift", [unsafe(values.W_Fixnum), unsafe(values.W_Fixnum)])
def unsafe_fxlshift(w_a, w_b):
    res = rarithmetic.intmask(w_a.value << w_b.value)
    return values.W_Fixnum(res)

@expose("unsafe-fxrshift", [unsafe(values.W_Fixnum), unsafe(values.W_Fixnum)])
def unsafe_fxrshift(w_a, w_b):
    res = w_a.value >> w_b.value
    return values.W_Fixnum(res)

@expose("unsafe-fxand", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxand(w_a, w_b):
    return w_a.arith_and(w_b)

@expose("unsafe-fxior", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxior(w_a, w_b):
    return w_a.arith_or(w_b)

@expose("unsafe-fxxor", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxxor(w_a, w_b):
    return w_a.arith_xor(w_b)

@expose("unsafe-fxnot", [unsafe(values.W_Fixnum)])
def unsafe_fxnot(w_a):
    return w_a.arith_not()

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

@expose("unsafe-fxmax", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxmax(a, b):
    return values.W_Fixnum(max(a.value, b.value))

@expose("unsafe-fxmodulo", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxtimes(a, b):
    return values.W_Fixnum(a.value % b.value)

@expose("unsafe-fxquotient", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxquotient(a, b):
    return values.W_Fixnum(rarithmetic.int_c_div(a.value, b.value))

@expose("unsafe-fxremainder", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxquotient(w_a, w_b):
    a = abs(w_a.value)
    b = abs(w_b.value)
    res = a % b
    if w_a.value < 0:
        res = -res
    return values.W_Fixnum(res)

@expose("fx->fl", [values.W_Fixnum])
def fxfl(a):
    return values.W_Flonum(float(a.value))

@expose("unsafe-fx->fl", [unsafe(values.W_Fixnum)])
def unsafe_fxfl(a):
    return values.W_Flonum(float(a.value))

@expose("->fl", [values.W_Object])
def to_fl(n):
    if isinstance(n, values.W_Fixnum):
        return values.W_Flonum(float(n.value))
    if isinstance(n, values.W_Bignum):
        return values.W_Flonum(rbigint.tofloat(n.value))
    raise SchemeException("->fl: expected an exact-integer")

@expose("real->floating-point-bytes",
        [values.W_Number, values.W_Fixnum, default(values.W_Bool, values.w_false)])
def real_floating_point_bytes(n, _size, big_endian):
    if isinstance(n, values.W_Flonum):
        v = n.value
    elif isinstance(n, values.W_Fixnum):
        v = float(n.value)
    elif isinstance(n, values.W_Bignum):
        v = rbigint.tofloat(n.value)
    else:
        raise SchemeException("real->floating-point-bytes: expected real")

    size = _size.value
    if size != 4 and size != 8:
        raise SchemeException("real->floating-point-bytes: size not 4 or 8")

    intval = longlong2float.float2longlong(v)

    if big_endian is not values.w_false:
        intval = rarithmetic.byteswap(intval)

    chars  = [chr((intval >> (i * 8)) % 256) for i in range(size)]
    return values.W_Bytes.from_charlist(chars)




# this duplicates, slightly differently, a definition in longlong2float.py
# using unsigned long long fixes an overflow issue
# this happens only in the interpreter, so we do it only then
def pycket_longlong2float(llval):
    """ NOT_RPYTHON """
    from rpython.rtyper.lltypesystem import lltype, rffi
    DOUBLE_ARRAY_PTR = lltype.Ptr(lltype.Array(rffi.DOUBLE))
    ULONGLONG_ARRAY_PTR = lltype.Ptr(lltype.Array(rffi.ULONGLONG))
    with lltype.scoped_alloc(DOUBLE_ARRAY_PTR.TO, 1) as d_array:
        ll_array = rffi.cast(ULONGLONG_ARRAY_PTR, d_array)
        ll_array[0] = llval
        floatval = d_array[0]
        return floatval

@expose("floating-point-bytes->real", [values.W_Bytes, default(values.W_Object, values.w_false)])
def float_bytes_to_real_(bstr, signed):
    bytes = bstr.as_bytes_list()
    return float_bytes_to_real(bytes, signed)

def float_bytes_to_real(bytes, signed):
    # XXX Currently does not make use of the signed parameter
    if len(bytes) not in (4, 8):
        raise SchemeException(
                "floating-point-bytes->real: byte string must have length 2, 4, or 8")

    try:
        if objectmodel.we_are_translated():
            val = rarithmetic.r_int64(0)
            for i, v in enumerate(bytes):
                val += rarithmetic.r_int64(ord(v)) << (i * 8)
            return values.W_Flonum(longlong2float.longlong2float(val))
        else:
            # use unsigned to avoid rlib bug
            val = rarithmetic.r_uint64(0)
            for i, v in enumerate(bytes):
                val += rarithmetic.r_uint64(ord(v)) << (i * 8)
            return values.W_Flonum(pycket_longlong2float(val))
    except OverflowError as e:
        # Uncomment the check below to run Pycket on the
        # interpreter with compiled (zo) files
        # (fasl makes a call that blows the longlong2float on rpython)

        # if val == 18442240474082181120L:
        #     return values.W_Flonum.NEGINF
        raise SchemeException("RPython overflow : %s" % e)

@expose("integer-bytes->integer",
        [values.W_Bytes,
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_false),
         default(values.W_Fixnum, values.W_Fixnum.ZERO),
         default(values.W_Fixnum, None)])
def integer_bytes_to_integer(bstr, signed, big_endian, w_start, w_end):
    bytes = bstr.as_bytes_list()
    return _integer_bytes_to_integer(bytes, signed, big_endian, w_start.value, w_end)

def _integer_bytes_to_integer(bytes, signed, big_endian, start=0, w_end=None):

    if w_end is None:
        end = len(bytes)
    else:
        end = w_end.value

    if not (0 <= start < len(bytes)):
        raise SchemeException(
                "integer-bytes->integer: start position not in byte string")
    if not (0 <= end <= len(bytes)):
        raise SchemeException(
                "integer-bytes->integer: end position not in byte string")
    if end < start:
        raise SchemeException(
                "integer-bytes->integer: end position less than start position")

    length = end - start
    if length not in (2, 4, 8):
        raise SchemeException(
                "integer-bytes->integer: byte string must have length 2, 4, or 8")

    if start != 0 or end != len(bytes):
        bytes = bytes[start:end]

    byteorder = "little" if big_endian is values.w_false else "big"
    is_signed = signed is not values.w_false
    big = rbigint.frombytes(bytes, byteorder, is_signed)
    try:
        result = values.W_Fixnum(big.toint())
    except OverflowError:
        result = values.W_Bignum(big)
    return result

@expose("integer->integer-bytes",
        [values.W_Number,
         values.W_Fixnum,
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_false),
         default(values.W_Bytes, None),
         default(values.W_Fixnum, values.W_Fixnum.ZERO)])
@jit.unroll_safe
def integer_to_integer_bytes(n, w_size, signed, big_endian, w_dest, w_start):
    from rpython.rtyper.lltypesystem import rffi
    if isinstance(n, values.W_Fixnum):
        intval = n.value
    elif isinstance(n, values.W_Bignum):
        raise NotImplementedError("not implemented yet")
    else:
        raise SchemeException("integer->integer-bytes: expected exact integer")

    size = jit.promote(w_size.value)
    if size not in (2, 4, 8):
        raise SchemeException("integer->integer-bytes: size not 2, 4, or 8")

    size  = size
    start = w_start.value
    if w_dest is not None:
        chars = w_dest.as_bytes_list()
        result = w_dest
    else:
        chars = ['\x00'] * size
        result = values.W_Bytes.from_charlist(chars, immutable=False)

    if start < 0:
        raise SchemeException(
            "integer->integer-bytes: start value less than zero")

    if start + size > len(chars):
        raise SchemeException(
            "integer->integer-bytes: byte string length is less than starting "
            "position plus size")

    is_signed = signed is not values.w_false
    for i in range(start, start+size):
        chars[i] = chr(intval & 0xFF)
        intval >>= 8

    if big_endian is values.w_false:
        return result

    # Swap the bytes if for big endian
    left = start
    right = start + size - 1
    while left < right:
        chars[left], chars[right] = chars[right], chars[left]
        left, right = left + 1, right - 1

    return result

@expose("integer-length", [values.W_Object])
@jit.elidable
def integer_length(obj):

    if isinstance(obj, values.W_Fixnum):
        val = obj.value
        if val < 0:
            val = ~val

        n = r_uint(val)
        result = 0
        while n:
            n >>= r_uint(1)
            result += 1
        return values.wrap(result)

    if isinstance(obj, values.W_Bignum):
        # XXX The bit_length operation on rbigints is off by one for negative
        # powers of two (this may be intentional?).
        # So, we detect this case and apply a correction.
        bignum = obj.value
        negative_power_of_two = True

        if not bignum.tobool():
            return values.W_Fixnum.ZERO
        elif bignum.get_sign() != -1:
            negative_power_of_two = False
        else:
            for i in range(bignum.numdigits() - 1):
                if bignum.udigit(i) != 0:
                    negative_power_of_two = False
                    break

            msd = bignum.udigit(r_uint(bignum.numdigits() - 1))
            while msd:
                if (msd & r_uint(0x1)) and msd != r_uint(1):
                    negative_power_of_two = False
                    break
                msd >>= r_uint(1)

        bit_length = bignum.bit_length()
        if negative_power_of_two:
            bit_length -= 1
        return values.wrap(bit_length)

    raise SchemeException("integer-length: expected exact-integer? got %s" % obj.tostring())

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

@expose("unsafe-flmin", [unsafe(values.W_Flonum)] * 2)
def unsafe_flmin(a, b):
    return values.W_Flonum(min(a.value, b.value))

@expose("unsafe-flmax", [unsafe(values.W_Flonum)] * 2)
def unsafe_flmax(a, b):
    return values.W_Flonum(max(a.value, b.value))

@expose("unsafe-flabs", [unsafe(values.W_Flonum)])
def unsafe_flabs(a):
    return values.W_Flonum(abs(a.value))

@expose("extflonum-available?", [])
def extflonum_available():
    return values.w_false

@expose("real-part", [values.W_Number])
def real_part(n):
    if isinstance(n, values.W_Complex):
        return n.real
    return n

@expose("imag-part", [values.W_Number])
def imag_part(n):
    if isinstance(n, values.W_Complex):
        return n.imag
    return values.W_Fixnum.ZERO
