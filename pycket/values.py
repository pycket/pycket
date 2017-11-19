#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket                   import config
from pycket.arity             import Arity
from pycket.base              import W_Object, W_ProtoObject, UnhashableType
from pycket.cont              import continuation, label, NilCont
from pycket.env               import ConsEnv
from pycket.error             import SchemeException
from pycket.prims.expose      import make_call_method
from pycket.small_list        import inline_small_list
from pycket.util              import add_copy_method, memoize_constructor

from rpython.tool.pairtype    import extendabletype
from rpython.rlib             import jit, runicode, rarithmetic, rweaklist
from rpython.rlib.rstring     import StringBuilder
from rpython.rlib.objectmodel import always_inline, r_dict, compute_hash, we_are_translated
from rpython.rlib.objectmodel import specialize, try_inline, import_from_mixin
from rpython.rlib.rarithmetic import r_longlong, intmask

import rpython.rlib.rweakref as weakref
from rpython.rlib.rbigint import rbigint, NULLRBIGINT
from rpython.rlib.debug import check_list_of_chars, make_sure_not_resized, check_regular_int

UNROLLING_CUTOFF = 5

@inline_small_list(immutable=True, attrname="vals", factoryname="_make")
class Values(W_ProtoObject):
    _attrs_ = []
    _immutable_ = True
    def __init__(self):
        pass

    @staticmethod
    def make(values_w):
        if len(values_w) == 1:
            return Values.make1(values_w[0])
        return Values._make(values_w)

    @staticmethod
    def make1(w_value):
        assert w_value is not None
        return w_value

    @staticmethod
    def make2(w_value1, w_value2):
        return Values._make2(w_value1, w_value2)

    def num_values(self):
        return self._get_size_list()

    def get_value(self, index):
        return self._get_list(index)

    def get_all_values(self):
        return self._get_full_list()

    def tostring(self):
        vals = self._get_full_list()
        if len(vals) == 1:
            return vals[0].tostring()
        if len(vals) == 0:
            return "(values)"
        else: #fixme
            return "MULTIPLE VALUES"


class W_Cell(W_Object): # not the same as Racket's box
    _attrs_ = ["w_value"]
    def __init__(self, v):
        assert not isinstance(v, W_Cell)
        if isinstance(v, W_Fixnum):
            v = W_CellIntegerStrategy(v.value)
        elif isinstance(v, W_Flonum):
            v = W_CellFloatStrategy(v.value)
        self.w_value = v

    def get_val(self):
        w_value = self.w_value
        if isinstance(w_value, W_CellIntegerStrategy):
            return W_Fixnum(w_value.value)
        elif isinstance(w_value, W_CellFloatStrategy):
            return W_Flonum(w_value.value)
        return w_value

    def set_val(self, w_value):
        from pycket import config
        if not config.strategies:
            self.w_value = w_value
            return
        if isinstance(w_value, W_Fixnum):
            w_v = self.w_value
            if isinstance(w_v, W_CellIntegerStrategy):
                w_v.value = w_value.value
            else:
                self.w_value = W_CellIntegerStrategy(w_value.value)
        elif isinstance(w_value, W_Flonum):
            w_v = self.w_value
            if isinstance(w_v, W_CellFloatStrategy):
                w_v.value = w_value.value
            else:
                self.w_value = W_CellFloatStrategy(w_value.value)
        else:
            self.w_value = w_value

class W_CellIntegerStrategy(W_Object):
    _attrs_ = ["value"]
    # can be stored in cells only, is mutated when a W_Fixnum is stored
    def __init__(self, value):
        self.value = value

class W_CellFloatStrategy(W_Object):
    _attrs_ = ["value"]
    # can be stored in cells only, is mutated when a W_Flonum is stored
    def __init__(self, value):
        self.value = value


class W_Undefined(W_Object):
    errorname = "unsafe-undefined"
    _attrs_ = []
    def __init__(self):
        pass

w_unsafe_undefined = W_Undefined()

# FIXME: not a real implementation
class W_Syntax(W_Object):
    _attrs_ = _immutable_fields_ = ["val"]
    errorname = "syntax"
    def __init__(self, o):
        self.val = o
    def tostring(self):
        return "#'%s" % self.val.tostring()

class W_ModulePathIndex(W_Object):
    errorname = "module-path-index"
    _attrs_ = []
    def __init__(self):
        pass
    def tostring(self):
        return "#<module-path-index>"

class W_ResolvedModulePath(W_Object):
    errorname = "resolved-module-path"
    _attrs_ = _immutable_fields_ = ["name"]
    def __init__(self, name):
        self.name = name
    def tostring(self):
        return "#<resolved-module-path:%s>" % self.name

class W_Logger(W_Object):
    errorname = "logger"

    _immutable_fields_ = ['topic', 'parent', 'propagate_level', 'propagate_topic[*]']
    _attrs_ = ['topic', 'parent', 'propagate_level', 'propagate_topic']

    def __init__(self, topic, parent, propagate_level, propagate_topic):
        self.topic           = topic
        self.parent          = parent
        self.propagate_level = propagate_level
        self.propagate_topic = propagate_topic

    def tostring(self):
        return "#<logger>"

class W_ContinuationPromptTag(W_Object):
    errorname = "continuation-prompt-tag"
    _attrs_ = _immutable_fields_ = ["name"]

    def __init__(self, name):
        self.name = name

    def tostring(self):
        if self.name is None:
            return "#<continuation-prompt-tag>"
        name = self.name.utf8value
        return "#<continuation-prompt-tag:%s>" % name

w_default_continuation_prompt_tag = W_ContinuationPromptTag(None)

class W_ContinuationMarkSet(W_Object):
    errorname = "continuation-mark-set"

    _attrs_ = _immutable_fields_ = ["cont", "prompt_tag"]

    def __init__(self, cont, prompt_tag):
        self.cont = cont
        self.prompt_tag = prompt_tag

    def tostring(self):
        return "#<continuation-mark-set>"

class W_ContinuationMarkKey(W_Object):
    errorname = "continuation-mark-key"
    _attrs_ = _immutable_fields_ = ["name"]
    def __init__(self, name):
        self.name = name

    def get_cmk(self, value, env, cont):
        from pycket.interpreter import return_value
        return return_value(value, env, cont)

    def set_cmk(self, body, value, update, env, cont):
        update.update_cm(self, value)
        return body.call([], env, cont)

    def tostring(self):
        return "#<continuation-mark-name>"

class W_VariableReference(W_Object):
    errorname = "variable-reference"
    _attrs_ = ['varref']
    def __init__(self, varref):
        self.varref = varref
    def tostring(self):
        return "#<#%variable-reference>"

# A super class for both fl/fx/regular vectors
class W_VectorSuper(W_Object):
    errorname = "vector"
    _attrs_ = []
    def __init__(self):
        raise NotImplementedError("abstract base class")

    def vector_set(self, i, new, env, cont, app=None):
        raise NotImplementedError("abstract base class")

    def vector_ref(self, i, env, cont, app=None):
        raise NotImplementedError("abstract base class")

    def length(self):
        raise NotImplementedError("abstract base class")

    def immutable(self):
        raise NotImplementedError("abstract base class")

    # abstract methods for vector implementations that use strategies
    # we would really not like to have them here, but would need multiple
    # inheritance to express that
    # impersonators can just not implement them

    def get_storage(self):
        raise NotImplementedError

    def set_storage(self, storage):
        raise NotImplementedError

    def get_strategy(self):
        raise NotImplementedError

    def set_strategy(self, strategy):
        raise NotImplementedError

# Things that are vector?
class W_MVector(W_VectorSuper):
    errorname = "vector"
    _attrs_ = []

class W_List(W_Object):
    errorname = "list"
    _attrs_ = []
    def __init__(self):
        raise NotImplementedError("abstract base class")

class W_Cons(W_List):
    "Abstract for specialized conses. Concrete general in W_WrappedCons"
    errorname = "pair"
    _attrs_ = []
    @staticmethod
    @specialize.arg(2)
    def make(car, cdr, force_proper=False):
        from pycket import config
        if not config.type_size_specialization:
            if cdr.is_proper_list():
                return W_WrappedConsProper(car, cdr)
            return W_WrappedCons(car, cdr)
        elif isinstance(car, W_Fixnum):
            if force_proper or cdr.is_proper_list():
                return W_UnwrappedFixnumConsProper(car.value, cdr)
            return W_UnwrappedFixnumCons(car.value, cdr)
        elif isinstance(car, W_Flonum):
            if force_proper or cdr.is_proper_list():
                return W_UnwrappedFlonumConsProper(car.value, cdr)
            return W_UnwrappedFlonumCons(car.value, cdr)
        else:
            if force_proper or cdr.is_proper_list():
                return W_WrappedConsProper(car, cdr)
            return W_WrappedCons(car, cdr)

    def car(self):
        raise NotImplementedError("abstract base class")

    def cdr(self):
        raise NotImplementedError("abstract base class")

    def tostring(self):
        cur = self
        acc = []
        while isinstance(cur, W_Cons):
            acc.append(cur.car().tostring())
            cur = cur.cdr()
        # Are we a dealing with a proper list?
        if cur is w_null:
            return "(%s)" % " ".join(acc)
        # Must be an improper list
        return "(%s . %s)" % (" ".join(acc), cur.tostring())

    def immutable(self):
        return True

    def hash_equal(self, info=None):
        x = 0x345678
        while isinstance(self, W_Cons):
            car, self = self.car(), self.cdr()
            y = car.hash_equal(info=info)
            x = rarithmetic.intmask((1000003 * x) ^ y)
        return x

    def equal(self, other):
        if not isinstance(other, W_Cons):
            return False
        if self is other:
            return True
        w_curr1 = self
        w_curr2 = other
        while isinstance(w_curr1, W_Cons) and isinstance(w_curr2, W_Cons):
            if not w_curr1.car().equal(w_curr2.car()):
                return False
            w_curr1 = w_curr1.cdr()
            w_curr2 = w_curr2.cdr()
        return w_curr1.equal(w_curr2)

    def _unsafe_set_cdr(self, val):
        raise NotImplementedError("abstract base class")

    def clone(self):
        raise NotImplementedError("abstract base class")

@add_copy_method(copy_method="clone")
class W_UnwrappedFixnumCons(W_Cons):
    _attrs_ = _immutable_fields_ = ["_car", "_cdr"]
    def __init__(self, a, d):
        self._car = a
        self._cdr = d

    def car(self):
        return W_Fixnum(self._car)

    def cdr(self):
        return self._cdr

    def _unsafe_set_cdr(self, val):
        self._cdr = val

@add_copy_method(copy_method="clone")
class W_UnwrappedFixnumConsProper(W_UnwrappedFixnumCons):
    def is_proper_list(self):
        return True

@add_copy_method(copy_method="clone")
class W_UnwrappedFlonumCons(W_Cons):
    _immutable_fields_ = ["_car", "_cdr"]
    def __init__(self, a, d):
        self._car = a
        self._cdr = d

    def car(self):
        return W_Flonum(self._car)

    def cdr(self):
        return self._cdr

    def _unsafe_set_cdr(self, val):
        self._cdr = val

@add_copy_method(copy_method="clone")
class W_UnwrappedFlonumConsProper(W_UnwrappedFlonumCons):
    def is_proper_list(self):
        return True

@add_copy_method(copy_method="clone")
class W_WrappedCons(W_Cons):
    _attrs_ = _immutable_fields_ = ["_car", "_cdr"]
    def __init__(self, a, d):
        self._car = a
        self._cdr = d

    def car(self):
        return self._car

    def cdr(self):
        return self._cdr

    def _unsafe_set_cdr(self, val):
        self._cdr = val

@add_copy_method(copy_method="clone")
class W_WrappedConsProper(W_WrappedCons):
    def is_proper_list(self):
        return True

class W_WrappedConsMaybe(W_WrappedCons):
    def is_proper_list(self):
        return self._cdr.is_proper_list()

class W_Box(W_Object):
    errorname = "box"
    _attrs_ = []
    def __init__(self):
        raise NotImplementedError("abstract base class")

    def hash_equal(self, info=None):
        raise UnhashableType

    def unbox(self, env, cont):
        raise NotImplementedError("abstract base class")

    def set_box(self, val, env, cont):
        raise NotImplementedError("abstract base class")

class W_MBox(W_Box):
    errorname = "mbox"
    _attrs_ = ['value']
    def __init__(self, value):
        self.value = value

    def unbox(self, env, cont):
        from pycket.interpreter import return_value
        return return_value(self.value, env, cont)

    def set_box(self, val, env, cont):
        from pycket.interpreter import return_value
        self.value = val
        return return_value(w_void, env, cont)

    def tostring(self):
        return "'#&%s" % self.value.tostring()

class W_IBox(W_Box):
    errorname = "ibox"
    _attrs_ = _immutable_fields_ = ["value"]

    def __init__(self, value):
        self.value = value

    def immutable(self):
        return True

    def unbox(self, env, cont):
        from pycket.interpreter import return_value
        return return_value(self.value, env, cont)

    def set_box(self, val, env, cont):
        raise SchemeException("set-box!: not supported on immutable boxes")

    def tostring(self):
        return "'#&%s" % self.value.tostring()

# A weak box does not test as a box for most operations and cannot be
# chaperoned/impersonated, so we start it from W_Object rather than W_Box.
class W_WeakBox(W_Object):
    errorname = "weak-box"
    _attrs_ = _immutable_fields_ = ["value"]

    def __init__(self, value):
        assert isinstance(value, W_Object)
        self.value = weakref.ref(value)

    def get(self):
        return self.value()

    def tostring(self):
        return "#<weak-box>"

class W_Ephemeron(W_Object):
    errorname = "ephemeron"
    _attrs_ = _immutable_fields_ = ["key", "mapping"]

    def __init__(self, key, value):
        assert isinstance(key, W_Object)
        assert isinstance(value, W_Object)
        self.key = weakref.ref(key)
        self.mapping = weakref.RWeakKeyDictionary(W_Object, W_Object)
        self.mapping.set(key, value)

    def get(self):
        return self.mapping.get(self.key())

    def tostring(self):
        return "#<ephemeron>"

class W_Placeholder(W_Object):
    errorname = "placeholder"
    _attrs_ = ['value']
    def __init__(self, value):
        self.value = value
    def tostring(self):
        return "#<placeholder>"

class W_HashTablePlaceholder(W_Object):
    errorname = "hash-table-placeholder"
    _attrs_ = []
    def __init__(self, keys, vals):
        pass
    def tostring(self):
        return "#<hash-table-placeholder>"

class W_MList(W_Object):
    errorname = "mlist"
    _attrs_ = []
    def __init__(self):
        raise NotImplementedError("abstract base class")

class W_MCons(W_MList):
    errorname = "mpair"
    _attrs_ = ["_car", "_cdr"]
    def __init__(self, a, d):
        self._car = a
        self._cdr = d
    def tostring(self):
        return "(mcons %s %s)" % (self.car().tostring(), self.cdr().tostring())
    def car(self):
        return self._car
    def cdr(self):
        return self._cdr
    def set_car(self, a):
        self._car = a
    def set_cdr(self, d):
        self._cdr = d

class W_Number(W_Object):
    _attrs_ = []
    errorname = "number"
    def __init__(self):
        raise NotImplementedError("abstract base class")

    def immutable(self):
        return True

    def eqv(self, other):
        return self.equal(other)

    def hash_eqv(self):
        return self.hash_equal(info=None)

class W_Real(W_Number):
    errorname = "real"
    _attrs_ = []

class W_Rational(W_Real):
    _attrs_ = _immutable_fields_ = ["_numerator", "_denominator"]
    errorname = "rational"
    def __init__(self, num, den):
        assert isinstance(num, rbigint)
        assert isinstance(den, rbigint)
        self._numerator = num
        self._denominator = den
        if not we_are_translated():
            assert den.gt(NULLRBIGINT)

    @staticmethod
    def make(num, den):
        if isinstance(num, W_Fixnum):
            num = rbigint.fromint(num.value)
        else:
            assert isinstance(num, W_Bignum)
            num = num.value
        if isinstance(den, W_Fixnum):
            den = rbigint.fromint(den.value)
        else:
            assert isinstance(den, W_Bignum)
            den = den.value
        return W_Rational.frombigint(num, den)

    @staticmethod
    def fromint(n, d=1, need_to_check=True):
        assert isinstance(n, int)
        assert isinstance(d, int)
        from fractions import gcd
        g = gcd(n, d)
        n = n // g
        d = d // g
        if need_to_check and d == 1:
            return W_Fixnum(n)
        return W_Rational(rbigint.fromint(n), rbigint.fromint(d))

    @staticmethod
    def frombigint(n, d=rbigint.fromint(1), need_to_check=True):
        from pycket.arithmetic import gcd
        g = gcd(n, d)
        n = n.floordiv(g)
        d = d.floordiv(g)
        if need_to_check and d.eq(rbigint.fromint(1)):
            return W_Bignum.frombigint(n)
        return W_Rational(n, d)

    @staticmethod
    def fromfloat(f):
        # FIXME: this is the temporary not exact implementation
        assert isinstance(f, float)
        d = 1000000
        n = int(f * d)
        from fractions import gcd
        _gcd = gcd(n, d)
        return W_Rational.fromint(n/_gcd, d/_gcd)

    def tostring(self):
        return "%s/%s" % (self._numerator.str(), self._denominator.str())

    def equal(self, other):
        if not isinstance(other, W_Rational):
            return False
        return (self._numerator.eq(other._numerator) and
                self._denominator.eq(other._denominator))

    def hash_equal(self, info=None):
        hash1 = self._numerator.hash()
        hash2 = self._denominator.hash()
        return rarithmetic.intmask(hash1 + 1000003 * hash2)

class W_Integer(W_Real):
    errorname = "integer"
    _attrs_ = []

    def toint(self):
        raise NotImplementedError("abstract base class")

    @staticmethod
    def frombigint(value):
        try:
            num = value.toint()
        except OverflowError:
            pass
        else:
            return W_Fixnum(num)
        return W_Bignum(value)

    @staticmethod
    def fromfloat(value):
        try:
            val = rarithmetic.ovfcheck_float_to_int(value)
        except OverflowError:
            return W_Bignum(rbigint.fromfloat(value))
        return W_Fixnum(val)

@memoize_constructor
class W_Fixnum(W_Integer):

    _immutable_ = True
    _attrs_ = _immutable_fields_ = ["value"]
    errorname = "fixnum"

    MIN_INTERNED   = -5
    MAX_INTERNED   = 256
    INTERNED_RANGE = (MIN_INTERNED, MAX_INTERNED)
    cache = []

    def tostring(self):
        return str(self.value)

    def __init__(self, val):
        if not we_are_translated():
            # this is not safe during translation
            assert isinstance(val, int)
        check_regular_int(val)
        self.value = val

    def toint(self):
        return self.value

    def equal(self, other):
        if not isinstance(other, W_Fixnum):
            return False
        return self.value == other.value

    def hash_equal(self, info=None):
        return self.value

    @staticmethod
    @try_inline
    def make_or_interned(val):
        from rpython.rlib.rarithmetic import int_between
        if int_between(W_Fixnum.MIN_INTERNED, val, W_Fixnum.MAX_INTERNED):
            return W_Fixnum.cache[val - W_Fixnum.MIN_INTERNED]
        return W_Fixnum(val)

W_Fixnum.ZERO = W_Fixnum.make(0)
W_Fixnum.ONE  = W_Fixnum.make(1)
W_Fixnum.TWO  = W_Fixnum.make(2)
W_Fixnum.cache = map(W_Fixnum.make, range(*W_Fixnum.INTERNED_RANGE))

class W_Flonum(W_Real):
    _immutable_ = True
    _attrs_ = _immutable_fields_ = ["value"]
    errorname = "flonum"

    def __init__(self, val):
        self.value = val

    @staticmethod
    def make(val):
        return W_Flonum(val)

    def tostring(self):
        from rpython.rlib.rfloat import formatd, DTSF_STR_PRECISION, DTSF_ADD_DOT_0
        return formatd(self.value, 'g', DTSF_STR_PRECISION, DTSF_ADD_DOT_0)

    def hash_equal(self, info=None):
        return compute_hash(self.value)

    def equal(self, other):
        from rpython.rlib.longlong2float import float2longlong
        import math
        if not isinstance(other, W_Flonum):
            return False
        v1 = self.value
        v2 = other.value
        ll1 = float2longlong(v1)
        ll2 = float2longlong(v2)
        # Assumes that all non-NaN values are canonical
        return ll1 == ll2 or (math.isnan(v1) and math.isnan(v2))

W_Flonum.ZERO   = W_Flonum(0.0)
W_Flonum.INF    = W_Flonum(float("inf"))
W_Flonum.NEGINF = W_Flonum(-float("inf"))
W_Flonum.NAN    = W_Flonum(float("nan"))

class W_Bignum(W_Integer):
    _immutable_ = True
    _attrs_ = _immutable_fields_ = ["value"]

    def tostring(self):
        return self.value.str()

    def __init__(self, val):
        self.value = val

    def toint(self):
        """ raises OverflowError on failure """
        return self.value.toint()

    def toflonum(self):
        bignum = self.value
        try:
            floatval = bignum.tofloat()
        except OverflowError:
            return W_Flonum.NEGINF if bignum.sign < 0 else W_Flonum.INF
        return W_Flonum(floatval)

    def equal(self, other):
        if not isinstance(other, W_Bignum):
            return False
        return self.value.eq(other.value)

    def hash_equal(self, info=None):
        return self.value.hash()

@memoize_constructor
class W_Complex(W_Number):
    _immutable_ = True
    _attrs_ = _immutable_fields_ = ["real", "imag"]
    def __init__(self, re, im):
        assert isinstance(re, W_Real)
        assert isinstance(im, W_Real)
        self.real = re
        self.imag = im

    @staticmethod
    def from_real_pair(real, imag):
        if W_Fixnum.ZERO.eqv(imag):
            return real
        return W_Complex(real, imag)

    def eqv(self, other):
        if not isinstance(other, W_Complex):
            return False
        return self.real.eqv(other.real) and self.imag.eqv(other.imag)

    def hash_equal(self, info=None):
        hash1 = self.real.hash_equal()
        hash2 = self.imag.hash_equal()
        return rarithmetic.intmask(hash1 + 1000003 * hash2)

    def tostring(self):
        return "%s+%si" % (self.real.tostring(), self.imag.tostring())

@memoize_constructor
class W_Character(W_Object):
    _attrs_ = _immutable_fields_ = ["value"]
    errorname = "char"
    def __init__(self, val):
        self.value = val

    def tostring(self):
        return "#\\%s" % runicode.unicode_encode_utf_8(
                self.value, len(self.value), "strict")

    def immutable(self):
        return True

    def eqv(self, other):
        if not isinstance(other, W_Character):
            return False
        return self.value == other.value

    def hash_eqv(self):
        return ord(self.value)

    def hash_equal(self, info=None):
        return self.hash_eqv()


class W_Thread(W_Object):
    errorname = "thread"
    _attrs_ = []
    def __init__(self):
        pass
    def tostring(self):
        return "#<thread>"

class W_Semaphore(W_Object):
    errorname = "semaphore"
    _attrs_ = ['n']
    def __init__(self, n):
        self.n = n
    def post(self):
        self.n += 1
    def wait(self):
        if self.n >= 1:
            return
        else:
            raise SchemeException("Waiting for a semaphore will never finish")
    def tostring(self):
        return "#<semaphore>"

class W_Evt(W_Object):
    errorname = "evt"
    _attrs_ = []

class W_SemaphorePeekEvt(W_Evt):
    errorname = "semaphore-peek-evt"
    _attrs_ = _immutable_fields_ = ["sema"]
    def __init__(self, sema):
        self.sema = sema
    def tostring(self):
        return "#<semaphore-peek-evt>"

class W_PseudoRandomGenerator(W_Object):
    errorname = "pseudo-random-generator"
    _attrs_ = []
    def __init__(self):
        pass

class W_Path(W_Object):
    errorname = "path"
    _attrs_ = _immutable_fields_ = ["path"]
    def __init__(self, p):
        self.path = p
    def equal(self, other):
        if not isinstance(other, W_Path):
            return False
        return self.path == other.path
    def tostring(self):
        return "#<path:%s>" % self.path

class W_Void(W_Object):
    _attrs_ = []
    def __init__(self):
        pass
    def tostring(self):
        return "#<void>"

class W_Null(W_List):
    _attrs_ = []
    def __init__(self):
        pass

    def tostring(self):
        return "()"

    def is_proper_list(self):
        return True

w_void = W_Void()
w_null = W_Null()

class W_Bool(W_Object):
    errorname = "boolean"
    _attrs_ = []
    @staticmethod
    def make(b):
        if b: return w_true
        else: return w_false

    def __init__(self):
        """ NOT_RPYTHON """
        # the previous line produces an error if somebody makes new bool
        # objects from primitives
        pass

    def tostring(self):
        return "#t" if self is w_true else "#f"

w_false = W_Bool()
w_true = W_Bool()

class ThreadCellTable(rweaklist.RWeakListMixin):
    def __init__(self):
        self.initialize()

    def __iter__(self):
        handles = self.get_all_handles()
        for ref in handles:
            val = ref()
            if val is not None:
                yield val

class W_ThreadCellValues(W_Object):
    errorname = "thread-cell-values"
    _immutable_fields_ = ["assoc"]
    _attrs_ = ["assoc", "value"]
    def __init__(self):
        self.assoc = {}
        for threadcell in W_ThreadCell._table:
            if threadcell.preserved:
                self.assoc[threadcell] = threadcell.value

class W_ThreadCell(W_Object):
    errorname = "thread-cell"
    _immutable_fields_ = ["initial", "preserved"]
    _attrs_ = ["initial", "preserved", "value"]
    # All the thread cells in the system
    # TODO: Use a weak list to store the existing thread cells
    _table = ThreadCellTable()

    def __init__(self, val, preserved):
        # TODO: This should eventually be a mapping from thread ids to values
        self.value = val
        self.initial = val
        self.preserved = preserved

        W_ThreadCell._table.add_handle(self)

    def set(self, val):
        self.value = val

    def get(self):
        return self.value

class BytesMixin(object):

    def tostring(self):
        # TODO: No printable byte values should be rendered as base 8
        return "#\"%s\"" % "".join(["\\%d" % ord(i) for i in self.value])

    def as_bytes_list(self):
        return self.value

    def equal(self, other):
        if not isinstance(other, W_Bytes):
            return False
        b1 = self.as_bytes_list()
        b2 = other.as_bytes_list()
        return b1 == b2

    def hash_equal(self, info=None):
        from rpython.rlib.rarithmetic import intmask
        # like CPython's string hash
        s = self.value
        length = len(s)
        if length == 0:
            return -1
        x = ord(s[0]) << 7
        i = 0
        while i < length:
            x = intmask((1000003*x) ^ ord(s[i]))
            i += 1
        x ^= length
        return intmask(x)

    def ref(self, n):
        l = len(self.value)
        if n < 0 or n >= l:
            raise SchemeException("bytes-ref: index %s out of bounds for length %s"% (n, l))
        return W_Fixnum(ord(self.value[n]))

    def ref_char(self, n):
        l = len(self.value)
        if n < 0 or n >= l:
            raise SchemeException("bytes-ref: index %s out of bounds for length %s"% (n, l))
        return self.value[n]

    def as_str(self):
        return "".join(self.value)

    def getslice(self, start, end):
        assert start >= 0 and end >= 0
        bytes = self.value
        return bytes[start:end]

    def length(self):
        return len(self.value)

class W_Bytes(W_Object):
    errorname = "bytes"
    _immutable_fields_ = []
    _attrs_ = []

    def __init__(self, bs):
        raise NotImplementedError("abstract base class")

    def as_bytes_list(self):
        raise NotImplementedError("abstract base class")

    def length(self):
        raise NotImplementedError("abstract base class")

    @staticmethod
    def from_string(str, immutable=True):
        if immutable:
            return W_ImmutableBytes(list(str))
        else:
            return W_MutableBytes(list(str))

    @staticmethod
    def from_charlist(chars, immutable=True):
        if immutable:
            return W_ImmutableBytes(chars)
        else:
            return W_MutableBytes(chars)

    def tostring(self):
        raise NotImplementedError("abstract base class")

    def equal(self, other):
        raise NotImplementedError("abstract base class")

    def hash_equal(self, info=None):
        raise NotImplementedError("abstract base class")

    def immutable(self):
        raise NotImplementedError("abstract base class")

    def ref(self, n):
        raise NotImplementedError("abstract base class")

    def ref_char(self, n):
        raise NotImplementedError("abstract base class")

    def set(self, n, v):
        raise NotImplementedError("abstract base class")

    def set_char(self, n, v):
        raise NotImplementedError("abstract base class")

    def as_str(self):
        raise NotImplementedError("abstract base class")

    def getslice(self, start, end):
        raise NotImplementedError("abstract base class")

class W_MutableBytes(W_Bytes):
    errorname = "bytes"
    _attrs_ = ['value']
    _immutable_fields_ = ['value']

    import_from_mixin(BytesMixin)

    def __init__(self, bs):
        assert bs is not None
        self.value = check_list_of_chars(bs)
        make_sure_not_resized(self.value)

    def immutable(self):
        return False

    def set(self, n, v):
        l = len(self.value)
        if n < 0 or n >= l:
            raise SchemeException("bytes-set!: index %s out of bounds for length %s"% (n, l))
        self.value[n] = chr(v)

    def set_char(self, n, v):
        l = len(self.value)
        assert n >= 0 and n < len(self.value)
        self.value[n] = v

class W_ImmutableBytes(W_Bytes):
    errorname = "bytes"
    _attrs_ = ['value']
    _immutable_fields_ = ['value[*]']

    import_from_mixin(BytesMixin)

    def __init__(self, bs):
        assert bs is not None
        self.value = check_list_of_chars(bs)
        make_sure_not_resized(self.value)

    def immutable(self):
        return True

    def set(self, n, v):
        raise SchemeException("bytes-set!: can't mutate immutable bytes")

    def set_char(self, n, v):
        assert False

DEFINITELY_NO, MAYBE, DEFINITELY_YES = (-1, 0, 1)

class W_Symbol(W_Object):
    errorname = "symbol"
    _attrs_ = ["unreadable", "_isascii", "_unicodevalue", "utf8value"]
    _immutable_fields_ = ["unreadable", "utf8value"]

    def __init__(self, val, unreadable=False):
        assert isinstance(val, str)
        self._unicodevalue = None
        self.utf8value = val
        self.unreadable = unreadable
        self._isascii = MAYBE

    @staticmethod
    def _cache_is_ascii(self):
        from pycket.values_string import _is_ascii
        if not we_are_translated():
            assert self._isascii == MAYBE
        if _is_ascii(self.utf8value):
            self._isascii = DEFINITELY_YES
        else:
            self._isascii = DEFINITELY_NO
        return self._isascii

    def asciivalue(self):
        isascii = jit.conditional_call_elidable(
            self._isascii, W_Symbol._cache_is_ascii, self)
        if isascii == DEFINITELY_NO:
            return None
        return self.utf8value

    @jit.elidable
    def unicodevalue(self):
        if self._unicodevalue is None:
            self._unicodevalue = self.utf8value.decode("utf-8")
        return self._unicodevalue

    @staticmethod
    @jit.elidable
    def make(string):
        # This assert statement makes the lowering phase of rpython break...
        # Maybe comment back in and check for bug.
        assert isinstance(string, str)
        w_result = W_Symbol.all_symbols.get(string, None)
        if w_result is None:
            w_result = W_Symbol(string)
            W_Symbol.all_symbols[string] = w_result
        return w_result

    @staticmethod
    @jit.elidable
    def make_unreadable(string):
        w_result = W_Symbol.unreadable_symbols.get(string, None)
        if w_result is None:
            w_result = W_Symbol(string, unreadable=True)
            W_Symbol.unreadable_symbols[string] = w_result
        return w_result

    def __repr__(self):
        return self.utf8value

    @jit.elidable
    def is_interned(self):
        string = self.utf8value
        symbol = W_Symbol.all_symbols.get(string, None)
        if symbol is self:
            return True
        symbol = W_Symbol.unreadable_symbols.get(string, None)
        if symbol is self:
            return True
        return False

    def tostring(self):
        return "%s" % self.utf8value

    def variable_name(self):
        return self.utf8value

# According to samth, its not safe to use a weak table for symbols
W_Symbol.all_symbols = {}
W_Symbol.unreadable_symbols = {}

# XXX what are these for?
break_enabled_key = W_Symbol("break-enabled-key")
exn_handler_key = W_Symbol("exnh")
parameterization_key = W_Symbol("parameterization")

class W_Keyword(W_Object):
    errorname = "keyword"
    _attrs_=  _immutable_fields_ = ["value"]
    all_symbols = {}
    @staticmethod
    def make(string):
        # This assert statement makes the lowering phase of rpython break...
        # Maybe comment back in and check for bug.
        #assert isinstance(string, str)
        w_result = W_Keyword.all_symbols.get(string, None)
        if w_result is None:
            W_Keyword.all_symbols[string] = w_result = W_Keyword(string)
        return w_result
    def __repr__(self):
        return self.value
    def __init__(self, val):
        self.value = val
    def tostring(self):
        return "'#:%s" % self.value

class W_Procedure(W_Object):
    _attrs_ = []
    def __init__(self):
        raise NotImplementedError("Abstract base class")
    def iscallable(self):
        return True
    def immutable(self):
        return True
    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)
    def call_with_extra_info(self, args, env, cont, app):
        return self.call(args, env, cont)
    def tostring(self):
        return "#<procedure>"


class W_AssignmentTransformer(W_Object):
    _attrs_ = []
    def __init__(self):
        raise NotImplementedError("Abstract base class")


# These next two classes allow for a uniform input to the `set_cmk` operation.
# They are procedures which do the appropriate processing after `set_cmk` is done
# computing.
# This is needed because with-continuation-mark operates over the AST while
# W_InterposeProcedure can do a `set_cmk` with a closure.
class W_ThunkBodyCMK(W_Procedure):
    _attrs_ = _immutable_fields_ = ["body"]

    def __init__(self, body):
        self.body = body

    @make_call_method([], simple=False)
    def call(self, env, cont):
        return self.body, env, cont

class W_ThunkProcCMK(W_Procedure):
    _attrs_ = _immutable_fields_ = ["proc", "args"]

    def __init__(self, proc, args):
        self.proc = proc
        self.args = args

    @label
    @make_call_method([], simple=False)
    def call(self, env, cont):
        return self.proc.call(self.args, env, cont)


class W_Prim(W_Procedure):
    _attrs_ = _immutable_fields_ = ["name", "code", "arity", "result_arity", "simple1", "simple2"]

    def __init__ (self, name, code, arity=Arity.unknown, result_arity=None, simple1=None, simple2=None):
        self.name = W_Symbol.make(name)
        self.code = code
        assert isinstance(arity, Arity)
        self.arity = arity
        self.result_arity = result_arity
        self.simple1 = simple1
        self.simple2 = simple2

    def get_arity(self, promote=False):
        if promote:
            self = jit.promote(self)
        return self.arity

    def get_result_arity(self):
        return self.result_arity

    def call_with_extra_info(self, args, env, cont, extra_call_info):
        jit.promote(self)
        return self.code(args, env, cont, extra_call_info)

    def tostring(self):
        return "#<procedure:%s>" % self.name.variable_name()

@always_inline
def to_list(l, start=0):
    return to_improper(l, w_null, start=start)

def to_improper(l, curr, start=0):
    return to_improper_impl(l, curr, start)

@jit.look_inside_iff(
    lambda l, curr, start: jit.loop_unrolling_heuristic(l, len(l) - start, UNROLLING_CUTOFF))
def to_improper_impl(l, curr, start):
    assert start >= 0
    for i in range(len(l) - 1, start - 1, -1):
        curr = W_Cons.make(l[i], curr)
    return curr

@jit.look_inside_iff(lambda v, curr: v.unrolling_heuristic())
def vector_to_improper(v, curr):
    for i in range(v.len - 1, -1, -1):
        curr = W_Cons.make(v.ref(i), curr)
    return curr

def to_mlist(l):
    return to_mimproper(l, w_null)

@jit.look_inside_iff(
    lambda l, curr: jit.loop_unrolling_heuristic(l, len(l), UNROLLING_CUTOFF))
def to_mimproper(l, curr):
    for i in range(len(l) - 1, -1, -1):
        curr = W_MCons(l[i], curr)
    return curr

def from_list_unroll_pred(lst, idx, unroll_to=0, force=False):
    if not jit.we_are_jitted():
        return False
    if unroll_to == -1:
        return False
    if force:
        return idx > unroll_to
    else:
        return not jit.isvirtual(lst) and idx > unroll_to

@jit.elidable
def from_list_elidable(w_curr):
    result = []
    while isinstance(w_curr, W_Cons):
        result.append(w_curr.car())
        w_curr = w_curr.cdr()
    if w_curr is w_null:
        return result[:] # copy to make result non-resizable
    else:
        raise SchemeException("Expected list, but got something else")

@jit.unroll_safe
@specialize.arg(2)
def from_list(w_curr, unroll_to=0, force=False):
    result = []
    n = 0
    while isinstance(w_curr, W_Cons):
        if from_list_unroll_pred(w_curr, n, unroll_to=unroll_to, force=force):
            return result + from_list_elidable(w_curr)
        result.append(w_curr.car())
        w_curr = w_curr.cdr()
        n += 1
    if w_curr is w_null:
        return result[:] # copy to make result non-resizable
    else:
        raise SchemeException("Expected list, but got something else")

def from_list_iter(lst):
    if not lst.is_proper_list():
        raise SchemeException("Expected a list")
    while isinstance(lst, W_Cons):
        val, lst = lst.car(), lst.cdr()
        yield val
    assert lst is w_null, "is_proper_list lied"

class W_Continuation(W_Procedure):
    errorname = "continuation"

    _attrs_ = _immutable_fields_ = ["cont", "prompt_tag"]

    escape = False

    def __init__(self, cont, prompt_tag=None):
        self.cont = cont
        self.prompt_tag = prompt_tag

    def get_arity(self, promote=False):
        # FIXME: see if Racket ever does better than this
        return Arity.unknown

    def call(self, args, env, cont):
        from pycket.prims.control import install_continuation
        return install_continuation(self.cont, self.prompt_tag, args, env, cont,
                                    escape=self.escape)

    def tostring(self):
        return "#<continuation>"

class W_EscapeContinuation(W_Continuation):
    _attrs_ = []
    escape = True

class W_ComposableContinuation(W_Procedure):
    errorname = "composable-continuation"

    _attrs_ = _immutable_fields_ = ["cont", "prompt_tag"]

    def __init__(self, cont, prompt_tag=None):
        self.cont = cont
        self.prompt_tag = prompt_tag

    def get_arity(self, promote=False):
        return Arity.unknown

    def call(self, args, env, cont):
        from pycket.prims.control import install_continuation
        return install_continuation(
                self.cont, self.prompt_tag, args, env, cont, extend=True)

    def tostring(self):
        return "#<continuation>"

@inline_small_list(immutable=True, attrname="envs", factoryname="_make")
class W_Closure(W_Procedure):
    _immutable_ = True
    _immutable_fields_ = ["caselam"]
    _attrs_ = ["caselam", "current_linklet_instance"]

    @jit.unroll_safe
    def __init__(self, caselam, env):
        self.caselam = caselam
        for (i,lam) in enumerate(caselam.lams):
            vals = lam.collect_frees(caselam.recursive_sym, env, self)
            self._set_list(i, ConsEnv.make(vals, env.toplevel_env()))
        self.current_linklet_instance = env.get_current_linklet_instance()

    def enable_jitting(self):
        self.caselam.enable_jitting()

    def tostring(self):
        return self.caselam.tostring_as_closure()

    @staticmethod
    @jit.unroll_safe
    def make(caselam, env):
        from pycket.interpreter import CaseLambda
        assert isinstance(caselam, CaseLambda)
        num_lams = len(caselam.lams)
        if num_lams == 1 and caselam.any_frees:
            env_size = len(caselam.lams[0].frees.elems)
            vals = caselam.lams[0].collect_frees_without_recursive(
                    caselam.recursive_sym, env)
            return W_Closure1AsEnv.make(vals, caselam, env.toplevel_env())
        envs = [None] * num_lams
        return W_Closure._make(envs, caselam, env)

    def get_arity(self, promote=False):
        caselam = self.caselam
        if promote:
            caselam = jit.promote(caselam)
        return self.caselam.get_arity()

    @jit.unroll_safe
    def _find_lam(self, args):
        jit.promote(self.caselam)
        for i, lam in enumerate(self.caselam.lams):
            actuals = lam.match_args(args)
            if actuals is not None:
                frees = self._get_list(i)
                return actuals, frees, lam
        if len(self.caselam.lams) == 1:
            single_lambda = self.caselam.lams[0]
            single_lambda.raise_nice_error(args)
        raise SchemeException("No matching arity in case-lambda")

    def call_with_extra_info(self, args, env, cont, calling_app):
        env_structure = None
        if calling_app is not None:
            env_structure = calling_app.env_structure
        jit.promote(self.caselam)
        jit.promote(env_structure)
        (actuals, frees, lam) = self._find_lam(args)
        if not jit.we_are_jitted() and env.pycketconfig().callgraph:
            env.toplevel_env().callgraph.register_call(lam, calling_app, cont, env)
        # specialize on the fact that often we end up executing in the
        # same environment.
        prev = lam.env_structure.prev.find_env_in_chain_speculate(
                frees, env_structure, env)
        curr_linkl_inst = self.current_linklet_instance
        return lam.make_begin_cont(
            ConsEnv.make(actuals, prev, curr_linkl_inst),
            cont)

    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

@inline_small_list(immutable=True, attrname="vals", factoryname="_make", unbox_num=True, nonull=True)
class W_Closure1AsEnv(ConsEnv):
    _immutable_ = True
    _attrs_ = _immutable_fields_ = ['caselam']

    def __init__(self, caselam, prev):
        ConsEnv.__init__(self, prev)
        self.caselam = caselam

    @staticmethod
    @jit.unroll_safe
    def make(vals, caselam, prev):
        recursive_sym = caselam.recursive_sym
        if not we_are_translated() and not vals:
            for s in caselam.lams[0].frees.elems:
                assert s is recursive_sym
        return W_Closure1AsEnv._make(vals, caselam, prev)

    def iscallable(self):
        return True

    def enable_jitting(self):
        self.caselam.enable_jitting()

    def immutable(self):
        return True

    def tostring(self):
        return self.caselam.tostring_as_closure()

    def get_arity(self, promote=False):
        caselam = self.caselam
        if promote:
            caselam = jit.promote(caselam)
        return caselam.get_arity()

    def call_with_extra_info(self, args, env, cont, calling_app):
        env_structure = None
        if calling_app is not None:
            env_structure = calling_app.env_structure
        jit.promote(self.caselam)
        jit.promote(env_structure)
        lam = self.caselam.lams[0]
        if not jit.we_are_jitted() and env.pycketconfig().callgraph:
            env.toplevel_env().callgraph.register_call(lam, calling_app, cont, env)
        actuals = lam.match_args(args)
        if actuals is None:
            lam.raise_nice_error(args)
        # specialize on the fact that often we end up executing in the
        # same environment.
        
        prev = lam.env_structure.prev.find_env_in_chain_speculate(
                self, env_structure, env)
        return lam.make_begin_cont(
            ConsEnv.make(actuals, prev),
            cont)

    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

    # ____________________________________________________________
    # methods as a ConsEnv

    @jit.unroll_safe
    def consenv_get_size(self):
        result = self._get_size_list()
        for s in self.caselam.lams[0].frees.elems:
            result += s is self.caselam.recursive_sym
        return result

    @jit.unroll_safe
    def lookup(self, sym, env_structure):
        jit.promote(env_structure)
        if len(env_structure.elems) == self._get_size_list():
            return ConsEnv.lookup(self, sym, env_structure)
        recursive_sym = jit.promote(self.caselam).recursive_sym
        if sym is recursive_sym:
            return self
        i = 0
        for s in env_structure.elems:
            if s is recursive_sym:
                continue
            if s is sym:
                v = self._get_list(i)
                assert v is not None
                return v
            i += 1 # only count non-self references
        prev = self.get_prev(env_structure)
        return prev.lookup(sym, env_structure.prev)

class W_PromotableClosure(W_Procedure):
    """ A W_Closure that is promotable, ie that is cached in some place and
    unlikely to change. """

    _attrs_ = _immutable_fields_ = ["closure", "arity"]

    def __init__(self, caselam, toplevel_env):
        envs = [toplevel_env] * len(caselam.lams)
        self.closure = W_Closure._make(envs, caselam, toplevel_env)
        self.arity   = caselam._arity

    def enable_jitting(self):
        self.closure.enable_jitting()

    def call(self, args, env, cont):
        jit.promote(self)
        return self.closure.call(args, env, cont)

    def call_with_extra_info(self, args, env, cont, calling_app):
        jit.promote(self)
        return self.closure.call_with_extra_info(args, env, cont, calling_app)

    def get_arity(self, promote=False):
        if promote:
            self = jit.promote(self)
        return self.arity

    def tostring(self):
        return self.closure.tostring()

class W_EnvVarSet(W_Object):
    errorname = "environment-variable-set"
    _attrs_ = []
    def __init__(self):
        pass

class W_EOF(W_Object):
    errorname = "eof"
    _attrs_ = []
    def __init__(self):
        pass
    def tostring(self):
        return "#<eof>"

eof_object = W_EOF()

class W_ReadTable(W_Object):
    errorname = "readtable"

    _attrs_ = _immutable_fields_ = ["parent", "key", "mode", "action"]

    def __init__(self, parent, key, mode, action):
        self.parent = parent
        self.key = key
        self.mode = mode
        self.action = action

class W_Port(W_Object):
    errorname = "port"
    _attrs_ = ['closed']

    def __init__(self):
        self.closed = False

    def tostring(self):
        raise NotImplementedError("abstract base classe")

    def close(self):
        self.closed = True

    def seek(self, offset, end=False):
        raise NotImplementedError("abstract base classe")

    def tell(self):
        raise NotImplementedError("abstract base classe")

class W_OutputPort(W_Port):
    errorname = "output-port"
    _attrs_ = []
    def __init__(self):
        pass

    def write(self, str):
        raise NotImplementedError("abstract base classe")

    def flush(self):
        raise NotImplementedError("abstract base classe")

    def tostring(self):
        return "#<output-port>"

class W_StringOutputPort(W_OutputPort):
    errorname = "output-port"
    _attrs_ = ['closed', 'str']
    def __init__(self):
        self.closed = False
        self.str = StringBuilder()
    def write(self, s):
        self.str.append(s)
    def contents(self):
        return self.str.build()
    def seek(self, offset, end=False):
        if end or offset == self.str.getlength():
            return
        if offset > self.str.getlength():
            self.str.append("\0" * (self.str.getlength() - offset))
        else:
            # FIXME: this is potentially slow.
            content = self.contents()
            self.str = StringBuilder(offset)
            self.str.append_slice(content, 0, offset)

    def tell(self):
        return self.str.getlength()

class W_InputPort(W_Port):
    errorname = "input-port"
    _attrs_ = []
    def read(self, n):
        raise NotImplementedError("abstract class")
    def peek(self):
        raise NotImplementedError("abstract class")
    def readline(self):
        raise NotImplementedError("abstract class")
    def get_read_handler(self):
        raise NotImplementedError("abstract class")
    def set_read_handler(self, handler):
        raise NotImplementedError("abstract class")
    def tostring(self):
        return "#<input-port>"
    def _length_up_to_end(self):
        raise NotImplementedError("abstract class")

class W_StringInputPort(W_InputPort):
    errorname = "input-port"
    _immutable_fields_ = ["str"]
    _attrs_ = ['closed', 'str', 'ptr', 'read_handler']

    def __init__(self, str):
        self.closed = False
        self.str = str
        self.ptr = 0
        self.read_handler = None

    def get_read_handler(self):
        return self.read_handler

    def set_read_handler(self, handler):
        self.read_handler = handler
        
    def readline(self):
        from rpython.rlib.rstring import find
        start = self.ptr
        assert start >= 0
        pos = find(self.str, "\n", start, len(self.str))
        if pos < 0:
            return self.read()
        else:
            pos += 1
            stop = self.ptr = pos
            return self.str[start:stop]
        return line

    def peek(self):
        if self.ptr >= len(self.str):
            return ""
        return self.str[self.ptr]


    def read(self, n=-1):
        if self.ptr >= len(self.str):
            return ""
        p = self.ptr
        assert p >= 0
        if n == -1 or n >= (len(self.str) - self.ptr):
            self.ptr = len(self.str)
            assert self.ptr >= 0
            return self.str[p:]
        else:
            self.ptr += n
            stop = self.ptr
            assert stop < len(self.str)
            assert stop >= 0
            return self.str[p:stop]

    def seek(self, offset, end=False):
        if end or offset == self.ptr:
            self.ptr = len(self.str)
            return
        if offset > len(self.str):
            raise SchemeException("index out of bounds")
        else:
            self.ptr = offset

    def tell(self):
        return self.ptr

    def _length_up_to_end(self):
        return len(self.str) - self.ptr

class W_FileInputPort(W_InputPort):
    errorname = "input-port"
    _immutable_fields_ = ["file"]
    _attrs_ = ['closed', 'file', 'read_handler']

    def __init__(self, f):
        self.closed = False
        self.file = f
        self.read_handler = None

    def close(self):
        self.closed = True
        self.file.close()
        #self.file = None

    def read(self, n):
        return self.file.read(n)

    def get_read_handler(self):
        return self.read_handler

    def set_read_handler(self, handler):
        self.read_handler = handler
    
    def readline(self):
        return self.file.readline()

    def peek(self):
        offset, string = self.file.peek()
        if offset < len(string):
            # fast path:
            return string[offset]
        pos = self.file.tell()
        res = self.file.read(1)
        self.file.seek(pos, 0)
        return res

    def seek(self, offset, end=False):
        if end:
            self.file.seek(0, 2)
        else:
            self.file.seek(offset, 0)

    def tell(self):
        # XXX this means we can only deal with 4GiB files on 32bit systems
        return int(intmask(self.file.tell()))

    def _length_up_to_end(self):
        old_ptr = self.tell()
        self.seek(0, end=True)
        new_ptr = self.tell()
        self.seek(old_ptr)
        return new_ptr - old_ptr

class W_FileOutputPort(W_OutputPort):
    errorname = "output-port"
    _immutable_fields_ = ["file"]
    _attrs_ = ['closed', 'file']

    def __init__(self, f):
        self.closed = False
        self.file = f

    def write(self, str):
        self.file.write(str)

    def flush(self):
        self.file.flush()

    def close(self):
        self.closed = True
        self.file.close()
        #self.file = None

    def seek(self, offset, end=False):
        if end:
            self.file.seek(0, 2)
        else:
            self.file.seek(offset, 0)

    def tell(self):
        # XXX this means we can only deal with 4GiB files on 32bit systems
        return int(intmask(self.file.tell()))

@specialize.call_location()
def wrap_list(pyval):
    assert isinstance(pyval, list)
    acc = w_null
    for val in reversed(pyval):
        acc = wrap(val, acc)
    return acc

@specialize.ll()
def wrap(*_pyval):
    # Smart constructor for converting Python values to Racket values
    if len(_pyval) == 1:
        pyval = _pyval[0]
        if isinstance(pyval, bool):
            return w_true if pyval else w_false
        if isinstance(pyval, int):
            return W_Fixnum(pyval)
        if isinstance(pyval, float):
            return W_Flonum(pyval)
        if isinstance(pyval, W_Object):
            return pyval
    elif len(_pyval) == 2:
        car = _pyval[0]
        cdr = wrap(_pyval[1])
        if isinstance(car, bool):
            if cdr.is_proper_list():
                return W_WrappedConsProper(wrap(car), cdr)
            return W_WrappedCons(wrap(car), cdr)
        if isinstance(car, int):
            if cdr.is_proper_list():
                return W_UnwrappedFixnumConsProper(car, cdr)
            return W_UnwrappedFixnumCons(car, cdr)
        if isinstance(car, float):
            if cdr.is_proper_list():
                return W_UnwrappedFlonumConsProper(car, cdr)
            return W_UnwrappedFlonumCons(car, cdr)
        if isinstance(car, W_Object):
            return W_Cons.make(car, cdr)
    assert False

