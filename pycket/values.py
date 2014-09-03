#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont import continuation, label
from pycket.error import SchemeException
from pycket.small_list import inline_small_list
from rpython.tool.pairtype import extendabletype
from rpython.rlib  import jit, runicode
from rpython.rlib.objectmodel import r_dict, compute_hash

import rpython.rlib.rweakref as weakref

UNROLLING_CUTOFF = 5

# This is not a real value, so it's not a W_Object
class Values(object):
    def tostring(self):
        vals = self._get_full_list()
        if len(vals) == 1:
            return vals[0].tostring()
        if len(vals) == 0:
            return "(values)"
        else: #fixme
            return "MULTIPLE VALUES"
    @jit.unroll_safe
    def __init__(self):
        pass

inline_small_list(Values, immutable=True, attrname="vals")

class W_Object(object):
    __metaclass__ = extendabletype
    _attrs_ = []
    errorname = "%%%%unreachable%%%%"
    def __init__(self):
        raise NotImplementedError("abstract base class")

    def iscallable(self):
        return False

    # The general `call` method is setup to return control to the CEK machine
    # before executing the body of the function being called. Unless you know
    # what you are doing, please override the `_call` method. This is needed to
    # get around (R)Python's lack of tail call elimination.
    # `_call` should always be safe to override, but `call` is safe to override
    # if it implements a simple primitive.
    def _call(self, args, env, cont):
        raise NotImplementedError("abstract base class")

    def call(self, args, env, cont):
        if self.iscallable():
            from pycket.interpreter import tailcall
            return tailcall(self._call, args, env, cont)
        raise SchemeException("%s is not callable" % self.tostring())

    def mark_non_loop(self):
        pass

    # an arity is a pair of a list of numbers and either -1 or a non-negative integer
    def get_arity(self):
        if self.iscallable():
            return ([],0)
        else:
            raise SchemeException("%s does not have arity" % self.tostring())

    def is_impersonator(self):
        return self.is_chaperone()
    def is_chaperone(self):
        return False
    def is_proxy(self):
        return self.is_chaperone() or self.is_impersonator()
    def get_proxied(self):
        return self
    def get_properties(self):
        return {}

    def immutable(self):
        return False
    def equal(self, other):
        return self is other # default implementation
    def eqv(self, other):
        return self is other # default implementation
    def tostring(self):
        return str(self)

class W_Cell(W_Object): # not the same as Racket's box
    def __init__(self, v):
        assert not isinstance(v, W_Cell)
        if isinstance(v, W_Fixnum):
            v = W_CellIntegerStrategy(v.value)
        self.w_value = v

    def get_val(self):
        w_value = self.w_value
        if isinstance(w_value, W_CellIntegerStrategy):
            return W_Fixnum(w_value.value)
        return w_value

    def set_val(self, w_value):
        if isinstance(w_value, W_Fixnum):
            w_v = self.w_value
            if isinstance(w_v, W_CellIntegerStrategy):
                w_v.value = w_value.value
            else:
                self.w_value = W_CellIntegerStrategy(w_value.value)
        else:
            self.w_value = w_value

class W_CellIntegerStrategy(W_Object):
    # can be stored in cells only, is mutated when a W_Fixnum is stored
    def __init__(self, value):
        self.value = value

class W_Undefined(W_Object):
    errorname = "unsafe-undefined"
    def __init__(self):
        pass

w_unsafe_undefined = W_Undefined()

# FIXME: not a real implementation
class W_Syntax(W_Object):
    _immutable_fields_ = ["val"]
    errorname = "syntax"
    def __init__(self, o):
        self.val = o
    def tostring(self):
        return "#'%s" % self.val.tostring()

class W_ModulePathIndex(W_Object):
    errorname = "module-path-index"
    def __init__(self):
        pass
    def tostring(self):
        return "#<module-path-index>"

class W_ResolvedModulePath(W_Object):
    _immutable_fields_ = ["name"]
    errorname = "resolved-module-path"
    def __init__(self, name):
        self.name = name
    def tostring(self):
        return "#<resolved-module-path:%s>" % self.name

class W_Logger(W_Object):
    errorname = "logger"
    def __init__(self):
        pass
    def tostring(self):
        return "#<logger>"

current_logger = W_Logger()

class W_ContinuationPromptTag(W_Object):
    errorname = "continuation-prompt-tag"
    _immutable_fields_ = ["name"]
    def __init__(self, name):
        self.name = name
    def tostring(self):
        return "#<continuation-prompt-tag>"

class W_ContinuationMarkSet(W_Object):
    errorname = "continuation-mark-set"
    _immutable_fields_ = ["cont"]
    def __init__(self, cont):
        self.cont = cont
    def tostring(self):
        return "#<continuation-mark-set>"

class W_ContinuationMarkKey(W_Object):
    errorname = "continuation-mark-key"
    _immutable_fields_ = ["name"]
    def __init__(self, name):
        self.name = name
    def tostring(self):
        return "#<continuation-mark-name>"

class W_VariableReference(W_Object):
    errorname = "variable-reference"
    def __init__(self, varref):
        self.varref = varref
    def tostring(self):
        return "#<#%variable-reference>"

class W_MVector(W_Object):
    errorname = "vector"
    def __init__(self):
        raise NotImplementedError("abstract base class")

    @label
    def vector_set(self, i, new, env, cont):
        raise NotImplementedError("abstract base class")

    @label
    def vector_ref(self, i, env, cont):
        raise NotImplementedError("abstract base class")

    def length(self):
        raise NotImplementedError("abstract base class")

class W_List(W_Object):
    errorname = "list"
    def __init__(self):
        raise NotImplementedError("abstract base class")

class W_Cons(W_List):
    "Abstract for specialized conses. Concrete general in W_WrappedCons"
    errorname = "pair"

    @staticmethod
    def make(car, cdr):
        if not _enable_cons_specialization:
            return W_WrappedCons(car, cdr)
        elif isinstance(car, W_Fixnum):
            return W_UnwrappedFixnumCons(car, cdr)
        else:
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
        if isinstance(cur, W_Null):
            return "(%s)" % " ".join(acc)
        # Must be an improper list
        return "(%s . %s)" % (" ".join(acc), cur.tostring())

    def immutable(self):
        return True

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

class W_Box(W_Object):
    errorname = "box"
    def __init__(self):
        raise NotImplementedError("abstract base class")

    @label
    def unbox(self, env, cont):
        raise NotImplementedError("abstract base class")

    @label
    def set_box(self, val, env, cont):
        raise NotImplementedError("abstract base class")

class W_MBox(W_Box):
    errorname = "mbox"

    def __init__(self, value):
        self.value = value

    @label
    def unbox(self, env, cont):
        from pycket.interpreter import return_value
        return return_value(self.value, env, cont)

    @label
    def set_box(self, val, env, cont):
        from pycket.interpreter import return_value
        self.value = val
        return return_value(w_void, env, cont)

    def tostring(self):
        return "'#&%s" % self.value.tostring()

class W_IBox(W_Box):
    errorname = "ibox"
    _immutable_fields_ = ["value"]

    def __init__(self, value):
        self.value = value

    def immutable(self):
        return True

    @label
    def unbox(self, env, cont):
        from pycket.interpreter import return_value
        return return_value(self.value, env, cont)

    @label
    def set_box(self, val, env, cont):
        raise SchemeException("set-box!: not supported on immutable boxes")

    def tostring(self):
        return "'#&%s" % self.value.tostring()

# A weak box does not test as a box for most operations and cannot be
# chaperoned/impersonated, so we start it from W_Object rather than W_Box.
class W_WeakBox(W_Object):
    errorname = "weak-box"
    _immutable_fields_ = ["value"]

    def __init__(self, value):
        assert isinstance(value, W_Object)
        self.value = weakref.ref(value)

    def get(self):
        return self.value()

    def tostring(self):
        return "#<weak-box>"

class W_Ephemeron(W_Object):
    errorname = "ephemeron"
    _immutable_fields_ = ["key", "mapping"]

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
    def __init__(self, value):
        self.value = value
    def tostring(self):
        return "#<placeholder>"

class W_HashTablePlaceholder(W_Object):
    errorname = "hash-table-placeholder"
    def __init__(self, keys, vals):
        pass
    def tostring(self):
        return "#<hash-table-placeholder>"

class W_UnwrappedFixnumCons(W_Cons):
    _immutable_fields_ = ["_car", "_cdr"]
    def __init__(self, a, d):
        assert isinstance(a, W_Fixnum)
        self._car = a.value
        self._cdr = d

    def car(self):
        return W_Fixnum(self._car)

    def cdr(self):
        return self._cdr

class W_WrappedCons(W_Cons):
    _immutable_fields_ = ["_car", "_cdr"]
    def __init__(self, a, d):
        self._car = a
        self._cdr = d
    def car(self):
        return self._car
    def cdr(self):
        return self._cdr

_enable_cons_specialization = True


class W_MList(W_Object):
    errorname = "mlist"
    def __init__(self):
        raise NotImplementedError("abstract base class")

class W_MCons(W_MList):
    errorname = "mpair"
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
    errorname = "number"
    def __init__(self):
        raise NotImplementedError("abstract base class")

    def immutable(self):
        return True

    def eqv(self, other):
        return self.equal(other)

class W_Complex(W_Number):
    _immutable_fields_ = ["real", "imag"]
    errorname = "complex"
    def __init__(self, r, i):
        self.real = r
        self.imag = i
    def tostring(self):
        return "%s+%si" % (self.real.tostring(), self.imag.tostring())

class W_Rational(W_Number):
    _immutable_fields_ = ["num", "den"]
    errorname = "rational"
    def __init__(self, n, d):
        assert isinstance(n, W_Integer)
        assert isinstance(d, W_Integer)
        self.num = n
        self.den = d
    def tostring(self):
        return "%s/%s" % (self.num.tostring(), self.den.tostring())

class W_Integer(W_Number):
    errorname = "integer"

class W_Fixnum(W_Integer):
    _immutable_fields_ = ["value"]
    errorname = "fixnum"
    def tostring(self):
        return str(self.value)
    def __init__(self, val):
        assert isinstance(val, int)
        self.value = val

    def equal(self, other):
        if not isinstance(other, W_Fixnum):
            return False
        return self.value == other.value

class W_Flonum(W_Number):
    _immutable_fields_ = ["value"]
    errorname = "flonum"
    def tostring(self):
        return str(self.value)
    def __init__(self, val):
        self.value = val

    def equal(self, other):
        if not isinstance(other, W_Flonum):
            return False
        return self.value == other.value

class W_Bignum(W_Integer):
    _immutable_fields_ = ["value"]
    def tostring(self):
        return str(self.value)
    def __init__(self, val):
        self.value = val

    def equal(self, other):
        if not isinstance(other, W_Bignum):
            return False
        return self.value.eq(other.value)

class W_Complex(W_Number):
    _immutable_fields_ = ["real", "imag"]
    def __init__(self, re, im):
        assert isinstance(re, W_Number)
        assert isinstance(im, W_Number)
        self.real = re
        self.imag = im

    def tostring(self):
        return "%s+%si" % (self.real.tostring(), self.imag.tostring())

class W_Character(W_Object):
    _immutable_fields_ = ["value"]
    errorname = "char"
    def __init__(self, val):
        self.value = val

    def tostring(self):
        return "#\\%s" % runicode.unicode_encode_utf_8(
                self.value, len(self.value), "strict")

    def immutable(self):
        return True

    def equal(self, other):
        if not isinstance(other, W_Character):
            return False
        return self.value == other.value
    eqv = equal

class W_Thread(W_Object):
    errorname = "thread"
    def __init__(self):
        pass
    def tostring(self):
        return "#<thread>"

class W_OutputPort(W_Object):
    errorname = "output-port"
    def __init__(self):
        pass
    def tostring(self):
        return "#<output-port>"

class W_StringOutputPort(W_OutputPort):
    errorname = "output-port"
    def __init__(self):
        self.str = ""


class W_Semaphore(W_Object):
    errorname = "semaphore"
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

class W_SemaphorePeekEvt(W_Evt):
    errorname = "semaphore-peek-evt"
    _immutable_fields_ = ["sema"]
    def __init__(self, sema):
        self.sema = sema
    def tostring(self):
        return "#<semaphore-peek-evt>"

class W_PseudoRandomGenerator(W_Object):
    errorname = "pseudo-random-generator"
    def __init__(self):
        pass

class W_Path(W_Object):
    _immutable_fields_ = ["path"]
    errorname = "path"
    def __init__(self, p):
        self.path = p
    def tostring(self):
        return "#<path:%s>" % self.path

class W_Void(W_Object):
    def __init__(self): pass
    def tostring(self):
        return "#<void>"

class W_Null(W_List):
    def __init__(self): pass
    def tostring(self): return "()"

w_void = W_Void()
w_null = W_Null()

class W_Bool(W_Object):
    _immutable_fields_ = ["value"]
    errorname = "boolean"
    @staticmethod
    def make(b):
        if b: return w_true
        else: return w_false

    def __init__(self, val):
        """ NOT_RPYTHON """
        # the previous line produces an error if somebody makes new bool
        # objects from primitives
        self.value = val
    def tostring(self):
        if self.value: return "#t"
        else: return "#f"

w_false = W_Bool(False)
w_true = W_Bool(True)

class W_ThreadCellValues(W_Object):
    _immutable_fields_ = ["assoc"]
    errorname = "thread-cell-values"
    def __init__(self):
        self.assoc = {}
        for c in W_ThreadCell._table:
            if c.preserved.value:
                self.assoc[c] = c.value

class W_ThreadCell(W_Object):
    _immutable_fields_ = ["initial", "preserved"]
    errorname = "thread-cell"
    # All the thread cells in the system
    _table = []

    def __init__(self, val, preserved):
        # TODO: This should eventually be a mapping from thread ids to values
        self.value = val
        self.initial = val
        self.preserved = preserved

        W_ThreadCell._table.append(self)


def eq_hash(k):
    if isinstance(k, W_Fixnum):
        return compute_hash(k.value)
    else:
        return compute_hash(k)

class W_HashTable(W_Object):
    errorname = "hash"
    def __init__(self, keys, vals, cmp=None, hash=eq_hash):
        from pycket.prims.equal import eqp_logic
        if not cmp:
            cmp = eqp_logic
        assert len(keys) == len(vals)
        self.data = r_dict(cmp, hash, force_non_null=True)
        for (i, k) in enumerate(keys):
            self.data[k] = vals[i]

    def tostring(self):
        lst = [W_Cons.make(k, v).tostring() for (k,v) in self.data.iteritems()]
        return "#hash(%s)" % " ".join(lst)

    def set(self, k, v):
        self.data[k] = v

    def ref(self, k):
        if k in self.data:
            return self.data[k]
        else:
            return None

class W_AnyRegexp(W_Object):
    _immutable_fields_ = ["str"]
    errorname = "regexp"
    def __init__(self, str):
        self.str = str

class W_Regexp(W_AnyRegexp): pass
class W_PRegexp(W_AnyRegexp): pass
class W_ByteRegexp(W_AnyRegexp): pass
class W_BytePRegexp(W_AnyRegexp): pass

class W_Bytes(W_Object):
    errorname = "bytes"
    _immutable_fields_ = ["value"]
    def __init__(self, val):
        self.value = val
    def tostring(self):
        return "#%s" % self.value
    def equal(self, other):
        if not isinstance(other, W_Bytes):
            return False
        return self.value == other.value
    def immutable(self):
        return True

class W_String(W_Object):
    errorname = "string"
    def __init__(self, val, immutable=False):
        assert val is not None
        self.value = val
        self.imm   = immutable
    def tostring(self):
        from pypy.objspace.std.bytesobject import string_escape_encode
        #return string_escape_encode(self.value, '"')
        return self.value
    def equal(self, other):
        if not isinstance(other, W_String):
            return False
        return self.value == other.value
    def immutable(self):
        return self.imm

class W_Symbol(W_Object):
    _immutable_fields_ = ["value", "unreadable"]
    errorname = "symbol"
    all_symbols = {}
    unreadable_symbols = {}
    @staticmethod
    def make(string):
        # This assert statement makes the lowering phase of rpython break...
        # Maybe comment back in and check for bug.
        #assert isinstance(string, str)
        if string in W_Symbol.all_symbols:
            return W_Symbol.all_symbols[string]
        else:
            W_Symbol.all_symbols[string] = w_result = W_Symbol(string)
            return w_result
    @staticmethod
    def make_unreadable(string):
        if string in W_Symbol.unreadable_symbols:
            return W_Symbol.unreadable_symbols[string]
        else:
            W_Symbol.unreadable_symbols[string] = w_result = W_Symbol(string, True)
            return w_result
    def __repr__(self):
        return self.value
    def __init__(self, val, unreadable=False):
        self.value = val
        self.unreadable = unreadable
    def is_interned(self):
        string = self.value
        if string in W_Symbol.all_symbols:
            return W_Symbol.all_symbols[string] is self
        if string in W_Symbol.unreadable_symbols:
            return W_Symbol.unreadable_symbols[string] is self
        return False
    def tostring(self):
        return "'%s" % self.value

exn_handler_key = W_Symbol("exnh")
parameterization_key = W_Symbol("parameterization")

class W_Keyword(W_Object):
    _immutable_fields_ = ["value"]
    errorname = "keyword"
    all_symbols = {}
    @staticmethod
    def make(string):
        # This assert statement makes the lowering phase of rpython break...
        # Maybe comment back in and check for bug.
        #assert isinstance(string, str)
        if string in W_Keyword.all_symbols:
            return W_Keyword.all_symbols[string]
        else:
            W_Keyword.all_symbols[string] = w_result = W_Keyword(string)
            return w_result
    def __repr__(self):
        return self.value
    def __init__(self, val):
        self.value = val
    def tostring(self):
        return "'#:%s" % self.value

# FIXME: this should really be a struct
class W_ArityAtLeast(W_Object):
    _immutable_fields_ = ["val"]
    errorname = "arity-at-least"
    def __init__(self, n):
        self.val = n

class W_Procedure(W_Object):
    def __init__(self):
        raise NotImplementedError("Abstract base class")
    def iscallable(self):
        return True
    def immutable(self):
        return True
    def tostring(self):
        return "#<procedure>"

class W_SimplePrim(W_Procedure):
    _immutable_fields_ = ["name", "code", "arity"]
    def __init__ (self, name, code, arity=([],0)):
        self.name = name
        self.code = code
        self.arity = arity

    def get_arity(self):
        return self.arity

    def call(self, args, env, cont):
        from pycket.interpreter import return_value
        jit.promote(self)
        return return_value(self.code(args), env, cont)

    def tostring(self):
        return "<procedure:%s>" % self.name

class W_Prim(W_Procedure):
    _immutable_fields_ = ["name", "code", "arity"]
    def __init__ (self, name, code, arity=([],0)):
        self.name = name
        self.code = code
        self.arity = arity

    def get_arity(self):
        return self.arity

    def _call(self, args, env, cont):
        jit.promote(self)
        return self.code(args, env, cont)

    def tostring(self):
        return "#<procedure:%s>" % self.name

def to_list(l): return to_improper(l, w_null)

@jit.look_inside_iff(
    lambda l, curr: jit.loop_unrolling_heuristic(l, len(l), UNROLLING_CUTOFF))
def to_improper(l, curr):
    for i in range(len(l) - 1, -1, -1):
        curr = W_Cons.make(l[i], curr)
    return curr

def to_mlist(l): return to_mimproper(l, w_null)

@jit.look_inside_iff(
    lambda l, curr: jit.loop_unrolling_heuristic(l, len(l), UNROLLING_CUTOFF))
def to_mimproper(l, curr):
    for i in range(len(l) - 1, -1, -1):
        curr = W_MCons(l[i], curr)
    return curr

def from_list(w_curr):
    result = []
    while isinstance(w_curr, W_Cons):
        result.append(w_curr.car())
        w_curr = w_curr.cdr()
    if w_curr is w_null:
        return result[:] # copy to make result non-resizable
    else:
        raise SchemeException("Expected list, but got something else")

class W_Continuation(W_Procedure):
    _immutable_fields_ = ["cont"]
    def __init__ (self, cont):
        self.cont = cont
    def get_arity(self):
        # FIXME: see if Racket ever does better than this
        return ([],0)
    def call(self, args, env, cont):
        from pycket.interpreter import return_multi_vals
        return return_multi_vals(Values.make(args), env, self.cont)
    def tostring(self):
        return "#<continuation>"

class W_Closure(W_Procedure):
    _immutable_fields_ = ["caselam"]
    @jit.unroll_safe
    def __init__ (self, caselam, env):
        from pycket.interpreter import ConsEnv
        self.caselam = caselam
        for (i,lam) in enumerate(caselam.lams):
            for s in lam.frees.elems:
                assert isinstance(s, W_Symbol)
            vals = [None] * len(lam.frees.elems)
            for j, v in enumerate(lam.frees.elems):
                if v is caselam.recursive_sym:
                    vals[j] = self
                else:
                    vals[j] = env.lookup(v, lam.enclosing_env_structure)
            self._set_list(i, ConsEnv.make(vals, env.toplevel_env, env.toplevel_env))

    @staticmethod
    @jit.unroll_safe
    def make(caselam, env):
        from pycket.interpreter import ConsEnv, CaseLambda
        assert isinstance(caselam, CaseLambda)
        envs = [None] * len(caselam.lams)
        return W_Closure._make(envs, caselam, env)

    def get_arity(self):
        arities = []
        rest = -1
        for l in self.caselam.lams:
            n = l.get_arity()
            if n < 0:
                r = (-n - 1)
                if rest >= 0:
                    rest = min(r, rest)
                else:
                    rest = r
            else:
                arities = arities + [n]
        return (arities, rest)

    def mark_non_loop(self):
        for l in self.caselam.lams:
            l.body[0].should_enter = False
    @jit.unroll_safe
    def _find_lam(self, args):
        jit.promote(self.caselam)
        for (i, lam) in enumerate(self.caselam.lams):
            try:
                actuals = lam.match_args(args)
                new_env = self._get_list(i)
                return (actuals, new_env, lam)
            except SchemeException:
                if len(self.caselam.lams) == 1:
                    raise
        raise SchemeException("No matching arity in case-lambda")
    def call(self, args, env, cont):
        from pycket.interpreter import ConsEnv
        jit.promote(self.caselam)
        (actuals, new_env, lam) = self._find_lam(args)
        # specialize on the fact that often we end up executing in the
        # same environment. we try the current environment and its
        # parent -- perhaps a more principled approach is needed
        prev = new_env
        if isinstance(env, ConsEnv):
            e = env.prev
            if e is new_env:
                prev = e
            elif isinstance(e, ConsEnv):
                e_prev = e.prev
                if e_prev is new_env:
                    prev = e_prev
                # Uncomment to speculate more; doesn't help sumloop
                # elif isinstance(e_prev, ConsEnv) and e_prev.prev is new_env:
                #     prev = e_prev.prev
        return lam.make_begin_cont(
            ConsEnv.make(actuals, prev, new_env.toplevel_env),
            cont)

inline_small_list(W_Closure, immutable=True, attrname="envs", factoryname="_make")


class W_PromotableClosure(W_Procedure):
    """ A W_Closure that is promotable, ie that is cached in some place and
    unlikely to change. """

    _immutable_fields_ = ["closure"]

    def __init__(self, caselam, toplevel_env):
        from pycket.interpreter import ConsEnv
        self.closure = W_Closure._make([ConsEnv.make([], toplevel_env, toplevel_env)] * len(caselam.lams), caselam, toplevel_env)

    def mark_non_loop(self):
        self.closure.mark_non_loop()

    def _call(self, args, env, cont):
        jit.promote(self)
        return self.closure.call(args, env, cont)

    def get_arity(self):
        return self.closure.get_arity()

    def tostring(self):
        return self.closure.tostring()

class W_Parameterization(W_Object):
    errorname = "parameterization"
    def __init__(self): pass
    def extend(self, param, val): return self
    def tostring(self):
        return "#<parameterization>"

class W_Parameter(W_Object):
    errorname = "parameter"
    _immutable_fields_ = ["guard"]
    def __init__(self, val, guard):
        self.val = val
        self.guard = guard

    def iscallable(self):
        return True

    def call(self, args, env, cont):
        from pycket.interpreter import return_value
        if len(args) == 0:
            return return_value(self.val, env, cont)
        elif len(args) == 1:
            self.val = args[0]
            return return_value(w_void, env, cont)
        else:
            raise SchemeException("wrong number of arguments to parameter")

    def tostring(self):
        return "#<parameter>"

class W_EnvVarSet(W_Object):
    errorname = "environment-variable-set"
    def __init__(self): pass
