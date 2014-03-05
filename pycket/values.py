#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.error import SchemeException
from pycket.small_list import inline_small_list
from rpython.tool.pairtype import extendabletype
from rpython.rlib  import jit

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
    def tostring(self):
        return str(self)
    def call(self, args, env, cont):
        raise SchemeException("%s is not callable" % self.tostring())

    def equal(self, other):
        return self is other # default implementation

class W_Cell(W_Object): # not the same as Racket's box
    def __init__(self, v):
        assert not isinstance(v, W_Cell)
        self.value = v

    def set_val(self, w_value):
        self.value = w_value


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
        return "(%s . %s)"%(self.car().tostring(), self.cdr().tostring())

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
        return "(mcons %s %s)"%(self.car().tostring(), self.cdr().tostring())
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


class W_Fixnum(W_Number):
    _immutable_fields_ = ["value"]
    errorname = "fixnum"
    def tostring(self):
        return str(self.value)
    def __init__(self, val):
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

class W_Bignum(W_Number):
    _immutable_fields_ = ["value"]
    def tostring(self):
        return str(self.value)
    def __init__(self, val):
        self.value = val

    def equal(self, other):
        if not isinstance(other, W_Bignum):
            return False
        return self.value.eq(other.value)

class W_Character(W_Object):
    _immutable_fields_ = ["value"]
    errorname = "char"
    def tostring(self):
        return "#\\%s" % val.encode("utf-8")
    def __init__(self, val):
        self.value = val
    def equal(self, other):
        if not isinstance(other, W_Character):
            return False
        return self.value == other.value

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

class W_String(W_Object):
    errorname = "string"
    def __init__(self, val):
        self.value = val
    def tostring(self):
        return self.value

class W_Symbol(W_Object):
    _immutable_fields_ = ["value"]
    errorname = "symbol"
    all_symbols = {}
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
    def __repr__(self):
        return self.value
    def __init__(self, val):
        self.value = val
    def tostring(self):
        return "'%s"%self.value

class W_Procedure(W_Object):
    errorname = "procedure"
    def __init__(self):
        raise NotImplementedError("abstract base class")

class W_SimplePrim(W_Procedure):
    _immutable_fields_ = ["name", "code"]
    def __init__ (self, name, code):
        self.name = name
        self.code = code

    def call(self, args, env, cont):
        from pycket.interpreter import return_value
        jit.promote(self)
        #print self.name
        return return_value(self.code(args), env, cont)

    def tostring(self):
        return "SimplePrim<%s>" % self.name

class W_Prim(W_Procedure):
    _immutable_fields_ = ["name", "code"]
    def __init__ (self, name, code):
        self.name = name
        self.code = code

    def call(self, args, env, cont):
        jit.promote(self)
        return self.code(args, env, cont)

    def tostring(self):
        return "Prim<%s>" % self.name

def to_list(l): return to_improper(l, w_null)

@jit.look_inside_iff(lambda l, curr: jit.isconstant(len(l)) and len(l) < UNROLLING_CUTOFF)
def to_improper(l, curr):
    for i in range(len(l) - 1, -1, -1):
        curr = W_Cons.make(l[i], curr)
    return curr

def to_mlist(l): return to_mimproper(l, w_null)

@jit.look_inside_iff(lambda l, curr: jit.isconstant(len(l)) and len(l) < UNROLLING_CUTOFF)
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
    def call(self, args, env, cont):
        from pycket.interpreter import return_multi_vals
        return return_multi_vals(Values.make(args), env, self.cont)

class W_Closure(W_Procedure):
    _immutable_fields_ = ["lam", "env"]
    @jit.unroll_safe
    def __init__ (self, lam, env):
        from pycket.interpreter import ConsEnv
        self.lam = lam
        vals = [env.lookup(i, lam.enclosing_env_structure)
                    for i in lam.frees.elems]
        self.env = ConsEnv.make(vals, env.toplevel_env, env.toplevel_env)
    def call(self, args, env, cont):
        from pycket.interpreter import ConsEnv
        lam = jit.promote(self.lam)
        fmls_len = len(lam.formals)
        args_len = len(args)
        if fmls_len != args_len and not lam.rest:
            raise SchemeException("wrong number of arguments to %s, expected %s but got %s"%(self.tostring(), fmls_len,args_len))
        if fmls_len > args_len:
            raise SchemeException("wrong number of arguments to %s, expected at least %s but got %s"%(self.tostring(), fmls_len,args_len))
        if lam.rest:
            actuals = args[0:fmls_len] + [to_list(args[fmls_len:])]
        else:
            actuals = args
        # specialize on the fact that often we end up executing in the same
        # environment
        if isinstance(env, ConsEnv) and env.prev is self.env:
            prev = env.prev
        else:
            prev = self.env
        return lam.make_begin_cont(
                          ConsEnv.make(actuals, prev, self.env.toplevel_env),
                          cont)


