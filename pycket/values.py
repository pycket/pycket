#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont import continuation, call_cont
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

    def eqv(self, other):
        return self is other # default implementation

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

class W_MVector(W_Object):
    errorname = "vector"
    def __init__(self):
        raise NotImplementedError("abstract base class")

    def length(self):
        raise NotImplementedError("abstract base class")

class W_ImpVector(W_MVector):
    _immutable_fields_ = ["vec", "refh", "seth"]
    def __init__(self, v, r, s):
        assert isinstance(v, W_MVector)
        self.vec = v
        self.refh = r
        self.seth = s

    def length(self):
        return self.vec.length()

    def equal(self, other):
        if not isinstance(other, W_MVector):
            return False
        if self is other:
            return True
        if self.vec is other:
            return True
        if self.length() != other.length():
            return False
        for i in range(self.length()):
            # FIXME: we need to call user code here
            if not self.vec.ref(i).equal(other.ref(i)):
               return False
            #return False
        return True

class W_ChpVector(W_MVector):
    _immutable_fields_ = ["vec", "refh", "seth"]
    def __init__(self, v, r, s):
        assert isinstance(v, W_MVector)
        self.vec  = v
        self.refh = r
        self.seth = s

    def length(self):
        return self.vec.length()

    def equal(self, other):
        if not isinstance(other, W_MVector):
            return False
        if self is other:
            return True
        if self.length() != other.length():
            return False
        for i in range(self.length()):
            if not self.vec.ref(i).equal(other.ref(i)):
                return False
        return True

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

    def eqv(self, other):
        return self.equal(other)


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

class W_Character(W_Object):
    _immutable_fields_ = ["value"]
    errorname = "char"
    def tostring(self):
        return "#\\%s" % self.value.encode("utf-8")
    def __init__(self, val):
        self.value = val
    def equal(self, other):
        if not isinstance(other, W_Character):
            return False
        return self.value == other.value
    eqv = equal

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
        from pypy.objspace.std.bytesobject import string_escape_encode
        #return string_escape_encode(self.value, '"')
        return self.value
    def equal(self, other):
        if not isinstance(other, W_String):
            return False
        return self.value == other.value

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
        return "'#:%s"%self.value

class W_Procedure(W_Object):
    errorname = "procedure"
    def __init__(self):
        raise NotImplementedError("abstract base class")
    def mark_non_loop(self): pass

def is_impersonator_of(a, b):
    if a is b:
        return True
    if isinstance(a, W_ImpVector):
        return is_impersonator_of(a.vec, b)
    if isinstance(a, W_ImpProcedure):
        return is_impersonator_of(a.code, b)
    return is_chaperone_of(a, b)

# Check that one value is a chaperone of the other
def is_chaperone_of(a, b):
    if a is b:
        return True
    if isinstance(a, W_ChpVector):
        return is_chaperone_of(a.vec, b)
    if isinstance(a, W_ChpProcedure):
        return is_chaperone_of(a.code, b)
    return False

# Continuation used when calling an impersonator of a procedure.
@continuation
def imp_proc_cont(arg_count, proc, env, cont, _vals):
    vals = _vals._get_full_list()
    if len(vals) == arg_count:
        return proc.call(vals, env, cont)
    elif len(vals) == arg_count + 1:
        args, check = vals[:-1], vals[-1]
        return proc.call(args, env, call_cont(check, env, cont))
    else:
        assert False

#def chp_proc_cont(args, proc, env, cont, _vals):
    #vals = _vals._get_full_list()

class W_ImpProcedure(W_Procedure):
    _immutable_fields_ = ["code", "check"]
    def __init__(self, code, check):
        assert isinstance(code, W_Procedure)
        assert isinstance(check, W_Procedure)
        self.code  = code
        self.check = check

    def call(self, args, env, cont):
        jit.promote(self)
        return self.check.call(
                args, env, imp_proc_cont(len(args), self.code, env, cont))

    def equal(self, other):
        if not isinstance(other, W_Procedure):
            return False
        # We are the same procedure if we have the same identity or
        # our underlying procedure is equal to our partner.
        return self is other or other.equal(self.code)

    def tostring(self):
        return "ImpProcedure<%s>" % self.code.tostring()

# Check that the results of che call to check are all chaperones of
# the original function outputs.
@continuation
def chp_proc_ret_cont(orig, env, cont, _vals):
    from pycket.interpreter import return_multi_vals
    vals = _vals._get_full_list()
    assert len(vals) == len(orig)
    for i in range(len(vals)):
        if not is_chaperone_of(vals[i], orig[i]):
            raise SchemeException("Expecting original value or chaperone")
    return return_multi_vals(_vals, env, cont)

# Capture the original output of the function to compare agains the result of
# the check operation
@continuation
def chp_proc_call_check_cont(check, env, cont, _vals):
    vals = _vals._get_full_list()
    return check.call(vals, env, chp_proc_ret_cont(vals, env, cont))

# Continuation used when calling a chaperone of a procedure.
@continuation
def chp_proc_cont(args, proc, env, cont, _vals):
    vals = _vals._get_full_list()
    assert len(vals) >= len(args)
    for i in range(len(args)):
        if not is_chaperone_of(vals[i], args[i]):
            raise SchemeException("Expecting original value or chaperone")
    if len(vals) == len(args):
        return proc.call(vals, env, cont)
    elif len(vals) == len(args) + 1:
        args, check = vals[:-1], vals[-1]
        return proc.call(args, env, chp_proc_call_check_cont(check, env, cont))
    else:
        assert False

class W_ChpProcedure(W_Procedure):
    _immutable_fields_ = ["code", "check"]
    def __init__(self, code, check):
        assert isinstance(code, W_Procedure)
        assert isinstance(check, W_Procedure)
        self.code  = code
        self.check = check

    def call(self, args, env, cont):
        jit.promote(self)
        return self.check.call(
                args, env, chp_proc_cont(args, self.code, env, cont))
        #return self.code.call(args, env, cont)

    def equal(self, other):
        if not isinstance(other, W_Procedure):
            return False

        return self is other or other.equal(self.code)

    def tostring(self):
        return "ChpProcedure<%s>" % self.code.tostring()

class W_SimplePrim(W_Procedure):
    _immutable_fields_ = ["name", "code"]
    def __init__ (self, name, code):
        self.name = name
        self.code = code

    def call(self, args, env, cont):
        from pycket.interpreter import return_value
        jit.promote(self)
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
            elif isinstance(e, ConsEnv) and e.prev is new_env:
                prev = e.prev
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

    def call(self, args, env, cont):
        jit.promote(self)
        return self.closure.call(args, env, cont)

