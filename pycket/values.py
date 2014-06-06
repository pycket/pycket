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

    def eqv(self, other):
        return self is other # default implementation

class W_Cell(W_Object): # not the same as Racket's box
    def __init__(self, v):
        assert not isinstance(v, W_Cell)
        self.value = v

    def set_val(self, w_value):
        self.value = w_value

class W_MVector(W_Object):
    errorname = "vector"
    def __init__(self):
        raise NotImplementedError("abstract base class")

    def length(self):
        raise NotImplementedError("abstract base class")

class W_ImpVector(W_MVector):
    _immutable_fields_ = ["vec", "refh", "seth"]
    def __init__(self, v, r, s):
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
            # if not self.ref(i).equal(other.ref(i)):
            #    return False
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
        for i in lam.frees.elems:
            assert isinstance(i, W_Symbol)
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

class W_PromotableClosure(W_Closure):
    """ A W_Closure that is promotable, ie that is cached in some place and
    unlikely to change. """

    def call(self, args, env, cont):
        jit.promote(self)
        return W_Closure.call(self, args, env, cont)

#
# It's a very early support of structs that IS NOT compatible with RPython
#
class W_StructType(W_Object):
    all_structs = {}
    errorname = "struct"
    _immutable_fields_ = ["_id", "_fields[:]"]
    
    @staticmethod
    def make(struct_id, super_type, fields):
        if struct_id in W_StructType.all_structs:
            #TODO: report an error
            return W_StructType.all_structs[struct_id]
        else:
            W_StructType.all_structs[struct_id] = w_result = W_StructType(struct_id, super_type, fields)
            return w_result
    
    def __init__(self, struct_id, super_type, fields):
        self._id = struct_id
        self._fields = from_list(fields)
        self.w_constr = self.make_constructor_procedure(super_type, fields)
        self.w_pred = self.make_predicate_procedure()
        self.w_acc = self.make_accessor_procedure()
        self.w_mut = self.make_mutator_procedure()

    def constr_proc(self, args_w, env, cont):
        from pycket.interpreter import return_value
        struct_data = {}
        for idx, field in enumerate(self._fields):
            struct_data[str(field.value)] = args_w[idx]
        return return_value(W_Struct(self, struct_data), env, cont)

    def pred_proc(self, args_w, env, cont):
        result = W_Bool.make(False)
        if (hasattr(args_w[0], "_struct_type") and args_w[0]._struct_type == self):
            result = W_Bool.make(True)
        from pycket.interpreter import return_value
        return return_value(result, env, cont)

    def acc_proc(self, args_w, env, cont):
        return args_w[0]._fields[str(args_w[1])]

    def mut_proc(self, args_w, env, cont):
        result = False
        from pycket.interpreter import return_value
        return return_value(result, env, cont)

    def make_constructor_procedure(self, super_type, fields):
        return W_Prim(self._id.value + '_constr_proc', self.constr_proc)

    def make_predicate_procedure(self):
        return W_Prim(self._id.value + '_pred_proc', self.pred_proc)

    def make_accessor_procedure(self):
        return W_Prim(self._id.value + '_acc_proc', self.acc_proc)

    def make_mutator_procedure(self):
        return W_Prim(self._id.value + '_mut_proc', self.mut_proc)

    def make_struct_tuple(self):
        return [self._id, self.w_constr, self.w_pred, self.w_acc, self.w_mut]

class W_Struct_Field_Accessor(W_Object):
    _immutable_fields_ = ["_accessor", "_field"]
    def __init__(self, accessor, field):
        self._accessor = accessor
        #FIXME: it is not RPython compatible
        self._field = int(field.value)
    def call(self, args, env, cont):
        from pycket.interpreter import return_value
        acc_args = [args[0], self._field]
        return return_value(self._accessor.call(acc_args, env, cont), env, cont)

class W_Struct(W_Object):
    def __init__(self, struct_type, fields):
        self._struct_type = struct_type
        self._fields = fields
