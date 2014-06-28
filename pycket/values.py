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

    def simplecall(self, args):
        jit.promote(self)
        return self.code(args)

    def call(self, args, env, cont):
        from pycket.interpreter import return_value
        return return_value(self.simplecall(args), env, cont)

    def tostring(self):
        return "SimplePrim<%s>" % self.name

class W_Prim(W_Procedure):
    _immutable_fields_ = ["name", "code", "params"]
    def __init__ (self, name, code, params = None):
        self.name = name
        self.code = code
        self.params = params if params is not None else []

    def call(self, args, env, cont):
        jit.promote(self)
        return self.code(self.params + args, env, cont)

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
# It's a very early support of structs.
# Not all features are implemented.
#
class W_StructType(W_Object):
    all_structs = {}
    errorname = "struct"
    _immutable_fields_ = ["_id", "_super", "_init_field_cnt", "_auto_field_cnt", "_auto_v", "_inspector", "_constr_name"]
    
    @staticmethod 
    def make(args):
        struct_id = args[0]
        if struct_id in W_StructType.all_structs:
            # TODO: raise exception (module: duplicate definition for identifier in: struct:XXX) or not?
            return W_StructType.lookup_struct_type(struct_id)
        W_StructType.all_structs[struct_id] = w_result = W_StructType(args)
        return w_result

    @staticmethod
    def lookup_struct_type(struct_id):
        if struct_id in W_StructType.all_structs:
            return W_StructType.all_structs[struct_id]
        else:
            return w_false
    
    def __init__(self, args):
        self._id = args[0]
        super = args[1]
        self._super = W_StructType.lookup_struct_type(super.id()) if super != w_false else None
        init_field_cnt = args[2]
        assert isinstance(init_field_cnt, W_Fixnum)
        self._init_field_cnt = init_field_cnt.value
        auto_field_cnt = args[3]
        assert isinstance(auto_field_cnt, W_Fixnum)
        self._auto_field_cnt = auto_field_cnt.value
        # Next arguments are optional
        self._auto_v = args[4] if len(args) > 4 else None
        # self._props = args[5] if len(args) > 5 else None
        self._inspector = args[6] if len(args) > 6 else None
        # self._proc_spec = args[7] if len(args) > 7 else None
        # self._fields = from_list(args[8]) if len(args) > 8 else []
        # self._guard = args[9] if len(args) > 9 else None
        if len(args) > 10:
            constr_name = args[10]
            assert isinstance(constr_name, W_Symbol)
            self._constr_name = constr_name.value
        else:
            constr_name = self._id
            assert isinstance(constr_name, W_Symbol)
            self._constr_name = "make-" + constr_name.value

        # TODO: Structure types are opaque by default
        self._isopaque = True if self._inspector != w_false else False

        self.w_desc = W_StructTypeDescriptor(self._id)
        self.w_constr = W_StructConstructor(self._id, self._super, self._init_field_cnt, self._auto_field_cnt, \
                                            self._auto_v, self._isopaque, self._constr_name)
        self.w_pred = W_StructPredicate(self._id)
        self.w_acc = W_StructAccessor(self._id)
        self.w_mut = W_StructMutator(self._id)

    def id(self):
        return self._id
    def super(self):
        return self._super
    def init_field_cnt(self):
        return self._init_field_cnt
    def auto_field_cnt(self):
        return self._auto_field_cnt
    def isopaque(self):
        return self._opaque
    def constr(self):
        return self.w_constr
    def make_struct_tuple(self):
        return [self.w_desc, self.w_constr, self.w_pred, self.w_acc, self.w_mut]
    def tostring(self):
        return "StructType<%s>" % self._id

class W_StructTypeDescriptor(W_Object):
    _immutable_fields_ = ["_id"]
    def __init__(self, id):
        self._id = id
    def id(self):
        return self._id
    def tostring(self):
        return "#<struct-type:%s>" % self._id

class W_StructConstructor(W_SimplePrim):
    _immutable_fields_ = ["struct_type"]
    def __init__ (self, struct_id, super_type, init_field_cnt, auto_field_cnt, auto_v, isopaque, name):
        self._struct_id = struct_id
        self._super_type = super_type
        self._init_field_cnt = init_field_cnt
        self._auto_field_cnt = auto_field_cnt
        self._auto_v = auto_v
        self._isopaque = isopaque
        self._name = name
    def simplecall(self, field_values):
        super = None
        if self._super_type is not None:
            def split_list(list, num):
                assert num >= 0
                return list[:num], list[num:]
            split_position = len(field_values) - self._init_field_cnt
            super_field_values, field_values = split_list(field_values, split_position)
            super = self._super_type.constr().simplecall(super_field_values)
        auto_values = [self._auto_v] * self._auto_field_cnt
        return W_Struct(self._struct_id, super, self._isopaque, field_values + auto_values)
    def tostring(self):
        return "#<procedure:%s>" % self._name

class W_StructPredicate(W_SimplePrim):
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self._struct_id = struct_id
    def simplecall(self, args):
        struct = args[0]
        result = w_false
        if (isinstance(struct, W_Struct)):
            while True:
                if struct.type() == self._struct_id:
                    result = w_true
                    break
                if struct.super() is None: break
                else: struct = struct.super()
        return result
    def tostring(self):
        return "#<procedure:%s?>" % self._struct_id

class W_StructAccessor(W_SimplePrim):
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self._struct_id = struct_id
    def simplecall(self, args):
        struct, field = args
        assert isinstance(field, W_Fixnum)
        index = field.value
        result = struct.get_value(self._struct_id, index)
        return result
    def tostring(self):
        return "#<procedure:%s-ref>" % self._struct_id

class W_StructMutator(W_SimplePrim):
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self._struct_id = struct_id
    def simplecall(self, args):
        struct, field, val = args
        assert isinstance(field, W_Fixnum)
        index = field.value
        struct.set_value(self._struct_id, index, val)
    def tostring(self):
        return "#<procedure:%s-set!>" % self._struct_id

class W_Struct(W_Object):
    _immutable_fields_ = ["_type", "_super", "_isopaque", "_fields"]
    def __init__(self, struct_id, super, isopaque, fields):
        self._type = struct_id
        self._super = super
        self._isopaque = isopaque
        self._fields = fields

    def _lookup(self, struct, struct_id, field):
        if struct.type() == struct_id:
            return struct._fields[field]
        elif struct.super() is not None:
            return struct._lookup(struct.super(), struct_id, field)
        else:
            raise SchemeException("invalid field index %s" % field)

    def _save(self, struct, struct_id, field, val):
        if struct.type() == struct_id:
            struct._fields[field] = val
        else:
            struct._save(struct.super(), struct_id, field, val)

    def _vals(self, struct):
        result = [field.tostring() for field in struct.fields()]
        if struct.super() is not None: return self._vals(struct.super()) + result
        else: return result

    # FIXME: racket replaces superclass fields with dots, do same?
    def tostring(self):
        if self._isopaque: result =  "#<%s>" % self._type
        else: result = "(%s %s)" % (self._type, ' '.join(self._vals(self)))
        return result

    def equal(self, other):
        if not isinstance(other, W_Struct):
            result = False
        else:
            if self._isopaque: result = self == other
            else: result = self._type == other.type() and self._vals(self) == self._vals(other)
        return result

    def type(self):
        return self._type

    def super(self):
        return self._super

    def fields(self):
        return self._fields

    def get_value(self, struct_id, field):
        return self._lookup(self, struct_id, field)

    def set_value(self, struct_id, field, val):
        self._save(self, struct_id, field, val)
