from pycket.values import w_false, w_true, W_Object, W_Fixnum, W_SimplePrim, W_Symbol
from rpython.rlib import jit

#
# Structs are partially supported
# 
# Not yet implemented:
# 1) inspector
# 2) prefab -- '#s(sprout bean): update expand.rkt
# 3) methods overriding (including equal) -- generic-interfaces.rkt
# 4) guard
# 5) properties and super as an argument -- kw.rkt
class W_StructType(W_Object):
    all_structs = {}
    errorname = "struct"
    _immutable_fields_ = ["_id", "_super", "_init_field_cnt", "_auto_field_cnt", "_auto_v", "_inspector", "_constr_name", "_guard"]
    
    @staticmethod 
    def make(args):
        struct_id = W_StructTypeDescriptor(args[0])
        W_StructType.all_structs[struct_id] = w_result = W_StructType(struct_id, args)
        return w_result

    @staticmethod
    def lookup_struct_type(struct_id):
        if struct_id in W_StructType.all_structs:
            return W_StructType.all_structs[struct_id]
        else:
            return w_false
    
    def __init__(self, struct_id, args):
        super = args[1]
        self._super = W_StructType.lookup_struct_type(super) if super != w_false else None
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
        # self._immutables = from_list(args[8]) if len(args) > 8 else []
        self._guard = args[9] if len(args) > 9 and args[9] != w_false else None
        if len(args) > 10:
            constr_name = args[10]
            assert isinstance(constr_name, W_Symbol)
            self._constr_name = constr_name.value
        else:
            constr_name = struct_id.id()
            assert isinstance(constr_name, W_Symbol)
            self._constr_name = "make-" + constr_name.value

        # structure types are opaque by default
        self._isopaque = True if self._inspector != w_false else False

        self._desc = struct_id
        self._constr = W_StructConstructor(self._desc, self._super, self._init_field_cnt, self._auto_field_cnt, \
                                            self._auto_v, self._isopaque, self._constr_name, self._guard)
        self._pred = W_StructPredicate(self._desc)
        self._acc = W_StructAccessor(self._desc)
        self._mut = W_StructMutator(self._desc)

    def struct_id(self):
        return self._desc
    def super(self):
        return self._super
    def init_field_cnt(self):
        return self._init_field_cnt
    def auto_field_cnt(self):
        return self._auto_field_cnt
    def isopaque(self):
        return self._opaque
    def setmutable(self, field):
        # TODO: save the mutability of field
    def constr(self):
        return self._constr
    def make_struct_tuple(self):
        return [self._desc, self._constr, self._pred, self._acc, self._mut]
    def tostring(self):
        return "StructType<%s>" % self._desc.tostring()

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
    def __init__ (self, struct_id, super_type, init_field_cnt, auto_field_cnt, auto_v, isopaque, name, guard):
        self._struct_id = struct_id
        self._super_type = super_type
        self._init_field_cnt = init_field_cnt
        self._auto_field_cnt = auto_field_cnt
        self._auto_v = auto_v
        self._isopaque = isopaque
        self._name = name
        self._guard = guard
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
    def call(self, field_values, env, cont):
        from pycket.interpreter import return_value
        result = field_values
        if self._guard is not None:
            pass
            # TODO: prepare arguments
            # import pdb; pdb.set_trace()
            # LET  (result <- (apply guard old-value))
            # make_let()
        return return_value(self.simplecall(result), env, cont)
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
        return "#<procedure:%s?>" % self._struct_id.id()

class W_StructFieldAccessor(W_SimplePrim):
    _immutable_fields_ = ["accessor", "field"]
    def __init__ (self, accessor, field):
        self._accessor = accessor
        self._field = field
    def simplecall(self, args):
        struct = args[0]
        return self._accessor.simplecall([struct, self._field])

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
        return "#<procedure:%s-ref>" % self._struct_id.id()

class W_StructFieldMutator(W_SimplePrim):
    _immutable_fields_ = ["mutator", "field"]
    def __init__ (self, mutator, field):
        self._mutator = mutator
        self._field = field
        mutator.setmutable(field)
    def simplecall(self, args):
        struct = args[0]
        val = args[1]
        return self._mutator.simplecall([struct, self._field, val])

class W_StructMutator(W_SimplePrim):
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self._struct_id = struct_id
    def simplecall(self, args):
        struct, field, val = args
        assert isinstance(field, W_Fixnum)
        index = field.value
        struct.set_value(self._struct_id, index, val)
    def setmutable(self, field):
        struct = W_StructType.lookup_struct_type(self._struct_id)
        struct.setmutable(field)
    def tostring(self):
        return "#<procedure:%s-set!>" % self._struct_id.id()

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
        elif struct.type().id() == struct_id.id():
            raise SchemeException("given value instantiates a different structure type with the same name")
        elif struct.super() is not None:
            return struct._lookup(struct.super(), struct_id, field)

    def _save(self, struct, struct_id, field, val):
        if struct.type() == struct_id:
            struct._fields[field] = val
        else:
            struct._save(struct.super(), struct_id, field, val)

    # FIXME: racket replaces superclass field values with dots
    def _vals(self, struct):
        result = [field.tostring() for field in struct.fields()]
        if struct.super() is not None: return self._vals(struct.super()) + result
        else: return result

    def tostring(self):
        if self._isopaque: result =  "#<%s>" % self._type.id()
        else: result = "(%s %s)" % (self._type.id(), ' '.join(self._vals(self)))
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
