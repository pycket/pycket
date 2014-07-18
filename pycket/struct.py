from pycket.cont import continuation
from pycket.error import SchemeException
from pycket.values import from_list, w_false, w_true, W_Object, W_Fixnum, W_SimplePrim, W_Symbol, w_null, W_Procedure
from pycket.exposeprim import make_call_method
from rpython.rlib import jit

#
# Structs are partially supported
# 
# Not implemented:
# 1) prefab -- '#s(sprout bean): need update in expand.rkt
# 2) methods overriding (including equal) -- generic-interfaces.rkt
# 3) properties and super as an argument -- kw.rkt

# TODO: inspector currently does nothing
class W_StructInspector(W_Object):
    errorname = "struct-inspector"
    _immutable_fields_ = ["_super"]

    @staticmethod 
    def make(inspector, issibling = False):
        super = inspector
        if issibling:
            super = inspector.super() if inspector is not None else None
        return W_StructInspector(super)

    def __init__(self, super):
        self._super = super
    def super(self):
        return self._super

current_inspector = W_StructInspector(None)

class W_StructType(W_Object):
    all_structs = {}
    errorname = "struct-type"
    _immutable_fields_ = ["_id", "_super", "_init_field_cnt", "_auto_field_cnt", "_auto_v", "_inspector", \
                          "_immutables", "_guard", "_constr_name"]
    @staticmethod
    def make(name, super_type, init_field_cnt, auto_field_cnt, auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        struct_id = W_StructTypeDescriptor(name)
        W_StructType.all_structs[struct_id] = w_result = W_StructType(struct_id, super_type, init_field_cnt, auto_field_cnt, \
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name)
        return w_result

    @staticmethod
    def lookup_struct_type(struct_id):
        if struct_id in W_StructType.all_structs:
            return W_StructType.all_structs[struct_id]
        else:
            return w_false
    
    def __init__(self, struct_id, super_type, init_field_cnt, auto_field_cnt, \
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        self._super = W_StructType.lookup_struct_type(super_type) if super_type != w_false else None
        self._init_field_cnt = init_field_cnt.value
        self._auto_field_cnt = auto_field_cnt.value
        # Next arguments are optional
        self._auto_v = auto_v
        # self._props = props
        self._inspector = inspector
        # self._proc_spec = proc_spec
        self._immutables = immutables
        self._guard = guard
        if isinstance(constr_name, W_Symbol):
            self._constr_name = constr_name.value
        else:
            constr_name = struct_id.id()
            assert isinstance(constr_name, W_Symbol)
            self._constr_name = "make-" + constr_name.value

        # structure types are opaque by default
        self._isopaque = True if self._inspector != w_false else False

        self._desc = struct_id
        self._constr = W_StructConstructor(self._desc, self._super, self._init_field_cnt, self._auto_field_cnt, \
                                            self._auto_v, self._isopaque, self._guard, self._constr_name)
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
    def immutables(self):
        return self._immutables
    def isopaque(self):
        return self._opaque
    def setmutable(self, field):
        pass
        # TODO: save the mutability of field
    def constr(self):
        return self._constr
    def pred(self):
        return self._pred
    def acc(self):
        return self._acc
    def mut(self):
        return self._mut
    def make_struct_tuple(self):
        return [self._desc, self._constr, self._pred, self._acc, self._mut]
    def tostring(self):
        return "StructType<%s>" % self._desc.tostring()

class W_StructTypeDescriptor(W_Object):
    errorname = "struct-type-descriptor"
    _immutable_fields_ = ["_id"]
    def __init__(self, id):
        self._id = id
    def id(self):
        return self._id
    def tostring(self):
        return "#<struct-type:%s>" % self._id

class W_StructConstructor(W_Procedure):
    _immutable_fields_ = ["_struct_id", "_super_type", "_init_field_cnt", "_auto_values", "_isopaque", "_guard", "_name"]
    def __init__ (self, struct_id, super_type, init_field_cnt, auto_field_cnt, auto_v, isopaque, guard, name):
        self._struct_id = struct_id
        self._super_type = super_type
        self._init_field_cnt = init_field_cnt
        self._auto_values = [auto_v] * auto_field_cnt
        self._isopaque = isopaque
        self._guard = guard
        self._name = name

    def extcode(self, field_values, env, cont):
        if self._guard != w_false:
            cont = guard_check(self._guard, field_values, env, cont)
        super = None
        if self._super_type is not None:
            def split_list(list, num):
                assert num >= 0
                return list[:num], list[num:]
            split_position = len(field_values) - self._init_field_cnt
            super_field_values, field_values = split_list(field_values, split_position)
            super, env, cont = self._super_type.constr().extcode(super_field_values, env, cont)
        result = W_Struct(self._struct_id, super, self._isopaque, field_values + self._auto_values)
        return result, env, cont

    def call(self, args, env, cont):
        from pycket.interpreter import return_value
        result, env, cont = self.extcode(args, env, cont)
        return return_value(result, env, cont)

    def tostring(self):
        return "#<procedure:%s>" % self._name

@continuation
def guard_check(proc, field_values, env, cont, _vals):
    vals = _vals._get_full_list()
    struct = vals[0]
    assert isinstance(struct, W_Struct)
    args = [struct._type] + field_values
    return proc.call(args, env, cont)

class W_StructProperty(W_Object):
    errorname = "struct-type-property"
    _immutable_fields_ = ["name", "guard", "supers", "can_imp"]
    def __init__(self, name, guard, supers=w_null, can_imp=False):
        self.name = name
        self.guard = guard
        self.supers = supers
        self.can_imp = can_imp
    def tostring(self):
        return "#<struct-type-property:%s>"%self.name

class W_Struct(W_Object):
    errorname = "struct"
    _immutable_fields_ = ["_type", "_super", "_isopaque", "_fields"]
    def __init__(self, struct_id, super, isopaque, fields):
        self._type = struct_id
        self._super = super
        self._isopaque = isopaque
        self._fields = fields
    def vals(self):
        result = self._fields
        if self._super is not None: 
            return self._super.vals() + result
        else:
            return result
    def ref(self, struct_id, field):
        if self._type == struct_id:
            return self._fields[field]
        elif self._type.id() == struct_id.id():
            raise SchemeException("given value instantiates a different structure type with the same name")
        elif self._super is not None:
            return self._super.ref(struct_id, field)
        else:
            assert False
    def set(self, struct_id, field, val):
        type = jit.promote(self._type)
        if type == struct_id:
            self._fields[field] = val
        else:
            self._super.set(struct_id, field, val)
    def tostring(self):
        if self._isopaque:
            result =  "#<%s>" % self._type.id().value
        else:
            result = "(%s %s)" % (self._type.id().value, ' '.join([val.tostring() for val in self.vals()]))
        return result

class W_StructPropertyPredicate(W_SimplePrim):
    errorname = "struct-property-predicate"
    _immutable_fields_ = ["property"]
    def __init__(self, prop):
        self.property = prop
    def code(self, args):
        raise SchemeException("StructPropertyPredicate NYI")

class W_StructPropertyAccessor(W_SimplePrim):
    errorname = "struct-property-accessor"
    _immutable_fields_ = ["property"]
    def __init__(self, prop):
        self.property = prop
    def code(self, args):
        raise SchemeException("StructPropertyAccessor NYI")

class W_StructPredicate(W_Procedure):
    errorname = "struct-predicate"
    _immutable_fields_ = ["_struct_id"]
    def __init__ (self, struct_id):
        self._struct_id = struct_id

    @make_call_method([W_Struct])
    def call(self, struct):
        result = w_false
        if (isinstance(struct, W_Struct)):
            while True:
                if struct._type == self._struct_id:
                    result = w_true
                    break
                if struct._super is None: break
                else: struct = struct._super
        return result
    def tostring(self):
        return "#<procedure:%s?>" % self._struct_id.id()

class W_StructFieldAccessor(W_Procedure):
    errorname = "struct-field-accessor"
    _immutable_fields_ = ["_accessor", "_field"]
    def __init__ (self, accessor, field):
        assert isinstance(accessor, W_StructAccessor)
        self._accessor = accessor
        self._field = field

    @make_call_method([W_Struct])
    def call(self, struct):
        return self._accessor.access(struct, self._field)

class W_StructAccessor(W_Procedure):
    errorname = "struct-accessor"
    _immutable_fields_ = ["_struct_id"]
    def __init__ (self, struct_id):
        self._struct_id = struct_id

    def access(self, struct, field):
        return struct.ref(self._struct_id, field.value)

    call = make_call_method([W_Struct, W_Fixnum])(access)

    def tostring(self):
        return "#<procedure:%s-ref>" % self._struct_id.id()

class W_StructFieldMutator(W_Procedure):
    errorname = "struct-field-mutator"
    _immutable_fields_ = ["_mutator", "_field"]
    def __init__ (self, mutator, field):
        assert isinstance(mutator, W_StructMutator)
        self._mutator = mutator
        self._field = field
        mutator.setmutable(field)

    @make_call_method([W_Struct, W_Object])
    def call(self, struct, val):
        return self._mutator.mutate(struct, self._field, val)

class W_StructMutator(W_Procedure):
    errorname = "struct-mutator"
    _immutable_fields_ = ["_struct_id"]
    def __init__ (self, struct_id):
        self._struct_id = struct_id

    def mutate(self, struct, field, val):
        struct.set(self._struct_id, field.value, val)

    call = make_call_method([W_Struct, W_Fixnum, W_Object])(mutate)

    def setmutable(self, field):
        struct = W_StructType.lookup_struct_type(self._struct_id)
        struct.setmutable(field)
    def tostring(self):
        return "#<procedure:%s-set!>" % self._struct_id.id()

