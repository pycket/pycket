from pycket.cont import continuation
from pycket.error import SchemeException
from pycket.values import from_list, w_false, w_true, W_Object, W_Fixnum, W_SimplePrim, W_Symbol, w_null
from pycket.small_list import inline_small_list
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
    _immutable_fields_ = ["super"]

    @staticmethod 
    def make(inspector, issibling = False):
        super = inspector
        if issibling:
            super = inspector.super if inspector is not None else None
        return W_StructInspector(super)

    def __init__(self, super):
        self.super = super

current_inspector = W_StructInspector(None)

class W_StructType(W_Object):
    all_structs = {}
    errorname = "struct-type"
    _immutable_fields_ = ["id", "super", "init_field_cnt", "auto_field_cnt", "auto_v", "inspector", "immutables", "guard", "constr_name"]
    @staticmethod
    def make(name, super_type, init_field_cnt, auto_field_cnt, auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        struct_id = W_StructTypeDescriptor(name.value)
        W_StructType.all_structs[struct_id] = w_result = W_StructType(struct_id, super_type, init_field_cnt, auto_field_cnt, auto_v, \
            props, inspector, proc_spec, immutables, guard, constr_name)
        return w_result

    @staticmethod
    def lookup_struct_type(struct_id):
        if struct_id in W_StructType.all_structs:
            return W_StructType.all_structs[struct_id]
        else:
            return w_false
    
    def __init__(self, struct_id, super_type, init_field_cnt, auto_field_cnt, \
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        self.super = W_StructType.lookup_struct_type(super_type) if super_type != w_false else None
        self.init_field_cnt = init_field_cnt.value
        self.auto_field_cnt = auto_field_cnt.value
        # Next arguments are optional
        self.auto_v = auto_v
        # self.props = props
        self.inspector = inspector
        # self.proc_spec = proc_spec
        self.immutables = immutables
        self.guard = guard
        if isinstance(constr_name, W_Symbol):
            self.constr_name = constr_name.value
        else:
            self.constr_name = "make-" + struct_id.value

        self.isopaque = True if self.inspector != w_false else False
        self.mutable_fields = []

        self.id = struct_id
        self.constr = W_StructConstructor(self.id, self.super, self.init_field_cnt, self.auto_field_cnt, self.auto_v, \
            self.mutable_fields, self.isopaque, self.guard, self.constr_name)
        self.pred = W_StructPredicate(self.id)
        self.acc = W_StructAccessor(self.id)
        self.mut = W_StructMutator(self.id)
    def constructor(self):
        return self.constr
    def set_mutable(self, field):
        assert isinstance(field, W_Fixnum)
        self.mutable_fields.append(field.value)
    def make_struct_tuple(self):
        return [self.id, self.constr, self.pred, self.acc, self.mut]

class W_StructTypeDescriptor(W_Object):
    errorname = "struct-type-descriptor"
    _immutable_fields_ = ["value"]
    def __init__(self, value):
        self.value = value
    def tostring(self):
        return "#<struct-type:%s>" % self.value

class W_StructConstructor(W_SimplePrim):
    _immutable_fields_ = ["struct_id", "super_type", "init_field_cnt", "auto_values", "isopaque", "guard", "name"]
    def __init__ (self, struct_id, super_type, init_field_cnt, auto_field_cnt, auto_v, mutable_fields, isopaque, guard, name):
        self.struct_id = struct_id
        self.super_type = super_type
        self.init_field_cnt = init_field_cnt
        self.auto_values = [auto_v] * auto_field_cnt
        self.mutable_fields = mutable_fields
        self.isopaque = isopaque
        self.guard = guard
        self.name = name
    def extcode(self, field_values, env, cont):
        if self.guard != w_false:
            cont = guard_check(self.guard, field_values, env, cont)
        super = None
        if isinstance(self.super_type, W_StructType):
            def split_list(list, num):
                assert num >= 0
                return list[:num], list[num:]
            split_position = len(field_values) - self.init_field_cnt
            super_field_values, field_values = split_list(field_values, split_position)
            super, env, cont = self.super_type.constructor().extcode(super_field_values, env, cont)
        vals = field_values + self.auto_values
        immutable_vals, mutable_vals = [], []
        for idx, val in enumerate(vals):
            if idx in self.mutable_fields: mutable_vals.append(val)
            else: immutable_vals.append(val)
        result = W_Struct.make(immutable_vals, self.mutable_fields, mutable_vals, self.struct_id, super, self.isopaque)
        return result, env, cont
    def call(self, args, env, cont):
        from pycket.interpreter import return_value
        result, env, cont = self.extcode(args, env, cont)
        return return_value(result, env, cont)
    def tostring(self):
        return "#<procedure:%s>" % self.name

@continuation
def guard_check(proc, field_values, env, cont, _vals):
    vals = _vals._get_full_list()
    struct = vals[0]
    assert isinstance(struct, W_Struct)
    args = [struct.type] + field_values
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

class W_StructPredicate(W_SimplePrim):
    errorname = "struct-predicate"
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self.struct_id = struct_id
    def code(self, args):
        struct = args[0]
        result = w_false
        if (isinstance(struct, W_Struct)):
            while True:
                if struct.type == self.struct_id:
                    result = w_true
                    break
                if struct.super is None: break
                else: struct = struct.super
        return result
    def tostring(self):
        return "#<procedure:%s?>" % self.struct_id.value

class W_StructFieldAccessor(W_SimplePrim):
    errorname = "struct-field-accessor"
    _immutable_fields_ = ["accessor", "field"]
    def __init__ (self, accessor, field):
        assert isinstance(accessor, W_StructAccessor)
        self.accessor = accessor
        self.field = field
    def code(self, args):
        struct = args[0]
        return self.accessor.code([struct, self.field])

class W_StructAccessor(W_SimplePrim):
    errorname = "struct-accessor"
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self.struct_id = struct_id
    def code(self, args):
        struct, field = args
        assert isinstance(struct, W_Struct)
        assert isinstance(field, W_Fixnum)
        return struct.ref(self.struct_id, field.value)
    def tostring(self):
        return "#<procedure:%s-ref>" % self.struct_id.value

class W_StructFieldMutator(W_SimplePrim):
    errorname = "struct-field-mutator"
    _immutable_fields_ = ["mutator", "field"]
    def __init__ (self, mutator, field):
        assert isinstance(mutator, W_StructMutator)
        self.mutator = mutator
        self.field = field
        mutator.set_mutable(field)
    def code(self, args):
        struct = args[0]
        val = args[1]
        return self.mutator.code([struct, self.field, val])

class W_StructMutator(W_SimplePrim):
    errorname = "struct-mutator"
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self.struct_id = struct_id
    def code(self, args):
        struct, field, val = args
        assert isinstance(struct, W_Struct)
        assert isinstance(field, W_Fixnum)
        struct.set(self.struct_id, field.value, val)
    def set_mutable(self, field):
        struct = W_StructType.lookup_struct_type(self.struct_id)
        struct.set_mutable(field)
    def tostring(self):
        return "#<procedure:%s-set!>" % self.struct_id.value

class W_Struct(W_Object):
    errorname = "struct"
    _immutable_fields_ = ["mutable_fields", "mutable_vals", "type", "super", "isopaque"]
    def __init__(self, mutable_fields, mutable_vals, struct_id, super, isopaque):
        self.mutable_fields = mutable_fields
        self.mutable_vals = mutable_vals
        self.type = struct_id
        self.super = super
        self.isopaque = isopaque
    # FIXME: make me more beautiful
    @jit.elidable
    def map(self, field):
        """
        Example:
        original struct values: (a: mutable, b: mutable, c: immutable)
        stored data:
          immutable_vals: (0: c) -- saved inline
          mutable_vals: (0: a, 1: b)
          mutable_fields: (0, 1)
        
        1. field (int) is a mutable field (self.mutable_fields is an array of integers)
        strategy: find all immutable fields before and subtract this number from field index
        """
        if field in self.mutable_fields:
            for immutable_field in [item for item in xrange(field) if item not in self.mutable_fields]:
                if immutable_field < field: field -= 1
                else: break
        """
        2. field is an immutable field.
        stratege: do the same, but subtract the number of all mutable fields before
        """
        else:
            for mutable_field in self.mutable_fields:
                if mutable_field < field: field -= 1
                else: break
        return field
    def vals(self):
        result = self._get_full_list()
        if self.super is not None: 
            return self.super.vals() + result
        else:
            return result
    def ref(self, struct_id, field):
        if self.type == struct_id:
            return self._get_list(self.map(field)) if field not in self.mutable_fields else self.mutable_vals[self.map(field)]
        elif self.type.value == struct_id.value:
            raise SchemeException("given value instantiates a different structure type with the same name")
        elif self.super is not None:
            return self.super.ref(struct_id, field)
        else:
            assert False
    def set(self, struct_id, field, val):
        type = jit.promote(self.type)
        if type == struct_id:
            self.mutable_vals[self.map(field)] = val
        else:
            assert isinstance(self.super, W_Struct)
            self.super.set(struct_id, field, val)
    def tostring(self):
        if self.isopaque:
            result =  "#<%s>" % self.type.value
        else:
            result = "(%s %s)" % (self.type.value, ' '.join([val.tostring() for val in self.vals()]))
        return result
inline_small_list(W_Struct, immutable=True, attrname="immutable_vals")
