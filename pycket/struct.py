from pycket import values
from pycket.cont import continuation
from pycket.error import SchemeException
from pycket.exposeprim import make_call_method
from pycket.small_list import inline_small_list
from pycket.values import from_list, w_void, w_null, w_false, w_true, W_Object, W_Fixnum, W_Symbol, W_Procedure, W_Prim
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
    _immutable_fields_ = ["id", "super", "init_field_cnt", "auto_field_cnt", "auto_v", "inspector", \
                          "immutables", "guard", "constr_name"]
    @staticmethod
    def make(name, super_type, init_field_cnt, auto_field_cnt, auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        struct_id = W_StructTypeDescriptor(name.value)
        W_StructType.all_structs[struct_id] = w_result = W_StructType(struct_id, super_type, init_field_cnt, auto_field_cnt, \
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name)
        return w_result

    @staticmethod
    def lookup_struct_type(struct_id):
        return W_StructType.all_structs.get(struct_id, w_false)
    
    def __init__(self, struct_id, super_type, init_field_cnt, auto_field_cnt,
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        self.super = W_StructType.lookup_struct_type(super_type) if super_type is not w_false else None
        self.init_field_cnt = init_field_cnt.value
        self.auto_field_cnt = auto_field_cnt.value
        # Next arguments are optional
        self.auto_v = auto_v
        # self.props = props
        self.inspector = inspector
        # self.proc_spec = proc_spec
        self.immutables = []
        for i in values.from_list(immutables):
            assert isinstance(i, values.W_Fixnum)
            self.immutables.append(i.value)
        self.guard = guard
        if isinstance(constr_name, W_Symbol):
            self.constr_name = constr_name.value
        else:
            self.constr_name = "make-" + struct_id.value

        self.auto_values = [self.auto_v] * self.auto_field_cnt
        self.isopaque = self.inspector is not w_false
        self.mutable_fields = []

        self.desc = struct_id
        self.constr = W_StructConstructor(self.desc, self.super, self.init_field_cnt, self.auto_values,
                                          self.make_mapping, self.isopaque, self.guard, self.constr_name)
        self.pred = W_StructPredicate(self.desc)
        self.acc = W_StructAccessor(self.desc)
        self.mut = W_StructMutator(self.desc)

    def constructor(self):
        return self.constr
    def set_mutable(self, field):
        assert isinstance(field, W_Fixnum)
        self.mutable_fields.append(field.value)

    """
    This method has to be called after initialization, otherwise self.mutable_fields will be empty
    """
    def make_mapping(self):
        result = {}
        immutable_cnt = 0
        for field in range(self.init_field_cnt + self.auto_field_cnt):
            if field in self.mutable_fields:
                result[field] = (False, self.mutable_fields.index(field))
            else:
                result[field] = (True, immutable_cnt)
                immutable_cnt += 1
        return result
    def make_struct_tuple(self):
        return [self.desc, self.constr, self.pred, self.acc, self.mut]

class W_RootStruct(W_Object):
    errorname = "root-struct"
    _immutable_fields_ = ["type", "super", "isopaque"]

    def __init__(self, struct_id, super, isopaque):
        self.type = struct_id
        self.super = super
        self.isopaque = isopaque

    @continuation
    def ref(self, struct_id, field, env, cont, _vals):
        raise NotImplementedError("base class")

    @continuation
    def set(self, struct_id, field, val, env, cont, _vals):
        raise NotImplementedError("base class")

    def vals(self):
        raise NotImplementedError("base class")

class W_Struct(W_RootStruct):
    errorname = "struct"
    _immutable_fields_ = ["type", "super", "isopaque", "fields"]
    def __init__(self, mutable_vals, fields_map, struct_id, super, isopaque):
        W_RootStruct.__init__(self, struct_id, super, isopaque)
        self.mutable_vals = mutable_vals
        self.fields_map = fields_map

    def vals(self):
        immutable_vals = self._get_full_list()
        result = []
        for item in self.fields_map:
            isImmutable, pos = self.fields_map[item]
            result.append(immutable_vals[pos] if isImmutable else self.mutable_vals[pos])
        if self.super is not None: 
            return self.super.vals() + result
        else:
            return result

    # Rather than reference functions, we store the continuations. This is
    # necessarray to get constant stack usage without adding extra preamble
    # continuations.
    @continuation
    def ref(self, struct_id, field, env, cont, _vals):
        from pycket.interpreter import return_value, jump
        super = self.super
        if self.type == struct_id:
            isImmutable, pos = self.fields_map[field]
            result = self._get_list(pos) if isImmutable else self.mutable_vals[pos]
            return return_value(result, env, cont)
        elif self.type.value == struct_id.value:
            raise SchemeException("given value instantiates a different structure type with the same name")
        elif isinstance(super, W_Struct):
            return jump(env, super.ref(struct_id, field, env, cont))
        else:
            assert False

    @continuation
    def set(self, struct_id, field, val, env, cont, _vals):
        from pycket.interpreter import return_value, jump
        super = self.super
        type = jit.promote(self.type)
        if type == struct_id:
            self.mutable_vals[self.fields_map[field][1]] = val
            return return_value(w_void, env, cont)
        elif type.value == struct_id.value:
            raise SchemeException("given value instantiates a different structure type with the same name")
        elif isinstance(super, W_Struct):
            return jump(env, super.set(struct_id, field, val, env, cont))
        else:
            assert False

    def tostring(self):
        if self.isopaque:
            result =  "#<%s>" % self.type.value
        else:
            result = "(%s %s)" % (self.type.value, ' '.join([val.tostring() for val in self.vals()]))
        return result

inline_small_list(W_Struct, immutable=True, attrname="immutable_vals")

class W_StructTypeDescriptor(W_Object):
    errorname = "struct-type-descriptor"
    _immutable_fields_ = ["value"]
    def __init__(self, value):
        self.value = value
    def tostring(self):
        return "#<struct-type:%s>" % self.value

class W_StructConstructor(W_Procedure):
    _immutable_fields_ = ["struct_id", "super_type", "init_field_cnt", "auto_values", "isopaque", "guard", "name"]
    def __init__ (self, struct_id, super_type, init_field_cnt, auto_values, make_mapping, isopaque, guard, name):
        self.struct_id = struct_id
        self.super_type = super_type
        self.init_field_cnt = init_field_cnt
        self.auto_values = auto_values
        self.make_mapping = make_mapping
        self.fields_map = None
        self.isopaque = isopaque
        self.guard = guard
        self.name = name

    @continuation
    def constr_proc_cont(self, field_values, env, cont, _vals):
        from pycket.interpreter import return_value
        vals = _vals._get_full_list()
        super = None
        if len(vals) == 1: super = vals[0]
        immutable_vals, mutable_vals = [], []
        if not self.fields_map: self.fields_map = self.make_mapping()
        for idx, val in enumerate(field_values + self.auto_values):
            if self.fields_map[idx][0]: immutable_vals.append(val)
            else: mutable_vals.append(val)
        result = W_Struct.make(immutable_vals, mutable_vals, self.fields_map, self.struct_id, super, self.isopaque)
        return return_value(result, env, cont)

    @continuation
    def constr_proc_wrapper_cont(self, field_values, env, cont, _vals):
        from pycket.interpreter import jump
        if isinstance(self.super_type, W_StructType):
            def split_list(list, num):
                assert num >= 0
                return list[:num], list[num:]
            split_position = len(field_values) - self.init_field_cnt
            super_field_values, field_values = split_list(field_values, split_position)
            super_constr = self.super_type.constructor()
            return jump(env, self.constr_proc_cont(field_values, env, super_constr.constr_proc_cont(super_field_values, env, cont)))
        else:
            return jump(env, self.constr_proc_cont(field_values, env, cont))

    def call(self, args, env, cont):
        from pycket.interpreter import jump
        if self.guard is w_false:
            return jump(env, self.constr_proc_wrapper_cont(args, env, cont))
        else:
            assert isinstance(self.struct_id, W_StructTypeDescriptor)
            guard_args = args + [W_Symbol.make(self.struct_id.value)]
            return self.guard.call(guard_args, env, self.constr_proc_wrapper_cont(args, env, cont))

    def tostring(self):
        return "#<procedure:%s>" % self.name

class W_StructPredicate(W_Procedure):
    errorname = "struct-predicate"
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self.struct_id = struct_id

    @make_call_method([W_Object])
    def call(self, struct):
        result = w_false
        if (isinstance(struct, W_Struct)):
            while True:
                assert isinstance(struct, W_Struct)
                if struct.type == self.struct_id:
                    result = w_true
                    break
                if struct.super is None: break
                else: struct = struct.super
        return result
    def tostring(self):
        return "#<procedure:%s?>" % self.struct_id.value

class W_StructFieldAccessor(W_Procedure):
    errorname = "struct-field-accessor"
    _immutable_fields_ = ["accessor", "field"]
    def __init__ (self, accessor, field):
        assert isinstance(accessor, W_StructAccessor)
        self.accessor = accessor
        self.field = field

    @make_call_method([W_RootStruct], simple=False)
    def call(self, struct, env, cont):
        return self.accessor.access(struct, self.field, env, cont)

class W_StructAccessor(W_Procedure):
    errorname = "struct-accessor"
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self.struct_id = struct_id

    def access(self, struct, field, env, cont):
        from pycket.interpreter import jump
        return jump(env, struct.ref(self.struct_id, field.value, env, cont))

    call = make_call_method([W_Struct, W_Fixnum], simple=False)(access)

    def tostring(self):
        return "#<procedure:%s-ref>" % self.struct_id.value

class W_StructFieldMutator(W_Procedure):
    errorname = "struct-field-mutator"
    _immutable_fields_ = ["mutator", "field"]
    def __init__ (self, mutator, field):
        assert isinstance(mutator, W_StructMutator)
        self.mutator = mutator
        self.field = field
        mutator.set_mutable(field)

    @make_call_method([W_RootStruct, W_Object], simple=False)
    def call(self, struct, val, env, cont):
        return self.mutator.mutate(struct, self.field, val, env, cont)

class W_StructMutator(W_Procedure):
    errorname = "struct-mutator"
    _immutable_fields_ = ["struct_id"]
    def __init__ (self, struct_id):
        self.struct_id = struct_id

    def mutate(self, struct, field, val, env, cont):
        from pycket.interpreter import jump
        return jump(env, struct.set(self.struct_id, field.value, val, env, cont))

    call = make_call_method([W_RootStruct, W_Fixnum, W_Object], simple=False)(mutate)

    def set_mutable(self, field):
        struct = W_StructType.lookup_struct_type(self.struct_id)
        struct.set_mutable(field)
    def tostring(self):
        return "#<procedure:%s-set!>" % self.struct_id.value

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

class W_StructPropertyPredicate(W_Procedure):
    errorname = "struct-property-predicate"
    _immutable_fields_ = ["property"]
    def __init__(self, prop):
        self.property = prop
    @make_call_method([W_Object])
    def call(self, args):
        raise SchemeException("StructPropertyPredicate %s NYI"%self.property.name)

class W_StructPropertyAccessor(W_Procedure):
    errorname = "struct-property-accessor"
    _immutable_fields_ = ["property"]
    def __init__(self, prop):
        self.property = prop
    @make_call_method([W_Object])
    def call(self, args):
        raise SchemeException("StructPropertyAccessor %s NYI"%self.property.name)
