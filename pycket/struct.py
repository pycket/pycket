from collections import namedtuple
from pycket import values
from pycket.cont import continuation, label
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

class W_RootStructType(W_Object):
    all_structs = {}
    errorname = "root-struct-type"
    _immutable_fields_ = ["id", "super", "init_field_cnt", "auto_field_cnt", "auto_v", "inspector", \
                          "immutables", "guard", "constr_name"]
    @staticmethod
    def make(name, super_type, init_field_cnt, auto_field_cnt, auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        struct_id = W_StructTypeDescriptor(name.value)
        W_StructType.all_structs[struct_id] = w_result = W_StructType(struct_id, super_type, init_field_cnt, auto_field_cnt, \
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name)
        return w_result

    @staticmethod
    def make_callable(name, super_type, init_field_cnt, auto_field_cnt, auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        struct_id = W_StructTypeDescriptor(name.value)
        W_StructType.all_structs[struct_id] = w_result = W_CallableStructType(struct_id, super_type, init_field_cnt, auto_field_cnt, \
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
        self.auto_v = auto_v
        self.props = values.from_list(props) if not props is values.w_null else []
        self.inspector = inspector
        self.proc_spec = proc_spec
        self.immutables = []
        if isinstance(proc_spec, W_Fixnum):
            # TODO: Providing a non-#f value for proc-spec is the same as pairing the value with prop:procedure at the end of props
            self.immutables.append(proc_spec.value)
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
        # self.mutable_fields = []

        self.desc = struct_id
        self.pred = W_StructPredicate(self.desc)
        self.acc = W_StructAccessor(self.desc)
        self.mut = W_StructMutator(self.desc)

    def set_mutable(self, field):
        pass
        # assert isinstance(field, W_Fixnum)
        # self.mutable_fields.append(field.value)

    # """
    # This method has to be called after initialization, otherwise self.mutable_fields will be empty
    # """
    def calculate_offset(self):
        result = {}
        struct_type = self
        vals_offset = 0
        while True:
            vals_offset += struct_type.init_field_cnt + struct_type.auto_field_cnt
            struct_type = struct_type.super
            if isinstance(struct_type, W_StructType):
                result[struct_type.desc] = vals_offset
            else:
                break
        return result

    def make_struct_tuple(self):
        return [self.desc, self.constr, self.pred, self.acc, self.mut]

class W_StructType(W_RootStructType):
    errorname = "struct-type"
    def __init__(self, struct_id, super_type, init_field_cnt, auto_field_cnt,
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        W_RootStructType.__init__(self, struct_id, super_type, init_field_cnt, auto_field_cnt,
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name)
        self.constr = W_StructConstructor(self.desc, self.super, self.init_field_cnt, self.auto_values, self.props, 
            self.proc_spec, self.calculate_offset, self.isopaque, self.guard, self.constr_name)

class W_CallableStructType(W_RootStructType):
    errorname = "callable-struct-type"
    def __init__(self, struct_id, super_type, init_field_cnt, auto_field_cnt,
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name):
        W_RootStructType.__init__(self, struct_id, super_type, init_field_cnt, auto_field_cnt,
            auto_v, props, inspector, proc_spec, immutables, guard, constr_name)
        self.constr = W_CallableStructConstructor(self.desc, self.super, self.init_field_cnt, self.auto_values, self.props, 
            self.proc_spec, self.calculate_offset, self.isopaque, self.guard, self.constr_name)

class W_CallableStruct(W_Procedure):
    errorname = "callable-struct"
    _immutable_fields_ = ["struct"]
    def __init__(self, struct):
        assert isinstance(struct, W_Struct)
        self.struct = struct

    @make_call_method(simple=False)
    def call(self, args, env, cont):
        # FIXME: what if self arg is used in property
        p = self.struct.props[0].cdr()
        property = self.struct._get_list(p.value) if isinstance(p, W_Fixnum) else p
        assert (property, W_Procedure)
        return property.call(args, env, cont)

class W_RootStruct(W_Object):
    errorname = "root-struct"
    _immutable_fields_ = ["type", "super", "isopaque", "ref", "set"]

    def __init__(self, struct_id, super, isopaque):
        self.type = struct_id
        self.super = super
        self.isopaque = isopaque

    @label
    def ref(self, struct_id, field, env, cont):
        raise NotImplementedError("base class")

    @label
    def set(self, struct_id, field, val, env, cont):
        raise NotImplementedError("base class")

    def vals(self):
        raise NotImplementedError("base class")

class W_Struct(W_RootStruct):
    errorname = "struct"
    _immutable_fields_ = ["values", "type", "super", "props", "proc_spec", "isopaque"]
    def __init__(self, offset, struct_id, super, props, proc_spec, isopaque):
        W_RootStruct.__init__(self, struct_id, super, isopaque)
        self.offset = offset
        self.props = props
        self.proc_spec = proc_spec

    def vals(self):
        return self._get_full_list()

    # Rather than reference functions, we store the continuations. This is
    # necessarray to get constant stack usage without adding extra preamble
    # continuations.
    @label
    def ref(self, struct_id, field, env, cont):
        from pycket.interpreter import return_value, jump
        if self.type == struct_id:
            result = self._get_list(field)
            return return_value(result, env, cont)
        elif self.type.value == struct_id.value:
            raise SchemeException("given value instantiates a different structure type with the same name")
        elif self.super is not None:
            result = self._get_list(field + self.offset[struct_id])
            return return_value(result, env, cont)
        else:
            assert False

    @label
    def set(self, struct_id, field, val, env, cont):
        from pycket.interpreter import return_value, jump
        type = jit.promote(self.type)
        if type == struct_id:
            self._set_list(field, val)
            return return_value(w_void, env, cont)
        else:
            self._set_list(field + self.offset[struct_id], val)
            return return_value(w_void, env, cont)

    def tostring(self):
        if self.isopaque:
            result =  "#<%s>" % self.type.value
        else:
            result = "(%s %s)" % (self.type.value, ' '.join([val.tostring() for val in self.vals()]))
        return result

inline_small_list(W_Struct, immutable=False, attrname="values")

class W_StructTypeDescriptor(W_Object):
    errorname = "struct-type-descriptor"
    _immutable_fields_ = ["value"]
    def __init__(self, value):
        self.value = value
    def tostring(self):
        return "#<struct-type:%s>" % self.value

class W_StructRootConstructor(W_Procedure):
    _immutable_fields_ = ["struct_id", "super_type", "init_field_cnt", "auto_values", "props", "proc_spec",  "isopaque", "guard", "name"]
    def __init__ (self, struct_id, super_type, init_field_cnt, auto_values, props, proc_spec, calculate_offset, isopaque, guard, name):
        self.struct_id = struct_id
        self.super_type = super_type
        self.init_field_cnt = init_field_cnt
        self.auto_values = auto_values
        self.props = props
        self.proc_spec = proc_spec
        self.calculate_offset = calculate_offset
        self.offset = None
        self.isopaque = isopaque
        self.guard = guard
        self.name = name

    def make_struct(self, field_values, offset, struct_id, super, props, proc_spec, isopaque):
        raise NotImplementedError("abstract base class")

    @continuation
    def constr_proc_cont(self, field_values, env, cont, _vals):
        from pycket.interpreter import return_value
        super = None
        vals = _vals._get_full_list()
        if len(vals) == 1:
            super = vals[0]
        if not self.offset:
            self.offset = self.calculate_offset()
        field_values = field_values + self.auto_values
        if isinstance(super, W_Struct):
            field_values = field_values + super._get_full_list()
        result = self.make_struct(field_values, self.offset, self.struct_id, super, self.props, self.proc_spec, self.isopaque)
        return return_value(result, env, cont)

    @continuation
    def constr_proc_wrapper_cont(self, field_values, env, cont, _vals):
        from pycket.interpreter import jump
        super_type = self.super_type
        if isinstance(super_type, W_StructType):
            def split_list(list, num):
                assert num >= 0
                return list[:num], list[num:]
            split_position = len(field_values) - self.init_field_cnt
            super_field_values, field_values = split_list(field_values, split_position)
            return super_type.constr.call(super_field_values, env, self.constr_proc_cont(field_values, env, cont))
        else:
            return jump(env, self.constr_proc_cont(field_values, env, cont))

    def call(self, args, env, cont):
        from pycket.interpreter import jump
        if self.guard is w_false:
            return jump(env, self.constr_proc_wrapper_cont(args, env, cont))
        else:
            assert isinstance(self.struct_id, W_StructTypeDescriptor)
            guard_args = args + [W_Symbol.make(self.struct_id.value)]
            jit.promote(self)
            return self.guard.call(guard_args, env, self.constr_proc_wrapper_cont(args, env, cont))

    def tostring(self):
        return "#<procedure:%s>" % self.name

class W_StructConstructor(W_StructRootConstructor):
    def __init__ (self, struct_id, super_type, init_field_cnt, auto_values, props, proc_spec, calculate_offset, isopaque, guard, name):
        W_StructRootConstructor.__init__(self, struct_id, super_type, init_field_cnt, auto_values, props, proc_spec, 
            calculate_offset, isopaque, guard, name)
    def make_struct(self, field_values, offset, struct_id, super, props, proc_spec, isopaque):
        return W_Struct.make(field_values, offset, struct_id, super, props, proc_spec, isopaque)

class W_CallableStructConstructor(W_StructRootConstructor):
    def __init__ (self, struct_id, super_type, init_field_cnt, auto_values, props, proc_spec, calculate_offset, isopaque, guard, name):
        W_StructRootConstructor.__init__(self, struct_id, super_type, init_field_cnt, auto_values, props, proc_spec, 
            calculate_offset, isopaque, guard, name)
    def make_struct(self, field_values, offset, struct_id, super, props, proc_spec, isopaque):
        struct = W_Struct.make(field_values, offset, struct_id, super, props, proc_spec, isopaque)
        return W_CallableStruct(struct)

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

    @make_call_method([W_Object], simple=False)
    def call(self, struct, env, cont):
        if isinstance(struct, W_CallableStruct):
            struct = struct.struct
        assert isinstance(struct, W_Struct)
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

    @make_call_method([W_Object, W_Object], simple=False)
    def call(self, struct, val, env, cont):
        if isinstance(struct, W_CallableStruct):
            struct = struct.struct
        assert isinstance(struct, W_Struct)
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
