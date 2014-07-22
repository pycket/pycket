from pycket import values
from pycket.cont import continuation, jump_call
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

        self.isopaque = self.inspector is not w_false
        self.mutable_fields = []

        self.desc = struct_id
        self.constr = W_StructConstructor(self.desc, self.super, self.init_field_cnt, self.auto_field_cnt,
                                          self.auto_v, self.mutable_fields, self.isopaque, self.guard, self.constr_name)
        self.pred = W_StructPredicate(self.desc)
        self.acc = W_StructAccessor(self.desc)
        self.mut = W_StructMutator(self.desc)

    def constructor(self):
        return self.constr
    def set_mutable(self, field):
        assert isinstance(field, W_Fixnum)
        self.mutable_fields.append(field.value)
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
    def __init__(self, mutable_fields, mutable_vals, struct_id, super, isopaque):
        W_RootStruct.__init__(self, struct_id, super, isopaque)
        self.mutable_fields = mutable_fields
        self.mutable_vals = mutable_vals

    # FIXME:
    def vals(self):
        result = self._get_full_list()
        if self.super is not None: 
            return self.super.vals() + result
        else:
            return result

    # FIXME: will be replaced by static mapping
    @jit.elidable
    def map(self, field):
        if field in self.mutable_fields:
            for immutable_field in [item for item in range(field) if item not in self.mutable_fields]:
                if immutable_field < field: field -= 1
                else: break
        else:
            for mutable_field in self.mutable_fields:
                if mutable_field < field: field -= 1
                else: break
        return field

    # Rather than reference functions, we store the continuations. This is
    # necessarray to get constant stack usage without adding extra preamble
    # continuations.
    @continuation
    def ref(self, struct_id, field, env, cont, _vals):
        from pycket.interpreter import return_value, jump
        if self.type == struct_id:
            result = self._get_list(self.map(field)) if field not in self.mutable_fields else self.mutable_vals[self.map(field)]
            return return_value(result, env, cont)
        elif self.type.value == struct_id.value:
            raise SchemeException("given value instantiates a different structure type with the same name")
        elif self.super is not None:
            return jump(env, self.super.ref(struct_id, field, env, cont))
        else:
            assert False

    @continuation
    def set(self, struct_id, field, val, env, cont, _vals):
        from pycket.interpreter import return_value, jump
        type = jit.promote(self.type)
        if type == struct_id:
            self.mutable_vals[self.map(field)] = val
            return return_value(w_void, env, cont)
        return jump(env, self.super.set(struct_id, field, val, env, cont))

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

@continuation
def constr_proc_cont(args, env, cont, _vals):
    from pycket.interpreter import return_value
    struct_id, super_type, init_field_cnt, auto_values, mutable_fields, isopaque, field_values = args
    super = None
    if isinstance(super_type, W_StructType):
        def split_list(list, num):
            assert num >= 0
            return list[:num], list[num:]
        split_position = len(field_values) - init_field_cnt
        super_field_values, field_values = split_list(field_values, split_position)
        something, env, cont = super_type.constructor().call(super_field_values, env, cont)
        # TODO: how to create a super instance using continuations?
    vals = field_values + auto_values
    immutable_vals, mutable_vals = [], []
    for idx, val in enumerate(vals):
        if idx in mutable_fields: mutable_vals.append(val)
        else: immutable_vals.append(val)
    result = W_Struct.make(immutable_vals, mutable_fields, mutable_vals, struct_id, super, isopaque)
    return return_value(result, env, cont)

class W_StructConstructor(W_Procedure):
    _immutable_fields_ = ["struct_id", "super_type", "init_field_cnt", "auto_values", "mutable_fields[:]", "isopaque", "guard", "name"]
    def __init__ (self, struct_id, super_type, init_field_cnt, auto_field_cnt, auto_v, mutable_fields, isopaque, guard, name):
        self.struct_id = struct_id
        self.super_type = super_type
        self.init_field_cnt = init_field_cnt
        self.auto_values = [auto_v] * auto_field_cnt
        self.mutable_fields = mutable_fields
        self.isopaque = isopaque
        self.guard = guard
        self.name = name

    def call(self, args, env, cont):
        from pycket.interpreter import jump
        constr_args = [self.struct_id, self.super_type, self.init_field_cnt, self.auto_values, self.mutable_fields, self.isopaque, args]
        if self.guard is w_false:
            return jump(env, constr_proc_cont(constr_args, env, cont))
        else:
            # TODO: here args have to be splitted
            guard_args = [self.struct_id] + args
            return jump(env, jump_call(self.guard, guard_args, env, constr_proc_cont(constr_args, env, cont)))

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
        raise SchemeException("StructPropertyPredicate NYI")

class W_StructPropertyAccessor(W_Procedure):
    errorname = "struct-property-accessor"
    _immutable_fields_ = ["property"]
    def __init__(self, prop):
        self.property = prop
    @make_call_method([W_Object])
    def call(self, args):
        raise SchemeException("StructPropertyAccessor NYI")
