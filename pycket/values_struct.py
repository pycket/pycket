from collections import namedtuple
from pycket import values
from pycket.cont import continuation, label
from pycket.error import SchemeException
from pycket.exposeprim import make_call_method
from pycket.small_list import inline_small_list
from rpython.rlib import jit

#
# Structs are partially supported
#
# Not implemented:
# 1) prefab: need update in expand.rkt
# 2) methods overriding (including equal)

# TODO: inspector currently does nothing
class W_StructInspector(values.W_Object):
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

class W_StructType(values.W_Object):
    errorname = "struct-type-descriptor"
    _immutable_fields_ = ["name", "super", "init_field_cnt", "auto_field_cnt", "total_field_cnt", "auto_v", "props", \
        "inspector", "immutables", "guard", "constr_name", "auto_values[:]", "offsets[:]"]

    @jit.unroll_safe
    def initialize_props(self, props):
        proplist = values.from_list(props)
        self.props =  [(None, None)] * len(proplist)
        self.prop_procedure = None
        for i, prop in enumerate(proplist):
            w_car = prop.car()
            w_prop = prop.cdr()
            if w_car.isinstance(w_prop_procedure):
                if self.prop_procedure:
                    raise SchemeException("duplicate property binding")
                self.prop_procedure = w_prop
            elif w_car.isinstance(w_prop_checked_procedure):
                if self.total_field_cnt < 2:
                    raise SchemeException("need at least two fields in the structure type")
            self.props[i] = (w_car, w_prop)
        struct_type = self.super
        while isinstance(struct_type, W_StructType):
            self.props = self.props + struct_type.props
            if not self.prop_procedure:
                self.prop_procedure = struct_type.prop_procedure
            struct_type = struct_type.super

    def __init__(self, name, super_type, init_field_cnt, auto_field_cnt,
                 auto_v=values.w_false, props=values.w_null,
                 inspector=values.w_false, proc_spec=values.w_false,
                 immutables=values.w_null, guard=values.w_false,
                 constr_name=values.w_false):
        self.name = name.value
        self.super = super_type
        self.init_field_cnt = init_field_cnt.value
        self.auto_field_cnt = auto_field_cnt.value
        self.total_field_cnt = self.init_field_cnt + self.auto_field_cnt + \
            (super_type.total_field_cnt if isinstance(super_type, W_StructType) else 0)
        self.auto_v = auto_v
        self.initialize_props(props)
        self.inspector = inspector
        self.proc_spec = proc_spec
        self.immutables = []
        if isinstance(proc_spec, values.W_Fixnum):
            self.immutables.append(proc_spec.value)
        for i in values.from_list(immutables):
            assert isinstance(i, values.W_Fixnum)
            self.immutables.append(i.value)
        self.guard = guard
        if isinstance(constr_name, values.W_Symbol):
            self.constr_name = constr_name.value
        else:
            self.constr_name = "make-" + self.name

        self.auto_values = [self.auto_v] * self.auto_field_cnt
        self.isopaque = self.inspector is not values.w_false
        self.offsets = self.calculate_offsets()

        self.constr = W_StructConstructor(self)
        self.pred = W_StructPredicate(self)
        self.acc = W_StructAccessor(self)
        self.mut = W_StructMutator(self)

    @jit.unroll_safe
    def calculate_offsets(self):
        result = {}
        struct_type = self
        while isinstance(struct_type, W_StructType):
            result[struct_type] = struct_type.total_field_cnt - \
            struct_type.init_field_cnt - struct_type.auto_field_cnt
            struct_type = struct_type.super
        return result

    @jit.elidable
    def get_offset(self, type):
        return self.offsets.get(type, -1)

    def make_struct_tuple(self):
        return [self, self.constr, self.pred, self.acc, self.mut]

    def tostring(self):
        return "#<struct-type:%s>" % self.name

@continuation
def arity_error_cont(_vals):
    from pycket.interpreter import check_one_val
    msg = check_one_val(_vals)
    raise SchemeException(msg.tostring())

@continuation
def receive_proc_cont(args, env, cont, _vals):
    from pycket.interpreter import check_one_val
    proc = check_one_val(_vals)
    return proc.call(args, env, cont)

class W_RootStruct(values.W_Object):
    errorname = "root-struct"
    _immutable_fields_ = ["type", "ref", "set"]

    def __init__(self, type):
        self.type = type

    @make_call_method(simple=False)
    def _call(self, args, env, cont):
        from pycket.interpreter import jump
        if self.type.proc_spec is not values.w_false:
            args = [self.type.proc_spec] + args
        proc = self.type.prop_procedure
        if isinstance(proc, values.W_Fixnum):
            return jump(env,
                    self.ref(self.type, proc.value, env,
                        receive_proc_cont(args, env, cont)))

        args = [self] + args
        # FIXME: arities value is wrong?
        (arities, rest) = proc.get_arity()
        if len(args) not in arities:
            for w_car, w_prop in self.type.props:
                if w_car.isinstance(w_prop_arity_string):
                    return w_prop.call([self], env, arity_error_cont())
        return proc.call(args, env, cont)

    @label
    def ref(self, type, field, env, cont):
        raise NotImplementedError("base class")

    @label
    def set(self, type, field, val, env, cont):
        raise NotImplementedError("base class")

    def vals(self):
        raise NotImplementedError("base class")

class W_Struct(W_RootStruct):
    errorname = "struct"
    _immutable_fields_ = ["values"]
    def __init__(self, type):
        W_RootStruct.__init__(self, type)

    def iscallable(self):
        return self.type.prop_procedure is not None

    def vals(self):
        return self._get_full_list()

    # Rather than reference functions, we store the continuations. This is
    # necessarray to get constant stack usage without adding extra preamble
    # continuations.
    @label
    def ref(self, type, field, env, cont):
        from pycket.interpreter import return_value
        offset = jit.promote(self.type).get_offset(type)
        if offset == -1:
            raise SchemeException("cannot reference an identifier before its definition")
        result = self._get_list(field + offset)
        return return_value(result, env, cont)

    @label
    def set(self, type, field, val, env, cont):
        from pycket.interpreter import return_value
        offset = jit.promote(self.type).get_offset(type)
        if offset == -1:
            raise SchemeException("cannot reference an identifier before its definition")
        self._set_list(field + offset, val)
        return return_value(values.w_void, env, cont)

    # unsafe versions
    def _ref(self, k):
        return self._get_list(k)
    def _set(self, k, val):
        self._set_list(k, val)

    def tostring(self):
        if self.type.isopaque:
            result =  "#<%s>" % self.type.name
        else:
            result = "(%s %s)" % (self.type.name, ' '.join([val.tostring() for val in self.vals()]))
        return result

inline_small_list(W_Struct, immutable=False, attrname="values")

class W_StructConstructor(values.W_Object):
    _immutable_fields_ = ["type"]
    def __init__(self, type):
        self.type = type

    def iscallable(self):
        return True

    def make_struct(self, field_values):
        raise NotImplementedError("abstract base class")

    @continuation
    def constr_proc_cont(self, field_values, env, cont, _vals):
        from pycket.interpreter import return_value
        if len(self.type.auto_values) > 0:
            field_values = field_values + self.type.auto_values
        result = W_Struct.make(field_values, self.type)
        return return_value(result, env, cont)

    @jit.unroll_safe
    def _splice(self, array, array_len, index, insertion, insertion_len):
        new_len = array_len + insertion_len
        assert new_len >= 0
        new_array = [None] * new_len
        for pre_index in range(index):
            new_array[pre_index] = array[pre_index]
        for insert_index in range(insertion_len):
            new_array[index + insert_index] = insertion[insert_index]
        for post_index in range(index, array_len):
            new_array[post_index + insertion_len] = array[post_index]
        return new_array

    @continuation
    def constr_proc_wrapper_cont(self, field_values, issuper, env, cont, _vals):
        from pycket.interpreter import return_value, jump
        super_type = jit.promote(self.type.super)
        if isinstance(super_type, W_StructType):
            split_position = len(field_values) - self.type.init_field_cnt
            super_auto = super_type.constr.type.auto_values
            assert split_position >= 0
            field_values = self._splice(
                field_values, len(field_values), split_position,
                super_auto, len(super_auto))
            return super_type.constr.code(field_values[:split_position], True,
                env, self.constr_proc_cont(field_values, env, cont))
        else:
            if issuper:
                return jump(env, cont)
            else:
                return jump(env, self.constr_proc_cont(field_values, env, cont))

    def code(self, field_values, issuper, env, cont):
        from pycket.interpreter import jump
        if self.type.guard is values.w_false:
            return jump(env, self.constr_proc_wrapper_cont(field_values, issuper, env, cont))
        else:
            guard_args = field_values + [values.W_Symbol.make(self.type.name)]
            jit.promote(self)
            return self.type.guard.call(guard_args, env,
                self.constr_proc_wrapper_cont(field_values, issuper, env, cont))

    @make_call_method(simple=False)
    def _call(self, args, env, cont):
        return self.code(args, False, env, cont)

    def tostring(self):
        return "#<procedure:%s>" % self.type.name

class W_StructPredicate(values.W_Object):
    errorname = "struct-predicate"
    _immutable_fields_ = ["type"]
    def __init__ (self, type):
        self.type = type

    def iscallable(self):
        return True

    @make_call_method([values.W_Object])
    def _call(self, struct):
        if isinstance(struct, W_Struct):
            struct_type = struct.type
            while isinstance(struct_type, W_StructType):
                if struct_type == self.type:
                    return values.w_true
                struct_type = struct_type.super
        return values.w_false
    def tostring(self):
        return "#<procedure:%s?>" % self.type.name

class W_StructFieldAccessor(values.W_Object):
    errorname = "struct-field-accessor"
    _immutable_fields_ = ["accessor", "field"]
    def __init__ (self, accessor, field):
        assert isinstance(accessor, W_StructAccessor)
        self.accessor = accessor
        self.field = field

    def iscallable(self):
        return True

    @make_call_method([W_RootStruct], simple=False)
    def _call(self, struct, env, cont):
        return self.accessor.access(struct, self.field, env, cont)

class W_StructAccessor(values.W_Object):
    errorname = "struct-accessor"
    _immutable_fields_ = ["type"]
    def __init__ (self, type):
        self.type = type

    def iscallable(self):
        return True

    def access(self, struct, field, env, cont):
        from pycket.interpreter import jump
        return jump(env, struct.ref(self.type, field.value, env, cont))

    _call = make_call_method([W_RootStruct, values.W_Fixnum], simple=False)(access)

    def tostring(self):
        return "#<procedure:%s-ref>" % self.type.name

class W_StructFieldMutator(values.W_Object):
    errorname = "struct-field-mutator"
    _immutable_fields_ = ["mutator", "field"]
    def __init__ (self, mutator, field):
        assert isinstance(mutator, W_StructMutator)
        self.mutator = mutator
        self.field = field

    def iscallable(self):
        return True

    @make_call_method([W_RootStruct, values.W_Object], simple=False)
    def _call(self, struct, val, env, cont):
        return self.mutator.mutate(struct, self.field, val, env, cont)

class W_StructMutator(values.W_Object):
    errorname = "struct-mutator"
    _immutable_fields_ = ["type"]
    def __init__ (self, type):
        self.type = type

    def iscallable(self):
        return True

    def mutate(self, struct, field, val, env, cont):
        from pycket.interpreter import jump
        return jump(env, struct.set(self.type, field.value, val, env, cont))

    _call = make_call_method([W_RootStruct, values.W_Fixnum, values.W_Object], simple=False)(mutate)

    def tostring(self):
        return "#<procedure:%s-set!>" % self.type.name

class W_StructProperty(values.W_Object):
    errorname = "struct-type-property"
    _immutable_fields_ = ["name", "guard", "supers", "can_imp"]
    def __init__(self, name, guard, supers=values.w_null, can_imp=False):
        self.name = name.value
        self.guard = guard
        self.supers = values.from_list(supers)
        self.can_imp = can_imp
    def isinstance(self, prop):
        if self is prop:
            return True
        elif len(self.supers) > 0:
            for super in self.supers:
                if super.car() is prop:
                    return True
        return False
    def tostring(self):
        return "#<struct-type-property:%s>"%self.name

w_prop_procedure = W_StructProperty(values.W_Symbol.make("prop:procedure"), values.w_false)
w_prop_checked_procedure = W_StructProperty(values.W_Symbol.make("prop:checked-procedure"), values.w_false)
w_prop_arity_string = W_StructProperty(values.W_Symbol.make("prop:arity-string"), values.w_false)

class W_StructPropertyPredicate(values.W_Object):
    errorname = "struct-property-predicate"
    _immutable_fields_ = ["property"]
    def __init__(self, prop):
        self.property = prop
    def iscallable(self):
        return True
    @make_call_method([values.W_Object])
    def _call(self, arg):
        if isinstance(arg, W_Struct):
            props = arg.type.props
        else:
            return values.w_false
        for (p, val) in props:
            # FIXME: parent properties
            if p is self.property:
                return values.w_true
        return values.w_false

class W_StructPropertyAccessor(values.W_Object):
    errorname = "struct-property-accessor"
    _immutable_fields_ = ["property"]
    def __init__(self, prop):
        self.property = prop
    def iscallable(self):
        return True
    @make_call_method([values.W_Object])
    def _call(self, arg):
        if isinstance(arg, W_Struct):
            props = arg.type.props
        else:
            raise SchemeException("%s-accessor: expected %s? but got %s"%(self.property.name, self.property.name, arg.tostring()))
        for (p, val) in props:
            # FIXME: parent properties
            if p is self.property:
                return val
        raise SchemeException("%s-accessor: expected %s? but got %s"%(self.property.name, self.property.name, arg.tostring()))
