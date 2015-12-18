import itertools

from pycket import config
from pycket import values
from pycket import vector as values_vector
from pycket.arity import Arity
from pycket.cont import continuation, label
from pycket.error import SchemeException
from pycket.prims.expose import make_call_method
from pycket.small_list import inline_small_list
from pycket.values_parameter import W_Parameter

from rpython.rlib import jit
from rpython.rlib.unroll import unrolling_iterable

PREFAB = values.W_Symbol.make("prefab")

class W_StructInspector(values.W_Object):
    errorname = "struct-inspector"
    _immutable_fields_ = ["super"]

    @staticmethod
    def make(inspector, issibling=False):
        assert isinstance(inspector, W_StructInspector)
        super = inspector
        if issibling:
            super = inspector.super if inspector is not None else None
        return W_StructInspector(super)

    def __init__(self, super):
        self.super = super

    @jit.elidable
    def has_control(self, struct_type):
        inspector = struct_type.inspector
        if not isinstance(inspector, W_StructInspector):
            return True
        inspector = inspector.super
        while isinstance(inspector, W_StructInspector):
            if inspector is self:
                return True
            inspector = inspector.super
        return False

current_inspector = W_StructInspector(None)
current_inspector_param = W_Parameter(current_inspector)

class W_StructType(values.W_Object):
    errorname = "struct-type-descriptor"
    _immutable_fields_ = ["name", "super", "init_field_cnt", "auto_field_cnt",
            "total_field_cnt", "auto_v", "props", "inspector", "immutables[*]",
            "immutable_fields[*]", "guard", "auto_values[*]", "offsets[*]",
            "constr", "predicate", "accessor", "mutator", "prop_procedure"]
    unbound_prefab_types = {}

    @staticmethod
    def make(name, super_type, init_field_cnt, auto_field_cnt,
             auto_v=values.w_false, props=values.w_null,
             inspector=values.w_false, proc_spec=values.w_false,
             immutables=values.w_null, guard=values.w_false,
             constr_name=values.w_false, env=None, cont=None):
        """
        This method returns five instances:
            W_StructType
            W_StructConstructor
            W_StructPredicate
            W_StructAccessor
            W_StructMutator
        """
        w_struct_type = W_StructType.make_simple(name, super_type,
            init_field_cnt, auto_field_cnt, auto_v, props, inspector,
            proc_spec, immutables, guard, constr_name)
        return w_struct_type.initialize_props(props, proc_spec, env, cont)

    @staticmethod
    def make_simple(name, super_type, init_field_cnt, auto_field_cnt,
            auto_v=values.w_false, props=values.w_null,
            inspector=values.w_false, proc_spec=values.w_false,
            immutables=values.w_null, guard=values.w_false,
            constr_name=values.w_false):
        """
        This method returns an instance of W_StructType only.
        It does not support properties.
        """
        if inspector is PREFAB:
            prefab_key = W_PrefabKey.from_raw_params(name, init_field_cnt,\
                auto_field_cnt, auto_v, immutables, super_type)
            if prefab_key in W_StructType.unbound_prefab_types:
                return W_StructType.unbound_prefab_types.pop(prefab_key)
        return W_StructType(name, super_type, init_field_cnt, auto_field_cnt,
            auto_v, inspector, proc_spec, immutables, guard, constr_name)

    @staticmethod
    @jit.elidable
    def make_prefab(prefab_key):
        if prefab_key in W_StructType.unbound_prefab_types:
            w_struct_type = W_StructType.unbound_prefab_types[prefab_key]
        else:
            name, init_field_cnt, auto_field_cnt, auto_v, mutables, super_key =\
                prefab_key.make_key_tuple()
            super_type = W_StructType.make_prefab(super_key) if super_key else\
                values.w_false
            immutables = []
            for i in range(init_field_cnt):
                if i not in mutables:
                    immutables.append(values.W_Fixnum(i))
            w_struct_type = W_StructType.make_simple(values.W_Symbol.make(name),
                super_type, values.W_Fixnum(init_field_cnt),
                values.W_Fixnum(auto_field_cnt), auto_v, values.w_null,
                PREFAB, values.w_false, values.to_list(immutables))
            W_StructType.unbound_prefab_types[prefab_key] = w_struct_type
        return w_struct_type

    @continuation
    def save_prop_value(self, props, idx, is_checked, env, cont, _vals):
        from pycket.interpreter import check_one_val
        prop = props[idx][0]
        prop_val = check_one_val(_vals)
        props[idx] = (prop, prop_val, None)
        return self.attach_prop(props, idx, is_checked, env, cont)

    @label
    def attach_prop(self, props, idx, is_checked, env, cont):
        from pycket.interpreter import return_multi_vals
        if idx < len(props):
            (prop, prop_val, sub_prop) = props[idx]
            if sub_prop is not None:
                for p in props:
                    if p[0] is sub_prop:
                        return prop_val.call([p[1]], env,
                            self.save_prop_value(props, idx, False, env, cont))
            assert isinstance(prop, W_StructProperty)
            if not is_checked and prop.guard.iscallable():
                return prop.guard.call([prop_val, values.to_list(self.struct_type_info(cont))],
                    env, self.save_prop_value(props, idx, True, env, cont))
            if prop.isinstance(w_prop_procedure):
                self.prop_procedure = prop_val
            self.props.append((prop, prop_val))
            return self.attach_prop(props, idx + 1, False, env, cont)
        # at this point all properties are saved, next step is to copy
        # propertyes from super types
        struct_type = self.super
        while isinstance(struct_type, W_StructType):
            self.props = self.props + struct_type.props
            if not self.prop_procedure and struct_type.prop_procedure:
                self.prop_procedure = struct_type.prop_procedure
                self.procedure_source = struct_type.procedure_source
            struct_type = struct_type.super
        struct_tuple = self.make_struct_tuple()
        return return_multi_vals(values.Values.make(struct_tuple), env, cont)

    @jit.unroll_safe
    def initialize_prop(self, props, p, sub_prop=None):
        prop = p.car()
        prop_val = p.cdr()
        if sub_prop is None:
            if prop.isinstance(w_prop_procedure):
                if self.prop_procedure is not None and\
                    self.prop_procedure is not prop_val:
                    raise SchemeException(
                        "make-struct-type: duplicate property binding\nproperty: %s" %
                            prop.tostring())
                self.prop_procedure = prop_val
                self.procedure_source = self
            elif prop.isinstance(w_prop_checked_procedure):
                if self.total_field_cnt < 2:
                    raise SchemeException("need at least two fields in the structure type")
        props.append((prop, prop_val, sub_prop))
        assert isinstance(prop, W_StructProperty)
        for super_p in prop.supers:
            self.initialize_prop(props, super_p, prop)

    @jit.unroll_safe
    def initialize_props(self, props, proc_spec, env, cont):
        """
        Properties initialization contains few steps:
            1. call initialize_prop for each property from the input list,
               it extracts all super values and stores them into props array
               with a flat structure
            2. recursively call attach_prop for each property from props and
               prepare the value:
               * if the current property has a subproperty, the value is the result
                 of calling value procedure with a sub value as an argument
               * if the current property has a guard, the value is the result of
                 calling guard with a value and struct type info as arguments
               * in other case, just keep the current value
        """
        proplist = values.from_list(props)
        props = []
        for p in proplist:
            self.initialize_prop(props, p)
        if proc_spec is not values.w_false:
            self.initialize_prop(props, values.W_Cons.make(w_prop_procedure, proc_spec))
        return self.attach_prop(props, 0, False, env, cont)

    def __init__(self, name, super_type, init_field_cnt, auto_field_cnt,
            auto_v, inspector, proc_spec, immutables, guard, constr_name):
        self.name = name.utf8value
        self.super = super_type
        self.init_field_cnt = init_field_cnt.value
        self.auto_field_cnt = auto_field_cnt.value
        self.total_field_cnt = self.init_field_cnt + self.auto_field_cnt + \
            (super_type.total_field_cnt if isinstance(super_type, W_StructType)
            else 0)
        self.auto_v = auto_v
        self.props = []
        self.prop_procedure = None
        self.procedure_source = None
        self.inspector = inspector
        imm = []
        if isinstance(proc_spec, values.W_Fixnum):
            imm.append(proc_spec.value)
        for i in values.from_list(immutables):
            assert isinstance(i, values.W_Fixnum)
            imm.append(i.value)
        self.immutables = imm[:]
        self.guard = guard

        self.auto_values = [self.auto_v] * self.auto_field_cnt
        self.isprefab = inspector is PREFAB
        if self.isprefab:
            self.isopaque = False
        else:
            self.isopaque = self.inspector is not values.w_false

        self.calculate_offsets()

        constr_name = (constr_name.utf8value if
            isinstance(constr_name, values.W_Symbol) else "make-" + self.name)
        self.constr = W_StructConstructor(self, constr_name)
        self.predicate = W_StructPredicate(self)
        self.accessor = W_StructAccessor(self)
        self.mutator = W_StructMutator(self)

    @jit.unroll_safe
    def calculate_offsets(self):
        offsets = []
        immutable_fields = [] # absolut indices
        struct_type = self
        while isinstance(struct_type, W_StructType):
            offset = struct_type.total_field_cnt - \
                struct_type.init_field_cnt - struct_type.auto_field_cnt
            offsets.append((struct_type, offset))
            for immutable_field in struct_type.immutables:
                immutable_fields.append(immutable_field + offset)
            struct_type = struct_type.super
        self.offsets = offsets[:]
        self.immutable_fields = immutable_fields[:]

    @jit.elidable
    def get_offset(self, type):
        for t, v in self.offsets:
            if t is type:
                return v
        return -1

    @jit.elidable
    def is_immutable_field_index(self, i):
        return i in self.immutable_fields

    def struct_type_info(self, cont):
        name = values.W_Symbol.make(self.name)
        init_field_cnt = values.W_Fixnum(self.init_field_cnt)
        auto_field_cnt = values.W_Fixnum(self.auto_field_cnt)
        immutable_k_list = values.to_list(
            [values.W_Fixnum(i) for i in self.immutables])
        current_inspector = current_inspector_param.get(cont)
        super = values.w_false
        typ = self.super
        while isinstance(typ, W_StructType):
            if current_inspector.has_control(typ):
                super = typ
            typ = typ.super
        skipped = values.W_Bool.make(super is values.w_false and
            isinstance(self.super, W_StructType))
        return [name, init_field_cnt, auto_field_cnt, self.accessor,
                self.mutator, immutable_k_list, super, skipped]

    def make_struct_tuple(self):
        return [self, self.constr, self.predicate, self.accessor, self.mutator]

    @jit.elidable_promote('all')
    def read_prop_precise(self, prop):
        for p, val in self.props:
            if p is prop:
                return val
        return None

    @jit.elidable_promote('all')
    def read_prop(self, prop):
        for p, val in self.props:
            if p.isinstance(prop):
                return val
        return None

    @jit.elidable
    def all_opaque(self):
        if not self.isopaque:
            return False
        elif isinstance(self.super, W_StructType):
            return self.super.all_opaque()
        return True

    @jit.elidable
    def is_transparent(self):
        while self is not None:
            if self.inspector is not values.w_false:
                return False
            self = self.super
        return True

    def hash_value(self):
        pass

    def tostring(self):
        return "#<struct-type:%s>" % self.name

class W_PrefabKey(values.W_Object):
    _immutable_fields_ = ["name", "init_field_cnt", "auto_field_cnt",\
        "auto_v", "mutables", "super_key"]
    all_keys = []

    @staticmethod
    @jit.elidable
    def make(name, init_field_cnt, auto_field_cnt, auto_v, mutables, super_key):
        for key in W_PrefabKey.all_keys:
            if key.equal_tuple((name, init_field_cnt, auto_field_cnt, auto_v,
                mutables, super_key)):
                return key
        key = W_PrefabKey(name, init_field_cnt, auto_field_cnt, auto_v,
            mutables, super_key)
        W_PrefabKey.all_keys.append(key)
        return key

    @staticmethod
    def from_struct_type(struct_type):
        assert isinstance(struct_type, W_StructType)
        name = struct_type.name
        init_field_cnt = struct_type.init_field_cnt
        auto_field_cnt = struct_type.auto_field_cnt
        auto_v = struct_type.auto_v
        mutables = []
        prev_idx = 1
        for i in struct_type.immutables:
            for j in range(prev_idx, i):
                mutables.append(j)
            prev_idx = i + 1
        super_key = W_PrefabKey.from_struct_type(struct_type.super) if\
            struct_type.super is not values.w_false else None
        return W_PrefabKey.make(name, init_field_cnt, auto_field_cnt, auto_v,
            mutables, super_key)

    @staticmethod
    def from_raw_params(w_name, w_init_field_cnt, w_auto_field_cnt, auto_v,
            w_immutables, super_type):
        assert isinstance(w_name, values.W_Symbol)
        name = w_name.utf8value
        assert isinstance(w_init_field_cnt, values.W_Fixnum)
        init_field_cnt = w_init_field_cnt.value
        assert isinstance(w_auto_field_cnt, values.W_Fixnum)
        auto_field_cnt = w_auto_field_cnt.value
        mutables = []
        prev_idx = 1
        for i in values.from_list(w_immutables):
            assert isinstance(i, values.W_Fixnum)
            for j in range(prev_idx, i.value):
                mutables.append(j)
            prev_idx = i.value + 1
        super_key = W_PrefabKey.from_struct_type(super_type) if\
            super_type is not values.w_false else None
        return W_PrefabKey.make(name, init_field_cnt, auto_field_cnt, auto_v,
            mutables, super_key)

    @staticmethod
    @jit.elidable
    def from_raw_key(w_key, total_field_cnt=0):
        init_field_cnt = -1
        auto_field_cnt = 0
        auto_v = values.w_false
        super_key = None
        mutables = []
        if isinstance(w_key, values.W_Symbol):
            name = w_key.utf8value
            init_field_cnt = total_field_cnt
        else:
            key = values.from_list(w_key)
            w_name = key[0]
            assert isinstance(w_name, values.W_Symbol)
            name = w_name.utf8value
            idx = 1
            w_init_field_cnt = key[idx]
            if isinstance(w_init_field_cnt, values.W_Fixnum):
                init_field_cnt = w_init_field_cnt.value
                idx += 1
            if len(key) > idx:
                w_auto = key[idx]
                if isinstance(w_auto, values.W_Cons):
                    auto = values.from_list(w_auto)
                    w_auto_field_cnt = auto[0]
                    assert isinstance(w_auto_field_cnt, values.W_Fixnum)
                    auto_field_cnt = w_auto_field_cnt.value
                    auto_v = auto[1]
                    idx += 1
            if len(key) > idx:
                v = key[idx]
                if isinstance(v, values_vector.W_Vector):
                    for i in range(v.len):
                        mutable = v.ref(i)
                        assert isinstance(mutable, values.W_Fixnum)
                        mutables.append(mutable.value)
                    idx += 1
            if len(key) > idx:
                w_super_key = values.to_list(key[idx:])
                super_key = W_PrefabKey.from_raw_key(w_super_key)
        if init_field_cnt == -1:
            init_field_cnt = total_field_cnt
            s_key = super_key
            while s_key:
                super_name, super_init_field_cnt, super_auto_field_cnt,\
                    super_auto_v, super_mutables, s_key = s_key.make_key_tuple()
                init_field_cnt -= super_init_field_cnt
        return W_PrefabKey.make(name, init_field_cnt, auto_field_cnt, auto_v,
            mutables, super_key)

    @staticmethod
    def is_prefab_key(v):
        if isinstance(v, values.W_Symbol):
            return values.w_true
        elif isinstance(v, values.W_Cons):
            key = values.from_list(v)
            if not isinstance(key[0], values.W_Symbol):
                return values.w_false
            idx = 1
            if isinstance(key[idx], values.W_Fixnum):
                idx += 1
            if len(key) > idx:
                if isinstance(key[idx], values.W_Cons):
                    idx += 1
            if len(key) > idx:
                if isinstance(key[idx], values_vector.W_Vector):
                    idx += 1
            if len(key) > idx:
                w_super_key = values.to_list(key[idx:])
                return W_PrefabKey.is_prefab_key(w_super_key)
            return values.W_Bool.make(len(key) == idx)
        else:
            return values.w_false

    def __init__(self, name, init_field_cnt, auto_field_cnt, auto_v, mutables,
            super_key):
        self.name = name
        self.init_field_cnt = init_field_cnt
        self.auto_field_cnt = auto_field_cnt
        self.auto_v = auto_v
        self.mutables = mutables
        self.super_key = super_key

    def equal(self, other):
        if isinstance(other, W_PrefabKey):
            return self.make_key_tuple() == other.make_key_tuple()
        return False

    def equal_tuple(self, other):
        return self.make_key_tuple() == other

    def key(self):
        key = []
        key.append(values.W_Symbol.make(self.name))
        key.append(values.W_Fixnum(self.init_field_cnt))
        if self.auto_field_cnt > 0:
            key.append(values.to_list(
                [values.W_Fixnum(self.auto_field_cnt), self.auto_v]))
        mutables = []
        for i in self.mutables:
            mutables.append(values.W_Fixnum(i))
        if mutables:
            key.append(values_vector.W_Vector.fromelements(mutables))
        if self.super_key:
            key.extend(self.super_key.key())
        return key

    def short_key(self):
        key = self.key()
        short_key = key[:1] + key[2:]
        return values.to_list(short_key) if len(short_key) > 1 else key[0]

    def make_key_tuple(self):
        return self.name, self.init_field_cnt, self.auto_field_cnt,\
            self.auto_v, self.mutables, self.super_key

class W_RootStruct(values.W_Object):
    errorname = "root-struct"
    _attrs_ = []
    _settled_ = True

    def __init__(self):
        raise NotImplementedError("abstract base class")

    def iscallable(self):
        return self.struct_type().prop_procedure is not None

    @continuation
    def arity_error_cont(self, env, cont, _vals):
        from pycket.interpreter import check_one_val
        msg = check_one_val(_vals)
        raise SchemeException("expected: " + msg.tostring())

    @continuation
    def receive_proc_cont(self, args, app, env, cont, _vals):
        from pycket.interpreter import check_one_val
        proc = check_one_val(_vals)
        return self.checked_call(proc, args, env, cont, app)

    def checked_call(self, proc, args, env, cont, app):
        args_len = len(args)
        arity = proc.get_arity()
        if ((args_len < arity.at_least or arity.at_least == -1) and
            not arity.list_includes(args_len)):
            w_prop_val = self.struct_type().read_prop(w_prop_arity_string)
            if w_prop_val:
                return w_prop_val.call_with_extra_info([self], env,
                    self.arity_error_cont(env, cont), app)
        return proc.call_with_extra_info(args, env, cont, app)

    def call_with_extra_info(self, args, env, cont, app):
        type = self.struct_type()
        proc = type.prop_procedure
        if isinstance(proc, values.W_Fixnum):
            return type.procedure_source.accessor.access(self, proc.value,
                env, self.receive_proc_cont(args, app, env, cont), app)
        args = [self] + args
        return self.checked_call(proc, args, env, cont, app)

    # For all subclasses, it should be sufficient to implement ref, set, and
    # struct_type for call and iscallable to work properly.
    @label
    @make_call_method(simple=False)
    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

    def get_arity(self):
        raise NotImplementedError("abstract base class")

    def struct_type(self):
        raise NotImplementedError("abstract base class")

    def ref(self, field, env, cont):
        return self.ref_with_extra_info(field, None, env, cont)

    def ref_with_extra_info(self, field, app, env, cont):
        raise NotImplementedError("abstract base class")

    def set(self, field, val, env, cont):
        return self.set_with_extra_info(field, val, None, env, cont)

    def set_with_extra_info(self, field, val, app, env, cont):
        raise NotImplementedError("abstract base class")

    # unsafe versions
    def _ref(self, k):
        raise NotImplementedError("abstract base class")

    def _set(self, k, val):
        raise NotImplementedError("abstract base class")

    @label
    def get_prop(self, property, env, cont):
        raise NotImplementedError("abstract base class")

    def get_struct_info(self, env, cont):
        raise NotImplementedError("abstract base class")

    def vals(self):
        raise NotImplementedError("abstract base class")

@inline_small_list(immutable=True, attrname="storage", unbox_num=True)
class W_Struct(W_RootStruct):
    errorname = "struct"
    _immutable_fields_ = ["_type"]

    @staticmethod
    @jit.unroll_safe
    def make_prefab(w_key, w_values):
        w_struct_type = W_StructType.make_prefab(
            W_PrefabKey.from_raw_key(w_key, len(w_values)))
        constant_false = []
        for i, value in enumerate(w_values):
            if not w_struct_type.is_immutable_field_index(i):
                w_values[i] = values.W_Cell(value)
            elif value is values.w_false:
                constant_false.append(i)
        cls = lookup_struct_class(constant_false)
        if cls is not W_Struct:
            w_values = reduce_field_values(w_values, constant_false)
        return cls.make(w_values, w_struct_type)

    def __init__(self, type):
        self._type = type

    def struct_type(self):
        return jit.promote(self._type)

    @jit.unroll_safe
    def vals(self):
        size = self._get_size_list()
        values = [None] * size
        for i in range(size):
            values[i] = self._ref(i)
        return values

    # Rather than reference functions, we store the continuations. This is
    # necessarray to get constant stack usage without adding extra preamble
    # continuations.
    def ref_with_extra_info(self, field, app, env, cont):
        from pycket.interpreter import return_value
        value = self._ref(field)
        return return_value(value, env, cont)

    def set_with_extra_info(self, field, val, app, env, cont):
        from pycket.interpreter import return_value
        w_cell = self._set(field, val)
        return return_value(values.w_void, env, cont)

    # unsafe versions
    def _ref(self, i):
        w_res = self._get_list(i)
        immutable = self.struct_type().is_immutable_field_index(i)
        if not immutable:
            assert isinstance(w_res, values.W_Cell)
            w_res = w_res.get_val()
        return w_res

    def _set(self, k, val):
        w_cell = self._get_list(k)
        assert isinstance(w_cell, values.W_Cell)
        w_cell.set_val(val)

    # We provide a method to get properties from a struct rather than a struct_type,
    # since impersonators can override struct properties.
    def get_prop(self, property, env, cont):
        from pycket.interpreter import return_value
        val = self.struct_type().read_prop_precise(property)
        if val is not None:
            return return_value(val, env, cont)
        raise SchemeException("%s-accessor: expected %s? but got %s" %
            (property.name, property.name, self.tostring()))

    def get_arity(self):
        if self.iscallable():
            typ = self.struct_type()
            proc = typ.prop_procedure
            if isinstance(proc, values.W_Fixnum):
                offset = typ.get_offset(typ.procedure_source)
                proc = self._ref(proc.value + offset)
                return proc.get_arity()
            else:
                # -1 for the self argument
                arity = proc.get_arity()
                ls = [-1] * len(arity.arity_list)
                for i, val in enumerate(arity.arity_list):
                    ls[i] = val - 1
                at_least = arity.at_least
                if arity.at_least != -1:
                    at_least -= 1
                return Arity([val for val in ls if val != -1], at_least)
        else:
            raise SchemeException("%s does not have arity" % self.tostring())

    def get_struct_info(self, env, cont):
        from pycket.interpreter import return_multi_vals
        return return_multi_vals(
                values.Values.make([self.struct_type(), values.w_false]), env, cont)

    # TODO: currently unused
    def tostring_proc(self, env, cont):
        w_val = self.struct_type().read_prop(w_prop_custom_write)
        if w_val is not None:
            assert isinstance(w_val, values_vector.W_Vector)
            w_write_proc = w_val.ref(0)
            port = values.W_StringOutputPort()
            # TODO: #t for write mode, #f for display mode,
            # or 0 or 1 indicating the current quoting depth for print mode
            mode = values.w_false
            return w_write_proc.call([self, port, mode], env, cont)
        return self.tostring()

    def tostring_prefab(self):
        prefab_key = W_PrefabKey.from_struct_type(self.struct_type())
        return ("#s(%s %s)" %
                (prefab_key.short_key().tostring(),
                 ' '.join([val.tostring() for val in self.vals()])))

    @jit.unroll_safe
    def tostring_values(self, fields, w_type, is_super=False):
        " fill fields with tostring() version of field if applicable "
        assert isinstance(w_type, W_StructType)
        w_super = w_type.super
        has_super = isinstance(w_super, W_StructType)
        if has_super:
            self.tostring_values(fields=fields,w_type=w_super,is_super=True)
        offset = self.struct_type().get_offset(w_type)
        count = w_type.total_field_cnt
        if has_super:
            count -= w_super.total_field_cnt
        assert len(fields) >= count + offset
        if w_type.isopaque:
            fields[offset] = "..."
        else:
            for i in range(offset, offset + count):
                fields[i] = self._ref(i).tostring()

    @jit.unroll_safe
    def _string_from_list(self, l):
        return ' '.join([s for s in l if s is not None])

    def tostring(self):
        w_type = self.struct_type()
        if w_type.isprefab:
            return self.tostring_prefab()
        elif w_type.all_opaque():
            return "#<%s>" % w_type.name
        else:
            fields = [None] * w_type.total_field_cnt
            self.tostring_values(fields=fields, w_type=w_type, is_super=False)
            return "(%s %s)" % (w_type.name, self._string_from_list(fields))

"""
This method generates a new structure class with inline stored immutable #f
values on positions from constant_false array. If a new structure instance get
immutable #f fields on the same positions, this class will be used, thereby
reducing its size.
"""
def generate_struct_class(constant_false):

    if not len(constant_false):
        return W_Struct
    unrolling_constant_false = unrolling_iterable(constant_false)
    clsname = 'W_ImmutableBooleanStruct_' + \
              '_'.join([str(i) for i in constant_false])

    @jit.unroll_safe
    def _ref(self, i):
        pos = i
        for j in unrolling_constant_false:
            if i > j:
                pos -= 1
            elif i == j:
                return values.w_false
        # original index
        immutable = self.struct_type().is_immutable_field_index(i)
        # altered index
        w_res = self._get_list(pos)
        if not immutable:
            assert isinstance(w_res, values.W_Cell)
            w_res = w_res.get_val()
        return w_res

    @jit.unroll_safe
    def _set(self, i, val):
        pos = i
        for j in unrolling_constant_false:
            if i > j:
                pos -= 1
        # altered index
        w_cell = self._get_list(pos)
        assert isinstance(w_cell, values.W_Cell)
        w_cell.set_val(val)

    cls = type(clsname, (W_Struct,), {'_ref':_ref, '_set': _set})
    cls = inline_small_list(sizemax=min(11,CONST_FALSE_SIZE),
                            immutable=True,
                            attrname="storage",
                            unbox_num=True)(cls)
    return cls

if config.immutable_boolean_field_elision:
    CONST_FALSE_SIZE = 5 # the complexity grows exponentially
else:
    CONST_FALSE_SIZE = 0 # disabled

struct_classes = []
for i in range(0, CONST_FALSE_SIZE):
    for comb in itertools.combinations(range(CONST_FALSE_SIZE), i+1):
        struct_classes.append(generate_struct_class(comb))
struct_class_iter = unrolling_iterable(enumerate(struct_classes))

@jit.elidable
def fac(n):
    return n * fac(n-1) if n > 1 else 1

@jit.elidable
def ncr(n,r):
    if n == 0:
        return 0
    return fac(n) / fac(r) / fac(n-r)

@jit.unroll_safe
def lookup_struct_class(constant_false):
    if constant_false and constant_false[-1] < CONST_FALSE_SIZE:
        n = CONST_FALSE_SIZE
        pos = 0
        # offset of combinations with smaller amount of fields
        for r in range(1, len(constant_false)):
            pos += ncr(n, r)
        # and the precise position
        r = len(constant_false)
        last_idx = 0
        for idx in constant_false:
            pos += ncr(n, r) - ncr(n-idx+last_idx, r)
            n -= idx - last_idx + 1
            r -= 1
            last_idx = idx + 1
        # lookup class by its position
        for i, cls in struct_class_iter:
            if i == pos:
                return cls
    return W_Struct

@jit.unroll_safe
def reduce_field_values(field_values, constant_false):
    reduced_field_values = [None] * (len(field_values) - len(constant_false))
    k = 0
    for i, val in enumerate(field_values):
        found = False
        for j in constant_false:
            if j == i:
                found = True
        if not found:
            reduced_field_values[k] = val
            k += 1
    return reduced_field_values

class W_StructConstructor(values.W_Procedure):
    _immutable_fields_ = ["type", "constr_name"]
    def __init__(self, type, constr_name):
        self.type = type
        self.constr_name = constr_name

    def make_struct(self, field_values):
        raise NotImplementedError("abstract base class")

    @continuation
    @jit.unroll_safe
    def constr_proc_cont(self, field_values, env, cont, _vals):
        from pycket.interpreter import return_value
        guard_super_values = _vals.get_all_values()
        struct_type = jit.promote(self.type)
        if guard_super_values:
            field_values = guard_super_values + field_values[len(guard_super_values):]
        if len(struct_type.auto_values) > 0:
            field_values = field_values + struct_type.auto_values
        if len(field_values) != struct_type.total_field_cnt:
            raise SchemeException("%s: arity mismatch" % self.constr_name)
        constant_false = []
        for i, value in enumerate(field_values):
            if not struct_type.is_immutable_field_index(i):
                value = values.W_Cell(value)
                field_values[i] = value
            elif value is values.w_false:
                constant_false.append(i)
        cls = lookup_struct_class(constant_false)
        if cls is not W_Struct:
            field_values = reduce_field_values(field_values, constant_false)
        result = cls.make(field_values, struct_type)
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
    def constr_proc_wrapper_cont(self, field_values, struct_type_name, issuper,
        app, env, cont, _vals):
        from pycket.interpreter import return_multi_vals, jump
        guard_values = _vals.get_all_values()
        type = jit.promote(self.type)
        if guard_values:
            field_values = guard_values
        super_type = jit.promote(type.super)
        if isinstance(super_type, W_StructType):
            split_position = len(field_values) - type.init_field_cnt
            super_auto = super_type.constr.type.auto_values
            assert split_position >= 0
            field_values = self._splice(field_values, len(field_values),\
                split_position, super_auto, len(super_auto))
            if issuper:
                return super_type.constr.code(field_values[:split_position],
                    struct_type_name, True, env, cont, app)
            else:
                return super_type.constr.code(field_values[:split_position],
                    struct_type_name, True,
                    env, self.constr_proc_cont(field_values, env, cont), app)
        else:
            if issuper:
                return return_multi_vals(values.Values.make(field_values), env, cont)
            else:
                return jump(env, self.constr_proc_cont(field_values, env, cont))

    def code(self, field_values, struct_type_name, issuper, env, cont, app):
        from pycket.interpreter import jump
        type = jit.promote(self.type)
        if type.guard is values.w_false:
            return jump(env, self.constr_proc_wrapper_cont(field_values,
                struct_type_name, issuper, app, env, cont))
        else:
            guard_args = field_values + [values.W_Symbol.make(struct_type_name)]
            jit.promote(self)
            return type.guard.call_with_extra_info(guard_args, env,
                self.constr_proc_wrapper_cont(field_values, struct_type_name,
                    issuper, app, env, cont), app)

    @make_call_method(simple=False)
    def call_with_extra_info(self, args, env, cont, app):
        return self.code(args, self.type.name, False, env, cont, app)

    def tostring(self):
        return "#<procedure:%s>" % self.type.name

class W_StructPredicate(values.W_Procedure):
    errorname = "struct-predicate"
    _immutable_fields_ = ["type"]
    def __init__(self, type):
        self.type = type

    @make_call_method([values.W_Object])
    @jit.unroll_safe
    def call(self, struct):
        if isinstance(struct, W_RootStruct):
            struct_type = struct.struct_type()
            while isinstance(struct_type, W_StructType):
                if struct_type is self.type:
                    return values.w_true
                struct_type = struct_type.super
        return values.w_false

    def get_arity(self):
        return Arity.ONE

    def tostring(self):
        return "#<procedure:%s?>" % self.type.name

class W_StructFieldAccessor(values.W_Procedure):
    errorname = "struct-field-accessor"
    _immutable_fields_ = ["accessor", "field", "field_name"]
    def __init__(self, accessor, field, field_name):
        assert isinstance(accessor, W_StructAccessor)
        assert isinstance(field, values.W_Fixnum)
        self.accessor = accessor
        self.field = field.value
        self.field_name = field_name

    def get_arity(self):
        return Arity.ONE

    @make_call_method([W_RootStruct], simple=False,
        name="<struct-field-accessor-method>")
    def call_with_extra_info(self, struct, env, cont, app):
        jit.promote(self)
        return self.accessor.access(struct, self.field, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-%s>" % (self.accessor.type.name, self.field_name.variable_name())

class W_StructAccessor(values.W_Procedure):
    errorname = "struct-accessor"
    _immutable_fields_ = ["type"]
    def __init__(self, type):
        self.type = type

    def get_arity(self):
        return Arity.TWO

    def access(self, struct, field, env, cont, app):
        assert isinstance(struct, W_RootStruct)
        jit.promote(self)
        offset = struct.struct_type().get_offset(self.type)
        if offset == -1:
            raise SchemeException("cannot reference an identifier before its definition")
        return struct.ref_with_extra_info(field + offset, app, env, cont)

    @make_call_method([W_RootStruct, values.W_Fixnum], simple=False,
        name="<struct-accessor-method>")
    def call_with_extra_info(self, struct, field, env, cont, app):
        return self.access(struct, field.value, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-ref>" % self.type.name

class W_StructFieldMutator(values.W_Procedure):
    errorname = "struct-field-mutator"
    _immutable_fields_ = ["mutator", "field", "field_name"]
    def __init__ (self, mutator, field, field_name):
        assert isinstance(mutator, W_StructMutator)
        assert isinstance(field, values.W_Fixnum)
        self.mutator = mutator
        self.field = field.value
        self.field_name = field_name

    def get_arity(self):
        return Arity.TWO

    @make_call_method([W_RootStruct, values.W_Object], simple=False,
        name="<struct-field-mutator-method>")
    def call_with_extra_info(self, struct, val, env, cont, app):
        return self.mutator.mutate(struct, self.field, val, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-%s!>" % (self.mutator.type.name, self.field_name.variable_name())

class W_StructMutator(values.W_Procedure):
    errorname = "struct-mutator"
    _immutable_fields_ = ["type"]
    def __init__ (self, type):
        self.type = type

    def get_arity(self):
        return Arity.THREE

    def mutate(self, struct, field, val, env, cont, app):
        assert isinstance(struct, W_RootStruct)
        jit.promote(self)
        offset = struct.struct_type().get_offset(self.type)
        if offset == -1:
            raise SchemeException("cannot reference an identifier before its definition")
        return struct.set_with_extra_info(field + offset, val, app, env, cont)

    @make_call_method([W_RootStruct, values.W_Fixnum, values.W_Object],
        simple=False, name="<struct-mutator-method>")
    def call_with_extra_info(self, struct, field, val, env, cont, app):
        return self.mutate(struct, field.value, val, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-set!>" % self.type.name

class W_StructProperty(values.W_Object):
    errorname = "struct-type-property"
    _immutable_fields_ = ["name", "guard", "supers", "can_imp"]
    def __init__(self, name, guard, supers=values.w_null, can_imp=False):
        self.name = name.utf8value
        self.guard = guard
        self.supers = values.from_list(supers)
        self.can_imp = can_imp

    @jit.elidable
    def isinstance(self, prop):
        if self is prop:
            return True
        for super in self.supers:
            if super.car().isinstance(prop):
                return True
        return False

    def tostring(self):
        return "#<struct-type-property:%s>"%self.name

sym = values.W_Symbol.make

w_prop_procedure = W_StructProperty(sym("prop:procedure"), values.w_false)
w_prop_checked_procedure = W_StructProperty(sym("prop:checked-procedure"), values.w_false)
w_prop_arity_string = W_StructProperty(sym("prop:arity-string"), values.w_false)
w_prop_incomplete_arity = W_StructProperty(sym("prop:incomplete-arity"), values.w_false)
w_prop_custom_write = W_StructProperty(sym("prop:custom-write"), values.w_false)
w_prop_equal_hash = W_StructProperty(sym("prop:equal+hash"), values.w_false)
w_prop_chaperone_unsafe_undefined = W_StructProperty(sym("prop:chaperone-unsafe-undefined"), values.w_false)
w_prop_set_bang_transformer = W_StructProperty(sym("prop:set!-transformer"), values.w_false)
w_prop_rename_transformer = W_StructProperty(sym("prop:rename-transformer"), values.w_false)
w_prop_expansion_contexts = W_StructProperty(sym("prop:expansion-contexts"), values.w_false)
w_prop_output_port = W_StructProperty(sym("prop:output-port"), values.w_false)

del sym

class W_StructPropertyPredicate(values.W_Procedure):
    errorname = "struct-property-predicate"
    _immutable_fields_ = ["property"]
    def __init__(self, prop):
        self.property = prop

    def get_arity(self):
        return Arity.ONE

    @make_call_method([values.W_Object])
    @jit.unroll_safe
    def call(self, arg):
        if not isinstance(arg, W_RootStruct):
            return values.w_false
        w_val = arg.struct_type().read_prop_precise(self.property)
        if w_val is not None:
            return values.w_true
        return values.w_false

class W_StructPropertyAccessor(values.W_Procedure):
    errorname = "struct-property-accessor"
    _immutable_fields_ = ["property"]
    def __init__(self, prop):
        self.property = prop

    def get_arity(self):
        return Arity.ONE

    @make_call_method(simple=False)
    def call_with_extra_info(self, args, env, cont, app):
        from pycket.interpreter import return_value
        arg = args[0]
        if isinstance(arg, W_StructType):
            w_val = arg.read_prop_precise(self.property)
            if w_val is not None:
                return return_value(w_val, env, cont)
        elif isinstance(arg, W_RootStruct):
            return arg.get_prop(self.property, env, cont)
        elif len(args) > 1:
            failure_result = args[1]
            if failure_result.iscallable():
                return failure_result.call_with_extra_info([], env, cont, app)
            else:
                return return_value(failure_result, env, cont)
        raise SchemeException("%s-accessor: expected %s? but got %s" %
                (self.property.name, self.property.name, arg.tostring()))

def struct2vector(struct, immutable=False):
    struct_desc = struct.struct_type().name
    first_el = values.W_Symbol.make("struct:" + struct_desc)
    return values_vector.W_Vector.fromelements([first_el] + struct.vals(), immutable=immutable)
