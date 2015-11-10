import itertools

from pycket import config
from pycket import values
from pycket import vector as values_vector
from pycket.cont import continuation, label
from pycket.error import SchemeException
from pycket.prims.expose import make_call_method
from pycket.small_list import inline_small_list
from pycket.arity import Arity

from rpython.rlib import jit
from rpython.rlib.unroll import unrolling_iterable

w_prefab_symbol = values.W_Symbol.make("prefab")

class W_StructInspector(values.W_Object):
    errorname = "struct-inspector"
    _immutable_fields_ = ["w_super"]

    @staticmethod
    def make(w_inspector, issibling = False):
        w_super = w_inspector
        if issibling:
            w_super = w_inspector.w_super if w_inspector is not None else None
        return W_StructInspector(w_super)

    def __init__(self, w_super):
        self.w_super = w_super

    def has_control(self, struct_type):
        w_inspector = struct_type.w_inspector
        if not isinstance(w_inspector, W_StructInspector):
            return True
        else:
            w_inspector = w_inspector.w_super
            while isinstance(w_inspector, W_StructInspector):
                if w_inspector is self:
                    return True
                w_inspector = w_inspector.w_super
            return False

current_inspector = W_StructInspector(None)

class W_StructType(values.W_Object):
    errorname = "struct-type-descriptor"
    _immutable_fields_ = ["_tag", "name", "w_super", "init_field_count",
                          "auto_field_count", "total_field_count", "w_auto_value", "w_properties",
                          "w_inspector", "immutables[*]", "immutable_fields[*]", "w_guard",
                          "w_auto_values[*]", "offsets[*]", "constructor_name",
                          "constructor", "predicate", "accessor", "mutator",
                          "prop_procedure"]

    unbound_prefab_types = {}

    def __init__(self, name, w_super_type, init_field_count, auto_field_count,
                 w_auto_value, w_inspector, immutables,
                 w_proc_spec=values.w_false, w_guard=values.w_false,
                 constructor_name=None):
        self.name = name
        self.w_super = w_super_type
        self.init_field_count = init_field_count
        self.auto_field_count = auto_field_count
        self.total_field_count = self.init_field_count + self.auto_field_count
        if isinstance(w_super_type, W_StructType):
            self.total_field_count += w_super_type.total_field_count

        self.w_auto_value = w_auto_value
        self.properties = []
        self.prop_procedure = None
        self.procedure_source = None
        self.w_inspector = w_inspector
        if isinstance(w_proc_spec, values.W_Fixnum):
            immutables.insert(w_proc_spec.value, 0)
        self.immutables = immutables[:]
        self.w_guard = w_guard
        self.w_auto_values = [self.w_auto_value] * self.auto_field_count

        self.setup_tag()
        self.setup_prefab()
        self.calculate_offsets()
        self.setup_auxiliary_procedures(constructor_name)

    def setup_tag(self):
        from pycket.shape import get_struct_tag
        self._tag = get_struct_tag(self)

    def setup_prefab(self):
        self.isprefab = self.w_inspector is w_prefab_symbol
        if self.isprefab:
            self.isopaque = False
        else:
            self.isopaque = self.w_inspector is not values.w_false

    def setup_auxiliary_procedures(self, constructor_name):
        constructor_name = (constructor_name if constructor_name is not None \
                            else "make-" + self.name)
        self.constructor = W_StructConstructor(self, constructor_name)
        self.predicate = W_StructPredicate(self)
        self.accessor = W_StructAccessor(self)
        self.mutator = W_StructMutator(self)

    @jit.unroll_safe
    def calculate_offsets(self):
        offsets = []
        immutable_fields = [] # absolut indices
        w_struct_type = self
        while isinstance(w_struct_type, W_StructType):
            offset = w_struct_type.total_field_count - \
                w_struct_type.init_field_count - w_struct_type.auto_field_count
            offsets.append((w_struct_type, offset))
            for immutable_field in w_struct_type.immutables:
                immutable_fields.append(immutable_field + offset)
            w_struct_type = w_struct_type.w_super
        self.offsets = offsets[:]
        self.immutable_fields = immutable_fields[:]


    @staticmethod
    def make(w_name, w_super_type, w_init_field_count, w_auto_field_count,
             w_auto_value=values.w_false, w_properties=values.w_null,
             w_inspector=values.w_false, w_immutables=values.w_null,
             w_proc_spec=values.w_false, w_guard=values.w_false,
             w_constructor_name=values.w_false, env=None, cont=None):
        """
        This method returns five instances via cont:
            W_StructType, W_StructConstructor, W_StructPredicate, W_StructAccessor, W_StructMutator
        """
        w_struct_type = W_StructType.make_simple(
            w_name, w_super_type, w_init_field_count, w_auto_field_count,
            w_auto_value, w_properties, w_inspector, w_immutables, w_proc_spec,
            w_guard, w_constructor_name)
        return w_struct_type.initialize_properties(w_properties, w_proc_spec, env, cont)


    @staticmethod
    @jit.unroll_safe
    def make_prefab(prefab_key):
        if prefab_key in W_StructType.unbound_prefab_types:
            return W_StructType.unbound_prefab_types[prefab_key]

        if prefab_key.super_key:
            w_super_type = W_StructType.make_prefab(prefab_key.super_key)
        else:
            w_super_type = values.w_false

        immutables = [i for i in range(prefab_key.init_field_count) \
                      if i not in prefab_key.mutables]
        w_struct_type = W_StructType(
            name=prefab_key.name,
            w_super_type=w_super_type,
            init_field_count=prefab_key.init_field_count,
            auto_field_count=prefab_key.auto_field_count,
            w_auto_value=prefab_key.w_auto_value,
            w_inspector=w_prefab_symbol,
            immutables=immutables)
        W_StructType.unbound_prefab_types[prefab_key] = w_struct_type
        return w_struct_type

    @staticmethod
    def make_simple(w_name, w_super_type, w_init_field_count,
                    w_auto_field_count, w_auto_value=values.w_false,
                    w_properties=values.w_null, w_inspector=values.w_false,
                    w_immutables=values.w_null, w_proc_spec=values.w_false,
                    w_guard=values.w_false, w_constructor_name=values.w_false,
                    prefab_key=None):
        """
        Returns an instance of W_StructType (without properties)
        """
        if w_inspector is w_prefab_symbol:
            if prefab_key is None:
                prefab_key = W_PrefabKey.from_raw_params(
                    w_name, w_init_field_count, w_auto_field_count,
                    w_auto_value, w_immutables, w_super_type)
            return W_StructType.make_prefab(prefab_key)

        assert isinstance(w_name, values.W_Symbol)
        name = w_name.utf8value
        assert isinstance(w_init_field_count, values.W_Fixnum)
        init_field_count = w_init_field_count.value
        assert isinstance(w_auto_field_count, values.W_Fixnum)
        auto_field_count = w_auto_field_count.value
        if isinstance(w_constructor_name, values.W_Symbol):
            constructor_name = w_constructor_name.utf8value
        else:
            constructor_name = None
        immutables = []
        for i in values.from_list(w_immutables):
            assert isinstance(i, values.W_Fixnum)
            immutables.append(i.value)
        return W_StructType(
            name, w_super_type, init_field_count, auto_field_count,
            w_auto_value, w_inspector, immutables, w_proc_spec, w_guard,
            constructor_name)

    @continuation
    def save_property_value(self, properties, idx, is_checked, env, cont, _vals):
        from pycket.interpreter import check_one_val
        property = properties[idx][0]
        property_val = check_one_val(_vals)
        properties[idx] = (property, property_val, None)
        return self.attach_property(properties, idx, is_checked, env, cont)

    @label
    def attach_property(self, properties, idx, is_checked, env, cont):
        from pycket.interpreter import return_multi_vals
        if idx < len(properties):
            (property, property_val, sub_property) = properties[idx]
            if sub_property is not None:
                for p in properties:
                    if p[0] is sub_property:
                        return property_val.call([p[1]], env,
                            self.save_property_value(properties, idx, False, env, cont))
            assert isinstance(property, W_StructProperty)
            if not is_checked and property.w_guard.iscallable():
                return property.w_guard.call([property_val, values.to_list(self.struct_type_info())],
                    env, self.save_property_value(properties, idx, True, env, cont))
            if property.isinstance(w_prop_procedure):
                self.prop_procedure = property_val
            self.properties.append((property, property_val))
            return self.attach_property(properties, idx + 1, False, env, cont)
        # at this point all properties are saved, next step is to copy
        # propertyes from super types
        w_struct_type = self.w_super
        while isinstance(w_struct_type, W_StructType):
            self.properties = self.properties + w_struct_type.properties
            if not self.prop_procedure and w_struct_type.prop_procedure:
                self.prop_procedure = w_struct_type.prop_procedure
                self.procedure_source = w_struct_type.procedure_source
            w_struct_type = w_struct_type.w_super
        struct_tuple = self.make_struct_tuple()
        return return_multi_vals(values.Values.make(struct_tuple), env, cont)

    @jit.unroll_safe
    def initialize_property(self, properties, p, sub_property=None):
        property = p.car()
        property_val = p.cdr()
        if sub_property is None:
            if property.isinstance(w_prop_procedure):
                if self.prop_procedure is not None and\
                    self.prop_procedure is not property_val:
                    raise SchemeException(
                        "make-struct-type: duplicate property binding\nproperty: %s" %
                            property.tostring())
                self.prop_procedure = property_val
                self.procedure_source = self
            elif property.isinstance(w_prop_checked_procedure):
                if self.total_field_count < 2:
                    raise SchemeException("need at least two fields in the structure type")
        properties.append((property, property_val, sub_property))
        assert isinstance(property, W_StructProperty)
        for super_p in property.supers:
            self.initialize_property(properties, super_p, property)

    @jit.unroll_safe
    def initialize_properties(self, properties, proc_spec, env, cont):
        """
        Properties initialization contains few steps:
            1. call initialize_property for each property from the input list,
               it extracts all super values and stores them into properties array
               with a flat structure
            2. recursively call attach_property for each property from properties and
               prepare the value:
               * if the current property has a subproperty, the value is the result
                 of calling value procedure with a sub value as an argument
               * if the current property has a guard, the value is the result of
                 calling guard with a value and struct type info as arguments
               * in other case, just keep the current value
        """
        proplist = values.from_list(properties)
        properties = []
        for p in proplist:
            self.initialize_property(properties, p)
        if proc_spec is not values.w_false:
            self.initialize_property(properties, values.W_Cons.make(w_prop_procedure, proc_spec))
        return self.attach_property(properties, 0, False, env, cont)


    @jit.elidable
    def get_offset(self, type):
        for t, v in self.offsets:
            if t is type:
                return v
        return -1

    @jit.elidable
    def is_immutable_field_index(self, i):
        return i in self.immutable_fields

    def struct_type_info(self):
        w_name = values.W_Symbol.make(self.name)
        w_init_field_count = values.W_Fixnum.make(self.init_field_count)
        w_auto_field_count = values.W_Fixnum.make(self.auto_field_count)
        w_immutable_k_list = values.to_list(
            [values.W_Fixnum.make(i) for i in self.immutables])
        w_super = values.w_false
        w_struct_type = self.w_super
        while isinstance(w_struct_type, W_StructType):
            if current_inspector.has_control(w_struct_type):
                w_super = w_struct_type
            w_struct_type = w_struct_type.w_super
        w_skipped = values.W_Bool.make(w_super is values.w_false and
            isinstance(self.w_super, W_StructType))
        return [w_name, w_init_field_count, w_auto_field_count, self.accessor,
                self.mutator, w_immutable_k_list, w_super, w_skipped]

    def make_struct_tuple(self):
        if self.constructor is None or self.predicate is None or \
           self.accessor is None or self.mutator is None:
            self.setup_auxiliary_procedures()
        return [self, self.constructor, self.predicate, self.accessor, self.mutator]

    @jit.elidable_promote('all')
    def read_property_precise(self, property):
        for p, val in self.properties:
            if p is property:
                return val
        return None

    @jit.elidable_promote('all')
    def read_property(self, property):
        for p, val in self.properties:
            if p.isinstance(property):
                return val
        return None

    def tostring(self):
        return "#<struct-type:%s>" % self.name

class W_PrefabKey(values.W_Object):
    _immutable_fields_ = ["name", "init_field_count", "auto_field_count",
        "w_auto_value", "mutables", "super_key"]
    all_keys = []

    @staticmethod
    def make(name, init_field_count, auto_field_count, w_auto_value, mutables, super_key):
        for key in W_PrefabKey.all_keys:
            if key.equal_tuple((name, init_field_count, auto_field_count,
                                w_auto_value, mutables, super_key)):
                return key
        key = W_PrefabKey(name, init_field_count, auto_field_count,
                          w_auto_value, mutables, super_key)
        W_PrefabKey.all_keys.append(key)
        return key

    @staticmethod
    def from_struct_type(struct_type):
        assert isinstance(struct_type, W_StructType)
        name = struct_type.name
        init_field_count = struct_type.init_field_count
        auto_field_count = struct_type.auto_field_count
        w_auto_value = struct_type.w_auto_value
        super_key = None
        mutables = []
        prev_idx = 1
        for i in struct_type.immutables:
            for j in range(prev_idx, i):
                mutables.append(j)
            prev_idx = i + 1
        if struct_type.w_super is not values.w_false:
            super_key = W_PrefabKey.from_struct_type(struct_type.w_super)
        return W_PrefabKey.make(name, init_field_count, auto_field_count,
                                w_auto_value, mutables, super_key)

    @staticmethod
    def from_raw_params(w_name, w_init_field_count, w_auto_field_count,
                        w_auto_value, w_immutables, super_type):
        assert isinstance(w_name, values.W_Symbol)
        name = w_name.utf8value
        assert isinstance(w_init_field_count, values.W_Fixnum)
        init_field_count = w_init_field_count.value
        assert isinstance(w_auto_field_count, values.W_Fixnum)
        auto_field_count = w_auto_field_count.value
        mutables = []
        immutables = []
        super_key = None

        for i in values.from_list(w_immutables):
            assert isinstance(i, values.W_Fixnum)
            immutables.append(i.value)

        for j in range(init_field_count):
            if not j in immutables:
                mutables.append(j)

        if super_type is not values.w_false:
            super_key = W_PrefabKey.from_struct_type(super_type)
        return W_PrefabKey.make(name, init_field_count, auto_field_count,
                                w_auto_value, mutables, super_key)

    @staticmethod
    def from_raw_key(w_key, total_count=0, is_super=False):

        @jit.unroll_safe
        def parse_key(w_key, total_count, is_super):
            init_count = -1
            auto_count = 0
            w_auto_value = values.w_false
            super_key = None
            mutables = []

            name_seen = init_seen = auto_seen = mutable_seen = False

            if isinstance(w_key, values.W_Symbol):
                name = w_key.utf8value
                init_count = total_count
                w_key = values.w_null
                name_seen = True

            while w_key is not values.w_null:
                w_val, w_rest = w_key.to_tuple()
                if isinstance(w_val, values.W_Symbol):
                    if name_seen:
                        # super key
                        if init_seen:
                            super_total = total_count - init_count
                        else:
                            super_total = total_count
                        super_key = W_PrefabKey.from_raw_key(
                            w_key, super_total, True)
                        # there's nothing after a properly parsed super key
                        w_rest = values.w_null
                    else:
                        # prefab name
                        assert not (init_seen or auto_seen or mutable_seen)
                        name = w_val.utf8value
                        name_seen = True
                elif isinstance(w_val, values.W_Fixnum):
                    # init field count
                    assert name_seen  and not init_seen
                    init_count = w_val.value
                    init_seen = True
                elif isinstance(w_val, values.W_List):
                    # auto fields
                    assert name_seen and not auto_seen
                    if is_super: assert init_seen
                    w_auto_count, w_val = w_val.to_tuple()
                    w_auto_value, w_val = w_val.to_tuple()
                    assert isinstance(w_auto_count, values.W_Fixnum)
                    auto_count =  w_auto_count.value
                    auto_seen = True
                elif isinstance(w_val, values_vector.W_Vector):
                    # mutable fields
                    assert name_seen and not mutable_seen
                    if is_super: assert init_seen
                    for i in range(w_val.len):
                        mutable = w_val.ref(i)
                        assert isinstance(mutable, values.W_Fixnum)
                        mutables.append(mutable.value)
                    mutable_seen = True
                w_key = w_rest
            assert name_seen
            if is_super: assert init_seen
            return (name, init_count, auto_count,
                    w_auto_value, super_key, mutables)

        name, \
        init_count, \
        auto_count, \
        w_auto_value, \
        super_key, \
        mutables = parse_key(w_key, total_count, is_super)

        if init_count == -1:
            init_count = total_count - auto_count
            s_key = super_key
            while s_key:
                init_count -= (s_key.init_field_count + s_key.auto_field_count)
                s_key = s_key.super_key

        return W_PrefabKey.make(name, init_count, auto_count,
                                w_auto_value, mutables, super_key)

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

    def __init__(self, name, init_field_count, auto_field_count,
                 w_auto_value, mutables, super_key):
        self.name = name
        self.init_field_count = init_field_count
        self.auto_field_count = auto_field_count
        self.w_auto_value = w_auto_value
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
        key.append(values.W_Fixnum.make(self.init_field_count))
        if self.auto_field_count > 0:
            key.append(values.to_list(
                [values.W_Fixnum.make(self.auto_field_count), self.w_auto_value]))
        mutables = []
        for i in self.mutables:
            mutables.append(values.W_Fixnum.make(i))
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
        return self.name, self.init_field_count, self.auto_field_count,\
            self.w_auto_value, self.mutables, self.super_key

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
            w_property_val = self.struct_type().read_property(w_prop_arity_string)
            if w_property_val:
                return w_property_val.call_with_extra_info([self], env,
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
    def get_property(self, property, env, cont):
        raise NotImplementedError("abstract base class")

    def get_struct_info(self, env, cont):
        raise NotImplementedError("abstract base class")

    def vals(self):
        raise NotImplementedError("abstract base class")

# @inline_small_list(immutable=True, attrname="storage", unbox_num=True)
class W_Struct(W_RootStruct):
    errorname = "struct"
    _immutable_fields_ = ["_shape"]

    @staticmethod
    @jit.unroll_safe
    def make_prefab(w_key, w_values):
        w_struct_type = W_StructType.make_prefab(
            W_PrefabKey.from_raw_key(w_key, len(w_values)))
        if w_struct_type.auto_field_count > 0:
            # Auto-fields are _mutable_ without mutator
            for i, value in enumerate(w_values):
                if not w_struct_type.is_immutable_field_index(i):
                    value = values.W_Cell(value)
                    w_values[i] = value
        return W_Struct.make(w_values, w_struct_type)

    def __init__(self, shape):
        from pycket.shape import CompoundShape
        assert isinstance(shape, CompoundShape)
        self._shape = shape

    def get_storage(self):
        return []

    def get_storage_at(self, index):
        raise IndexError()

    def get_storage_width(self):
        return 0

    def get_tag(self):
        return self.shape()._tag

    def get_children(self):
        return self.shape().get_children(self)

    def get_child(self, index):
        return self.shape().get_child(self, index)

    def get_number_of_children(self):
        return self.shape().get_number_of_direct_children()

    def shape(self):
        return jit.promote(self._shape)

    def struct_type(self):
        import pycket.shape
        tag = self.get_tag()
        assert isinstance(tag, pycket.shape.StructTag)
        return jit.promote(tag.struct_type())

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
    def get_property(self, property, env, cont):
        from pycket.interpreter import return_value
        val = self.struct_type().read_property_precise(property)
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

    # # TODO: currently unused
    # def tostring_proc(self, env, cont):
    #     w_val = self.struct_type().read_property(w_prop_custom_write)
    #     if w_val is not None:
    #         assert isinstance(w_val, values_vector.W_Vector)
    #         w_write_proc = w_val.ref(0)
    #         port = values.W_StringOutputPort()
    #         # TODO: #t for write mode, #f for display mode,
    #         # or 0 or 1 indicating the current quoting depth for print mode
    #         mode = values.w_false
    #         return w_write_proc.call([self, port, mode], env, cont)
    #     return self.tostring()

    def tostring(self):
        typ = self.struct_type()
        if typ.isopaque:
            result =  "#<%s>" % typ.name
        else:
            if typ.isprefab:
                result = "#s(%s %s)" %\
                    (W_PrefabKey.from_struct_type(typ).short_key().tostring(),
                        ' '.join([val.tostring() for val in self.vals()]))
            else:
                result = "(%s %s)" % (typ.name,
                    ' '.join([val.tostring() for val in self.vals()]))
        return result

class W_NAryStruct(W_Struct):
    _immutable_fields_ = ['_storage[*]']

    def _init_storage(self, storage):
        self._storage = storage or []

    def get_storage(self):
        return self._storage
    _get_full_list = get_storage

    def get_storage_at(self, index):
        return self._storage[index]

    _get_list = get_storage_at

    def get_storage_width(self):
        return len(self._storage)
    _get_size_list = get_storage_width

    # Test only.
    def __eq__(self, other):
        "NOT_RPYTON"
        if isinstance(other, W_Struct):
            if self.get_number_of_children() == other.get_number_of_children():
                return self.get_children() == other.get_children()
        return False

STORAGE_ATTR_TEMPLATE = "storage_%d"



def _w_struct_make_from_shape(field_values, shape):
    # print "MAKING", shape.merge_point_string(), [s.tostring() for s in field_values]
    w_struct = W_NAryStruct(shape)
    w_struct._init_storage(field_values)
    return w_struct
W_Struct.make_from_shape = staticmethod(_w_struct_make_from_shape)

def _w_struct_make(field_values, w_structtype):
    from pycket.shape import get_struct_tag
    return W_Struct.make_from_shape(
        field_values, get_struct_tag(w_structtype).default_shape)
W_Struct.make = staticmethod(_w_struct_make)
# 

class W_StructTypeProcedure(values.W_Procedure):
    "Base for all Structure type related procedures"
    _immutable_fields_ = ["_struct_type"]
    def __init__(self, struct_type):
        self._struct_type = struct_type
    def struct_type_name(self):
        return self._struct_type.name
    def struct_type(self):
        return self._struct_type
    def struct_type_promote(self):
        return jit.promote(self.struct_type())

class W_StructConstructor(W_StructTypeProcedure):
    _immutable_fields_ = ["constructor_name"]
    def __init__(self, struct_type, constructor_name):
        W_StructTypeProcedure.__init__(self, struct_type)
        self.constructor_name = constructor_name

    @continuation
    @jit.unroll_safe
    def constructor_proc_cont(self, field_values, env, cont, _vals):
        from pycket.interpreter import return_value
        guard_super_values = _vals.get_all_values()
        struct_type = self.struct_type_promote()
        if guard_super_values:
            field_values = guard_super_values + field_values[len(guard_super_values):]
        if len(struct_type.w_auto_values) > 0:
            field_values = field_values + struct_type.w_auto_values
        # constant_false = []
        for i, value in enumerate(field_values):
            if not struct_type.is_immutable_field_index(i):
                value = values.W_Cell(value)
                field_values[i] = value
            # elif value is values.w_false:
            #     constant_false.append(i)
        result = W_Struct.make(field_values, struct_type)
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
    def constructor_proc_wrapper_cont(self, field_values, struct_type_name, issuper,
        app, env, cont, _vals):
        from pycket.interpreter import return_multi_vals, jump
        guard_values = _vals.get_all_values()
        struct_type = self.struct_type_promote()
        if guard_values:
            field_values = guard_values
        super_type = jit.promote(struct_type.w_super)
        if isinstance(super_type, W_StructType):
            split_position = len(field_values) - struct_type.init_field_count
            super_auto = super_type.constructor.struct_type().w_auto_values
            assert split_position >= 0
            field_values = self._splice(field_values, len(field_values),\
                split_position, super_auto, len(super_auto))
            if issuper:
                return super_type.constructor.code(field_values[:split_position],
                    struct_type_name, True, env, cont, app)
            else:
                return super_type.constructor.code(field_values[:split_position],
                    struct_type_name, True,
                    env, self.constructor_proc_cont(field_values, env, cont), app)
        else:
            if issuper:
                return return_multi_vals(values.Values.make(field_values), env, cont)
            else:
                return jump(env, self.constructor_proc_cont(field_values, env, cont))

    def code(self, field_values, struct_type_name, issuper, env, cont, app):
        from pycket.interpreter import jump
        struct_type = self.struct_type_promote()
        if struct_type.w_guard is values.w_false:
            return jump(env, self.constructor_proc_wrapper_cont(field_values,
                struct_type_name, issuper, app, env, cont))
        else:
            guard_args = field_values + [values.W_Symbol.make(struct_type_name)]
            jit.promote(self)
            return struct_type.w_guard.call_with_extra_info(guard_args, env,
                self.constructor_proc_wrapper_cont(field_values, struct_type_name,
                    issuper, app, env, cont), app)

    @make_call_method(simple=False)
    def call_with_extra_info(self, args, env, cont, app):
        return self.code(args, self.struct_type_name(), False, env, cont, app)

    def tostring(self):
        return "#<procedure:%s>" % self.struct_type_name()

class W_StructPredicate(W_StructTypeProcedure):
    errorname = "struct-predicate"

    @jit.elidable
    def is_for_same_type_as(self, other):
        return other is self._struct_type

    @make_call_method([values.W_Object])
    @jit.unroll_safe
    def call(self, struct):
        if isinstance(struct, W_RootStruct):
            struct_type = struct.struct_type()
            while isinstance(struct_type, W_StructType):
                if self.is_for_same_type_as(struct_type):
                    return values.w_true
                struct_type = struct_type.w_super
        return values.w_false

    def tostring(self):
        return "#<procedure:%s?>" % self.struct_type_name()

class W_StructFieldAccessor(values.W_Procedure):
    errorname = "struct-field-accessor"
    _immutable_fields_ = ["accessor", "field", "field_name"]
    def __init__(self, accessor, field, field_name):
        assert isinstance(accessor, W_StructAccessor)
        assert isinstance(field, values.W_Fixnum)
        self.accessor = accessor
        self.field = field.value
        self.field_name = field_name

    @make_call_method([W_RootStruct], simple=False,
        name="<struct-field-accessor-method>")
    def call_with_extra_info(self, struct, env, cont, app):
        jit.promote(self)
        return self.accessor.access(struct, self.field, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-%s>" % (self.accessor.struct_type_name(),
                                       self.field_name.variable_name())

class W_StructAccessor(W_StructTypeProcedure):
    errorname = "struct-accessor"

    def access(self, struct, field, env, cont, app):
        assert isinstance(struct, W_RootStruct)
        jit.promote(self)
        struct_type = self.struct_type_promote()
        offset = struct.struct_type().get_offset(struct_type)
        if offset == -1:
            raise SchemeException("cannot reference an identifier before its definition")
        return struct.ref_with_extra_info(field + offset, app, env, cont)

    @make_call_method([W_RootStruct, values.W_Fixnum], simple=False,
        name="<struct-accessor-method>")
    def call_with_extra_info(self, struct, field, env, cont, app):
        return self.access(struct, field.value, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-ref>" % self.struct_type_name()

class W_StructFieldMutator(values.W_Procedure):
    errorname = "struct-field-mutator"
    _immutable_fields_ = ["mutator", "field", "field_name"]
    def __init__ (self, mutator, field, field_name):
        assert isinstance(mutator, W_StructMutator)
        assert isinstance(field, values.W_Fixnum)
        self.mutator = mutator
        self.field = field.value
        self.field_name = field_name

    @make_call_method([W_RootStruct, values.W_Object], simple=False,
        name="<struct-field-mutator-method>")
    def call_with_extra_info(self, struct, val, env, cont, app):
        return self.mutator.mutate(struct, self.field, val, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-%s!>" % (self.mutator.struct_type_name(), self.field_name.variable_name())

class W_StructMutator(W_StructTypeProcedure):
    errorname = "struct-mutator"

    def mutate(self, struct, field, val, env, cont, app):
        assert isinstance(struct, W_RootStruct)
        jit.promote(self)
        struct_type = self.struct_type_promote()
        offset = struct.struct_type().get_offset(struct_type)
        if offset == -1:
            raise SchemeException("cannot reference an identifier before its definition")
        return struct.set_with_extra_info(field + offset, val, app, env, cont)

    @make_call_method([W_RootStruct, values.W_Fixnum, values.W_Object],
        simple=False, name="<struct-mutator-method>")
    def call_with_extra_info(self, struct, field, val, env, cont, app):
        return self.mutate(struct, field.value, val, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-set!>" % self.struct_type_name()

class W_StructProperty(values.W_Object):
    errorname = "struct-type-property"
    _immutable_fields_ = ["name", "w_guard", "supers", "can_imp"]
    def __init__(self, name, w_guard, supers=values.w_null, can_imp=False):
        self.name = name.utf8value
        self.w_guard = w_guard
        self.supers = values.from_list(supers)
        self.can_imp = can_imp

    def isinstance(self, property):
        if self is property:
            return True
        for super in self.supers:
            if super.car().isinstance(property):
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

del sym

class W_StructPropertyPredicate(values.W_Procedure):
    errorname = "struct-property-predicate"
    _immutable_fields_ = ["property"]
    def __init__(self, property):
        self.property = property

    @make_call_method([values.W_Object])
    @jit.unroll_safe
    def call(self, arg):
        if not isinstance(arg, W_RootStruct):
            return values.w_false
        w_val = arg.struct_type().read_property_precise(self.property)
        if w_val is not None:
            return values.w_true
        return values.w_false

class W_StructPropertyAccessor(values.W_Procedure):
    errorname = "struct-property-accessor"
    _immutable_fields_ = ["property"]
    def __init__(self, property):
        self.property = property

    @make_call_method(simple=False)
    def call_with_extra_info(self, args, env, cont, app):
        from pycket.interpreter import return_value
        arg = args[0]
        if isinstance(arg, W_StructType):
            w_val = arg.read_property_precise(self.property)
            if w_val is not None:
                return return_value(w_val, env, cont)
        elif isinstance(arg, W_RootStruct):
            return arg.get_property(self.property, env, cont)
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
