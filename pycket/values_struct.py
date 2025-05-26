import itertools, sys

from pycket import config
from pycket import values
from pycket import vector as values_vector
from pycket.arity import Arity
from pycket.base import SingleResultMixin, UnhashableType
from pycket.cont import continuation, label
from pycket.error import SchemeException
from pycket.prims.expose import default, make_call_method
from pycket.small_list import inline_small_list
from pycket.util import strip_immutable_field_name
from pycket.values_parameter import W_Parameter

from rpython.rlib import jit
from rpython.rlib.objectmodel import import_from_mixin, not_rpython
from rpython.rlib.unroll import unrolling_iterable

w_prefab_symbol = values.W_Symbol.make("prefab")

class W_StructInspector(values.W_Object):
    errorname = "struct-inspector"
    _immutable_fields_ = ["w_super"]
    _attrs_ = ["w_super"]

    @staticmethod
    def make(w_inspector, issibling = False):
        assert isinstance(w_inspector, W_StructInspector)
        w_super = w_inspector
        if issibling:
            w_super = w_inspector.w_super if w_inspector is not None else None
        return W_StructInspector(w_super)

    def __init__(self, w_super):
        self.w_super = w_super

    @jit.elidable
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
current_inspector_param = W_Parameter(current_inspector)

class W_StructType(values.W_Object):
    errorname = "struct-type-descriptor"
    _immutable_fields_ = [
            "name", "constructor_name", "w_super",
            "init_field_count", "auto_field_count", "total_field_count",
            "total_auto_field_count", "total_init_field_count",
            "w_auto_value", "properties", "w_inspector", "immutables[*]",
            "immutable_fields[*]", "w_guard", "auto_values_w[*]", "offsets[*]",
            "constructor", "predicate", "accessor", "mutator", "prop_procedure",
            "constructor_arity", "procedure_source", "isprefab", "isopaque",
            "prop_sealed"]

    _attrs_ = map(strip_immutable_field_name, _immutable_fields_)

    unbound_prefab_types = {}

    @jit.unroll_safe
    def __init__(self, w_name, w_super_type, init_field_count, auto_field_count,
                 w_auto_value, w_inspector, w_proc_spec, immutables, w_guard,
                 w_constructor_name):
        self.name = w_name
        self.constructor_name = w_constructor_name
        self.w_super = w_super_type
        self.init_field_count = init_field_count
        self.total_init_field_count = init_field_count
        self.auto_field_count = auto_field_count
        self.total_auto_field_count = auto_field_count
        self.total_field_count = init_field_count + auto_field_count

        if isinstance(w_super_type, W_StructType):
            self.total_field_count += w_super_type.total_field_count
            self.total_init_field_count += w_super_type.total_init_field_count
            self.total_auto_field_count += w_super_type.total_auto_field_count

        self.w_auto_value = w_auto_value
        self.properties = []
        self.prop_procedure = None
        self.prop_sealed = False
        self.procedure_source = None
        self.w_inspector = w_inspector

        if isinstance(w_proc_spec, values.W_Fixnum):
            immutables = [w_proc_spec.value] + immutables

        self.immutables = immutables
        self.w_guard = w_guard

        self.auto_values_w = [self.w_auto_value] * self.auto_field_count

        self.setup_prefab()
        self._calculate_offsets()
        self._generate_methods()

    def setup_prefab(self):
        self.isprefab = self.w_inspector is w_prefab_symbol
        if self.isprefab:
            self.isopaque = False
        else:
            self.isopaque = self.w_inspector is not values.w_false

    @jit.unroll_safe
    def _generate_methods(self):
        """ Generate constructor, predicate, mutator, and accessor """
        count = self.total_init_field_count
        self.constructor_arity = Arity([count], -1)

        self.constructor = W_StructConstructor(self)
        self.predicate   = W_StructPredicate(self)
        self.accessor    = W_StructAccessor(self)
        self.mutator     = W_StructMutator(self)

    @jit.unroll_safe
    def _calculate_offsets(self):
        offsets = []
        immutable_fields = [] # absolut indices
        w_struct_type = self
        while isinstance(w_struct_type, W_StructType):
            offset = (w_struct_type.total_field_count -
                      w_struct_type.init_field_count - 
                      w_struct_type.auto_field_count)
            offsets.append((w_struct_type, offset))
            for immutable_field in w_struct_type.immutables:
                immutable_fields.append(immutable_field + offset)
            w_struct_type = w_struct_type.w_super
        self.offsets = offsets[:]
        self.immutable_fields = immutable_fields[:]


    @staticmethod
    def make(w_name, w_super_type, init_field_count, auto_field_count,
             w_auto_value=values.w_false, w_properties=values.w_null,
             w_inspector=values.w_false, w_proc_spec=values.w_false, 
             immutables=[], w_guard=values.w_false,
             w_constructor_name=values.w_false, env=None, cont=None):
        """
        This method returns five instances:
            W_StructType
            W_StructConstructor
            W_StructPredicate
            W_StructAccessor
            W_StructMutator
        """
        w_struct_type = W_StructType.make_simple(
            w_name=w_name,
            w_super_type=w_super_type, 
            init_field_count=init_field_count,
            auto_field_count=auto_field_count,
            w_auto_value=w_auto_value,
            w_inspector=w_inspector,
            w_proc_spec=w_proc_spec,
            immutables=immutables,
            w_guard=w_guard,
            w_constructor_name=w_constructor_name)
        return w_struct_type.initialize_properties(w_properties, w_proc_spec, env, cont)


    @staticmethod
    @jit.elidable
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
            w_name=prefab_key.w_name,
            w_super_type=w_super_type,
            init_field_count=prefab_key.init_field_count,
            auto_field_count=prefab_key.auto_field_count,
            w_auto_value=prefab_key.w_auto_value,
            w_inspector=w_prefab_symbol,
            w_proc_spec=values.w_false,
            immutables=immutables,
            w_guard=values.w_false, 
            w_constructor_name=values.w_false)
        W_StructType.unbound_prefab_types[prefab_key] = w_struct_type

        return w_struct_type

    @staticmethod
    def make_simple(w_name, w_super_type, init_field_count,
                    auto_field_count, w_auto_value=values.w_false,
                    w_inspector=values.w_false,
                    w_proc_spec=values.w_false, immutables=[],
                    w_guard=values.w_false, w_constructor_name=values.w_false):
        """
        This method returns an instance of W_StructType only.
        It does not support properties.
        """
        w_struct_type = W_StructType(
            w_name=w_name,
            w_super_type=w_super_type, 
            init_field_count=init_field_count,
            auto_field_count=auto_field_count,
            w_auto_value=w_auto_value,
            w_inspector=w_inspector,
            w_proc_spec=w_proc_spec,
            immutables=immutables,
            w_guard=w_guard,
            w_constructor_name=w_constructor_name)
        if w_inspector is w_prefab_symbol:
            prefab_key = W_PrefabKey.from_raw_params(w_name, init_field_count,
                    auto_field_count, w_auto_value, immutables, w_super_type)
            if prefab_key in W_StructType.unbound_prefab_types:
                return W_StructType.unbound_prefab_types[prefab_key]
            W_StructType.unbound_prefab_types[prefab_key] = w_struct_type
        return w_struct_type

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
                return property.w_guard.call([property_val, values.to_list(self.struct_type_info(cont))],
                    env, self.save_property_value(properties, idx, True, env, cont))
            if property.isinstance(w_prop_procedure):
                self.prop_procedure = property_val
            if property.isinstance(w_prop_sealed):
                # The value associated with the property is ignored; the
                # presence of the property itself makes the structure
                # type sealed.
                self.prop_sealed = True
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
        struct_tuple = [self, self.constructor, self.predicate, self.accessor, self.mutator]
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
            self.initialize_property(properties, values.wrap(w_prop_procedure, proc_spec))
        return self.attach_property(properties, 0, False, env, cont)

    @jit.elidable
    def get_offset(self, type):
        for t, v in self.offsets:
            if t is type:
                return v
            elif t.isprefab and type.isprefab:
                ## They might not be the same object but they
                ## might be the same prefab struct, so
                ## check if their fields are the same
                if (t.name is type.name and
                    t.init_field_count is type.init_field_count and
                    t.total_init_field_count is type.total_init_field_count and
                    t.auto_field_count is type.auto_field_count and
                    t.total_auto_field_count is type.total_auto_field_count and
                    t.total_field_count is type.total_field_count and
                    t.w_auto_value is type.w_auto_value and
                    t.immutables == type.immutables and
                    t.auto_values_w == type.auto_values_w and
                    t.isopaque is type.isopaque and
                    t.immutable_fields == type.immutable_fields and
                    t.constructor_arity.get_arity_list() == type.constructor_arity.get_arity_list()

                    #t.constructor_name is type.constructor_name and
                    #t.super is type.super and
                    #t.props == type.props and
                    #t.prop_procedure is type.prop_procedure and
                    #t.procedure_source is type.procedure_source and
                    #t.inspector is type.inspector and
                    #t.guard is type.guard and
                    #t.isprefab is type.isprefab and
                    #t.offsets is type.offsets and
                    ):
                    return v

        return -1

    @jit.elidable
    def is_immutable_field_index(self, i):
        return i in self.immutable_fields

    def all_fields_immutable(self):
        self = jit.promote(self)
        return self.total_field_count == len(self.immutable_fields)

    def struct_type_info(self, cont):
        w_name = self.name
        w_init_field_count = values.wrap(self.init_field_count)
        w_auto_field_count = values.wrap(self.auto_field_count)
        w_immutable_k_list = values.wrap_list(self.immutables)
        current_inspector = current_inspector_param.get(cont)
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

    @jit.elidable
    def all_opaque(self):
        if not self.isopaque:
            return False
        elif isinstance(self.w_super, W_StructType):
            return self.w_super.all_opaque()
        return True

    @jit.elidable
    def is_transparent(self):
        while self is not None and self is not values.w_false:
            if self.get_inspector() is not values.w_false:
                return False
            self = self.get_super()
        return True

    def get_inspector(self):
        return self.w_inspector

    def get_super(self):
        return self.w_super

    @jit.elidable
    def has_subtype(self, type):
        while isinstance(type, W_StructType):
            if type is self:
                return True
            type = type.w_super
        return False


    def hash_value(self):
        pass

    def tostring(self):
        return "#<struct-type:%s>" % self.name.variable_name()

class W_PrefabKey(values.W_Object):
    _attrs_ = _immutable_fields_ = ["w_name", "init_field_count", "auto_field_count",
                                    "w_auto_value", "mutables", "super_key"]
    all_keys = []

    @staticmethod
    @jit.elidable
    def make(w_name, init_field_count, auto_field_count, w_auto_value, mutables, super_key):
        assert isinstance(w_name, values.W_Symbol)
        for key in W_PrefabKey.all_keys:
            if key.equal_tuple((w_name, init_field_count, auto_field_count,
                                w_auto_value, mutables, super_key)):
                return key
        key = W_PrefabKey(w_name, init_field_count, auto_field_count,
                          w_auto_value, mutables, super_key)
        W_PrefabKey.all_keys.append(key)
        return key

    @staticmethod
    def from_struct_type(struct_type):
        assert isinstance(struct_type, W_StructType)
        w_name = struct_type.name
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
        return W_PrefabKey.make(w_name, init_field_count, auto_field_count,
                                w_auto_value, mutables, super_key)

    @staticmethod
    def from_raw_params(w_name, init_field_count, auto_field_count,
                        w_auto_value, immutables, super_type):
        mutables = []
        super_key = None
        prev_idx = 1
        for i in immutables:
            for j in range(prev_idx, i):
                mutables.append(j)
            prev_idx = i + 1

        if super_type is not values.w_false:
            super_key = W_PrefabKey.from_struct_type(super_type)
        return W_PrefabKey.make(w_name, init_field_count, auto_field_count,
                                w_auto_value, mutables, super_key)

    @staticmethod
    @jit.unroll_safe
    def parse_key(w_key, total_count, is_super):
        init_count = -1
        auto_count = 0
        w_auto_value = values.w_false
        super_key = None
        mutables = []
        w_name = None

        name_seen = init_seen = auto_seen = mutable_seen = False

        if isinstance(w_key, values.W_Symbol):
            w_name = w_key
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
                    w_name = w_val
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
        return (w_name, init_count, auto_count,
                w_auto_value, super_key, mutables)

    @staticmethod
    @jit.elidable
    def from_raw_key(w_key, total_count=0, is_super=False):

        w_name, \
        init_count, \
        auto_count, \
        w_auto_value, \
        super_key, \
        mutables = W_PrefabKey.parse_key(w_key, total_count, is_super)

        if init_count == -1:
            init_count = total_count - auto_count
            s_key = super_key
            while s_key:
                init_count -= (s_key.init_field_count + s_key.auto_field_count)
                # TODO: originally this on shapes, but master has just that one:
                # init_count -= s_key.init_field_count
                s_key = s_key.super_key

        return W_PrefabKey.make(w_name, init_count, auto_count,
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

    def __init__(self, w_name, init_field_count, auto_field_count,
                 w_auto_value, mutables, super_key):
        self.w_name = w_name
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
        key.append(self.w_name)
        key.append(values.wrap(self.init_field_count))
        if self.auto_field_count > 0:
            lst = values.wrap(self.w_auto_value, values.w_null)
            lst = values.wrap(self.auto_field_count, lst)
            key.append(lst)
        if self.mutables:
            vector = values_vector.wrap_vector(self.mutables)
            key.append(vector)
        if self.super_key:
            key.extend(self.super_key.key())
        return key

    def short_key(self):
        key = self.key()
        short_key = key[:1] + key[2:]
        return values.to_list(short_key) if len(short_key) > 1 else key[0]

    def make_key_tuple(self):
        return self.w_name, self.init_field_count, self.auto_field_count,\
            self.w_auto_value, self.mutables, self.super_key

class W_RootStruct(values.W_Object):
    errorname = "root-struct"
    _attrs_ = []

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
        arity = proc.get_arity(promote=True)
        if not arity.arity_includes(args_len):
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

    def get_arity(self, promote=False):
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

    def get_hash_proc(self, prop):
        if isinstance(prop, values.W_Cons):
            return prop.cdr().car()
        elif isinstance(prop, values_vector.W_Vector):
            return prop.ref(1)
        else:
            raise SchemeException("unexpected property value for prop:equal+hash: %s"%prop.tostring())
    
    def hash_equal(self, info=None):
        struct_type = self.struct_type()
        prop_equal_hash = struct_type.read_property(w_prop_equal_hash)

        if not struct_type.is_transparent():
            # if not transparent, eqv?
            if prop_equal_hash:
                from pycket.prims.hash import equal_hash_code
                w_hash_proc = self.get_hash_proc(prop_equal_hash)
                w_hash_proc_recur = equal_hash_code.w_prim # equal-hash-code to recur
                h = w_hash_proc.call_interpret([self, w_hash_proc_recur])
                assert isinstance(h, values.W_Fixnum)
                return h.value

            return values.W_Object.hash_equal(self, info)
        else:
            if not prop_equal_hash:
                # if transparent, equal?
                size = self._get_size_list()
                struct_name = struct_type.name
                total_hash_val = struct_name.hash_equal()
                for n in range(0, size):
                    try:
                        field_hash = self._get_list(n).hash_equal()
                        total_hash_val = int((field_hash*total_hash_val)%sys.maxint)
                    except UnhashableType:
                        continue
                return total_hash_val
            else:
                from pycket.prims.hash import equal_hash_code
                w_hash_proc = prop_equal_hash.cdr().car()
                w_hash_proc_recur = equal_hash_code.w_prim
                h = w_hash_proc.call_interpret([self, w_hash_proc_recur])
                assert isinstance(h, values.W_Fixnum)
                return h.value

@inline_small_list(immutable=True, attrname="storage", unbox_num=True)
class W_Struct(W_RootStruct):
    errorname = "struct"
    _attrs_ = _immutable_fields_ = ["_type"]

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
        val = self.struct_type().read_property_precise(property)
        if val is not None:
            return return_value(val, env, cont)
        raise SchemeException("%s-accessor: expected %s? but got %s" %
            (property.name, property.name, self.tostring()))

    def get_arity(self, promote=False):
        # XXX: --> struct type?
        if self.iscallable():
            typ = self.struct_type()
            proc = typ.prop_procedure
            if isinstance(proc, values.W_Fixnum):
                offset = typ.get_offset(typ.procedure_source)
                proc = self._ref(proc.value + offset)
                return proc.get_arity(promote)
            else:
                # -1 for the self argument
                arity = proc.get_arity(promote)
                return arity.shift_arity(-1)
        else:
            raise SchemeException("%s does not have arity" % self.tostring())

    def get_struct_info(self, env, cont):
        from pycket.interpreter import return_multi_vals
        vals = values.Values._make2(self.struct_type(), values.w_false)
        return return_multi_vals(vals, env, cont)

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

    def tostring_prefab(self):
        prefab_key = W_PrefabKey.from_struct_type(self.struct_type())
        key_and_values_all_str = [prefab_key.short_key().tostring()] + [val.tostring() for val in self.vals()]
        return ("#s(%s)" % (' '.join(key_and_values_all_str)))

    def write_prefab(self, port, env):
        from pycket.prims.input_output import write_loop
        prefab_key = W_PrefabKey.from_struct_type(self.struct_type())
        port.write("#s( ")
        sk = prefab_key.short_key().tostring()
        port.write(sk)
        for val in self.vals():
            port.write(" ")
            write_loop(val, port, env)
        port.write(")")

    @jit.unroll_safe
    def tostring_values(self, fields, w_type, is_super=False):
        " fill fields with tostring() version of field if applicable "
        assert isinstance(w_type, W_StructType)
        w_super = w_type.w_super
        has_super = isinstance(w_super, W_StructType)
        if has_super:
            self.tostring_values(fields=fields,w_type=w_super,is_super=True)
        offset = self.struct_type().get_offset(w_type)
        count = w_type.total_field_count
        if has_super:
            count -= w_super.total_field_count
        assert len(fields) >= count + offset
        if w_type.isopaque and offset < len(fields):
            fields[offset] = "..."
        else:
            for i in range(offset, offset + count):
                fields[i] = self._ref(i).tostring()

    @jit.unroll_safe
    def _string_from_list(self, l):
        return ' '.join([s for s in l if s is not None])

    def write_values(self, port, w_type, env):
        from pycket.prims.input_output import write_loop
        assert isinstance(w_type, W_StructType)
        w_super = w_type.w_super
        has_super = isinstance(w_super, W_StructType)
        if has_super:
            self.write_values(port, w_super, env)
        offset = self.struct_type().get_offset(w_type)
        count = w_type.total_field_count
        if has_super:
            count -= w_super.total_field_count
        for i in range(offset, offset + count):
            write_loop(self._ref(i), port, env)
            port.write(" ")

    def write(self, port, env):
        w_type = self.struct_type()
        typename = w_type.name.utf8value
        if w_type.isprefab:
            self.write_prefab(port, env)
        elif w_type.all_opaque():
            port.write("#<%s>" % typename)
        else:
            w_val = w_type.read_property(w_prop_custom_write)
            if w_val is not None:
                assert isinstance(w_val, values_vector.W_Vector)
                w_write_proc = w_val.ref(0)
                # #t for write mode, #f for display mode,
                # or 0 or 1 indicating the current quoting depth for print mode
                mode = values.w_true
                w_write_proc.call_interpret([self, port, mode])
            else:
                port.write("(%s " % typename)
                self.write_values(port, w_type, env)
                port.write(")")

    def tostring(self):
        w_type = self.struct_type()
        typename = w_type.name.utf8value
        if w_type.isprefab:
            return self.tostring_prefab()
        elif w_type.all_opaque():
            # import pdb;pdb.set_trace()
            # ret_str = "#<%s" % typename
            # for i in range(0, self._get_size_list()):
            #     ret_str += ":%s" % self._ref(i).tostring()
            # ret_str += ">"
            #return ret_str
            return "#<%s>" % typename
        else:
            fields = [None] * w_type.total_field_count
            self.tostring_values(fields=fields, w_type=w_type, is_super=False)
            custom_huh = w_type.read_property(w_prop_custom_write)
            return "(%s %s)" % (typename, self._string_from_list(fields))

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
    if CONST_FALSE_SIZE and constant_false and constant_false[-1] < CONST_FALSE_SIZE:
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

@jit.unroll_safe
def splice_array(array, index, insertion):
    array_len = len(array)
    insertion_len = len(insertion)
    new_array = [None] * (array_len + insertion_len)
    for pre_index in range(index):
        new_array[pre_index] = array[pre_index]
    for insert_index in range(insertion_len):
        new_array[index + insert_index] = insertion[insert_index]
    for post_index in range(index, array_len):
        new_array[post_index + insertion_len] = array[post_index]
    return new_array

@jit.unroll_safe
def construct_struct_final(struct_type, field_values, env, cont):
    from pycket.interpreter import return_value
    assert len(field_values) == struct_type.total_field_count
    constant_false = [] if CONST_FALSE_SIZE else None
    for i, value in enumerate(field_values):
        if not struct_type.is_immutable_field_index(i):
            value = values.W_Cell(value)
            field_values[i] = value
        elif CONST_FALSE_SIZE and value is values.w_false:
            constant_false.append(i)
    cls = lookup_struct_class(constant_false)
    if cls is not W_Struct:
        field_values = reduce_field_values(field_values, constant_false)
    result = cls.make(field_values, struct_type)
    return return_value(result, env, cont)

def construct_struct_loop(init_type, struct_type, field_values, env, cont):
    struct_type = jit.promote(struct_type)
    if not isinstance(struct_type, W_StructType):
        return construct_struct_final(init_type, field_values, env, cont)

    auto_field_start = struct_type.total_init_field_count
    w_guard = struct_type.w_guard
    if w_guard is values.w_false:
        return construct_struct_loop_body(init_type, struct_type, field_values,
                                          auto_field_start, env, cont)

    assert auto_field_start >= 0
    typename = init_type.name
    args = field_values[:auto_field_start] + [typename]
    cont = receive_guard_values_cont(init_type, struct_type, field_values,
                                     auto_field_start, env, cont)
    return w_guard.call(args, env, cont)

def construct_struct_loop_body(init_type, struct_type, field_values,
                               auto_field_start, env, cont):
    # Figure out where in the array the auto values start for this struct type.
    # Recall, the struct is built from the bottom up in the inheritance heirarchy.
    auto_values_w  = struct_type.auto_values_w
    field_values = splice_array(field_values, auto_field_start, auto_values_w)
    w_super_type = struct_type.w_super
    return construct_struct_loop(init_type, w_super_type, field_values, env, cont)

@continuation
def receive_guard_values_cont(init_type, struct_type, field_values,
                              auto_field_start, env, cont, _vals):
    assert _vals.num_values() == auto_field_start, "XXX Turn me into an exception"
    for i in range(auto_field_start):
        field_values[i] = _vals.get_value(i)
    return construct_struct_loop_body(init_type, struct_type, field_values,
                                      auto_field_start, env, cont)

# 

class W_StructTypeProcedure(values.W_Procedure):
    "Base for all Structure type related procedures"
    _attrs_ = _immutable_fields_ = ["_struct_type"]
    import_from_mixin(SingleResultMixin)

    def __init__(self, struct_type):
        self._struct_type = struct_type
    def struct_type_name(self):
        return self._struct_type.name.variable_name()
    def struct_type(self):
        return self._struct_type
    def struct_type_promote(self):
        return jit.promote(self.struct_type())

class W_StructConstructor(W_StructTypeProcedure):

    @make_call_method(simple=False)
    def call_with_extra_info(self, args, env, cont, app):
        type  = self.struct_type_promote()
        arity = type.constructor_arity
        if not arity.arity_includes(len(args)):
            raise SchemeException("%s: wrong number of arguments; expected %s but got %s" % (self.tostring(),arity.tostring(), len(args)))
        return construct_struct_loop(type, type, args, env, cont)

    def get_arity(self, promote=False):
        if promote:
            self = jit.promote(self)
        return self.struct_type().constructor_arity

    def tostring(self):
        return "#<procedure:%s>" % self.struct_type_name()


class W_StructPredicate(W_StructTypeProcedure):
    errorname = "struct-predicate"

    @make_call_method([values.W_Object])
    @jit.unroll_safe
    def call(self, struct):
        from pycket.impersonators import get_base_object
        struct = get_base_object(struct)
        if isinstance(struct, W_RootStruct):
            struct_type = struct.struct_type()
            while isinstance(struct_type, W_StructType):
                if struct_type is self.struct_type():
                    return values.w_true
                struct_type = struct_type.w_super
        return values.w_false

    def get_arity(self, promote=False):
        return Arity.ONE

    def tostring(self):
        return "#<procedure:%s?>" % self.struct_type_name()

class W_StructFieldAccessor(values.W_Procedure):
    errorname = "struct-field-accessor"
    _attrs_ = _immutable_fields_ = ["accessor", "field", "field_name"]
    import_from_mixin(SingleResultMixin)

    def __init__(self, accessor, field, field_name):
        assert isinstance(accessor, W_StructAccessor)
        self.accessor = accessor
        self.field = field
        self.field_name = field_name

    def get_absolute_index(self, type):
        return type.get_offset(self.accessor.struct_type()) + self.field

    def get_arity(self, promote=False):
        return Arity.ONE

    @make_call_method([values.W_Object], simple=False,
                      name="<struct-field-accessor-method>")
    def call_with_extra_info(self, struct, env, cont, app):
        jit.promote(self)
        return self.accessor.access(struct, self.field, env, cont, app)

    def tostring(self):
        name = self.accessor.struct_type_name()
        return "#<procedure:%s-%s>" % (name, self.field_name.variable_name())

class W_StructAccessor(W_StructTypeProcedure):
    errorname = "struct-accessor"

    def get_arity(self, promote=False):
        return Arity.TWO

    def access(self, struct, field, env, cont, app=None):
        self = jit.promote(self)
        st = jit.promote(struct.struct_type())
        if st is None:
            raise SchemeException("%s got %s" % (self.tostring(), struct.tostring()))
        offset = st.get_offset(self.struct_type())
        if offset == -1:
            raise SchemeException("%s: expected a %s but got a %s" % (self.tostring(), self.struct_type_name(), st.name.variable_name()))
        return struct.ref_with_extra_info(field + offset, app, env, cont)

    @make_call_method([values.W_Object, values.W_Fixnum], simple=False,
                      name="<struct-accessor-method>")
    def call_with_extra_info(self, struct, field, env, cont, app):
        return self.access(struct, field.value, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-ref>" % self.struct_type_name()

class W_StructFieldMutator(values.W_Procedure):
    errorname = "struct-field-mutator"
    _attrs_ = _immutable_fields_ = ["mutator", "field", "field_name"]
    import_from_mixin(SingleResultMixin)
    def __init__ (self, mutator, field, field_name):
        assert isinstance(mutator, W_StructMutator)
        self.mutator = mutator
        self.field = field
        self.field_name = field_name

    def get_arity(self, promote=False):
        return Arity.TWO

    def get_absolute_index(self, type):
        return type.get_offset(self.mutator.struct_type()) + self.field

    @make_call_method([values.W_Object, values.W_Object], simple=False,
                      name="<struct-field-mutator-method>")
    def call_with_extra_info(self, struct, val, env, cont, app):
        return self.mutator.mutate(struct, self.field, val, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-%s!>" % (self.mutator.struct_type_name(), self.field_name.variable_name())

class W_StructMutator(W_StructTypeProcedure):
    errorname = "struct-mutator"

    def get_arity(self, promote=False):
        return Arity.THREE

    def mutate(self, struct, field, val, env, cont, app=None):
        self = jit.promote(self)
        st = jit.promote(struct.struct_type())
        if st is None:
            raise SchemeException("%s got %s" % (self.tostring(), struct.tostring()))
        offset = st.get_offset(self.struct_type())
        if offset == -1:
            raise SchemeException("cannot reference an identifier before its definition")
        return struct.set_with_extra_info(field + offset, val, app, env, cont)

    @make_call_method([values.W_Object, values.W_Fixnum, values.W_Object],
                      simple=False, name="<struct-mutator-method>")
    def call_with_extra_info(self, struct, field, val, env, cont, app):
        return self.mutate(struct, field.value, val, env, cont, app)

    def tostring(self):
        return "#<procedure:%s-set!>" % self.struct_type_name()

class W_StructProperty(values.W_Object):
    errorname = "struct-type-property"
    _attrs_ = _immutable_fields_ = ["name", "w_guard", "supers", "can_imp"]
    def __init__(self, w_name, w_guard, supers=values.w_null, can_imp=False):
        self.name = w_name.utf8value
        self.w_guard = w_guard
        self.supers = values.from_list(supers)
        self.can_imp = can_imp

    @jit.elidable
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

w_prop_object_name = W_StructProperty(sym("prop:object-name"), values.w_false)
w_prop_authentic = W_StructProperty(sym("prop:authentic"), values.w_false)
w_prop_authentic_override = W_StructProperty(sym("prop:unsafe-authentic-override"), values.w_false)
w_prop_sealed = W_StructProperty(sym("prop:sealed"), values.w_false)
#FIXME: check if these propeties need guards or not
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

#FIXME: add guards for these checking for immutable
w_prop_output_port = W_StructProperty(sym("prop:output-port"), values.w_false)
w_prop_input_port = W_StructProperty(sym("prop:input-port"), values.w_false)

del sym

class W_StructPropertyPredicate(values.W_Procedure):
    errorname = "struct-property-predicate"
    _attrs_ = _immutable_fields_ = ["property"]
    import_from_mixin(SingleResultMixin)
    def __init__(self, prop):
        self.property = prop

    def get_arity(self, promote=False):
        return Arity.ONE

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
    _attrs_ = _immutable_fields_ = ["property"]
    import_from_mixin(SingleResultMixin)
    def __init__(self, prop):
        self.property = prop

    def get_arity(self, promote=False):
        return Arity.ONE

    @make_call_method([values.W_Object, default(values.W_Object, None)], simple=False)
    def call_with_extra_info(self, arg, fail, env, cont, app):
        from pycket.interpreter import return_value
        if isinstance(arg, W_StructType):
            w_val = arg.read_property_precise(self.property)
            if w_val is not None:
                return return_value(w_val, env, cont)
        elif arg.struct_type() is not None:
            return arg.get_prop(self.property, env, cont)
        elif fail is not None:
            if fail.iscallable():
                return fail.call_with_extra_info([], env, cont, app)
            return return_value(fail, env, cont)
        raise SchemeException("%s-accessor: expected %s? but got %s" %
                (self.property.name, self.property.name, arg.tostring()))

def struct2vector(struct, immutable=False):
    struct_desc = struct.struct_type().name.utf8value
    first_el = values.W_Symbol.make("struct:" + struct_desc)
    return values_vector.W_Vector.fromelements([first_el] + struct.vals(), immutable=immutable)
