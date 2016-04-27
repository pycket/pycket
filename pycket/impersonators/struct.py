
from pycket                    import values, values_struct
from pycket.base               import SingletonMeta, W_Object
from pycket.cont               import call_cont, continuation, guarded_loop, label
from pycket.impersonators      import (
    ChaperoneMixin,
    ImpersonatorMixin,
    ProxyMixin,
    W_ImpPropertyDescriptor,
    chaperone_reference_cont,
    check_chaperone_results,
    get_base_object,
    make_property_map,
    impersonate_reference_cont
)
from pycket.hidden_classes     import make_caching_map_type, make_map_type, make_composite_map_type
from pycket.small_list         import inline_small_list
from rpython.rlib              import jit, unroll
from rpython.rlib.objectmodel  import import_from_mixin, specialize, always_inline

def is_static_handler(func):
    return isinstance(func, values.W_Prim) or isinstance(func, values.W_PromotableClosure)

# Check if a proxied struct has more than n levels to descend through
def enter_above_depth(n):
    @jit.unroll_safe
    def above_threshold(self, field, *args):
        if jit.we_are_jitted():
            return True
        for _ in range(n):
            if not isinstance(self, W_InterposeStructBase):
                return False
            self = self.inner
        return True
    return above_threshold

# Tag the lower two bits to distinguish the four cases.
# The lowest order bit distinguishes between accessors and mutators,
# and the second lowest bit distingiushes between handlers and overrides.
HANDLER_ACCESSOR_TAG  = 0b00
HANDLER_MUTATOR_TAG   = 0b01
OVERRIDE_ACCESSOR_TAG = 0b10
OVERRIDE_MUTATOR_TAG  = 0b11
TAG_BITS = 2

# Helper functions used for tagging accessor and mutator indices
# so we can use a single map to store both without segregating them
def tag_handler_accessor(idx):
    assert idx >= 0
    return (idx << TAG_BITS) | HANDLER_ACCESSOR_TAG

def tag_handler_mutator(idx):
    assert idx >= 0
    return (idx << TAG_BITS) | HANDLER_MUTATOR_TAG

def tag_override_accessor(idx):
    assert idx >= 0
    return (idx << TAG_BITS) | OVERRIDE_ACCESSOR_TAG

def tag_override_mutator(idx):
    assert idx >= 0
    return (idx << TAG_BITS) | OVERRIDE_MUTATOR_TAG

def is_accessor(key):
    return key >= 0 and (key & 0b01) == 0

def is_mutator(key):
    return key >= 0 and (key & 0b01) == 1

def is_handler(key):
    return key >= 0 and (key & 0b10) == 0

def is_override(key):
    return key >= 0 and (key & 0b10) == 1

def add_handler_field(map, handler_array, name, val):
    if is_static_handler(val):
        new_map = map.add_static_attribute(name, val)
    else:
        if handler_array is None:
            handler_array = []
        new_map = map.add_dynamic_attribute(name)
        handler_array.append(val)
    return handler_array, new_map

class Pair(W_Object):
    _attrs_ = ['fst', 'snd']
    _immutable_fields_ = ['fst', 'snd']

    def __init__(self, fst, snd):
        self.fst = fst
        self.snd = snd

    def __iter__(self):
        yield self.fst
        yield self.snd

    def __len__(self):
        return 2

    def __getitem__(self, idx):
        if idx == 0:
            return self.fst
        if idx == 1:
            return self.snd
        raise IndexError("Pair: index %s out of range" % idx)

NONE_PAIR = Pair(None, None)

CompositeMap = make_composite_map_type(shared_storage=True)

@jit.unroll_safe
def impersonator_args(struct, overrides, handlers, prop_keys, prop_vals):
    from pycket.prims.struct_structinfo import struct_info
    assert len(overrides) == len(handlers)

    _handlers = None
    struct_props = None
    struct_prop_keys = None
    struct_prop_vals = None

    handler_map = W_InterposeStructBase.EMPTY_HANDLER_MAP

    struct_type = jit.promote(struct.struct_type())
    for i, op in enumerate(overrides):
        base = get_base_object(op)
        if isinstance(base, values_struct.W_StructFieldAccessor):
            if handlers[i] is not values.w_false:
                field = base.get_absolute_index(struct_type)
                idx = tag_handler_accessor(field)
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, handlers[i])
            if type(op) is not values_struct.W_StructFieldAccessor:
                field = base.get_absolute_index(struct_type)
                idx = tag_override_accessor(field)
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, op)
        elif isinstance(base, values_struct.W_StructFieldMutator):
            if handlers[i] is not values.w_false:
                field = base.get_absolute_index(struct_type)
                idx = tag_handler_mutator(field)
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, handlers[i])
            if type(op) is not values_struct.W_StructFieldAccessor:
                field = base.get_absolute_index(struct_type)
                idx = tag_override_mutator(field)
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, op)
        elif base is struct_info and handlers[i] is not values.w_false:
            idx = INFO_HANDLER_IDX
            _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, handlers[i])
        elif isinstance(base, values_struct.W_StructPropertyAccessor):
            if struct_prop_keys is None:
                struct_prop_keys = []
                struct_prop_vals = []
            struct_prop_keys.append(base)
            struct_prop_vals.append(Pair(op, handlers[i]))
        else:
            assert False

    EMPTY = W_InterposeStructBase.EMPTY_PROPERTY_MAP
    property_map = make_property_map(struct_prop_keys, make_property_map(prop_keys, EMPTY))
    vals = concat(_handlers, concat(prop_vals, struct_prop_vals))
    storage = vals[:] if vals is not None else None

    map = CompositeMap.instantiate(handler_map, property_map)

    return map, storage

def concat(l1, l2):
    """ Join two possibly None lists """
    if l1 is None:
        return l2
    if l2 is None:
        return l1
    return l1 + l2

@jit.elidable
def has_accessor(map):
    for tag in map.iterkeys():
        if is_handler(tag) and is_accessor(tag):
            return True
    return False

@jit.elidable
def has_property_descriptor(map):
    for key in map.iterkeys():
        if type(key) is W_ImpPropertyDescriptor:
            return True
    return False

@specialize.arg(0)
def make_struct_proxy(cls, inner, overrides, handlers, prop_keys, prop_vals):
    assert isinstance(inner, values_struct.W_RootStruct)
    assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)
    map, _handlers = impersonator_args(inner, overrides, handlers, prop_keys, prop_vals)
    return cls.make(_handlers, inner, map)

INFO_HANDLER_IDX  = -1
INFO_OVERRIDE_IDX = -2

# Representation of a struct that allows interposition of operations
# onto accessors/mutators
class W_InterposeStructBase(values_struct.W_RootStruct):

    EMPTY_HANDLER_MAP = make_caching_map_type("get_storage_index").EMPTY
    EMPTY_PROPERTY_MAP = make_map_type("get_storage_index").EMPTY

    _attrs_ = ['inner', 'base', 'map']
    _immutable_fields_ = ['inner', 'base', 'map']

    def __init__(self, inner, map):
        self.inner = inner
        self.map = map

        if isinstance(inner, W_InterposeStructBase) and map is inner.map:
            self.base = inner.base
        else:
            self.base = inner

    def get_storage_index(self, idx):
        return self._get_list(idx)

    def get_proxied(self):
        return self.inner

    def get_base(self):
        return self.base

    def is_proxy(self):
        return True

    def get_property(self, prop, default=None):
        return self.map.lookup_property(prop, self, default=default)

    def immutable(self):
        return get_base_object(self.base).immutable()

    def tostring(self):
        return get_base_object(self.base).tostring()

    def post_ref_cont(self, interp, app, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, field, val, app, env, cont):
        raise NotImplementedError("abstract method")

    def is_non_interposing_chaperone(self):
        map = jit.promote(self.map)
        return (not has_accessor(map.handlers) and
                has_property_descriptor(map.properties))

    def struct_type(self):
        return get_base_object(self.base).struct_type()

    def get_handler_accessor(self, field):
        idx = tag_handler_accessor(field)
        return self.map.lookup_handler(idx, self)

    def get_override_accessor(self, field):
        idx = tag_override_accessor(field)
        return self.map.lookup_handler(idx, self)

    def get_handler_mutator(self, field):
        idx = tag_handler_mutator(field)
        return self.map.lookup_handler(idx, self)

    def get_override_mutator(self, field):
        idx = tag_override_mutator(field)
        return self.map.lookup_handler(idx, self)

    @guarded_loop(enter_above_depth(5), always_use_labels=False)
    def ref_with_extra_info(self, field, app, env, cont):
        handler = self.get_handler_accessor(field)
        override = self.get_override_accessor(field)
        if handler is None and override is None:
            return self.base.ref_with_extra_info(field, app, env, cont)
        if handler is not None:
            cont = self.post_ref_cont(handler, app, env, cont)
        if override is not None:
            return override.call_with_extra_info([self.inner], env, cont, app)
        return self.inner.ref_with_extra_info(field, app, env, cont)

    @guarded_loop(enter_above_depth(5), always_use_labels=False)
    def set_with_extra_info(self, field, val, app, env, cont):
        handler = self.get_handler_mutator(field)
        override = self.get_override_mutator(field)
        if handler is None and override is None:
            return self.base.set_with_extra_info(field, val, app, env, cont)
        if handler is None:
            return self.inner.set_with_extra_info(field, val, app, env, cont)
        after = self.post_set_cont(override, field, val, app, env, cont)
        return handler.call_with_extra_info([self, val], env, after, app)

    def get_prop(self, property, env, cont):
        pair = self.get_property(property, default=NONE_PAIR)
        # Struct properties can only be associated with Pairs which contain both
        # the override and handler for the property
        assert type(pair) is Pair
        op, interp = pair
        if op is None or interp is None:
            return self.inner.get_prop(property, env, cont)
        after = self.post_ref_cont(interp, None, env, cont)
        return op.call([self.inner], env, after)

    @guarded_loop(enter_above_depth(5), always_use_labels=False)
    def get_struct_info(self, env, cont):
        handler = self.map.lookup_handler(INFO_HANDLER_IDX, self)
        if handler is not None:
            cont = call_cont(handler, env, cont)
        return self.inner.get_struct_info(env, cont)

    def get_arity(self):
        return get_base_object(self.base).get_arity()

    # FIXME: This is incorrect
    def vals(self):
        base = get_base_object(self.base)
        assert isinstance(base, values_struct.W_RootStruct)
        return base.vals()

# Need to add checks that we are only impersonating mutable fields
@inline_small_list(immutable=True, unbox_num=True)
class W_ImpStruct(W_InterposeStructBase):
    import_from_mixin(ImpersonatorMixin)

    def post_ref_cont(self, interp, app, env, cont):
        return impersonate_reference_cont(interp, [self], app, env, cont)

    def post_set_cont(self, op, field, val, app, env, cont):
        return imp_struct_set_cont(self.inner, op, field, app, env, cont)

@inline_small_list(immutable=True, unbox_num=True)
class W_ChpStruct(W_InterposeStructBase):
    import_from_mixin(ChaperoneMixin)

    def post_ref_cont(self, interp, app, env, cont):
        args = values.Values.make1(self)
        return chaperone_reference_cont(interp, args, app, env, cont)

    def post_set_cont(self, op, field, val, app, env, cont):
        val = values.Values.make1(val)
        return check_chaperone_results(val, env,
                imp_struct_set_cont(self.inner, op, field, app, env, cont))

class W_InterposeStructStack(values_struct.W_RootStruct):
    import_from_mixin(ProxyMixin)

    _immutable_fields_ = ['handlers[*]', 'handler_map']

    def __init__(self, inner, handlers, handler_map):
        self.handlers = handlers
        self.handler_map = handler_map
        self.init_proxy(inner, prop_keys, prop_vals)

    def is_non_interposing_chaperone(self):
        return (not has_accessor(self.handler_map) and
                has_property_descriptor(self.property_map))

    def post_ref_cont(self, interp, app, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, field, val, app, env, cont):
        raise NotImplementedError("abstract method")

    def ref_with_extra_info(self, field, app, env, cont):
        pass

    def set_with_extra_info(self, field, val, app, env, cont):
        pass

    # @guarded_loop(enter_above_depth(5), always_use_labels=False)
    def get_prop(self, property, env, cont):
        pair = self.get_property(property, NONE_PAIR)
        # Struct properties can only be associated with Pairs which contain both
        # the override and handler for the property
        assert type(pair) is Pair
        op, interp = pair
        if op is None or interp is None:
            return self.inner.get_prop(property, env, cont)
        after = self.post_ref_cont(interp, None, env, cont)
        return op.call([self.inner], env, after)

    @guarded_loop(enter_above_depth(5), always_use_labels=False)
    def get_struct_info(self, env, cont):
        handler = self.handler_map.lookup(INFO_HANDLER_IDX, self.handlers)
        if handler is not None:
            cont = call_cont(handler, env, cont)
        return self.inner.get_struct_info(env, cont)

    def get_arity(self):
        return get_base_object(self.base).get_arity()

    # FIXME: This is incorrect
    def vals(self):
        return self.inner.vals()

# Are we dealing with a struct accessor/mutator/propert accessor or a
# chaperone/impersonator thereof.
def valid_struct_proc(x):
    v = get_base_object(x)
    return (isinstance(v, values_struct.W_StructFieldAccessor) or
            isinstance(v, values_struct.W_StructFieldMutator) or
            isinstance(v, values_struct.W_StructPropertyAccessor))

@continuation
def imp_struct_set_cont(orig_struct, setter, field, app, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    if setter is values.w_false:
        return orig_struct.set_with_extra_info(field, val, app, env, cont)
    return setter.call_with_extra_info([orig_struct, val], env, cont, app)

