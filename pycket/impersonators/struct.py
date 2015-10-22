
from pycket                    import values, values_struct
from pycket.base               import SingletonMeta, W_ProtoObject
from pycket.cont               import call_cont, continuation, guarded_loop, label
from pycket.impersonators      import (
    ChaperoneMixin,
    ImpersonatorMixin,
    ProxyMixin,
    W_ImpPropertyDescriptor,
    chaperone_reference_cont,
    check_chaperone_results,
    get_base_object,
    impersonate_reference_cont
)
from pycket.impersonators.map  import make_caching_map_type, make_map_type
from rpython.rlib              import jit, unroll
from rpython.rlib.objectmodel  import import_from_mixin, specialize

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

class Pair(W_ProtoObject):
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

@jit.unroll_safe
def impersonator_args(overrides, handlers, prop_keys, prop_vals):
    from pycket.prims.struct_structinfo import struct_info
    assert len(overrides) == len(handlers)

    _handlers = None
    struct_props = None
    struct_prop_keys = None
    struct_prop_vals = None

    handler_map = W_InterposeStructBase.EMPTY_MAP

    for i, op in enumerate(overrides):
        base = get_base_object(op)
        if isinstance(base, values_struct.W_StructFieldAccessor):
            if handlers[i] is not values.w_false:
                idx = tag_handler_accessor(base.field)
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, handlers[i])
            if type(op) is not values_struct.W_StructFieldAccessor:
                idx = tag_override_accessor(base.field)
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, op)
        elif isinstance(base, values_struct.W_StructFieldMutator):
            if handlers[i] is not values.w_false:
                idx = tag_handler_mutator(base.field)
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, handlers[i])
            if type(op) is not values_struct.W_StructFieldAccessor:
                idx = tag_override_mutator(base.field)
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, op)
        elif base is struct_info:
            if handlers[i] is not values.w_false:
                idx = W_InterposeStructBase.INFO_HANDLER_IDX
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, handlers[i])
            if type(op) is not values_struct.W_StructFieldAccessor:
                # XXX Can this happen?
                idx = W_InterposeStructBase.INFO_OVERRIDE_IDX
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, op)
        elif isinstance(base, values_struct.W_StructPropertyAccessor):
            if struct_prop_keys is None:
                struct_prop_keys = []
                struct_prop_vals = []
            struct_prop_keys.append(base)
            struct_prop_vals.append(Pair(op, handlers[i]))
        else:
            assert False

    _handlers = _handlers[:] if _handlers is not None else None
    keys = concat(prop_keys, struct_prop_keys)
    vals = concat(prop_vals, struct_prop_vals)

    return handler_map, _handlers, keys, vals

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
    args = impersonator_args(overrides, handlers, prop_keys, prop_vals)
    return cls(inner, *args)

# Representation of a struct that allows interposition of operations
# onto accessors/mutators
class W_InterposeStructBase(values_struct.W_RootStruct):
    import_from_mixin(ProxyMixin)

    EMPTY_MAP = make_caching_map_type().EMPTY
    INFO_HANDLER_IDX  = -1
    INFO_OVERRIDE_IDX = -2

    _immutable_fields_ = ['handler_map', 'handlers[*]']

    def __init__(self, inner, handler_map, handlers, prop_keys, prop_vals):
        self.handler_map = handler_map
        self.handlers = handlers
        self.init_proxy(inner, prop_keys, prop_vals)

    def post_ref_cont(self, interp, app, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, field, val, app, env, cont):
        raise NotImplementedError("abstract method")

    def is_non_interposing_chaperone(self):
        return not has_accessor(self.handler_map) and has_property_descriptor(self.property_map)

    def struct_type(self):
        return get_base_object(self.base).struct_type()

    def get_handler_accessor(self, field):
        idx = tag_handler_accessor(field)
        return self.handler_map.lookup(idx, self.handlers)

    def get_override_accessor(self, field):
        idx = tag_override_accessor(field)
        return self.handler_map.lookup(idx, self.handlers)

    def get_handler_mutator(self, field):
        idx = tag_handler_mutator(field)
        return self.handler_map.lookup(idx, self.handlers)

    def get_override_mutator(self, field):
        idx = tag_override_mutator(field)
        return self.handler_map.lookup(idx, self.handlers)

    @guarded_loop(enter_above_depth(5), always_use_labels=False)
    def ref_with_extra_info(self, field, app, env, cont):
        handler = self.get_handler_accessor(field)
        override = self.get_override_accessor(field)
        if handler is not None:
            cont = self.post_ref_cont(handler, app, env, cont)
        if override is not None:
            return override.call_with_extra_info([self.inner], env, cont, app)
        return self.inner.ref_with_extra_info(field, app, env, cont)

    @guarded_loop(enter_above_depth(5), always_use_labels=False)
    def set_with_extra_info(self, field, val, app, env, cont):
        handler = self.get_handler_mutator(field)
        if handler is None:
            return self.inner.set_with_extra_info(field, val, app, env, cont)
        override = self.get_override_mutator(field)
        after = self.post_set_cont(override, field, val, app, env, cont)
        return handler.call_with_extra_info([self, val], env, after, app)

    @guarded_loop(enter_above_depth(5), always_use_labels=False)
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
        handler = self.handler_map.lookup(W_InterposeStructBase.INFO_HANDLER_IDX, self.handlers)
        if handler is not None:
            cont = call_cont(handler, env, cont)
        return self.inner.get_struct_info(env, cont)

    def get_arity(self):
        return get_base_object(self.base).get_arity()

    # FIXME: This is incorrect
    def vals(self):
        return self.inner.vals()

# Need to add checks that we are only impersonating mutable fields
class W_ImpStruct(W_InterposeStructBase):
    import_from_mixin(ImpersonatorMixin)

    def post_ref_cont(self, interp, app, env, cont):
        return impersonate_reference_cont(interp, [self], app, env, cont)

    def post_set_cont(self, op, field, val, app, env, cont):
        return imp_struct_set_cont(self.inner, op, field, app, env, cont)

class W_ChpStruct(W_InterposeStructBase):
    import_from_mixin(ChaperoneMixin)

    def post_ref_cont(self, interp, app, env, cont):
        return chaperone_reference_cont(interp, [self], app, env, cont)

    def post_set_cont(self, op, field, val, app, env, cont):
        return check_chaperone_results([val], env,
                imp_struct_set_cont(self.inner, op, field, app, env, cont))

class W_InterposeStructStack(values_struct.W_RootStruct):
    import_from_mixin(ProxyMixin)

    def __init__(self, handlers, handler_map, handler_stack):
        pass

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

