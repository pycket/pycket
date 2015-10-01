
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
from pycket.impersonators.map  import CachingMap, make_map_type
from rpython.rlib              import jit, unroll
from rpython.rlib.objectmodel  import import_from_mixin

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

# Helper functions used for tagging accessor and mutator indices
# so we can use a single map to store both without segregating them
def tag_accessor(idx):
    assert idx >= 0
    return idx << 1

def tag_mutator(idx):
    assert idx >= 0
    return (idx << 1) | 0x1

def is_accessor(key):
    return key >= 0 and (key & 0x1) == 0

def is_mutator(key):
    return key >= 0 and (key & 0x1) == 1

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
def impersonator_args(overrides, handlers):
    from pycket.prims.struct_structinfo import struct_info
    assert len(overrides) == len(handlers)

    _handlers = None
    _overrides = None
    struct_props = None
    prop_keys = None
    prop_vals = None

    handler_map = W_InterposeStructBase.EMPTY_MAP
    override_map = W_InterposeStructBase.EMPTY_MAP

    for i, op in enumerate(overrides):
        base = get_base_object(op)
        if isinstance(base, values_struct.W_StructFieldAccessor):
            idx = tag_accessor(base.field)
            if handlers[i] is not values.w_false:
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, handlers[i])
            if type(op) is not values_struct.W_StructFieldAccessor:
                _overrides, override_map = add_handler_field(override_map, _overrides, idx, op)
        elif isinstance(base, values_struct.W_StructFieldMutator):
            idx = tag_mutator(base.field)
            if handlers[i] is not values.w_false:
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, handlers[i])
            if type(op) is not values_struct.W_StructFieldAccessor:
                _overrides, override_map = add_handler_field(override_map, _overrides, idx, op)
        elif base is struct_info:
            idx = W_InterposeStructBase.INFO_IDX
            if handlers[i] is not values.w_false:
                _handlers, handler_map = add_handler_field(handler_map, _handlers, idx, handlers[i])
            if type(op) is not values_struct.W_StructFieldAccessor:
                _overrides, override_map = add_handler_field(override_map, _overrides, idx, op)
        elif isinstance(base, values_struct.W_StructPropertyAccessor):
            if prop_keys is None:
                prop_keys = []
                prop_vals = []
            prop_keys.append(base)
            prop_vals.append(Pair(op, handlers[i]))
        else:
            assert False

    _handlers = _handlers[:] if _handlers is not None else None
    _overrides = _overrides[:] if _overrides is not None else None

    return handler_map, _handlers, override_map, _overrides, prop_keys, prop_vals

def concat(l1, l2):
    """ Join two possibly None lists """
    if l1 is None:
        return l2
    if l2 is None:
        return l1
    return l1 + l2

# Representation of a struct that allows interposition of operations
# onto accessors/mutators
class W_InterposeStructBase(values_struct.W_RootStruct):
    import_from_mixin(ProxyMixin)

    EMPTY_MAP = CachingMap.EMPTY
    INFO_IDX = -1

    _immutable_fields_ = ['inner', 'handler_map', 'handlers[*]', 'override_map', 'overrides[*]', 'struct_props']

    def __init__(self, inner, overrides, handlers, prop_keys, prop_vals):
        assert isinstance(inner, values_struct.W_RootStruct)
        assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)

        self.inner = inner

        self.handler_map, self.handlers, \
        self.override_map, self.overrides, \
        struct_prop_keys, struct_prop_vals = impersonator_args(overrides, handlers)

        prop_keys = concat(prop_keys, struct_prop_keys)
        prop_vals = concat(prop_vals, struct_prop_vals)

        self.init_properties(prop_keys, prop_vals)

    def post_ref_cont(self, interp, app, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, field, val, app, env, cont):
        raise NotImplementedError("abstract method")

    @jit.unroll_safe
    def is_non_interposing_chaperone(self):
        for tag in self.handler_map.iterkeys():
            if is_accessor(tag):
                return False
        for k, _ in self.iterprops():
            if type(k) is W_ImpPropertyDescriptor:
                return True
        return False

    def struct_type(self):
        return get_base_object(self.inner).struct_type()

    @guarded_loop(enter_above_depth(5))
    def ref_with_extra_info(self, field, app, env, cont):
        tag = tag_accessor(field)
        handler = self.handler_map.lookup(tag, self.handlers)
        override = self.override_map.lookup(tag, self.overrides)
        if handler is not None:
            cont = self.post_ref_cont(handler, app, env, cont)
        if override is not None:
            return override.call_with_extra_info([self.inner], env, cont, app)
        return self.inner.ref_with_extra_info(field, app, env, cont)

    @guarded_loop(enter_above_depth(5))
    def set_with_extra_info(self, field, val, app, env, cont):
        tag = tag_mutator(field)
        handler = self.handler_map.lookup(tag, self.handlers)
        if handler is None:
            return self.inner.set_with_extra_info(field, val, app, env, cont)
        override = self.override_map.lookup(tag, self.overrides, values.w_false)
        after = self.post_set_cont(override, field, val, app, env, cont)
        return handler.call_with_extra_info([self, val], env, after, app)

    @label
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

    def get_struct_info(self, env, cont):
        handler = self.handler_map.lookup(W_InterposeStructBase.INFO_IDX, self.handlers)
        if handler is not None:
            cont = call_cont(handler, env, cont)
        return self.inner.get_struct_info(env, cont)

    def get_arity(self):
        return get_base_object(self.inner).get_arity()

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

