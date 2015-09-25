
from pycket                    import values, values_struct
from pycket.base               import SingletonMeta
from pycket.cont               import call_cont, continuation, guarded_loop, label
from pycket.impersonators      import (
    ChaperoneMixin,
    ImpersonatorMixin,
    ProxyMixin,
    chaperone_reference_cont,
    check_chaperone_results,
    get_base_object,
    impersonate_reference_cont
)
from pycket.impersonators.map  import CachingMap, make_map_type
from rpython.rlib              import jit, unroll
from rpython.rlib.objectmodel  import import_from_mixin

def is_static_handler(func):
    return False #isinstance(func, values.W_Prim) # or isinstance(func, values.W_PromotableClosure)

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

def add_handler_field(map, handlers, name, val):
    if is_static_handler(val):
        new_map = map.add_static_attribute(name, val)
    else:
        new_map = map.add_dynamic_attribute(name)
        handlers.append(val)
    return new_map

# Representation of a struct that allows interposition of operations
# onto accessors/mutators
class W_InterposeStructBase(values_struct.W_RootStruct):
    import_from_mixin(ProxyMixin)

    # EMPTY_MAP = CachingMap.EMPTY
    EMPTY_MAP = make_map_type().EMPTY
    INFO_IDX = -1

    _immutable_fields_ = ['inner', 'handlers', 'overrides', 'struct_props', 'handler_map']

    @jit.unroll_safe
    def __init__(self, inner, overrides, handlers, prop_keys, prop_vals):
        from pycket.prims.struct_structinfo import struct_info
        assert isinstance(inner, values_struct.W_RootStruct)
        assert len(overrides) == len(handlers)
        assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)

        self.inner = inner
        self.handlers = []
        self.overrides = []

        handler_map = W_InterposeStructBase.EMPTY_MAP
        override_map = W_InterposeStructBase.EMPTY_MAP

        struct_props = None

        for i, op in enumerate(overrides):
            base = get_base_object(op)
            if isinstance(base, values_struct.W_StructFieldAccessor):
                idx = tag_accessor(base.field)
                if handlers[i] is not values.w_false:
                    handler_map = add_handler_field(handler_map, self.handlers, idx, handlers[i])
                    # handler_map = handler_map.add_dynamic_attribute(idx)
                    # self.handlers.append(handlers[i])
                if type(op) is not values_struct.W_StructFieldAccessor:
                    override_map = add_handler_field(override_map, self.overrides, idx, op)
                    # override_map = override_map.add_dynamic_attribute(idx)
                    # self.overrides.append(op)
            elif isinstance(base, values_struct.W_StructFieldMutator):
                idx = tag_mutator(base.field)
                if handlers[i] is not values.w_false:
                    handler_map = add_handler_field(handler_map, self.handlers, idx, handlers[i])
                if type(op) is not values_struct.W_StructFieldAccessor:
                    override_map = add_handler_field(override_map, self.overrides, idx, op)
            elif base is struct_info:
                idx = W_InterposeStructBase.INFO_IDX
                if handlers[i] is not values.w_false:
                    handler_map = add_handler_field(handler_map, self.handlers, idx, handlers[i])
                if type(op) is not values_struct.W_StructFieldAccessor:
                    override_map = add_handler_field(override_map, self.overrides, idx, op)
            elif isinstance(base, values_struct.W_StructPropertyAccessor):
                # TODO: Can we make use of existing property maps for this?
                if struct_props is None:
                    struct_props = {}
                struct_props[base] = (op, handlers[i])
            else:
                assert False

        self.init_properties(prop_keys, prop_vals)
        self.struct_props = struct_props
        self.override_map = override_map
        self.handler_map  = handler_map

    def post_ref_cont(self, interp, app, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, field, val, app, env, cont):
        raise NotImplementedError("abstract method")

    @jit.unroll_safe
    def is_non_interposing_chaperone(self):
        for tag in self.handler_map.iterkeys():
            if is_accessor(tag):
                return False
        return self.property_count() != 0

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
        if self.struct_props is None:
            return self.inner.get_prop(property, env, cont)
        op, interp = self.struct_props.get(property, (None, None))
        if op is None or interp is None:
            return self.inner.get_prop(property, env, cont)
        after = self.post_ref_cont(interp, None, env, cont)
        return op.call([self.inner], env, after)

    def get_struct_info(self, env, cont):
        handler = self.handler_map.lookup(W_InterposeStructBase.INFO_IDX, self.handlers)
        # idx = self.handler_map.get_index(W_InterposeStructBase.INFO_IDX)
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

