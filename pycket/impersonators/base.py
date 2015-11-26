
from pycket                   import values
from pycket.cont              import continuation
from pycket.error             import SchemeException
from pycket.hidden_classes    import make_map_type, make_caching_map_type
from pycket.prims.expose      import make_call_method
from rpython.rlib             import jit
from rpython.rlib.objectmodel import specialize

@jit.unroll_safe
def lookup_property(obj, prop):
    while obj.is_proxy():
        val = obj.get_property(prop)
        if val is not None:
            return val
        obj = obj.get_proxied()
    return None

class Counter(object):
    __attrs__ = ['value']
    def __init__(self):
        self.value = 0
    def inc(self):
        self.value += 1

def add_impersonator_counts(cls):
    cls.impersonators = Counter()
    cls.chaperones    = Counter()
    old_init = cls.__init__

    def counting_init(self, *args):
        old_init(self, *args)
        if self.is_impersonator():
            cls.impersonators.inc()
        elif self.is_chaperone():
            cls.chaperones.inc()
        else:
            assert False

    counting_init.__name__ = old_init.__name__
    cls.__init__ = counting_init
    return cls

@continuation
def check_chaperone_results(args, env, cont, vals):
    # We are allowed to receive more values than arguments to compare them to.
    # Additional ones are ignored for this checking routine.
    assert vals.num_values() >= args.num_values()
    return check_chaperone_results_loop(vals, args, 0, env, cont)

@jit.unroll_safe
def check_chaperone_results_loop(vals, args, idx, env, cont):
    from pycket.interpreter import return_multi_vals
    from pycket.prims.equal import equal_func_unroll_n, EqualInfo
    num_vals = args.num_values()
    while idx < num_vals and vals.get_value(idx) is None and args.get_value(idx) is None:
        idx += 1
    if idx >= num_vals:
        return return_multi_vals(vals, env, cont)
    info = EqualInfo.CHAPERONE_SINGLETON
    # XXX it would be best to store the parameter on the toplevel env and make
    # it changeable via a cmdline parameter to pycket-c
    unroll_n_times = 2 # XXX needs tuning
    return equal_func_unroll_n(vals.get_value(idx), args.get_value(idx), info, env,
            catch_equal_cont(vals, args, idx, env, cont), unroll_n_times)

@continuation
def catch_equal_cont(vals, args, idx, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    if val is values.w_false:
        raise SchemeException("Expecting original value or chaperone")
    return check_chaperone_results_loop(vals, args, idx + 1, env, cont)

@continuation
def impersonate_reference_cont(f, args, app, env, cont, _vals):
    old = _vals.get_all_values()
    return f.call_with_extra_info(args + old, env, cont, app)

@continuation
@jit.unroll_safe
def chaperone_reference_cont(f, args, app, env, cont, _vals):
    args_size = args.num_values()
    vals_size = _vals.num_values()
    all_args = [None] * (args_size + vals_size)
    idx = 0
    for i in range(args_size):
        all_args[idx] = args.get_value(i)
        idx += 1
    for i in range(vals_size):
        all_args[idx] = _vals.get_value(i)
        idx += 1
    return f.call_with_extra_info(all_args, env, check_chaperone_results(_vals, env, cont), app)

@jit.unroll_safe
def get_base_object(x):
    while x.is_proxy():
        x = x.get_base()
    return x

EMPTY_PROPERTY_MAP = make_caching_map_type("get_storage_index").EMPTY

@jit.unroll_safe
@specialize.argtype(1)
def make_property_map(prop_keys, map):
    if not prop_keys:
        return map
    for key in prop_keys:
        map = map.add_attribute(key)
    return map

@jit.unroll_safe
@specialize.argtype(1)
def make_specialized_property_map(prop_keys, map):
    if not prop_keys:
        return map
    for key in prop_keys:
        map = map.add_dynamic_attribute(key)
    return map

class ProxyMixin(object):

    EMPTY_MAP = make_map_type("get_property_index").EMPTY

    _immutable_fields_ = ['property_map', 'property_storage[*]', 'inner', 'base']

    @jit.unroll_safe
    def init_proxy(self, inner, prop_keys, prop_vals):
        assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)
        self.inner = inner
        self.base  = inner.get_base()

        self.property_map = make_property_map(prop_keys, ProxyMixin.EMPTY_MAP)
        self.property_storage = prop_vals[:] if prop_vals is not None else None # Ensure not resized

        if self.property_map is not ProxyMixin.EMPTY_MAP:
            assert self.property_map.storage_size() == len(prop_vals)

    def get_property_index(self, index):
        return self.property_storage[index]

    def iterprops(self):
        return self.property_map.iteritems()

    def property_count(self):
        return self.property_map.storage_size()

    def get_proxied(self):
        return self.inner

    def get_base(self):
        return self.base

    def is_proxy(self):
        return True

    def get_property(self, prop, default=None):
        return self.property_map.lookup(prop, self, default)

    def immutable(self):
        return self.base.immutable()

    def tostring(self):
        return self.base.tostring()

class InlineProxyMixin(object):

    _immutable_fields_ = ["inner", "base", "property_map"]

    def init_proxy(self, inner, property_map):
        self.inner = inner
        self.base  = inner.get_base()
        self.property_map = property_map

    def get_storage_index(self, idx):
        return self._get_list(idx)

    def get_proxied(self):
        return self.inner

    def get_base(self):
        return self.base

    def is_proxy(self):
        return True

    def get_property(self, prop, default=None):
        return self.property_map.lookup(prop, self, default=None)

    def immutable(self):
        return get_base_object(self.base).immutable()

    def tostring(self):
        return get_base_object(self.base).tostring()

class ChaperoneMixin(object):
    def is_chaperone(self):
        return True

class ImpersonatorMixin(object):
    def is_impersonator(self):
        return True

class W_ImpPropertyDescriptor(values.W_Object):
    errorname = "chaperone-property"
    _immutable_fields_ = ["name"]
    def __init__(self, name):
        self.name = name
    def tostring(self):
        return "#<chaperone-property>"

class W_ImpPropertyFunction(values.W_Procedure):
    _immutable_fields_ = ["descriptor"]
    def __init__(self, descriptor):
        self.descriptor = descriptor

class W_ImpPropertyPredicate(W_ImpPropertyFunction):
    errorname = "impersonator-property-predicate"

    @make_call_method([values.W_Object])
    def call(self, obj):
        return values.W_Bool.make(lookup_property(obj, self.descriptor) is not None)

class W_ImpPropertyAccessor(W_ImpPropertyFunction):
    errorname = "impersonator-property-accessor"

    @make_call_method([values.W_Object])
    def call(self, obj):
        return lookup_property(obj, self.descriptor)

w_impersonator_prop_application_mark = W_ImpPropertyDescriptor("impersonator-prop:application-mark")

