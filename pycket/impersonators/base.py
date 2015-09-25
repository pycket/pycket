
from pycket                   import values
from pycket.cont              import continuation
from pycket.error             import SchemeException
from pycket.impersonators.map import make_map_type
from pycket.prims.expose      import make_call_method
from rpython.rlib             import jit

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
    assert vals.num_values() >= len(args)
    return check_chaperone_results_loop(vals, args, 0, env, cont)

@jit.unroll_safe
def check_chaperone_results_loop(vals, args, idx, env, cont):
    from pycket.interpreter import return_multi_vals
    from pycket.prims.equal import equal_func_unroll_n, EqualInfo
    while idx < len(args) and vals.get_value(idx) is None and args[idx] is None:
        idx += 1
    if idx >= len(args):
        return return_multi_vals(vals, env, cont)
    info = EqualInfo.CHAPERONE_SINGLETON
    # XXX it would be best to store the parameter on the toplevel env and make
    # it changeable via a cmdline parameter to pycket-c
    unroll_n_times = 2 # XXX needs tuning
    return equal_func_unroll_n(vals.get_value(idx), args[idx], info, env,
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
def chaperone_reference_cont(f, args, app, env, cont, _vals):
    old = _vals.get_all_values()
    return f.call_with_extra_info(args + old, env, check_chaperone_results(old, env, cont), app)

@jit.unroll_safe
def get_base_object(x):
    from pycket.impersonators.struct import W_InterposeStructBase
    while x.is_proxy():
        x = x.get_proxied()
    return x

class ProxyMixin(object):

    EMPTY_MAP = make_map_type().EMPTY

    _immutable_fields_ = ['property_map', 'property_storage[*]']

    @jit.unroll_safe
    def init_properties(self, prop_keys, prop_vals):
        if prop_keys is None:
            self.property_map = ProxyMixin.EMPTY_MAP
            self.property_storage = None
            return

        map = ProxyMixin.EMPTY_MAP
        for key in prop_keys:
            map = map.add_attribute(key)

        assert map.storage_size() == len(prop_vals)
        self.property_map = map
        self.property_storage = prop_vals

    def property_count(self):
        return self.property_map.storage_size()

    def get_proxied(self):
        return self.inner

    def is_proxy(self):
        return True

    def get_property(self, prop):
        return self.property_map.lookup(prop, self.property_storage)

    def immutable(self):
        return get_base_object(self.inner).immutable()

    def tostring(self):
        return get_base_object(self.inner).tostring()

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

