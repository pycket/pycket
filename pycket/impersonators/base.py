
from pycket                          import values
from pycket.cont                     import continuation
from rpython.rlib                    import jit
from pycket.impersonators.properties import EMPTY_MAP

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

@jit.unroll_safe
def get_base_object(x):
    from pycket.impersonators.impersonators import W_InterposeStructBase
    if isinstance(x, W_InterposeStructBase):
        return x.base
    while x.is_proxy():
        x = x.get_proxied()
    return x

class ProxyMixin(object):

    _mixin_ = True
    _immutable_fields_ = ['properties', 'property_map', 'property_storage']

    def init_properties(self, prop_keys, prop_vals):
        if prop_keys is None:
            self.property_storage = None
            return

        map = EMPTY_MAP
        for key in prop_keys:
            map = map.add_attribute(key)

        assert map.storage_size() == len(prop_vals)
        self.property_map = map
        self.property_storage = prop_vals


    def get_proxied(self):
        return self.inner

    def is_proxy(self):
        return True

    def get_properties(self):
        return self.properties

    def immutable(self):
        return get_base_object(self.inner).immutable()

    def tostring(self):
        return get_base_object(self.inner).tostring()

class ChaperoneMixin(object):
    _mixin_ = True
    def is_chaperone(self):
        return True

class ImpersonatorMixin(object):
    _mixin_ = True
    def is_impersonator(self):
        return True
