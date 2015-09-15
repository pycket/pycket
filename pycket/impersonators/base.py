
from rpython.rlib import jit

__all__ = ['get_base_object', 'ProxyMixin', 'ChaperoneMixin', 'ImpersonatorMixin', 'Counter']

class Counter(object):
    __attrs__ = ['value']
    def __init__(self):
        self.value = 0
    def inc(self):
        self.value += 1

@jit.unroll_safe
def get_base_object(x):
    from pycket.impersonators.impersonators import W_InterposeStructBase
    if isinstance(x, W_InterposeStructBase):
        return x.base
    while x.is_proxy():
        x = x.get_proxied()
    return x

class ProxyMixin(object):
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
    def is_chaperone(self):
        return True

class ImpersonatorMixin(object):
    def is_impersonator(self):
        return True
