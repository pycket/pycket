#! /usr/bin/env python
# -*- coding: utf-8 -*-

from rpython.rlib import jit

def memoize(f):
    cache = {}
    @jit.elidable
    def wrapper(*val):
        lup = cache.get(val, None)
        if lup is None:
            lup = f(*val)
            cache[val] = lup
        return lup
    wrapper.__name__ = "Memoized(%s)" % f.__name__
    return wrapper

# Add a `make` method to a given class which memoizes constructor invocations.
def memoize_constructor(cls):
    setattr(cls, "make", staticmethod(memoize(cls)))
    return cls
