#! /usr/bin/env python
# -*- coding: utf-8 -*-

import inspect

from functools           import wraps
from rpython.rlib        import jit, objectmodel
from rpython.rlib.unroll import unrolling_iterable

def memoize(f):
    cache = {}
    @wraps(f)
    def wrapper(*val):
        if objectmodel.we_are_translated():
            return f(*val)
        try:
            result = cache[val]
        except KeyError:
            cache[val] = result = f(*val)
        return result
    return wrapper

# Add a `make` method to a given class which memoizes constructor invocations.
def memoize_constructor(cls):
    setattr(cls, "make", staticmethod(memoize(cls)))
    return cls

def strip_immutable_field_name(str):
    return str.replace("[*]", "")

def add_copy_method(copy_method="copy"):
    def wrapper(cls):
        """
        This attempts to produce a method which will copy the immutable contents of
        a given data type from the '_immutable_fields_' annotation of the class.

        The methods employed here will only work for certain types of class
        specifications (i.e. only works if all the desired fields are present in the
        '_immutable_fields_' annotation of the class definition).
        The mutable fields of the class must be copied separately as well.
        """
        field_names = []

        for base in inspect.getmro(cls):
            if base is object:
                continue
            fields = getattr(base, "_immutable_fields_", [])
            field_names.extend(map(strip_immutable_field_name, fields))

        field_names = unrolling_iterable(field_names)

        def copy(self):
            result = objectmodel.instantiate(cls)
            for attr in field_names:
                val = getattr(self, attr)
                setattr(result, attr, val)
            return result
        setattr(cls, copy_method, copy)
        return cls
    return wrapper

