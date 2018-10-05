#! /usr/bin/env python
# -*- coding: utf-8 -*-

import inspect
import string

from rpython.rlib        import jit, objectmodel, rtime
from rpython.rlib.unroll import unrolling_iterable

def active_break():
    from pycket.env import w_global_config as glob

    if glob.is_debug_active():
        import pdb;pdb.set_trace()

def active_log(print_str, given_verbosity_level=0, debug=False):
    from pycket.env import w_global_config as glob

    if glob.is_debug_active():
        console_log(print_str, given_verbosity_level, debug)

def console_log(print_str, given_verbosity_level=0, debug=False):
    # use the given_verbosity_level argument to control at which level
    # of verbosity you want this log to appear. Default is 0.

    # i.e. For a console log with a given_verbosity_level = 5 , the
    # user has to give "--verbose 5" at the entry for it to
    # appear. Note that in that case all the logs with less than 5
    # verbosity_levels will also appear.

    # use the debug parameter to print irregardless of the
    # verbosity_levels. You can use this to print only the specific
    # logs you want. Don't provide "--verbose" flag at the entry, and
    # only the ones having debug=True will appear.

    from pycket.env import w_global_config
    current_v_level = w_global_config.get_config_val('verbose')

    if given_verbosity_level <= current_v_level:
        current_str = str(rtime.time()) # str will trim it to 2 decimals
        decimal = len(current_str.split(".")[1])
        # decimal cannot be 0, since we know rtime.time() will always
        # return float(...), so it can actually be either 1 or 2
        if decimal == 1:
            current_str += "0"

        print("[%s] %s" % (current_str, print_str))

def snake_case(str):
    if not str:
        return str
    first = str[0]
    last = str[-1]
    body = str[1:-1]
    new = []
    for b in body:
        if b in string.uppercase:
            new.append("_" + b.lower())
        else:
            new.append(b)
    return first.lower() + "".join(new) + last.lower()

def memoize(f):
    cache = {}
    def wrapper(*val):
        if objectmodel.we_are_translated():
            return f(*val)
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
