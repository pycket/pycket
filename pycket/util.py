#! /usr/bin/env python
# -*- coding: utf-8 -*-

import inspect
import string
from rpython.rlib             import streamio as sio

from rpython.rlib.listsort import make_timsort_class
from rpython.rlib        import jit, objectmodel, rtime
from rpython.rlib.unroll import unrolling_iterable

def os_check_env_var(var_str):
    from pycket.env import w_global_config
    return w_global_config.env_var_exists(var_str)

def os_get_env_var(var_str):
    from pycket.env import w_global_config
    return w_global_config.get_env_var(var_str)

##############################################
# Performance Region
##############################################

## this code is a port of cs/linklet/performance.ss

class PerfRegion(object):
    def __init__(self, l):
        self.label = l

    def __enter__(self):
        start_perf_region(self.label)

    def __exit__(self,a,b,c):
        finish_perf_region(self.label)

class PerfRegionCPS(PerfRegion):
    def __exit__(self,a,b,c):
        if a is None:
            # normal return, except that this is a CPS function and so the
            # finish_perf_region call is in the continuation
            # If `with` gave us access to the return value we could do this
            # automatically here, but it doesn't
            pass
        else:
            # exception, so we have to call finish_perf_region
            finish_perf_region(self.label)
            # re-raise the exception
            return None # using False here confuses rtyper

def start_perf_region(label):
    from pycket.prims.general import current_gc_time
    if os_check_env_var("PLT_LINKLET_TIMES"):
        linklet_perf.current_start_time.append(rtime.time())
        linklet_perf.current_gc_start_time.append(current_gc_time())

def finish_perf_region(label):
    from pycket.prims.general import current_gc_time
    if os_check_env_var("PLT_LINKLET_TIMES"):
        assert (len(linklet_perf.current_start_time) > 0)
        delta = rtime.time() - linklet_perf.current_start_time[-1]
        delta_gc = current_gc_time() - linklet_perf.current_gc_start_time[-1]
        table_add(linklet_perf.region_times, label, delta)
        table_add(linklet_perf.region_gc_times, label, delta_gc)
        table_add(linklet_perf.region_counts, label, 1)
        linklet_perf.current_start_time.pop()
        linklet_perf.current_gc_start_time.pop()
        for i in range(len(linklet_perf.current_start_time)):
            linklet_perf.current_start_time[i] += delta
            linklet_perf.current_gc_start_time[i] += delta_gc


class LinkletPerf(object):
    def __init__(self):
        self.region_times = {}
        self.region_gc_times = {}
        self.region_counts = {}
        self.current_start_time = []
        self.current_gc_start_time = []
        self.name_len = 0
        self.total = 0
        self.total_gc = 0
        self.total_len = 0
        self.total_gc_len = 0
        self.region_subs = {}
        self.region_gc_subs = {}
        self.categories = {"read" : ["fasl->s-exp", "s-exp->ast", "assign-convert-deserialize"],
                           "run" : ["instantiate-linklet" "outer"],
                           "startup" : ["expander-linklet", "json-load", "json-to-ast",
                                        "fasl-linklet", "thread-linklet", "regexp-linklet", "pycket-boot-linklet", "set-params"],
                           "compile" : ["compile-linklet", "compile-sexp-to-ast",
                                        "compile-normalize", "compile-assign-convert",
                           ]}

    def init(self):
        self.region_times["boot"] = rtime.clock()
        self.region_gc_times["boot"] = 0

    def report_time(self, level, label, n, gc_ht):
        counts = self.region_counts.get(label,0)
        assert not(isinstance(counts,str))
        if counts == 0:
            c = ""
        else:
            c = " ; %d times"%int(counts)
        self.report(level, label, n,
                    " [%s]"%(pad_left(str(int(gc_ht.get(label, 0))), self.total_gc_len)),
                    "ms", c)

    def report(self, level, label, n, nextra, units, extra):
        lprintf(";; %s%s%s  %s%s %s%s\n",
                (spaces(level*2),
                 pad_right(label,self.name_len),
                 spaces((3-level) * 2),
                 pad_left(str(n),self.total_len),
                 nextra,
                 units,
                 extra))

    def loop(self, ht, gc_ht, level):
        for label in ht:  # Fixme I can't sort
            self.report_time(level, label, int(1000*ht[label]), gc_ht)
            sub_ht = self.region_subs.get(label, None)
            sub_gc_ht = self.region_gc_subs.get(label, None)
            if sub_ht:
                self.loop(sub_ht, sub_gc_ht, level+1)

    def print_report(self):
        if os_check_env_var("PLT_LINKLET_TIMES"):
            total = 0
            total_gc = 0
            self.name_len = 0
            for k in self.region_times:
                self.name_len = max(self.name_len,len(k))
                total += self.region_times[k]
                total_gc += self.region_gc_times[k]
            self.total = int(1000*total)
            self.total_gc = int(total_gc)
            self.total_len = len(str(total))
            self.total_gc_len = len(str(self.total_gc))
            
            for cat in self.categories:
                t = sum_values(self.region_times, self.categories[cat], cat, self.region_subs)
                t_gc = sum_values(self.region_gc_times, self.categories[cat], cat, self.region_gc_subs)
                if not(0 == t) and not(0 == t_gc):
                    self.region_times[cat] = t
                    self.region_gc_times[cat] = t_gc

            self.loop(self.region_times, self.region_gc_times, 0)
            self.report(0, "total", self.total, " [%s]"%self.total_gc, "ms", "")

linklet_perf = LinkletPerf()

def second(l,i):
    x,y = l[i]
    return y

Sorter = make_timsort_class(getitem=second)

def ht_to_sorted_list(ht):
    l = []
    for k,v in enumerate(ht):
        l.append((k,v))
    s = Sorter(l)
    s.sort()
    return l


def table_add(t, l, v):
    t.setdefault(l,0)
    t[l] += v

def lprintf(fmt, args):
    from pycket.prims.logging import w_main_logger
    sio.fdopen_as_stream(2, "w", buffering=1).write(fmt%args)
    return

def spaces(n):
    return " "*n

def pad_left(v,w):
    s = v
    return spaces(max(0, w - (len(s)))) + (s)

def pad_right(v,w):
    s = str(v)
    return s + (spaces(max(0, w - (len(s)))))

def sum_values(ht, keys, key, subs):
    sub_ht = {}
    subs[key] = sub_ht
    sum = 0
    for sub_key in keys:
        v = ht.get(sub_key,0)
        sum += v
        sub_ht[sub_key] = v
        if sub_key in ht:
            del ht[sub_key]
    return sum


##############################################
# Debug Outputs
##############################################

def active_break():
    from pycket.env import w_global_config as glob

    if glob.is_debug_active():
        import pdb;pdb.set_trace()

def active_log(print_str, given_verbosity_level=0, debug=False, keyword=""):
    from pycket.env import w_global_config as glob

    if glob.is_debug_active():
        console_log(print_str, given_verbosity_level, debug, keyword)

def console_log_after_boot(print_str, given_verbosity_level=0, debug=False, keyword=""):
    from pycket.env import w_global_config as glob

    if glob.is_boot_completed():
        console_log(print_str, given_verbosity_level, debug, keyword)

def console_log(print_str, given_verbosity_level=0, debug=False, keyword=""):
    # use the given_verbosity_level argument to control at which level
    # of verbosity you want this log to appear. Default is -1.

    # i.e. For a console log with a given_verbosity_level = 5 , the
    # user has to give "--verbose 5" at the entry for it to
    # appear. Note that in that case all the logs with less than 5
    # verbosity_levels will also appear.

    # use the debug parameter to print irregardless of the
    # verbosity_levels. You can use this to print only the specific
    # logs you want. Don't provide "--verbose" flag at the entry, and
    # only the ones having debug=True will appear.

    # The keyword parameterizes the logging. Each console_log can have
    # a keyword associated with it. Instead of a numeric
    # verbosity_level, a string keyword can be supplied at the boot
    # with the --verbose flag, and the console_logs that have that
    # keyword will appear at runtime.

    # The keywords and numeric verbosity_levels are not mutually
    # exclusive. Both can be used together at the same time with
    # multiple --verbose flags:

    # $$ pycket --verbose 2 --verbose keyword

    # Mutliple keywords can be supplied too:

    # $$ pycket --verbose regexp --verbose prims --verbose 2

    from pycket.env import w_global_config as glob
    current_v_level = glob.get_config_val('verbose')

    if given_verbosity_level <= current_v_level or debug or glob.is_keyword_active(keyword):
        current_str = str(rtime.time()) # str will trim it to 2 decimals
        decimal = len(current_str.split(".")[1])
        # decimal cannot be 0, since we know rtime.time() will always
        # return float(...), so it can actually be either 1 or 2
        if decimal == 1:
            current_str += "0"

        standout_str = "" if not debug else " -------------------------------- "

        print("[%s] %s%s" % (current_str, standout_str, print_str))

##############################################
# MISC
##############################################

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
