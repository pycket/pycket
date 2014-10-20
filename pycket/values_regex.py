from pycket.base import W_Object
from pycket.error import SchemeException
from pycket import values, values_string
from pycket import regexp

from rpython.rlib.rsre import rsre_core

CACHE = regexp.RegexpCache()



class W_AnyRegexp(W_Object):
    _immutable_fields_ = ["source"]
    errorname = "regexp"
    def __init__(self, source, flags=0):
        self.source = source
        code, flags, groupcount, groupindex, indexgroup, group_offsets = regexp.compile(CACHE, source, flags)
        self.code = code
        self.flags = flags
        self.groupcount = groupcount
        self.groupindex = groupindex
        self.indexgroup = indexgroup
        self.group_offsets = group_offsets

    def match_string(self, s):
        endpos = len(s)
        ctx = rsre_core.search(self.code, s)
        if not ctx:
            return None
        result = [ctx.group(i) for i in range(self.groupcount + 1)]
        return result


class W_Regexp(W_AnyRegexp): pass
class W_PRegexp(W_AnyRegexp): pass
class W_ByteRegexp(W_AnyRegexp): pass
class W_BytePRegexp(W_AnyRegexp): pass

