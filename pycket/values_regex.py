from pycket.base import W_Object
from pycket.error import SchemeException
from pycket import values, values_string
from pycket import regexp

from rpython.rlib.rsre import rsre_core

CACHE = regexp.RegexpCache()



class W_AnyRegexp(W_Object):
    _immutable_fields_ = ["source"]
    errorname = "regexp"
    def __init__(self, source):
        self.source = source
        self.code = None

    def ensure_compiled(self):
        if self.code is None:
            code, flags, groupcount, groupindex, indexgroup, group_offsets = regexp.compile(CACHE, self.source, 0)
            self.code = code
            self.flags = flags
            self.groupcount = groupcount
            self.groupindex = groupindex
            self.indexgroup = indexgroup
            self.group_offsets = group_offsets

    def match_string(self, s):
        self.ensure_compiled()
        endpos = len(s)
        ctx = rsre_core.search(self.code, s)
        if not ctx:
            return None
        result = [ctx.group(i) for i in range(self.groupcount + 1)]
        return result

    def match_port(self, w_port):
        max_match = w_port._length_up_to_end()
        pos = w_port.tell()
        for i in range(max_match):
            w_port.seek(pos)
            s = w_port.read(i)
            result = self.match_string(s)
            if result:
                return result
        return None

    def eqv(self, other):
        return type(self) is type(other) and \
            (self.code == other.code if (self.code or other.code) \
            else self.source == other.source)



class W_Regexp(W_AnyRegexp): pass
class W_PRegexp(W_AnyRegexp): pass
class W_ByteRegexp(W_AnyRegexp): pass
class W_BytePRegexp(W_AnyRegexp): pass

