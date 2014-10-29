from pycket.base import W_Object
from pycket.error import SchemeException
from pycket import values, values_string
from pycket import regexp

from rpython.rlib.rsre import rsre_core, rsre_char
from rpython.rlib import buffer

CACHE = regexp.RegexpCache()

class PortBuffer(buffer.Buffer):
    """match context for matching in a port."""
    # XXX how to extend to unicode?

    _immutable_fields_ = ["w_port"]

    def __init__(self, w_port):
        self.w_port = w_port
        l = w_port._length_up_to_end()
        assert l >= 0
        self.length = l
        self.read_so_far = []

    def getlength(self):
        return self.length

    def getitem(self, index):
        if index >= len(self.read_so_far):
            nchars = len(self.read_so_far) - index + 1
            self.read_so_far.extend(self.w_port.read(nchars))
        ch = self.read_so_far[index]
        return ch


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
        ctx = rsre_core.search(self.code, s)
        if not ctx:
            return None
        return _extract_result(ctx, self.groupcount)

    def match_port(self, w_port):
        self.ensure_compiled()
        buf = PortBuffer(w_port)
        ctx = rsre_core.BufMatchContext(self.code, buf, 0, buf.getlength(), 0)
        matched = rsre_core.search_context(ctx)
        if not matched:
            return None
        return _extract_result(ctx, self.groupcount)


    def eqv(self, other):
        if not isinstance(other, W_AnyRegexp):
            return False
        if type(self) is type(other):
            return self.source == other.source
        return False


@rsre_core.specializectx
def _extract_result(ctx, groupcount):
    result = []
    for i in range(groupcount + 1):
        start, end = ctx.span(i)
        assert 0 <= start
        assert 0 <= end
        result.append(''.join([chr(ctx.str(j)) for j in range(start, end)]))
    return result

class W_Regexp(W_AnyRegexp): pass
class W_PRegexp(W_AnyRegexp): pass
class W_ByteRegexp(W_AnyRegexp): pass
class W_BytePRegexp(W_AnyRegexp): pass

