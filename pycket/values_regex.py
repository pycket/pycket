from pycket.base  import W_Object
from pycket.error import SchemeException
from pycket       import values, values_string
from pycket       import regexp

from rpython.rlib.rsre        import rsre_core, rsre_char, rsre_re
from rpython.rlib             import buffer, jit, rstring
from rpython.rlib.objectmodel import specialize
import sys

CACHE = regexp.RegexpCache()

class PortBuffer(buffer.Buffer):
    """match context for matching in a port."""
    # XXX how to extend to unicode?

    _immutable_ = True

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
            self.read_so_far.extend(list(self.w_port.read(nchars)))
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

    @specialize.argtype(1)
    def make_ctx(self, s, start, end):
        self.ensure_compiled()
        start, end = rsre_core._adjust(start, end, len(s))
        if isinstance(s, unicode):
            return rsre_core.UnicodeMatchContext(self.code, s, start, end, self.flags)
        return rsre_core.StrMatchContext(self.code, s, start, end, self.flags)

    @specialize.argtype(1)
    def match_string(self, s, start=0, end=sys.maxint):
        ctx = self.make_ctx(s, start, end)
        if not rsre_core.search_context(ctx):
            return None
        return _extract_result(ctx, self.groupcount)

    @specialize.call_location()
    def _match_all_strings(self, extract, s, start, end):
        ctx = self.make_ctx(s, start, end)
        matchlist = []
        while ctx.match_start <= ctx.end:
            if not rsre_core.search_context(ctx):
                break
            match = extract(ctx, self.groupcount)
            matchlist.append(match)
            # Advance starting point for next match
            no_progress = (ctx.match_start == ctx.match_end)
            ctx.reset(ctx.match_end + no_progress)
        return matchlist

    @specialize.argtype(1)
    def match_all_strings(self, s, start=0, end=sys.maxint):
        return self._match_all_strings(_extract_result, s, start, end)

    @specialize.argtype(1)
    def match_all_string_positions(self, s, start=0, end=sys.maxint):
        return self._match_all_strings(_extract_spans, s, start, end)

    @specialize.argtype(1)
    def match_string_positions(self, s, start=0, end=sys.maxint):
        ctx = self.make_ctx(s, start, end)
        if not rsre_core.search_context(ctx):
            return None
        return _extract_spans(ctx, self.groupcount)

    def match_port_positions(self, w_port):
        raise NotImplementedError("match_port_position: not yet implemented")

    def match_port(self, w_port, start=0, end=sys.maxint):
        self.ensure_compiled()
        if isinstance(w_port, values.W_StringInputPort):
            # fast path
            ctx = rsre_core.search(self.code, w_port.str, start=w_port.ptr)
            if not ctx:
                return None
            start, end = ctx.span(0) # the whole match
            w_port.ptr = end
            return _extract_result(ctx, self.groupcount)
        buf = PortBuffer(w_port)
        end = min(end, buf.getlength())
        ctx = rsre_core.BufMatchContext(self.code, buf, 0, end, 0)
        matched = rsre_core.search_context(ctx)
        if not matched:
            return None
        return _extract_result(ctx, self.groupcount)

    def equal(self, other):
        if not isinstance(other, W_AnyRegexp):
            return False
        if type(self) is type(other):
            return self.source == other.source
        return False

    def tostring(self):
        return '#px"%s"' % self.source

@rsre_core.specializectx
@jit.unroll_safe
def _extract_spans(ctx, groupcount):
    return [ctx.span(i) for i in range(groupcount + 1)]

@rsre_core.specializectx
@jit.unroll_safe
def _extract_result(ctx, groupcount):
    result = []
    for i in range(groupcount + 1):
        start, end = ctx.span(i)
        if start == -1 and end == -1:
            result.append(None)
        else:
            assert 0 <= start
            assert 0 <= end
            result.append(_getslice(ctx, start, end))
    return result

@rsre_core.specializectx
def _getslice(ctx, start, end):
    if isinstance(ctx, rsre_core.StrMatchContext):
        return ctx._string[start:end]
    elif isinstance(ctx, rsre_core.UnicodeMatchContext):
        return ctx._unicodestr[start:end]
    else:
        return ''.join([chr(ctx.str(j)) for j in range(start, end)])

class W_Regexp(W_AnyRegexp): pass
class W_PRegexp(W_AnyRegexp): pass
class W_ByteRegexp(W_AnyRegexp): pass
class W_BytePRegexp(W_AnyRegexp): pass

class ReplacementOption(object):
    _attrs_ = []
    settled = True

    def replace(self, matches):
        raise NotImplementedError("abstract base class")

class StringLiteral(ReplacementOption):
    _immutable_fields_ = ['string']
    def __init__(self, string):
        self.string = string

    def replace(self, matches):
        return self.string

    def __repr__(self):
        return "StringLiteral(%r)" % self.string

class PositionalArg(ReplacementOption):
    _immutable_fields_ = ['position']
    def __init__(self, position):
        self.position = position

    def replace(self, matches):
        return matches[self.position]

    def __repr__(self):
        return "PositionalArg(%d)" % self.position

def parse_number(source):
    acc = 0
    while not source.at_end():
        oldpos = source.pos
        ch = source.get()
        if not rsre_char.is_digit(ord(ch[0])):
            source.pos = oldpos
            return acc
        acc = 10 * acc + int(ch)
    return acc

def parse_escape_sequence(source, buffer):
    if source.match(u"\\"):
        buffer.append(u"\\")
        return None
    elif source.match(u"&"):
        buffer.append(u"&")
        return None
    elif source.match(u"$"):
        return PositionalArg(0)
    n = parse_number(source)
    return PositionalArg(n)

def parse_insert_string(str):
    source = regexp.Source(str)
    buffer = rstring.UnicodeBuilder()
    result = []
    while not source.at_end():
        if source.match(u"\\"):
            escaped = parse_escape_sequence(source, buffer)
            if escaped is not None:
                if buffer.getlength():
                    result.append(StringLiteral(buffer.build()))
                    buffer = rstring.UnicodeBuilder()
                result.append(escaped)
        else:
            ch = source.get()
            buffer.append(ch)
    if buffer.getlength():
        result.append(StringLiteral(buffer.build()))
    return result

def do_input_substitution(formatter, input_string, matched_positions):
    matched_strings = [None] * len(matched_positions)
    for i, (start, end) in enumerate(matched_positions):
        assert start >= 0 and end >= 0
        matched_strings[i] = input_string[start:end]
    return u"".join([fmt.replace(matched_strings) for fmt in formatter])

