from pycket.base import W_Object, SingletonMeta
from pycket.error import SchemeException
from pycket import config

from rpython.rlib import rerased, jit
from rpython.rlib.objectmodel import compute_hash, we_are_translated
from rpython.rlib.unicodedata import unicodedb_9_0_0 as unicodedb
from rpython.rlib.rstring     import StringBuilder, UnicodeBuilder

@jit.unroll_safe
def _is_ascii(s):
    if not jit.loop_unrolling_heuristic(s, len(s)):
        return _is_ascii_elidable(s)
    for c in s:
        if ord(c) >= 128:
            return False
    return True

@jit.elidable
def _is_ascii_elidable(s):
    for c in s:
        if ord(c) >= 128:
            return False
    return True

class W_String(W_Object):
    errorname = "string"
    _attrs_ = []

    # factory methods

    @staticmethod
    def fromstr_utf8(s, immutable=False):
        # try to see whether it's ascii first
        if config.strategies:
            if _is_ascii(s):
                return W_String.fromascii(s, immutable)
        u = s.decode("utf-8")
        return W_String.fromunicode(u, immutable)

    @staticmethod
    def fromascii(s, immutable=False):
        if not config.strategies:
            u = s.decode("utf-8")
            return W_String.fromunicode(u, immutable)
        if not we_are_translated():
            assert s.decode("ascii") == s
        strategy = AsciiStringStrategy.singleton
        storage = strategy.erase(s)
        if immutable:
            cls = W_AsciiImmutableString
        else:
            cls = W_MutableString
        return cls(strategy, storage)

    @staticmethod
    def fromunicode(u, immutable=False):
        strategy = UnicodeStringStrategy.singleton
        storage = strategy.erase(u)
        if immutable:
            cls = W_UnicodeImmutableString
        else:
            cls = W_MutableString
        return cls(strategy, storage)

    cache = {}
    @staticmethod
    def make(val):
        lup = W_String.cache.get(val, None)
        if lup is None:
            lup = W_String.fromstr_utf8(val, immutable=True)
            W_String.cache[val] = lup
        return lup

    def make_immutable(self):
        raise NotImplementedError("abstract base class")

    def get_strategy(self):
        raise NotImplementedError("abstract base class")

    def get_storage(self):
        raise NotImplementedError("abstract base class")

    # methods that defer to the strategies

    def as_str_ascii(self):
        return self.get_strategy().as_str_ascii(self)

    def as_str_utf8(self):
        return self.get_strategy().as_str_utf8(self)

    def as_unicode(self):
        return self.get_strategy().as_unicode(self)

    def as_charlist_ascii(self):
        return self.get_strategy().as_charlist_ascii(self)

    def as_charlist_utf8(self):
        return self.get_strategy().as_charlist_utf8(self)

    def as_unicharlist(self):
        return self.get_strategy().as_unicharlist(self)

    def as_escaped_utf8(self):
        from pypy.objspace.std.bytesobject import string_escape_encode
        r = self.as_str_utf8()
        assert r is not None
        return string_escape_encode(r, '')

    # string operations
    def length(self):
        return self.get_strategy().length(self)

    def getitem(self, index):
        return self.get_strategy().getitem(self, index)

    def getslice(self, start, stop):
        return self.get_strategy().getslice(self, start, stop)

    def hash_equal(self, info=None):
        return self.get_strategy().hash(self)

    def equal(self, other):
        if not isinstance(other, W_String):
            return False
        return self.get_strategy().eq(self, other)

    def cmp(self, other):
        """
        returns
         - a negative number if self < other
         - a positive number if self > other
         - 0 if self == other
        only the sign of the result is relevant, not the value
        """
        return self.get_strategy().cmp(self, other)

    def cmp_case_insensitive(self, other):
        return self.get_strategy().cmp_case_insensitive(self, other)

    def upper(self):
        return self.get_strategy().upper(self)

    def lower(self):
        return self.get_strategy().lower(self)

    def __repr__(self):
        return "%s(%s, %s)" % (self.__class__.__name__, self.get_strategy(), self.get_storage())

    def tostring(self):
        from pypy.objspace.std.bytesobject import string_escape_encode
        r = self.as_str_utf8()
        assert r is not None
        return string_escape_encode(r, '"')

    def setitem(self, index, unichar):
        raise SchemeException("can't mutate string")

    def setslice(self, index, w_from, fromstart, fromend):
        raise SchemeException("can't mutate string")


class W_MutableString(W_String):

    _attrs_ = ['storage', 'strategy']

    def __init__(self, strategy, storage):
        self.change_strategy(strategy, storage)

    def get_strategy(self):
        return self.strategy

    def get_storage(self):
        return self.storage

    def change_strategy(self, strategy, storage):
        self.strategy = strategy
        self.storage = storage

    def make_immutable(self):
        try:
            s = self.as_str_ascii()
        except ValueError:
            strategy = UnicodeStringStrategy.singleton
            storage = strategy.erase(self.as_unicode())
            return W_UnicodeImmutableString(strategy, storage)
        else:
            strategy = AsciiStringStrategy.singleton
            storage = strategy.erase(s)
            return W_AsciiImmutableString(strategy, storage)

    def immutable(self):
        return False

    # mutation operations

    def setitem(self, index, unichar):
        return self.get_strategy().setitem(self, index, unichar)

    def setslice(self, index, w_from, fromstart, fromend):
        return self.get_strategy().setslice(self, index, w_from, fromstart, fromend)


class W_ImmutableString(W_String):
    # abstract base class of immutable strings
    # there are concrete subclasses for every immutable strategy

    _attrs_ = ['storage']
    _immutable_fields_ = ['storage']

    def __init__(self, strategy, storage):
        self.storage = storage
        assert strategy is self.get_strategy()

    def make_immutable(self):
        return self

    def immutable(self):
        return True

    def get_storage(self):
        return self.storage


class W_AsciiImmutableString(W_ImmutableString):
    def get_strategy(self):
        return AsciiStringStrategy.singleton


class W_UnicodeImmutableString(W_ImmutableString):
    def get_strategy(self):
        return UnicodeStringStrategy.singleton


class StringStrategy(object):
    __metaclass__ = SingletonMeta

    # strategy manipulation
    def make_mutable(self, w_str):
        raise NotImplementedError("abstract base class")

    def as_str_ascii(self, w_str):
        raise ValueError("can't convert")

    def as_str_utf8(self, w_str):
        raise NotImplementedError("abstract base class")

    def as_unicode(self, w_str):
        raise NotImplementedError("abstract base class")

    def as_charlist_ascii(self, w_str):
        raise ValueError("can't convert")

    def as_charlist_utf8(self, w_str):
        raise NotImplementedError("abstract base class")

    def as_unicharlist(self, w_str):
        raise NotImplementedError("abstract base class")


    # string operations

    def length(self, w_str):
        raise NotImplementedError("abstract base class")

    def getitem(self, w_str, index):
        """ returns a unichar """
        raise NotImplementedError("abstract base class")

    def getslice(self, w_str, start, stop):
        """ returns a W_String """
        raise NotImplementedError("abstract base class")

    def eq(self, w_str, w_other):
        # base implementations, subclasses should do better ones
        length = self.length(w_str)
        if length != w_other.length():
            return False
        otherstrategy = w_other.get_strategy()
        for i in range(length):
            if self.getitem(w_str, i) != otherstrategy.getitem(w_other, i):
                return False
        return True

    def cmp(self, w_str, w_other):
        # base implementations, subclasses should do better ones
        len1 = self.length(w_str)
        len2 = w_other.length()

        if len1 < len2:
            cmplen = len1
        else:
            cmplen = len2
        otherstrategy = w_other.get_strategy()
        for i in range(cmplen):
            diff = ord(self.getitem(w_str, i)) - ord(otherstrategy.getitem(w_other, i))
            if diff:
                return diff
            i += 1
        return len1 - len2

    def cmp_case_insensitive(self, w_str, w_other):
        # base implementations, subclasses should do better ones
        len1 = self.length(w_str)
        len2 = w_other.length()

        if len1 < len2:
            cmplen = len1
        else:
            cmplen = len2
        otherstrategy = w_other.get_strategy()
        for i in range(cmplen):
            ch1 = unicodedb.tolower(ord(self.getitem(w_str, i)))
            ch2 = unicodedb.tolower(ord(otherstrategy.getitem(w_other, i)))
            diff = ch1 - ch2
            if diff:
                return diff
            i += 1
        return len1 - len2

    def hash(self, w_str):
        # potentially inefficient default
        return compute_hash(w_str.as_unicode())

    def upper(self, w_str):
        raise NotImplementedError("abstract base class")

    def lower(self, w_str):
        raise NotImplementedError("abstract base class")


    # mutation operations

    def setitem(self, w_str, index, unichar):
        raise NotImplementedError("abstract base class")

    def setslice(self, w_str, index, w_from, fromstart, fromend):
        raise NotImplementedError("abstract base class")


class ImmutableStringStrategy(StringStrategy):
    def as_charlist_ascii(self, w_str):
        return list(self.as_str_ascii(w_str))

    def as_charlist_utf8(self, w_str):
        return list(self.as_str_utf8(w_str))

    def as_unicharlist(self, w_str):
        return list(self.as_unicode(w_str))

    def setitem(self, w_str, index, unichar):
        self.make_mutable(w_str)
        return w_str.setitem(index, unichar)

    def setslice(self, w_str, index, w_from, fromstart, fromend):
        self.make_mutable(w_str)
        return w_str.setslice(index, w_from, fromstart, fromend)


class MutableStringStrategy(StringStrategy):
    def as_str_ascii(self, w_str):
        return "".join(self.as_charlist_ascii(w_str))

    def as_str_utf8(self, w_str):
        return "".join(self.as_charlist_utf8(w_str))

    def as_unicode(self, w_str):
        return u"".join(self.as_unicharlist(w_str))

    def make_mutable(self, w_str):
        pass


    # mutation operations

    def setitem(self, w_str, index, unichar):
        raise NotImplementedError

    def setslice(self, w_str, index, w_from, fromstart, fromend):
        raise NotImplementedError("abstract base class")


class AsciiStringStrategy(ImmutableStringStrategy):
    erase, unerase = rerased.new_static_erasing_pair("ascii-string-strategy")

    def make_mutable(self, w_str):
        strategy = AsciiMutableStringStrategy.singleton
        storage = strategy.erase(self.as_charlist_ascii(w_str))
        w_str.change_strategy(strategy, storage)

    def as_str_utf8(self, w_str):
        return self.unerase(w_str.get_storage())
    as_str_ascii = as_str_utf8

    def as_unicode(self, w_str):
        return unicode(self.unerase(w_str.get_storage())) # change strategy?


    # string operations

    def length(self, w_str):
        return len(self.unerase(w_str.get_storage()))

    def getitem(self, w_str, index):
        return unichr(ord(self.unerase(w_str.get_storage())[index]))

    def getslice(self, w_str, start, stop):
        v = self.unerase(w_str.get_storage())[start:stop]
        return W_MutableString(self, self.erase(v))

    def eq(self, w_str, w_other):
        if w_other.get_strategy() is self:
            return self.unerase(w_str.get_storage()) == self.unerase(w_other.get_storage())
        return ImmutableStringStrategy.eq(self, w_str, w_other)

    def hash(self, w_str):
        return compute_hash(w_str.as_str_ascii())

    def upper(self, w_str):
        return W_String.fromascii(w_str.as_str_ascii().upper())

    def lower(self, w_str):
        return W_String.fromascii(w_str.as_str_ascii().lower())


class AsciiMutableStringStrategy(MutableStringStrategy):
    erase, unerase = rerased.new_static_erasing_pair("ascii-mutable-string-strategy")

    def make_unicode(self, w_str):
        strategy = UnicodeMutableStringStrategy.singleton
        storage = strategy.erase(self.as_unicharlist(w_str))
        w_str.change_strategy(strategy, storage)

    def as_charlist_utf8(self, w_str):
        return self.unerase(w_str.get_storage())[:]

    def as_unicharlist(self, w_str):
        return [unichr(ord(c)) for c in self.unerase(w_str.get_storage())]

    def as_str_utf8(self, w_str):
        return "".join(self.unerase(w_str.get_storage()))
    as_str_ascii = as_str_utf8

    # string operations

    def length(self, w_str):
        return len(self.unerase(w_str.get_storage()))

    def getitem(self, w_str, index):
        return unichr(ord(self.unerase(w_str.get_storage())[index]))

    def getslice(self, w_str, start, stop):
        v = self.unerase(w_str.get_storage())[start:stop]
        return W_MutableString(self, self.erase(v))

    def eq(self, w_str, w_other):
        if w_other.get_strategy() is self:
            return self.unerase(w_str.get_storage()) == self.unerase(w_other.get_storage())
        return MutableStringStrategy.eq(self, w_str, w_other)


    # mutation operations

    def setitem(self, w_str, index, unichar):
        val = ord(unichar.value)
        if val < 128:
            self.unerase(w_str.get_storage())[index] = chr(val)
        else:
            self.make_unicode(w_str)
            return w_str.setitem(index, unichar)

    def setslice(self, w_str, index, w_from, fromstart, fromend):
        if w_from.get_strategy() is self:
            target = self.unerase(w_str.get_storage())
            # XXX inefficient
            for sourceindex in range(fromstart, fromend):
                char = ord(w_from.getitem(sourceindex))
                assert char < 128 # XXX
                target[index] = chr(char)
                index += 1
        else:
            self.make_unicode(w_str)
            return w_str.setslice(index, w_from, fromstart, fromend)

    def upper(self, w_str):
        # XXX inefficient
        return W_String.fromascii(w_str.as_str_ascii().upper())

    def lower(self, w_str):
        # XXX inefficient
        return W_String.fromascii(w_str.as_str_ascii().lower())


class UnicodeStringStrategy(ImmutableStringStrategy):
    erase, unerase = rerased.new_static_erasing_pair("unicode-string-strategy")

    def make_mutable(self, w_str):
        strategy = UnicodeMutableStringStrategy.singleton
        storage = strategy.erase(self.as_unicharlist(w_str))
        w_str.change_strategy(strategy, storage)

    def as_str_ascii(self, w_str):
        raise ValueError # XXX or check?

    def as_str_utf8(self, w_str):
        s = self.unerase(w_str.get_storage())
        assert s is not None
        return s.encode("utf-8")

    def as_unicode(self, w_str):
        return self.unerase(w_str.get_storage())


    # string operations

    def length(self, w_str):
        return len(self.unerase(w_str.get_storage()))

    def getitem(self, w_str, index):
        return self.unerase(w_str.get_storage())[index]

    def getslice(self, w_str, start, stop):
        v = self.unerase(w_str.get_storage())[start:stop]
        return W_MutableString(self, self.erase(v))

    def eq(self, w_str, w_other):
        if w_other.get_strategy() is self:
            return self.unerase(w_str.get_storage()) == self.unerase(w_other.get_storage())
        return ImmutableStringStrategy.eq(self, w_str, w_other)

    def upper(self, w_str):
        value = self.unerase(w_str.get_storage())
        builder = UnicodeBuilder(len(value))
        for i, ch in enumerate(value):
            builder.append(unichr(unicodedb.toupper(ord(ch))))
        return W_MutableString(self, self.erase(builder.build()))

    def lower(self, w_str):
        value = self.unerase(w_str.get_storage())
        builder = UnicodeBuilder(len(value))
        for i, ch in enumerate(value):
            builder.append(unichr(unicodedb.tolower(ord(ch))))
        return W_MutableString(self, self.erase(builder.build()))


class UnicodeMutableStringStrategy(MutableStringStrategy):
    erase, unerase = rerased.new_static_erasing_pair("unicode-mutable-string-strategy")

    def as_charlist_ascii(self, w_str):
        raise ValueError("can't convert")

    def as_charlist_utf8(self, w_str):
        return list(self.as_str_utf8(w_str))

    def as_str_utf8(self, w_str):
        return u''.join(self.unerase(w_str.get_storage())).encode('utf-8')


    def as_unicharlist(self, w_str):
        return self.unerase(w_str.get_storage())[:]

    def as_unicode(self, w_str):
        return u"".join(self.unerase(w_str.get_storage()))

    # string operations

    def length(self, w_str):
        return len(self.unerase(w_str.get_storage()))

    def getitem(self, w_str, index):
        return self.unerase(w_str.get_storage())[index]

    def getslice(self, w_str, start, stop):
        v = self.unerase(w_str.get_storage())[start:stop]
        return W_MutableString(self, self.erase(v))

    def eq(self, w_str, w_other):
        if w_other.get_strategy() is self:
            return self.unerase(w_str.get_storage()) == self.unerase(w_other.get_storage())
        return MutableStringStrategy.eq(self, w_str, w_other)


    # mutation operations

    def setitem(self, w_str, index, unichar):
        self.unerase(w_str.get_storage())[index] = unichar.value

    def setslice(self, w_str, index, w_from, fromstart, fromend):
        target = self.unerase(w_str.get_storage())
        # XXX inefficient
        for sourceindex in range(fromstart, fromend):
            target[index] = w_from.getitem(sourceindex)
            index += 1


    def upper(self, w_str):
        # copy paste from above, but the types are different
        value = self.unerase(w_str.get_storage())
        builder = UnicodeBuilder(len(value))
        for i, ch in enumerate(value):
            builder.append(unichr(unicodedb.toupper(ord(ch))))
        return W_MutableString(self, self.erase(list(builder.build())))

    def lower(self, w_str):
        value = self.unerase(w_str.get_storage())
        builder = UnicodeBuilder(len(value))
        for i, ch in enumerate(value):
            builder.append(unichr(unicodedb.tolower(ord(ch))))
        return W_MutableString(self, self.erase(list(builder.build())))

