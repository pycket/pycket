from pycket.base import W_Object, SingletonMeta
from pycket.error import SchemeException

from rpython.rlib import rerased
from rpython.rlib.objectmodel import compute_hash, import_from_mixin

class W_String(W_Object):
    errorname = "string"

    # factory methods

    @staticmethod
    def fromstr_utf8(s, immutable=False):
        u = s.decode("utf-8")
        return W_String.fromunicode(u, immutable)

    @staticmethod
    def fromascii(s, immutable=False):
        assert s.decode("ascii") == s
        strategy = AsciiStringStrategy.singleton
        storage = strategy.erase(s)
        if immutable:
            cls = W_ImmutableString
        else:
            cls = W_MutableString
        return cls(strategy, storage)

    @staticmethod
    def fromunicode(u, immutable=False):
        strategy = UnicodeStringStrategy.singleton
        storage = strategy.erase(u)
        if immutable:
            cls = W_ImmutableString
        else:
            cls = W_MutableString
        return cls(strategy, storage)

    @staticmethod
    def frombytes(b, immutable=False):
        assert 0

    def __init__(self, strategy, storage):
        self.change_strategy(strategy, storage)

    def change_strategy(self, strategy, storage):
        self.strategy = strategy
        self.storage = storage

    def get_strategy(self):
        return self.strategy

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

    # string operations
    def length(self):
        return self.get_strategy().length(self)

    def getitem(self, index):
        return self.get_strategy().getitem(self, index)

    def getslice(self, start, stop):
        return self.get_strategy().getslice(self, start, stop)

    def hash_equal(self):
        return self.get_strategy().hash(self)

    def equal(self, other):
        return self.get_strategy().eq(self, other)

    def __repr__(self):
        return "%s(%s, %s)" % (self.__class__.__name__, self.get_strategy(), self.storage)

    def tostring(self):
        from pypy.objspace.std.bytesobject import string_escape_encode
        #return string_escape_encode(self.value, '"')
        result = self.as_str_utf8()
        assert result is not None
        return result

    def setitem(self, index, unichar):
        raise SchemeException("can't mutate string")

    def setslice(self, index, w_from, fromstart, fromend):
        raise SchemeException("can't mutate string")


class W_MutableString(W_String):

    def make_immutable(self):
        assert 0

    cache = {}
    @staticmethod
    def make(val):
        # XXX reactivate make
        assert 0
        lup = W_String.cache.get(val, None)
        if lup is None:
            lup = W_String(val, immutable=True)
            W_String.cache[val] = lup
        return lup

    def immutable(self):
        return False

    # mutation operations

    def setitem(self, index, unichar):
        return self.get_strategy().setitem(self, index, unichar)

    def setslice(self, index, w_from, fromstart, fromend):
        return self.get_strategy().setslice(self, index, w_from, fromstart, fromend)


class W_ImmutableString(W_String):
    # XXX don't store the strategy
    def make_immutable(self):
        return self

    def immutable(self):
        return True


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
        for i in range(length):
            if self.getitem(w_str, i) != w_other.getitem(i):
                return False
        return True

    def hash(self, w_str):
        return compute_hash(w_str.as_unicode()) # inefficient default


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

    def as_str_ascii(self, w_str):
        return self.unerase(w_str.storage)

    def as_str_utf8(self, w_str):
        return self.unerase(w_str.storage)

    def as_unicode(self, w_str):
        return unicode(self.unerase(w_str.storage)) # change strategy?


    # string operations

    def length(self, w_str):
        return len(self.unerase(w_str.storage))

    def getitem(self, w_str, index):
        return unichr(ord(self.unerase(w_str.storage)[index]))

    def getslice(self, w_str, start, stop):
        v = self.unerase(w_str.storage)[start:stop]
        return W_MutableString(self, self.erase(v))

    def eq(self, w_str, w_other):
        if w_other.get_strategy() is self:
            return self.unerase(w_str.storage) == self.unerase(w_other.storage)
        return ImmutableStringStrategy.eq(self, w_str, w_other)

    def hash(self, w_str):
        return compute_hash(w_str.as_str_ascii())


class AsciiMutableStringStrategy(MutableStringStrategy):
    erase, unerase = rerased.new_static_erasing_pair("ascii-mutable-string-strategy")

    def as_charlist_ascii(self, w_str):
        return self.unerase(w_str.storage)

    def as_charlist_utf8(self, w_str):
        return self.unerase(w_str.storage)

    def as_unicharlist(self, w_str):
        assert 0 # XXX


    # string operations

    def length(self, w_str):
        return len(self.unerase(w_str.storage))

    def getitem(self, w_str, index):
        return unichr(ord(self.unerase(w_str.storage)[index]))

    def getslice(self, w_str, start, stop):
        v = self.unerase(w_str.storage)[start:stop]
        return W_MutableString(self, self.erase(v))

    def eq(self, w_str, w_other):
        if w_other.get_strategy() is self:
            return self.unerase(w_str.storage) == self.unerase(w_other.storage)
        return MutableStringStrategy.eq(self, w_str, w_other)


    # mutation operations

    def setitem(self, w_str, index, unichar):
        val = ord(unichar.value)
        assert val < 128 # XXX
        self.unerase(w_str.storage)[index] = chr(val)

    def setslice(self, w_str, index, w_from, fromstart, fromend):
        target = self.unerase(w_str.storage)
        # XXX inefficient
        for sourceindex in range(fromstart, fromend):
            char = ord(w_from.getitem(sourceindex))
            assert char < 128 # XXX
            target[index] = chr(char)
            index += 1


class UnicodeStringStrategy(ImmutableStringStrategy):
    erase, unerase = rerased.new_static_erasing_pair("unicode-string-strategy")

    def make_mutable(self, w_str):
        assert 0

    def as_str_ascii(self, w_str):
        assert 0
        return self.unerase(w_str.storage)

    def as_str_utf8(self, w_str):
        assert 0
        return self.unerase(w_str.storage)

    def as_unicode(self, w_str):
        assert 0
        return unicode(self.unerase(w_str.storage)) # change strategy?


    # string operations

    def length(self, w_str):
        return len(self.unerase(w_str.storage))

    def getitem(self, w_str, index):
        return self.unerase(w_str.storage)[index]

    def getslice(self, w_str, start, stop):
        assert 0
        v = self.unerase(w_str.storage)[start:stop]
        return W_MutableString(self, self.erase(v))

    def eq(self, w_str, w_other):
        if w_other.get_strategy() is self:
            return self.unerase(w_str.storage) == self.unerase(w_other.storage)
        return ImmutableStringStrategy.eq(self, w_str, w_other)

    def hash(self, w_str):
        assert 0
        return compute_hash(w_str.as_str_ascii())




# what I need
# comparison
# upper case
# lower case
# appending
# make immutable


# "lattice"
#
#  mutable     immutable
#  unicode <-  unicode
#    |           |
# (latin1) <- (latin1)
#    |           |
#  ascii   <-  ascii
#    |           |
#   bot    <-   bot


# approach: try to be as far at the bottom as possible, but never move down
# try to store things immutably
# immutable strings still have to be mutable for going up the representation
# hierarchy

