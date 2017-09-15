#! /usr/bin/env python
# -*- coding: utf-8 -*-
import operator as op
from pycket import values
from pycket.values_string import W_String
from pycket.error import SchemeException
from pycket.prims.expose import default, expose, unsafe, subclass_unsafe
from rpython.rlib.unicodedata import unicodedb_6_2_0 as unicodedb
from rpython.rlib.rstring     import StringBuilder, UnicodeBuilder
from rpython.rlib import jit


@expose("symbol->string", [values.W_Symbol])
def symbol_to_string(v):
    asciivalue = v.asciivalue()
    if asciivalue is not None:
        return W_String.fromascii(asciivalue)
    return W_String.fromunicode(v.unicodevalue())

@expose("string->symbol", [W_String])
def string_to_symbol(v):
    return values.W_Symbol.make(v.as_str_utf8())

@expose("string->number", [W_String,
                           default(values.W_Integer, values.W_Fixnum(10)),
                           default(values.W_Symbol, values.W_Symbol.make("number-or-false")),
                           default(values.W_Symbol, values.W_Symbol.make("decimal-as-exact"))])
def str2num(w_s, radix, convert_mode, decimal_mode):
    from rpython.rlib import rarithmetic, rfloat, rbigint
    from rpython.rlib.rstring import ParseStringError, ParseStringOverflowError

    s = w_s.as_str_utf8()
    try:
        if "." in s:
            return values.W_Flonum(rfloat.string_to_float(s))
        else:
            try:
                return values.W_Fixnum(rarithmetic.string_to_int(s, base=10))
            except ParseStringOverflowError:
                return values.W_Bignum(rbigint.rbigint.fromstr(s))
    except ParseStringError as e:
        return values.w_false

@expose("number->string",
        [values.W_Number, default(values.W_Fixnum, values.W_Fixnum.make(10))])
def num2str(a, radix):
    from rpython.rlib.rbigint import BASE8, BASE16
    if radix.value == 10:
        return W_String.fromascii(a.tostring())
    else:
        if isinstance(a, values.W_Fixnum):
            if radix.value == 16:
                res = hex(a.value)
                if a.value >= 0:
                    res = res[2:]
                else:
                    res = "-" + res[3:]
                return W_String.fromascii(res)
            #elif radix.value == 8:
            #    return W_String.fromascii(oct(a.value))
            # elif radix.value == 2:
            #     return W_String.fromascii(bin(a.value))
            else:
                raise SchemeException("number->string: radix unsupported")
        elif isinstance(a, values.W_Bignum):
            if radix.value == 16:
                return W_String.fromascii(a.value.format(BASE16))
            elif radix.value == 8:
                return W_String.fromascii(a.value.format(BASE8))
            elif radix.value == 2:
                return W_String.fromascii(a.value.format("01"))
            else:
                raise SchemeException("number->string: radix unsupported")
        elif isinstance(a, values.W_Flonum):
            raise SchemeException("number->string: flonum only supports radix 10")
        else:
            assert 0 # not reached

@expose("string->path", [W_String])
def string_to_path(str):
    s = str.as_str_utf8()
    return values.W_Path(s)

@expose("string->unreadable-symbol", [W_String])
def string_to_unsymbol(v):
    return values.W_Symbol.make_unreadable(v.as_str_utf8())

@expose("string->keyword", [W_String])
def string_to_keyword(str):
    repr = str.as_str_utf8()
    return values.W_Keyword.make(repr)

@expose("string->immutable-string", [W_String])
def string_to_immutable_string(string):
    return string.make_immutable()

@expose("string->uninterned-symbol", [W_String])
def string_to_symbol(v):
    return values.W_Symbol(v.as_str_utf8())

@expose(["string->bytes/locale", "string->bytes/utf-8"],
        [W_String,
         default(values.W_Object, values.w_false),
         default(values.W_Fixnum, values.W_Fixnum.ZERO),
         default(values.W_Fixnum, None)])
def string_to_bytes_locale(str, errbyte, start, end):
    # assert errbyte is values.w_false
    # ignore for now
    assert start.value == 0
    assert end is None
    # FIXME: This ignores the locale
    return values.W_Bytes.from_charlist(str.as_charlist_utf8())

@expose("bytes->string/latin-1",
        [values.W_Bytes,
         default(values.W_Object, values.w_false),
         default(values.W_Fixnum, values.W_Fixnum.ZERO),
         default(values.W_Fixnum, None)])
def bytes_to_string_latin(w_bytes, err, start, end):
    # XXX Not a valid implementation
    str = w_bytes.as_str().decode("latin-1")
    return W_String.fromunicode(str)

@expose("string->bytes/latin-1",
        [W_String,
         default(values.W_Object, values.w_false),
         default(values.W_Fixnum, values.W_Fixnum.ZERO),
         default(values.W_Fixnum, None)])
def string_to_bytes_latin(w_str, err, start, end):
    # XXX Not a valid implementation
    bytes = w_str.as_unicode().encode("latin-1")
    return values.W_Bytes.from_string(bytes)

@expose("string->list", [W_String])
def string_to_list(s):
    data = s.as_unicode()
    acc = values.w_null
    for i in range(len(data) - 1, -1, -1):
        char = data[i]
        acc = values.W_Cons.make(values.W_Character(char), acc)
    return acc

@expose("list->string", [values.W_List])
def list_to_string(w_list):
    if not w_list.is_proper_list():
        raise SchemeException("list->string: expected proper list")
    if not isinstance(w_list, values.W_Cons):
        return W_String.fromascii("")
    builder = UnicodeBuilder()
    while isinstance(w_list, values.W_Cons):
        char, w_list = w_list.car(), w_list.cdr()
        if not isinstance(char, values.W_Character):
            raise SchemeException("list->string: expected list of characters")
        builder.append(char.value)
    return W_String.fromunicode(builder.build())

##################################

def define_string_comp(name, op):
    @expose(name)
    @jit.unroll_safe
    def comp(args):
        if len(args) < 2:
            raise SchemeException(name + ": requires at least 2 arguments")
        head = args[0]
        if not isinstance(head, W_String):
            raise SchemeException(name + ": not given a string")
        for i in range(1, len(args)):
            t = args[i]
            if not isinstance(t, W_String):
                raise SchemeException(name + ": not given a string")
            if not op(head, t):
                return values.w_false
            head = t
        return values.w_true

for a in [("string<?", lambda w_self, w_other: w_self.cmp(w_other) < 0),
          ("string<=?", lambda w_self, w_other: w_self.cmp(w_other) <= 0),
          ("string=?", lambda w_self, w_other: w_self.equal(w_other)),
          ("string>=?", lambda w_self, w_other: w_self.cmp(w_other) >= 0),
          ("string>?", lambda w_self, w_other: w_self.cmp(w_other) > 0),
          ("string-ci<?", lambda w_self, w_other: w_self.cmp_case_insensitive(w_other) < 0),
          ("string-ci<=?", lambda w_self, w_other: w_self.cmp_case_insensitive(w_other) <= 0),
          ("string-ci=?", lambda w_self, w_other: w_self.cmp_case_insensitive(w_other) == 0),
          ("string-ci>=?", lambda w_self, w_other: w_self.cmp_case_insensitive(w_other) >= 0),
          ("string-ci>?", lambda w_self, w_other: w_self.cmp_case_insensitive(w_other) > 0),
          ]:
    define_string_comp(*a)

@expose("make-string", [values.W_Fixnum, default(values.W_Character, None)])
def make_string(k, char):
    if char is None:
        char = u'\0'
        c = 0
    else:
        char = char.value
        c = ord(char)
    if k.value < 0:
        raise SchemeException("make-string: around negative")
    if c < 128:
        char = chr(c)
        return W_String.fromascii(char * k.value)
    else:
        char = unichr(c)
        return W_String.fromunicode(char * k.value)

@expose("string")
@jit.unroll_safe
def string(args):
    if len(args) == 0:
        return W_String.fromascii("")
    assert len(args) > 0
    builder = UnicodeBuilder()
    # XXX could do one less copy in the ascii case
    for char in args:
        if not isinstance(char, values.W_Character):
            raise SchemeException("string: expected a character")
        builder.append(char.value)
    return W_String.fromunicode(builder.build())

@expose("string-downcase", [W_String])
def string_downcase(v):
    return v.lower()

@expose("string-upcase", [W_String])
def string_upcase(v):
    return v.upper()

@jit.unroll_safe
def string_append_fastpath(args):
    try:
        joined = "".join([a.as_str_ascii() for a in args])
        result = W_String.fromascii(joined)
    except ValueError:
        joined = u"".join([a.as_unicode() for a in args])
        result = W_String.fromunicode(joined)
    return result

@expose("string-append")
@jit.unroll_safe
def string_append(args):
    if jit.isconstant(len(args)):
        return string_append_fastpath(args)
    if not args:
        return W_String.fromascii("")
    builder = StringBuilder(len(args))
    unibuilder = None
    ascii_idx = 0
    try:
        for ascii_idx in range(len(args)):
            arg = args[ascii_idx]
            if not isinstance(arg, W_String):
                raise SchemeException("string-append: expected a string")
            builder.append(arg.as_str_ascii())
    except ValueError:
        unibuilder = UnicodeBuilder(len(args))
        unibuilder.append(unicode(builder.build()))
        builder = None
        for i in range(ascii_idx, len(args)):
            arg = args[i]
            if not isinstance(arg, W_String):
                raise SchemeException("string-append: expected a string")
            unibuilder.append(arg.as_unicode())
    if unibuilder is None:
        assert builder is not None
        return W_String.fromascii(builder.build())
    else:
        assert unibuilder is not None
        return W_String.fromunicode(unibuilder.build())

@expose(["string-length", "unsafe-string-length"], [W_String])
def string_length(s1):
    return values.W_Fixnum(s1.length())

@expose("substring", [W_String, values.W_Fixnum, default(values.W_Fixnum, None)])
def substring(w_string, w_start, w_end):
    """
    (substring str start [end]) -> string?
        str : string?
        start : exact-nonnegative-integer?
        end : exact-nonnegative-integer? = (string-length str)
    """
    lenstring = w_string.length()
    start = w_start.value
    if start > lenstring or start < 0:
        raise SchemeException("substring: end index out of bounds")
    if w_end is not None:
        end = w_end.value
        if end > lenstring or end < 0:
            raise SchemeException("substring: end index out of bounds")
    else:
        end = lenstring
    if end < start:
        raise SchemeException(
            "substring: ending index is smaller than starting index")
    return w_string.getslice(start, end)

@expose("string-ref", [W_String, values.W_Fixnum])
def string_ref(s, n):
    n = n.value
    if not 0 <= n < s.length():
        raise SchemeException("string-ref: index out of bounds")
    return values.W_Character(s.getitem(n))

@expose("string-set!", [W_String, values.W_Fixnum, values.W_Character])
def string_set(w_str, w_index, w_char):
    idx = w_index.value
    if w_str.immutable():
        raise SchemeException("string-set!: given immutable string")
    if not (0 <= idx < w_str.length()):
        raise SchemeException("string-set!: given index is out of range")
    w_str.setitem(w_index.value, w_char)
    return values.w_void
    idx = k.value
    v = [i for i in str.value]
    v[idx] = char.value
    str.value = "".join(v)


@expose(["string-copy!"],
         [W_String, values.W_Fixnum, W_String,
          default(values.W_Fixnum, values.W_Fixnum.ZERO),
          default(values.W_Fixnum, None)])
def string_copy_bang(w_dest, w_dest_start, w_src, w_src_start, w_src_end):
    from pycket.interpreter import return_value

    # FIXME: custom ports
    if w_dest.immutable():
        raise SchemeException("string-copy!: given immutable string")

    dest_start = w_dest_start.value
    dest_len = w_dest.length()
    dest_max = (dest_len - dest_start)

    src_start =  w_src_start.value
    src_end = w_src.length() if w_src_end is None else w_src_end.value

    if src_end - src_start > dest_max:
        raise SchemeException("string-copy!: not enough room in target string")
    w_dest.setslice(dest_start, w_src, src_start, src_end)
    return values.w_void


################################################################################
# Byte stuff
@expose("make-bytes", [values.W_Fixnum, default(values.W_Object, values.W_Fixnum.ZERO)])
def make_bytes(length, byte):
    # assert byte_huh(byte) is values.w_true
    if isinstance(byte, values.W_Fixnum):
        v = byte.value
    else:
        raise SchemeException("make-bytes: not a byte")
    if not 0 <= v <= 255:
        raise SchemeException("make-bytes: argument out of range")
    bstr = [chr(v)] * length.value
    return values.W_MutableBytes(bstr)

@expose("bytes")
def bytes(args):
    if len(args) == 0:
        return values.W_MutableBytes([])
    assert len(args) > 0

    builder = StringBuilder()
    for char in args:
        if not (isinstance(char, values.W_Fixnum)
                and 0 <= char.value <= 255):
            raise SchemeException("string: expected a character int")
        builder.append(chr(char.value))
    return values.W_Bytes.from_string(builder.build(), immutable=False)

@expose("bytes-append")
def bytes_append(args):
    total_len = 0
    for a in args:
        if not isinstance(a, values.W_Bytes):
            raise SchemeException(
                "bytes-append: expected a byte string, but got %s" % a)
        total_len += a.length()

    result = ['\0'] * total_len # is this the fastest way to do things?
    cnt = 0
    for a in args:
        assert isinstance(a, values.W_Bytes)
        for i in range(a.length()):
            byte = a.ref_char(i)
            result[cnt] = byte
            cnt += 1

    assert cnt == total_len
    return values.W_MutableBytes(result)

@expose("bytes-length", [values.W_Bytes])
def bytes_length(s1):
    return values.W_Fixnum(s1.length())

@expose("bytes-ref", [values.W_Bytes, values.W_Fixnum])
def bytes_ref(s, n):
    return s.ref(n.value)

@expose("bytes-set!", [values.W_Bytes, values.W_Fixnum, values.W_Fixnum])
def bytes_set_bang(s, n, v):
    return s.set(n.value, v.value)

@expose("unsafe-bytes-length", [subclass_unsafe(values.W_Bytes)])
def unsafe_bytes_length(s1):
    return values.W_Fixnum(s1.length())

@expose("unsafe-bytes-ref", [subclass_unsafe(values.W_Bytes), unsafe(values.W_Fixnum)])
def unsafe_bytes_ref(s, n):
    return s.ref(n.value)

@expose("unsafe-bytes-set!", [unsafe(values.W_MutableBytes),
                              unsafe(values.W_Fixnum),
                              unsafe(values.W_Fixnum)])
def unsafe_bytes_set_bang(s, n, v):
    return s.set(n.value, v.value)


@expose("list->bytes", [values.W_List])
def list_to_bytes(w_list):
    if not w_list.is_proper_list():
        raise SchemeException("list->bytes: expected proper list, got %s" % w_list.tostring())

    ll = []
    while isinstance(w_list, values.W_UnwrappedFixnumConsProper):
        val = w_list._car
        if not (0 <= val < 256):
            break
        ll.append(chr(val))
        w_list = w_list.cdr()
    else:
        if w_list is values.w_null:
            return values.W_MutableBytes(ll[:])

    assert isinstance(w_list, values.W_Cons)
    raise SchemeException("list->bytes: expected a number between 0 and 255, got %s"
                          % w_list.car().tostring())

@expose("subbytes",
        [values.W_Bytes, values.W_Fixnum, default(values.W_Fixnum, None)])
def subbytes(w_bytes, w_start, w_end):
    """
    (subbytes bstr start [end]) → bytes?
        bstr : bytes?
        start : exact-nonnegative-integer?
        end : exact-nonnegative-integer? = (bytes-length str)
    """
    start = w_start.value
    length = w_bytes.length()
    if start > length or start < 0:
        raise SchemeException("subbytes: end index out of bounds")
    if w_end is not None:
        end = w_end.value
        if end > length or end < 0:
            raise SchemeException("subbytes: end index out of bounds")
    else:
        end = length
    if end < start:
        raise SchemeException(
            "subbytes: ending index is smaller than starting index")
    slice = w_bytes.getslice(start, end)
    return values.W_Bytes.from_charlist(slice, immutable=False)

@expose("bytes-copy!",
         [values.W_Bytes, values.W_Fixnum, values.W_Bytes,
          default(values.W_Fixnum, values.W_Fixnum.ZERO),
          default(values.W_Fixnum, None)])
def bytes_copy_bang(w_dest, w_dest_start, w_src, w_src_start, w_src_end):
    from pycket.interpreter import return_value

    # FIXME: custom ports
    if w_dest.immutable():
        raise SchemeException("bytes-copy!: given immutable bytes")

    dest_start = w_dest_start.value
    dest_len = w_dest.length()
    dest_max = (dest_len - dest_start)

    src_start =  w_src_start.value
    src_end = w_src.length() if w_src_end is None else w_src_end.value

    assert (src_end-src_start) <= dest_max

    for i in range(0, src_end - src_start):
        val = w_src.ref_char(src_start + i)
        w_dest.set_char(dest_start + i, val)

    return values.w_void

def define_bytes_comp(name, op):
    compare = make_bytes_compare(name, op)
    @expose(name)
    @jit.unroll_safe
    def comp(args):
        if len(args) < 2:
            raise SchemeException(name + ": requires at least 2 arguments")
        head, tail = args[0], args[1:]
        if not isinstance(head, values.W_Bytes):
            raise SchemeException(name + ": not given a bytes")
        for t in tail:
            if not isinstance(t, values.W_Bytes):
                raise SchemeException(name + ": not given a bytes")
            bs1 = head.as_bytes_list()
            bs2 = t.as_bytes_list()
            if not compare(bs1, bs2):
                return values.w_false
            head = t
        return values.w_true

def make_bytes_compare(name, op):
    def bytes_compare(x, y):
        lx = len(x)
        ly = len(y)
        length = min(lx, ly)
        for i in range(length):
            xi, yi = x[i], y[i]
            if xi == yi:
                continue
            return op(xi, yi)
        return op(lx, ly)
    bytes_compare.__name__ = "bytes_compare(%s)" % name
    return bytes_compare

for a in [("bytes<?"  , op.lt) ,
          ("bytes<=?" , op.le) ,
          ("bytes=?"  , op.eq) ,
          ("bytes>=?" , op.ge) ,
          ("bytes>?"  , op.gt) ,
          ]:
    define_bytes_comp(*a)

@expose(["bytes->string/locale",
         "bytes->string/utf-8"], [values.W_Bytes,
                                  default(values.W_Object, values.w_false),
                                  default(values.W_Integer, values.W_Fixnum.ZERO),
                                  default(values.W_Integer, None)])
def string_to_bytes_locale(bytes, errbyte, start, end):
    # FIXME: This ignores the locale
    return W_String.fromstr_utf8(bytes.as_str())

@expose("bytes->path", [values.W_Bytes])
def bytes_to_path(b):
    return values.W_Path(b.as_str())

@expose("bytes->immutable-bytes", [values.W_Bytes])
def bytes_to_immutable_bytes(b):
    if b.immutable():
        return b
    storage = b.as_bytes_list()
    return values.W_Bytes.from_charlist(storage, immutable=True)

@expose("bytes->list", [values.W_Bytes])
def bytes_to_list(bs):
    acc = values.w_null
    for i in range(bs.length()-1, -1, -1):
        byte = ord(bs.ref_char(i))
        acc = values.wrap(byte, acc)
    return acc

################################################################################

# Character


@expose("char->integer", [values.W_Character])
def char_to_integer(c):
    return values.W_Fixnum(ord(c.value))

@expose("integer->char", [values.W_Fixnum])
def integer_to_char(v):
    return values.W_Character(unichr(v.value))

@expose("char-downcase", [values.W_Character])
def char_downcase(v):
    return values.W_Character(unichr(unicodedb.tolower(ord(v.value))))

@expose("char-upcase", [values.W_Character])
def char_upcase(v):
    return values.W_Character(unichr(unicodedb.toupper(ord(v.value))))

@expose("char-foldcase", [values.W_Character])
def char_foldcase(w_char):
    char = ord(w_char.value)
    folded = unicodedb.casefold_lookup(char)
    if folded is None:
        lower = unicodedb.tolower(char)
        return values.W_Character(unichr(lower))
    # XXX: What to do if the case folded character consists of more than one code point?
    return values.W_Character(unichr(folded[0]))


def define_char_comp(name, op):
    @expose(name)
    @jit.unroll_safe
    def comp(args):
        if len(args) < 2:
            raise SchemeException(name + ": requires at least 2 arguments")
        head, tail = args[0], args[1:]
        if not isinstance(head, values.W_Character):
            raise SchemeException(name + ": not given a character")
        for t in tail:
            if not isinstance(t, values.W_Character):
                raise SchemeException(name + ": not given a character")
            if not op(head.value, t.value):
                return values.w_false
            head = t
        return values.w_true

# FIXME: Doing case insensitives like this will perform the lower operation
# every time a value is used for a comparison.
def make_ci(op):
    def lower(a, b):
        return op(unichr(unicodedb.tolower(ord(a))),
                  unichr(unicodedb.tolower(ord(b))))
    return lower

for a in [("char<?", op.lt),
          ("char<=?", op.le),
          ("char=?", op.eq),
          ("char>=?", op.ge),
          ("char>?", op.gt),
          ("char-ci<?", make_ci(op.lt)),
          ("char-ci<=?", make_ci(op.le)),
          ("char-ci=?", make_ci(op.eq)),
          ("char-ci>=?", make_ci(op.ge)),
          ("char-ci>?", make_ci(op.gt)),
          ]:
    define_char_comp(*a)

@expose("char-alphabetic?", [values.W_Character])
def char_alphabetic_huh(w_char):
    c = ord(w_char.value)
    return values.w_true if unicodedb.isalpha(c) else values.w_false

@expose("char-whitespace?", [values.W_Character])
def char_whitespace_huh(w_char):
    c = ord(w_char.value)
    return values.w_true if unicodedb.isspace(c) else values.w_false

@expose("char-numeric?", [values.W_Character])
def char_numeric_huh(w_char):
    c = ord(w_char.value)
    return values.w_true if unicodedb.isnumeric(c) else values.w_false
