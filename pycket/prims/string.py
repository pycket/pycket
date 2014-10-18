#! /usr/bin/env python
# -*- coding: utf-8 -*-
import operator as op
from pycket import values
from pycket.values_string import W_String
from pycket.error import SchemeException
from pycket.prims.expose import default, expose, unsafe
from rpython.rlib.unicodedata import unicodedb_6_2_0 as unicodedb
from rpython.rlib.rstring     import StringBuilder, UnicodeBuilder
from rpython.rlib import jit


@expose("symbol->string", [values.W_Symbol])
def symbol_to_string(v):
    if v.asciivalue:
        return W_String.fromascii(v.asciivalue)
    return W_String.fromunicode(v.value)

@expose("string->symbol", [W_String])
def string_to_symbol(v):
    return values.W_Symbol.make(v.as_str_utf8())

@expose("string->number", [W_String])
def str2num(w_s):
    from rpython.rlib import rarithmetic, rfloat, rbigint
    from rpython.rlib.rstring import ParseStringError, ParseStringOverflowError

    try:
        s = w_s.as_str_ascii()
        if "." in s:
            return values.W_Flonum(rfloat.string_to_float(s))
        else:
            try:
                return values.W_Fixnum(rarithmetic.string_to_int(s, base=0))
            except ParseStringOverflowError:
                return values.W_Bignum(rbigint.rbigint.fromstr(s))
    except ParseStringError as e:
        return values.w_false
    except ValueError:
        return values.w_false

@expose("number->string",
        [values.W_Number, default(values.W_Fixnum, values.W_Fixnum(10))])
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


@expose("string->unreadable-symbol", [W_String])
def string_to_unsymbol(v):
    return values.W_Symbol.make_unreadable(v.value)

@expose("string->immutable-string", [W_String])
def string_to_immutable_string(string):
    return string.make_immutable()

@expose("string->uninterned-symbol", [W_String])
def string_to_symbol(v):
    return values.W_Symbol(v.value)

@expose(["string->bytes/locale",
         "string->bytes/utf-8"], [W_String,
                                  default(values.W_Object, values.w_false),
                                  default(values.W_Fixnum, values.W_Fixnum(0)),
                                  default(values.W_Fixnum, None)])
def string_to_bytes_locale(str, errbyte, start, end):
    assert errbyte is values.w_false
    assert start.value == 0
    assert end is None
    # FIXME: This ignores the locale
    return values.W_Bytes(str.as_charlist_utf8())

@expose("string->list", [W_String])
def string_to_list(s):
    return values.to_list([values.W_Character(i) for i in s.as_unicode()])


##################################

def define_string_comp(name, op):
    @expose(name)
    def comp(args):
        if len(args) < 2:
            raise SchemeException(name + ": requires at least 2 arguments")
        head, tail = args[0], args[1:]
        if not isinstance(head, W_String):
            raise SchemeException(name + ": not given a string")
        for t in tail:
            if not isinstance(t, W_String):
                raise SchemeException(name + ": not given a string")
            if not op(head.as_str_ascii(), t.as_str_ascii()): # XXX do better
                return values.w_false
            head = t
        return values.w_true

# FIXME: Doing case insensitives like this will perform the lower operation
# every time a value is used for a comparison.
def make_ci(op):
    def lower(a, b):
        return op(a.lower(), b.lower())
    return lower

for a in [("string<?", op.lt),
          ("string<=?", op.le),
          ("string=?", op.eq),
          ("string>=?", op.ge),
          ("string>?", op.gt),
          ("string-ci<?", make_ci(op.lt)),
          ("string-ci<=?", make_ci(op.le)),
          ("string-ci=?", make_ci(op.eq)),
          ("string-ci>=?", make_ci(op.ge)),
          ("string-ci>?", make_ci(op.gt)),
          ]:
    define_string_comp(*a)

@expose("make-string", [values.W_Fixnum, default(values.W_Character, None)])
def make_string(k, char):
    if char is None:
        char = '\0'
    else:
        assert ord(char.value) < 128 # XXX for now
        char = chr(ord(char.value))
    return W_String.fromascii(char * k.value)

@expose("string")
def string(args):
    assert len(args) > 0
    builder = StringBuilder()
    for char in args:
        if not isinstance(char, values.W_Character):
            raise SchemeException("string: expected a character")
        if not ord(char.value) < 128:
            raise NotImplementedError("XXX")
        builder.append(chr(ord(char.value)))
    return W_String.fromascii(builder.build())


@expose("string-downcase", [W_String])
def string_downcase(v):
    return v.lower()

@expose("string-upcase", [W_String])
def string_upcase(v):
    return v.upper()


@expose("string-append")
@jit.unroll_safe
def string_append(args):
    if not args:
        return W_String.fromascii("")
    builder = StringBuilder()
    unibuilder = None
    for a in args:
        if not isinstance(a, W_String):
            raise SchemeException("string-append: expected a string")
        if unibuilder is None:
            try:
                builder.append(a.as_str_ascii())
                continue
            except ValueError:
                unibuilder = UnicodeBuilder()
                unibuilder.append(unicode(builder.build()))
        unibuilder.append(a.as_unicode())
    if unibuilder is None:
        return W_String.fromascii(builder.build())
    else:
        return W_String.fromunicode(unibuilder.build())

@expose("string-length", [W_String])
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
          default(values.W_Fixnum, values.W_Fixnum(0)),
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
@expose("make-bytes", [values.W_Fixnum, default(values.W_Object, values.W_Fixnum(0))])
def make_bytes(length, byte):
    # assert byte_huh(byte) is values.w_true
    if isinstance(byte, values.W_Fixnum):
        v = byte.value
    elif isinstance(byte, values.W_Bignum):
        try:
            v = byte.value.toint()
        except OverflowError:
            assert False
    else:
        assert False
    assert 0 <= v <= 255
    bstr = [chr(v)] * length.value
    return values.W_Bytes(bstr, immutable=False)

@expose("bytes")
def bytes(args):
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
    lens = 0
    for a in args:
        if not isinstance(a, values.W_Bytes):
            raise SchemeException("bytes-append: expected a byte string, but got %s"%a)
        lens += len(a.value)

    val = [' '] * lens # is this the fastest way to do things?
    cnt = 0
    for a in args:
        assert isinstance(a, values.W_Bytes)
        val[cnt:cnt+len(a.value)] = a.value
        cnt += len(a.value)

    return values.W_Bytes(val, immutable=False)

@expose("bytes-length", [values.W_Bytes])
def bytes_length(s1):
    return values.W_Fixnum(len(s1.value))

@expose("bytes-ref", [values.W_Bytes, values.W_Fixnum])
def bytes_ref(s, n):
    return s.ref(n.value)

@expose("bytes-set!", [values.W_Bytes, values.W_Fixnum, values.W_Fixnum])
def bytes_set_bang(s, n, v):
    return s.set(n.value, v.value)

@expose("unsafe-bytes-length", [unsafe(values.W_Bytes)])
def unsafe_bytes_length(s1):
    return values.W_Fixnum(len(s1.value))

@expose("unsafe-bytes-ref", [unsafe(values.W_Bytes), unsafe(values.W_Fixnum)])
def unsafe_bytes_ref(s, n):
    return s.ref(n.value)

@expose("unsafe-bytes-set!", [unsafe(values.W_Bytes),
                              unsafe(values.W_Fixnum),
                              unsafe(values.W_Fixnum)])
def unsafe_bytes_set_bang(s, n, v):
    return s.set(n.value, v.value)


@expose("list->bytes", [values.W_List])
def list_to_bytes(w_list):
    l = values.from_list(w_list)
    ll = [' '] * len(l)
    for (i,x) in enumerate(l):
        if not isinstance(x, values.W_Fixnum):
            raise SchemeException("list->bytes: expected fixnum, got %s"%x)
        if x.value < 0 or x.value >= 256:
            raise SchemeException("list->bytes: expected number between 0 and 255, got %s"%x)
        ll[i] = chr(x.value)
    return values.W_Bytes(ll, immutable=False)

@expose("subbytes",
        [values.W_Bytes, values.W_Fixnum, default(values.W_Fixnum, None)])
def subbytes(w_bytes, w_start, w_end):
    """
    (subbytes bstr start [end]) → bytes?
        bstr : bytes?
        start : exact-nonnegative-integer?
        end : exact-nonnegative-integer? = (bytes-length str)
    """
    bytes = w_bytes.value
    start = w_start.value
    if start > len(bytes) or start < 0:
        raise SchemeException("subbytes: end index out of bounds")
    if w_end is not None:
        end = w_end.value
        if end > len(bytes) or end < 0:
            raise SchemeException("subbytes: end index out of bounds")
    else:
        end = len(bytes)
    if end < start:
        raise SchemeException(
            "subbytes: ending index is smaller than starting index")
    return values.W_Bytes(bytes[start:end], immutable=False)

@expose(["bytes-copy!"],
         [values.W_Bytes, values.W_Fixnum, values.W_Bytes,
          default(values.W_Fixnum, values.W_Fixnum(0)),
          default(values.W_Fixnum, None)])
def bytes_copy_bang(w_dest, w_dest_start, w_src, w_src_start, w_src_end):
    from pycket.interpreter import return_value

    # FIXME: custom ports
    if w_dest.immutable():
        raise SchemeException("bytes-copy!: given immutable bytes")

    dest_start = w_dest_start.value
    dest_len = len(w_dest.value)
    dest_max = (dest_len - dest_start)

    src_start =  w_src_start.value
    src_end = len(w_src.value) if w_src_end is None else w_src_end.value

    assert (src_end-src_start) <= dest_max


    for i in range(0, src_end - src_start):
        w_dest.value[dest_start + i] = w_src.value[src_start + i]

    return values.w_void

def define_bytes_comp(name, op):
    @expose(name)
    def comp(args):
        if len(args) < 2:
            raise SchemeException(name + ": requires at least 2 arguments")
        head, tail = args[0], args[1:]
        if not isinstance(head, values.W_Bytes):
            raise SchemeException(name + ": not given a bytes")
        for t in tail:
            if not isinstance(t, values.W_Bytes):
                raise SchemeException(name + ": not given a bytes")
            if not op(str(head.value), str(t.value)):
                return values.w_false
            head = t
        return values.w_true

for a in [("bytes<?", op.lt),
          ("bytes<=?", op.le),
          ("bytes=?", op.eq),
          ("bytes>=?", op.ge),
          ("bytes>?", op.gt),
          ]:
    define_bytes_comp(*a)

@expose(["bytes->string/locale",
         "bytes->string/utf-8"], [values.W_Bytes,
                                  default(values.W_Object, values.w_false),
                                  default(values.W_Integer, values.W_Fixnum(0)),
                                  default(values.W_Integer, None)])
def string_to_bytes_locale(bytes, errbyte, start, end):
    # FIXME: This ignores the locale
    return W_String.fromstr_utf8(bytes.as_str())

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
