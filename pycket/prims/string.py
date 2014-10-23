#! /usr/bin/env python
# -*- coding: utf-8 -*-
import operator as op
from pycket import values
from pycket.error import SchemeException
from pycket.prims.expose import default, expose, unsafe, subclass_unsafe
from rpython.rlib.unicodedata import unicodedb_6_2_0 as unicodedb
from rpython.rlib.rstring     import StringBuilder
from rpython.rlib import jit


@expose("symbol->string", [values.W_Symbol])
def symbol_to_string(v):
    return values.W_String(v.value)

@expose("string->symbol", [values.W_String])
def string_to_symbol(v):
    return values.W_Symbol.make(v.value)

@expose("string->number", [values.W_String])
def str2num(w_s):
    from rpython.rlib import rarithmetic, rfloat, rbigint
    from rpython.rlib.rstring import ParseStringError, ParseStringOverflowError

    s = w_s.value
    try:
        if "." in s:
            return values.W_Flonum(rfloat.string_to_float(s))
        else:
            try:
                return values.W_Fixnum(rarithmetic.string_to_int(s, base=0))
            except ParseStringOverflowError:
                return values.W_Bignum(rbigint.rbigint.fromstr(s))
    except ParseStringError as e:
        return values.w_false

@expose("number->string",
        [values.W_Number, default(values.W_Fixnum, values.W_Fixnum(10))])
def num2str(a, radix):
    from rpython.rlib.rbigint import BASE8, BASE16
    if radix.value == 10:
        return values.W_String(a.tostring())
    else:
        if isinstance(a, values.W_Fixnum):
            if radix.value == 16:
                res = hex(a.value)
                if a.value >= 0:
                    res = res[2:]
                else:
                    res = "-" + res[3:]
                return values.W_String(res)
            #elif radix.value == 8:
            #    return values.W_String(oct(a.value))
            # elif radix.value == 2:
            #     return values.W_String(bin(a.value))
            else:
                raise SchemeException("number->string: radix unsupported")
        elif isinstance(a, values.W_Bignum):
            if radix.value == 16:
                return values.W_String(a.value.format(BASE16))
            elif radix.value == 8:
                return values.W_String(a.value.format(BASE8))
            elif radix.value == 2:
                return values.W_String(a.value.format("01"))
            else:
                raise SchemeException("number->string: radix unsupported")
        elif isinstance(a, values.W_Flonum):
            raise SchemeException("number->string: flonum only supports radix 10")
        else:
            assert 0 # not reached


@expose("string->unreadable-symbol", [values.W_String])
def string_to_unsymbol(v):
    return values.W_Symbol.make_unreadable(v.value)

@expose("string->immutable-string", [values.W_String])
def string_to_immutable_string(string):
    if string.immutable():
        return string
    return values.W_String(string.value, immutable=True)

@expose("string->uninterned-symbol", [values.W_String])
def string_to_symbol(v):
    return values.W_Symbol(v.value)

@expose(["string->bytes/locale",
         "string->bytes/utf-8"], [values.W_String,
                                  default(values.W_Object, values.w_false),
                                  default(values.W_Integer, values.W_Fixnum(0)),
                                  default(values.W_Integer, None)])
def string_to_bytes_locale(str, errbyte, start, end):
    # FIXME: This ignores the locale
    # FIXME: these are both wrong to some extend
    return values.W_Bytes.from_string(str.value)

@expose("string->list", [values.W_String])
def string_to_list(s):
    return values.to_list([values.W_Character(i) for i in s.value])

@expose("list->string", [values.W_List])
def list_to_string(w_list):
    l = values.from_list(w_list)
    return string(l)


##################################

def define_string_comp(name, op):
    @expose(name)
    def comp(args):
        if len(args) < 2:
            raise SchemeException(name + ": requires at least 2 arguments")
        head, tail = args[0], args[1:]
        if not isinstance(head, values.W_String):
            raise SchemeException(name + ": not given a string")
        for t in tail:
            if not isinstance(t, values.W_String):
                raise SchemeException(name + ": not given a string")
            if not op(head.value, t.value):
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

@expose("make-string", [values.W_Fixnum, default(values.W_Character, values.w_null)])
def make_string(k, char):
    char = str(char.value) if isinstance(char, values.W_Character) else '\0'
    return values.W_String(char * k.value)

@expose("string")
def string(args):
    if len(args) == 0:
        return values.W_String("")
    assert len(args) > 0
    builder = StringBuilder()
    for char in args:
        if not isinstance(char, values.W_Character):
            raise SchemeException("string: expected a character")
        builder.append(str(char.value))
    return values.W_String(builder.build())


@expose("string-downcase", [values.W_String])
def char_downcase(v):
    return values.W_String(v.value.lower())

@expose("string-upcase", [values.W_String])
def char_downcase(v):
    return values.W_String(v.value.upper())



@expose("string-append")
@jit.unroll_safe
def string_append(args):
    if not args:
        return values.W_String("")
    builder = StringBuilder()
    for a in args:
        if not isinstance(a, values.W_String):
            raise SchemeException("string-append: expected a string")
        builder.append(a.value)
    return values.W_String(builder.build())

@expose("string-length", [values.W_String])
def string_length(s1):
    return values.W_Fixnum(len(s1.value))

@expose("substring", [values.W_String, values.W_Fixnum, default(values.W_Fixnum, None)])
def substring(w_string, w_start, w_end):
    """
    (substring str start [end]) → string?
        str : string?
        start : exact-nonnegative-integer?
        end : exact-nonnegative-integer? = (string-length str)
    """
    string = w_string.value
    start = w_start.value
    if start > len(string) or start < 0:
        raise SchemeException("substring: end index out of bounds")
    if w_end is not None:
        end = w_end.value
        if end > len(string) or end < 0:
            raise SchemeException("substring: end index out of bounds")
    else:
        end = len(string)
    if end < start:
        raise SchemeException(
            "substring: ending index is smaller than starting index")
    return values.W_String(string[start:end])

@expose("string-ref", [values.W_String, values.W_Fixnum])
def string_ref(s, n):
    idx = n.value
    st  = s.value
    if idx < 0 or idx >= len(st):
        raise SchemeException("string-ref: index out of range")
    return values.W_Character(st[idx])

@expose("string-set!", [values.W_String, values.W_Fixnum, values.W_Character])
def string_set(str, k, char):
    if str.immutable():
        raise SchemeException("string-set!: given immutable string")
    idx = k.value
    v = [i for i in str.value]
    if not (0 <= idx < len(v)):
        raise SchemeException("string-set!: given index is out of range")
    v[idx] = char.value
    str.value = "".join(v)
    return values.w_void


@expose(["string-copy!"],
         [values.W_String, values.W_Fixnum, values.W_String,
          default(values.W_Fixnum, values.W_Fixnum(0)),
          default(values.W_Fixnum, None)])
def string_copy_bang(w_dest, w_dest_start, w_src, w_src_start, w_src_end):
    from pycket.interpreter import return_value

    # FIXME: custom ports
    if w_dest.immutable():
        raise SchemeException("string-copy!: given immutable string")

    dest_start = w_dest_start.value
    dest_len = len(w_dest.value)
    dest_max = (dest_len - dest_start)

    src_start =  w_src_start.value
    src_end = len(w_src.value) if w_src_end is None else w_src_end.value

    assert (src_end-src_start) <= dest_max

    builder = StringBuilder()
    if dest_start > 0:
        builder.append_slice(w_dest.value, 0, dest_start)

    if src_start == 0 and src_end == len(w_src.value):
        builder.append(w_src.value)
    else:
        assert src_start >= 0 and src_end >= 0 and src_end <= len(w_src.value)
        builder.append_slice(w_src.value, src_start, src_end)

    builder.append_slice(w_dest.value, dest_start + (src_end - src_start), len(w_dest.value))
    w_dest.value = builder.build()

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
    lens = 0
    for a in args:
        if not isinstance(a, values.W_Bytes):
            raise SchemeException(
                "bytes-append: expected a byte string, but got %s" % a)
        lens += len(a.value)

    val = [' '] * lens # is this the fastest way to do things?
    cnt = 0
    for a in args:
        assert isinstance(a, values.W_Bytes)
        val[cnt:cnt+len(a.value)] = a.value
        cnt += len(a.value)

    return values.W_MutableBytes(val)

@expose("bytes-length", [values.W_Bytes])
def bytes_length(s1):
    return values.W_Fixnum(len(s1.value))

@expose("bytes-ref", [values.W_Bytes, values.W_Fixnum])
def bytes_ref(s, n):
    return s.ref(n.value)

@expose("bytes-set!", [values.W_Bytes, values.W_Fixnum, values.W_Fixnum])
def bytes_set_bang(s, n, v):
    return s.set(n.value, v.value)

@expose("unsafe-bytes-length", [subclass_unsafe(values.W_Bytes)])
def unsafe_bytes_length(s1):
    return values.W_Fixnum(len(s1.value))

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
    l = values.from_list(w_list)
    ll = [' '] * len(l)
    for (i,x) in enumerate(l):
        if not isinstance(x, values.W_Fixnum):
            raise SchemeException("list->bytes: expected fixnum, got %s" % x)
        if x.value < 0 or x.value >= 256:
            raise SchemeException(
                "list->bytes: expected number between 0 and 255, got %s" % x)
        ll[i] = chr(x.value)
    return values.W_MutableBytes(ll)

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
    return values.W_MutableBytes(bytes[start:end])

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
    # FIXME: these are both wrong to some extend
    return values.W_String(bytes.as_str())

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
def char_downcase(v):
    return values.W_Character(unichr(unicodedb.toupper(ord(v.value))))

@expose("char=?")
def char_equal_huh(w_args):
    if len(w_args) < 2:
        raise SchemeException("char=?: requires at least 2 arguments")
    w_comparand = w_args[0]
    assert isinstance(w_comparand, values.W_Character)
    comparand = w_comparand.value
    w_res = values.w_true
    for w_arg in w_args:
        assert isinstance(w_arg, values.W_Character)
        if not w_arg.value == comparand:
            w_res = values.w_false
    return w_res
