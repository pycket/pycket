#! /usr/bin/env python
# -*- coding: utf-8 -*-
import operator as op
from pycket import values
from pycket.error import SchemeException
from pycket.prims.expose import default, expose, unsafe
from rpython.rlib.unicodedata import unicodedb_6_2_0 as unicodedb
from rpython.rlib.rstring     import StringBuilder


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
                return values.W_String(hex(a.value))
            elif radix.value == 8:
                return values.W_String(oct(a.value))
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
    return values.W_Bytes(str.value)

@expose("string->list", [values.W_String])
def string_to_list(s):
    return values.to_list([values.W_Character(i) for i in s.value])


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



# FIXME: this implementation sucks
@expose("string-append")
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
    bstr = chr(v) * length.value
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
    return values.W_Bytes(builder.build(), immutable=False)

@expose("bytes-append")
def bytes_append(args):
    if len(args) < 1:
        raise SchemeException("error bytes-append")

    # shortcut
    first = args[0]
    assert isinstance(first, values.W_Bytes)
    if len(args) == 1:
        return values.W_Bytes(first.value, immutable=False)

    builder = StringBuilder()
    for a in args:
        if not isinstance(a, values.W_Bytes):
            raise SchemeException("string-append: expected a byte string")
        builder.append(a.value)

    return values.W_Bytes(builder.build(), immutable=False)

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
    sb = StringBuilder(len(l))
    for w_c in l:
        assert isinstance(w_c, values.W_Fixnum)
        assert 0 <= w_c.value <= 255
        sb.append(chr(w_c.value))
    return values.W_Bytes(sb.build(), immutable=False)

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
