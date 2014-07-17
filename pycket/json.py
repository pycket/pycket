from rpython.rlib.rstring import StringBuilder, ParseStringError
from rpython.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from rpython.rlib.parsing.tree import Symbol, Nonterminal, RPythonVisitor
from rpython.tool.pairtype import extendabletype
from rpython.rlib.rarithmetic import string_to_int

_json_grammar = """
    STRING: "\\"([^\\"\\\\]|\\\\.)*\\"";
    NUMBER: "\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?";
    TRUE: "true";
    FALSE: "false";
    NULL: "null";
    IGNORE: " |\n";
    value: <STRING> | <NUMBER> | <object> | <array> | <NULL> |
           <TRUE> | <FALSE>;
    object: ["{"] ["}"] | ["{"] (entry [","])* entry ["}"];
    array: ["["] ["]"] | ["["] (value [","])* value ["]"];
    entry: STRING [":"] value;
"""

_regexs, _rules, _ToAST = parse_ebnf(_json_grammar)
parse = make_parse_function(_regexs, _rules, eof=True)

#
# Allow for deeply-nested structures in non-translated mode
# Just pick a large number, as `sys.getrecursionlimit` is not
# consistent between CPython and Pypy, appearantly.
#
import sys
sys.setrecursionlimit(10000)


# Union-Object to represent a json structure in a static way
class JsonBase(object):
    __metaclass__ = extendabletype

    is_string = is_int = is_float = is_bool = is_object = is_array = is_null = False

    def __init__(self):
        raise NotImplementedError("abstract base class")

    def tostring(self):
        raise NotImplementedError("abstract base class")

    def is_primitive(self):
        return False

    def _unpack_deep(self):
        "NON_RPYTHON"

    def value_array(self):
        raise TypeError

    def value_object(self):
        raise TypeError

    def value_string(self):
        raise TypeError

    def value_float(self):
        raise TypeError

class JsonPrimitive(JsonBase):
    def __init__(self):
        pass

    def is_primitive(self):
        return True

class JsonNull(JsonPrimitive):
    is_null = True

    def tostring(self):
        return "null"

    def _unpack_deep(self):
        return None

class JsonFalse(JsonPrimitive):
    is_false = True

    def tostring(self):
        return "false"

    def _unpack_deep(self):
        return False


class JsonTrue(JsonPrimitive):
    is_true = True

    def tostring(self):
        return "true"

    def _unpack_deep(self):
        return True

class JsonInt(JsonPrimitive):
    is_int = True

    def __init__(self, value):
        self.value = value

    def tostring(self):
        return str(self.value)

    def _unpack_deep(self):
        return self.value

class JsonFloat(JsonPrimitive):
    is_float = True

    def __init__(self, value):
        self.value = value

    def tostring(self):
        return str(self.value)

    def value_float(self):
        return self.value

    def _unpack_deep(self):
        return self.value

class JsonString(JsonPrimitive):
    is_string = True

    def __init__(self, value):
        self.value = value

    def tostring(self):
        # this function should really live in a slightly more accessible place
        from pypy.objspace.std.bytesobject import string_escape_encode
        return string_escape_encode(self.value, '"')

    def _unpack_deep(self):
        return self.value

    def value_string(self):
        return self.value

class JsonObject(JsonBase):
    is_object = True

    def __init__(self, dct):
        self.value = dct

    def tostring(self):
        return "{%s}" % ", ".join(["\"%s\": %s" % (key, self.value[key].tostring()) for key in self.value])

    def _unpack_deep(self):
        result = {}
        for key, value in self.value.iteritems():
            result[key] = value._unpack_deep()
        return result

    def value_object(self):
        return self.value

class JsonArray(JsonBase):
    is_array = True

    def __init__(self, lst):
        self.value = lst

    def tostring(self):
        return "[%s]" % ", ".join([e.tostring() for e in self.value])

    def _unpack_deep(self):
        return [e._unpack_deep() for e in self.value]

    def value_array(self):
        return self.value

json_null = JsonNull()

json_true = JsonTrue()

json_false = JsonFalse()

class Visitor(RPythonVisitor):
    def visit_STRING(self, node):
        s = node.token.source
        l = len(s) - 1
        # Strip the " characters
        if l < 0:
            return JsonString("")
        else:
            return JsonString(unescape(s)) # XXX escaping

    def visit_NUMBER(self, node):
        try:
            return JsonInt(string_to_int(node.token.source))
        except ParseStringError:
            return JsonFloat(float(node.token.source))

    def visit_NULL(self, node):
        return json_null

    def visit_TRUE(self, node):
        return json_true

    def visit_FALSE(self, node):
        return json_false

    def visit_object(self, node):
        d = {}
        for entry in node.children:
            key = self.dispatch(entry.children[0])
            if not key.is_string:
                assert 0, "Only strings allowed as object keys"
            d[key.value_string()] = self.dispatch(entry.children[1])
        return JsonObject(d)

    def visit_array(self, node):
        return JsonArray([self.dispatch(c) for c in node.children])

def unescape(chars):
    builder = StringBuilder(len(chars)*2) # just an estimate
    assert chars[0] == '"'
    i = 1
    while True:
        ch = chars[i]
        i += 1
        if ch == '"':
            return builder.build()
        elif ch == '\\':
            i = decode_escape_sequence(i, chars, builder)
        else:
            builder.append(ch)

def decode_escape_sequence(i, chars, builder):
    ch = chars[i]
    i += 1
    put = builder.append
    if ch == '\\':  put('\\')
    elif ch == '"': put('"' )
    elif ch == '/': put('/' )
    elif ch == 'b': put('\b')
    elif ch == 'f': put('\f')
    elif ch == 'n': put('\n')
    elif ch == 'r': put('\r')
    elif ch == 't': put('\t')
    elif ch == 'u':
        # TODO: make this work with actual unicode characters
        val = int(chars[i:i+4], 16)
        put(chr(val))
        i += 4
    else:
        raise ValueError("Invalid \\escape: %s" % (ch, ))
    return i

def loads(string):
    ast = parse(string)
    transformed = _ToAST().transform(ast)
    return Visitor().dispatch(transformed)
