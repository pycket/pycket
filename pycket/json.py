
from rpython.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from rpython.rlib.parsing.tree import Symbol, Nonterminal

_json_grammar = """
    STRING: "\\"[^\\\\"]*\\"";
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

# Union-Object to represent a json structure in a static way
class JsonObject(object):
    is_string = is_int = is_float = is_bool = is_object = is_array = is_null = False
    value_string = value_object = value_array = None
    value_int = 0
    value_bool = False
    value_float = 0.0
    
    @staticmethod
    def new_string(str):
        obj = JsonObject()
        obj.is_string = True
        obj.value_string = str
        return obj
    
    @staticmethod
    def new_object(dict):
        obj = JsonObject()
        obj.is_object = True
        obj.value_object = dict
        return obj
    
    @staticmethod
    def new_array(arr):
        obj = JsonObject()
        obj.is_array = True
        obj.value_array = arr
        return obj
    
    @staticmethod
    def new_int(i):
        obj = JsonObject()
        obj.is_int = True
        obj.value_int = i
        return obj
    
    @staticmethod
    def new_float(f):
        obj = JsonObject()
        obj.is_float = True
        obj.value_float = f
        return obj
    
    def tostring(self):
        if self.is_string:
            return "\"%s\"" % self.value_string
        elif self.is_int:
            return str(self.value_int)
        elif self.is_float:
            return str(self.value_float)
        elif self.is_bool:
            return str(self.value_bool)
        elif self.is_object:
            return "{%s}" % ", ".join(["\"%s\": %s" % (key, self.value_object[key].tostring()) for key in self.value_object])
        elif self.is_array:
            return "[%s]" % ", ".join([e.tostring() for e in self.value_array])
        elif self.is_null:
            return "null"
        else:
            return "<unknown json object>"
    
    def is_primitive(self):
        return self.is_string or self.is_int or self.is_float or self.is_null or self.is_bool
    
    def unpack(self):
        "NON_RPYTHON"
        if self.is_string:
            return self.value_string
        elif self.is_int:
            return self.value_int
        elif self.is_float:
            return self.value_float
        elif self.is_bool:
            return self.value_bool
        elif self.is_object:
            return self.value_object
        elif self.is_array:
            return self.value_array
        elif self.is_null:
            return None
        assert 0, "Illegal JsonObject instance!"

    def unpack_deep(self):
        "NON_RPYTHON"
        if self.is_primitive():
            return self.unpack()
        elif self.is_object:
            result = {}
            for key in self.value_object:
                result[key] = self.value_object[key].unpack_deep()
            return result
        elif self.is_array:
            return [e.unpack_deep() for e in self.value_array]
        assert 0, "Illegal JsonObject instance!"

json_null = JsonObject()
json_null.is_null = True

json_true = JsonObject()
json_true.is_bool = True
json_true.value_bool = True

json_false = JsonObject()
json_false.is_bool = True
json_false.value_bool = False

# Workaround, because the visit() methods in rlib.parsing.tree are not RPython
def _to_JsonObject(node):
    if isinstance(node, Symbol):
        if node.symbol == "STRING":
            s = node.token.source
            l = len(s) - 1
            # Strip the " characters
            if l < 0:
                return JsonObject.new_string("")
            else:
                return JsonObject.new_string(s[1:l])
        elif node.symbol == "NUMBER":
            try:
                return JsonObject.new_int(int(node.token.source))
            except ValueError:
                return JsonObject.new_float(float(node.token.source))
        elif node.symbol == "NULL":
            return json_null
        elif node.symbol == "TRUE":
            return json_true
        elif node.symbol == "FALSE":
            return json_false
        assert 0, "Unexpected symbol: %s" % node.symbol
    elif isinstance(node, Nonterminal):
        if node.symbol == "object":
            d = {}
            for entry in node.children:
                key = _to_JsonObject(entry.children[0])
                if not key.is_string:
                    assert 0, "Only strings allowed as object keys"
                d[key.value_string] = _to_JsonObject(entry.children[1])
            return JsonObject.new_object(d)
        elif node.symbol == "array":
            return JsonObject.new_array([_to_JsonObject(c) for c in node.children])
        else:
            assert 0, "Unexpected kind of nonterminal: %s" % node.symbol
    assert 0, "Unexpected kind of AST node: %s" % node.symbol

def loads(string):
    ast = parse(string)
    transformed = _ToAST().transform(ast)
    return _to_JsonObject(transformed)
