
from rpython.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from rpython.rlib.parsing.tree import RPythonVisitor

_json_grammar = """
    STRING: "\\"[^\\\\"]*\\"";
    NUMBER: "\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?";
    IGNORE: " |\n";
    value: <STRING> | <NUMBER> | <object> | <array> | <"null"> |
           <"true"> | <"false">;
    object: ["{"] ["}"] | ["{"] (entry [","])* entry ["}"];
    array: ["["] ["]"] | ["["] (value [","])* value ["]"];
    entry: STRING [":"] value;
"""

_regexs, _rules, _ToAST = parse_ebnf(_json_grammar)
parse = make_parse_function(_regexs, _rules, eof=True)

def _to_num(s):
    try:
        return int(s)
    except ValueError:
        return float(s)

class ToObjectVisitor(RPythonVisitor):
    def general_visit(self, o):
        assert 0, "All visitor cases should be covered"
    def visit_array(self, arr):
        return [c.visit(self) for c in arr.children]
    def visit_object(self, obj):
        return dict([entry.visit(self) for entry in obj.children])
    def visit_entry(self, entry):
        return (entry.children[0].visit(self), entry.children[1].visit(self))
    def visit_STRING(self, string):
        s = string.token.source
        return s[1:len(s)-1] # Strip the " characters
    def visit_NUMBER(self, num):
        return _to_num(num.token.source)
    def general_symbol_visit(self, sym):
        if sym.token.source == "null":
            return None
        elif sym.token.source == "true":
            return True
        elif sym.token.source == "false":
            return False
        assert 0, "unexpected kind of symbol: %s"%sym.token.source

def parse_ast(string):
    ast = parse(string)
    return _ToAST().transform(ast)

def loads(string):
    ast = parse_ast(string)
    obj = ast.visit(ToObjectVisitor())
    return obj
