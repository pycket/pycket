
from rpython.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from rpython.rlib.parsing.tree import RPythonVisitor, Symbol, Nonterminal

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

# Workaround, because the visit() methods in rlib.parsing.tree are not RPython
def _do_visit(node, visitor):
    if isinstance(node, Symbol):
        if node.symbol == "STRING":
            return visitor.visit_STRING(node)
        elif node.symbol == "NUMBER":
            return visitor.visit_NUMBER(node)
    if isinstance(node, Nonterminal):
        if node.symbol == "value":
            return visitor.visit_value(node)
        elif node.symbol == "object":
            return visitor.visit_object(node)
        elif node.symbol == "array":
            return visitor.visit_array(node)
        elif node.symbol == "entry":
            return visitor.visit_entry(node)
    assert 0, "Unexpected kind of AST node or symbol"

class ToObjectVisitor(RPythonVisitor):
    def general_visit(self, o):
        assert 0, "All visitor cases should be covered"
    def visit_array(self, arr):
        return [_do_visit(c, self) for c in arr.children]
    def visit_object(self, obj):
        return dict([_do_visit(entry, self) for entry in obj.children])
    def visit_entry(self, entry):
        return (_do_visit(entry.children[0], self), _do_visit(entry.children[1], self))
    def visit_STRING(self, string):
        s = string.token.source
        return s[1:-1] # Strip the " characters
    def visit_NUMBER(self, num):
        return _to_num(num.token.source)
    def general_symbol_visit(self, sym):
        s = sym.token.source
        assert isinstance(s, str)
        if s == "null":
            assert 0, "null not supported in rpython"
        elif s == "true":
            return True
        elif s == "false":
            return False
        assert 0, "unexpected kind of symbol: %s"%sym.token.source

def parse_ast(string):
    ast = parse(string)
    return _ToAST().transform(ast)

def loads(string):
    ast = parse_ast(string)
    obj = _do_visit(ast, ToObjectVisitor())
    return obj
