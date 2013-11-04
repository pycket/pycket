class W_Object:
    pass

class W_Cons(W_Object):
    def __init__(self, a, d):
        self.car = a
        self.cdr = d

class W_Fixnum(W_Object):
    def __init__(self, val):
        self.value = val

class W_String(W_Object):
    def __init__(self, val):
        self.value = val

class W_Symbol(W_Object):
    def __init__(self, val):
        self.value = val

class AST:
    pass

class Quote (AST):
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret (self):
        return self.w_val

class App (AST):
    def __init__ (self, rator, rands):
        self.rator = rator
        self.rands = rands

class Var (AST):
    def __init__ (self, name):
        self.name = name

class If (AST):
    def __init__ (self, tst, thn, els):
        self.tst = tst
        self.thn = thn
        self.els = els

def to_ast(json):
    if isinstance(json, list):
        if json[0] == {"symbol": "#%app"}:
            return App(to_ast(json[1]), [to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "if"}:
            return If(to_ast(json[1]), to_ast(json[2]),  to_ast(json[3]))
        if json[0] == {"symbol": "quote"}:
            return Quote(to_value(json[1]))

def to_value(json):
    if isinstance (json, dict):
        if "integer" in json:
            return W_Fixnum(int(json["integer"]))
        

def interpret(ast):
    return ast.interpret()
