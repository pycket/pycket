from pycket.values import W_Fixnum
from pycket.prims  import prim_env

class Cont:
    pass

class Call(Cont):
    # prev is the parent continuation
    def __init__ (self, vals_w, rest, prev):
        self.vals_w = vals_w
        self.rest = rest
        self.prev = prev
    def plug(self, w_val):
        if not self.rest:
            vals_w = self.vals_w + [w_val]
            return vals_w[0].call(vals_w[1:], self.prev)
        else:
            return self.rest[0], Call(self.vals_w + [w_val], self.rest[1:], self.prev)
        
class Done(Exception):
    def __init__(self, w_val):
        self.w_val = w_val

class AST:
    pass

class Value (AST):
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret (self, frame):
        if frame is None: raise Done(self.w_val)
        return frame.plug(self.w_val)


class Quote (AST):
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret (self, frame):
        return Value(self.w_val), frame

class App (AST):
    def __init__ (self, rator, rands):
        self.rator = rator
        self.rands = rands
    def interpret (self, frame):
        return self.rator, Call([], self.rands, frame)

class Var (AST):
    def __init__ (self, name):
        self.name = name
    def interpret(self, frame):
        if self.name in prim_env:
            return Value(prim_env[self.name]), frame
        else:
            raise Exception ("unbound variable")

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
        assert 0
    if isinstance(json, dict):
        if "symbol" in json:
            return Var(json["symbol"])
        assert 0
    assert 0

def to_value(json):
    if isinstance (json, dict):
        if "integer" in json:
            return W_Fixnum(int(json["integer"]))
        

def interpret(ast):
    frame = None
    #import pdb; pdb.set_trace()
    try:
        while True:
            ast, frame = ast.interpret(frame)
    except Done, e:
        return e.w_val
