from pycket.values import W_Fixnum, W_Closure, W_Symbol, w_true, w_false
from pycket.prims  import prim_env

class Env:
    pass

class EmptyEnv(Env):
    def __init__ (self):
        pass
    def lookup(self, sym):
        if sym in prim_env:
            return prim_env[sym]
        else:
            raise Exception ("unbound variable")
        
class ConsEnv(Env):
    def __init__ (self, syms, vals, prev):
        self.syms = syms
        self.vals = vals
        self.prev = prev
    def lookup(self, sym):
        for i, s in enumerate(self.syms):
            if s is sym:
                return self.vals[i]
        return self.prev.lookup(sym)
    

class Cont:
    pass

class IfCont(Cont):
    def __init__(self, thn, els, env, prev):
        self.thn = thn
        self.els = els
        self.env = env
        self.prev = prev
    def plug(self, w_val):
        if w_val is w_false:
            return self.els, env, prev
        else:
            return self.thn, env, prev

class Call(Cont):
    # prev is the parent continuation
    def __init__ (self, vals_w, rest, env, prev):
        self.vals_w = vals_w
        self.rest = rest
        self.env = env
        self.prev = prev
    def plug(self, w_val):
        if not self.rest:
            vals_w = self.vals_w + [w_val]
            return vals_w[0].call(vals_w[1:], self.env, self.prev)
        else:
            return self.rest[0], self.env, Call(self.vals_w + [w_val], self.rest[1:], 
                                                self.env, self.prev)

def make_begin(exprs, env, prev):
    assert exprs
    if len(exprs) == 1:
        return exprs[0], env, prev
    else:
        return exprs[0], env, Begin(exprs[1:], env, prev)

class Begin(Cont):
    def __init__(self, rest, env, prev):
        assert rest
        self.rest = rest
        self.env = env
        self.prev = prev
    def plug(self, w_val):
        return make_begin(self.rest, self.env, self.prev)
        
class Done(Exception):
    def __init__(self, w_val):
        self.w_val = w_val

class AST:
    pass

class Value (AST):
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret (self, env, frame):
        if frame is None: raise Done(self.w_val)
        return frame.plug(self.w_val)


class Quote (AST):
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret (self, env, frame):
        return Value(self.w_val), env, frame

class App (AST):
    def __init__ (self, rator, rands):
        self.rator = rator
        self.rands = rands
    def interpret (self, env, frame):
        return self.rator, env, Call([], self.rands, env, frame)

class Var (AST):
    def __init__ (self, sym):
        self.sym = sym
    def interpret(self, env, frame):
        return Value(env.lookup(self.sym)), env, frame

class Lambda (AST):
    def __init__ (self, formals, body):
        self.formals = formals
        self.body = body
    def interpret (self, env, frame):
        return Value(W_Closure (self, env)), env, frame

class If (AST):
    def __init__ (self, tst, thn, els):
        self.tst = tst
        self.thn = thn
        self.els = els
    def interpret(self, env, frame):
        return self.tst, env, IfCont(self.thn, self.els, env, frame)

def to_formals (json):
    return [W_Symbol.make(x["symbol"]) for x in json]

def to_ast(json):
    if isinstance(json, list):
        if json[0] == {"symbol": "#%app"}:
            return App(to_ast(json[1]), [to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "if"}:
            return If(to_ast(json[1]), to_ast(json[2]),  to_ast(json[3]))
        if json[0] == {"symbol": "quote"}:
            return Quote(to_value(json[1]))
        if json[0] == {"symbol": "lambda"}:
            return Lambda(to_formals(json[1]), [to_ast(x) for x in json[2:]])
        assert 0
    if json is False:
        return w_false
    if json is True:
        return w_true
    if isinstance(json, dict):
        if "symbol" in json:
            return Var(W_Symbol.make(json["symbol"]))
        assert 0
    assert 0

def to_value(json):
    if isinstance (json, dict):
        if "integer" in json:
            return W_Fixnum(int(json["integer"]))
        

def interpret(ast):
    frame = None
    env = EmptyEnv()
    #import pdb; pdb.set_trace()
    try:
        while True:
            ast, env, frame = ast.interpret(env, frame)
    except Done, e:
        return e.w_val
