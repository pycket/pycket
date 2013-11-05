from pycket import values
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
            raise Exception ("variable %s is unbound"%sym.value)

class ConsEnv(Env):
    def __init__ (self, syms, vals, prev):
        for i in syms:
            assert isinstance (i, values.W_Symbol)
        self.syms = syms
        self.vals = vals
        self.prev = prev
    def add(self, sym, val):
        self.syms = [sym] + self.syms
        self.vals = [val] + self.vals
    def lookup(self, sym):
        for i, s in enumerate(self.syms):
            if s is sym:
                return self.vals[i]
        return self.prev.lookup(sym)
    def set(self, sym, val):
        for i, s in enumerate(self.syms):
            if s is sym:
                self.vals[i] = val
                return
        return self.prev.set(sym, val)

class Cont:
    pass

class IfCont(Cont):
    def __init__(self, thn, els, env, prev):
        self.thn = thn
        self.els = els
        self.env = env
        self.prev = prev
    def plug_reduce(self, w_val):
        if w_val is values.w_false:
            return self.els, self.env, self.prev
        else:
            return self.thn, self.env, self.prev

class LetrecCont(Cont):
    def __init__(self, vars, rest, body, env, prev):
        self.vars  = vars
        self.rest = rest
        self.body = body
        self.env  = env
        self.prev = prev
    def plug_reduce(self, w_val):
        self.env.set(self.vars[- (len (self.rest) + 1)], w_val)
        if not self.rest:
            return make_begin(self.body, self.env, self.prev)
        else:
            return (self.rest[0], self.env, 
                    LetrecCont(self.vars, self.rest[1:], 
                               self.body, self.env, self.prev))

class LetCont(Cont):
    def __init__(self, vars, vals_w, rest, body, env, prev):
        self.vars  = vars
        self.vals_w  = vals_w
        self.rest = rest
        self.body = body
        self.env  = env
        self.prev = prev
    def plug_reduce(self, w_val):
        if not self.rest:
            vals_w = self.vals_w + [w_val]
            env = ConsEnv(self.vars, vals_w, self.env)
            return make_begin(self.body, env, self.prev)
        else:
            return (self.rest[0], self.env, 
                    LetCont(self.vars, self.vals_w + [w_val], self.rest[1:], 
                            self.body, self.env, self.prev))

class Call(Cont):
    # prev is the parent continuation
    def __init__ (self, vals_w, rest, env, prev):
        self.vals_w = vals_w
        self.rest = rest
        self.env = env
        self.prev = prev
    def plug_reduce(self, w_val):
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
        return exprs[0], env, BeginCont(exprs[1:], env, prev)

class SetBangCont(Cont):
    def __init__(self, var, env, prev):
        self.var = var
        self.env = env
        self.prev = prev
    def plug_reduce(self, w_val):
        self.env.set(self.var, w_val)
        return Value(values.w_void), self.env, self.prev

class BeginCont(Cont):
    def __init__(self, rest, env, prev):
        assert rest
        self.rest = rest
        self.env = env
        self.prev = prev
    def plug_reduce(self, w_val):
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
        return frame.plug_reduce(self.w_val)
    def __repr__(self):
        return "V(%r)"%self.w_val

class Quote (AST):
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret (self, env, frame):
        return Value(self.w_val), env, frame
    def __repr__(self):
        return "Quote(%r)"%self.w_val

class App (AST):
    def __init__ (self, rator, rands):
        self.rator = rator
        self.rands = rands
    def interpret (self, env, frame):
        return self.rator, env, Call([], self.rands, env, frame)
    def __repr__(self):
        return "(%r %r)"%(self.rator, self.rands)

class Begin(AST):
    def __init__(self, exprs):
        self.exprs = exprs
    def interpret(self, env, frame):
        return make_begin(self.exprs, env, frame)
    def __repr__(self):
        return "(begin %r)" % self.exprs

class Var (AST):
    def __init__ (self, sym):
        self.sym = sym
    def interpret(self, env, frame):
        return Value(env.lookup(self.sym)), env, frame
    def __repr__(self):
        return "%s"%self.sym.value

class SetBang (AST):
    def __init__(self, var, rhs):
        self.var = var
        self.rhs = rhs
    def interpret (self, env, frame):
        return self.rhs, env, SetBangCont(self.var, env, frame)
    def __repr__(self):
        return "(set! %r %r)"%(self.var, self.rhs)

class Lambda (AST):
    def __init__ (self, formals, rest, body):
        self.formals = formals
        self.rest = rest
        self.body = body
    def interpret (self, env, frame):
        return Value(values.W_Closure (self, env)), env, frame
    def __repr__(self):
        if self.rest and (not self.formals):
            return "(lambda %r %r)"%(self.rest, self.body)
        if self.rest:
            return "(lambda (%r . %r) %r)"%(self.formals, self.rest, self.body)
        else:
            return "(lambda (%r) %r)"%(self.formals, self.body)


class Letrec(AST):
    def __init__(self, vars, rhss, body):
        self.vars = vars
        self.rhss = rhss
        self.body = body
    def interpret (self, env, frame):
        env_new = ConsEnv(self.vars, [None]*len(self.vars), env)
        return self.rhss[0], env_new, LetrecCont(self.vars, self.rhss[1:], self.body, env_new, frame)
    def __repr__(self):
        return "(letrec (%r) %r)"%(zip(self.vars, self.rhss), self.body)

class Let(AST):
    def __init__(self, vars, rhss, body):
        self.vars = vars
        self.rhss = rhss
        self.body = body
    def interpret (self, env, frame):
        return self.rhss[0], env, LetCont(self.vars, [], self.rhss[1:], self.body, env, frame)
    def __repr__(self):
        return "(let (%r) %r)"%(zip(self.vars, self.rhss), self.body)


class If (AST):
    def __init__ (self, tst, thn, els):
        self.tst = tst
        self.thn = thn
        self.els = els
    def interpret(self, env, frame):
        return self.tst, env, IfCont(self.thn, self.els, env, frame)
    def __repr__(self):
        return "(if %r %r %r)"%(self.tst, self.thn, self.els)

class Define(AST):
    def __init__(self, name, rhs):
        self.name = name
        self.rhs = rhs
    def __repr__(self):
        return "(define %r %r)"%(self.name, self.rhs)

def to_formals (json):
    if "improper" in json:
        regular, last = json["improper"]
        return [values.W_Symbol.make(str(x["symbol"])) for x in regular], values.W_Symbol.make(str(last["symbol"]))
    elif isinstance (json, list):
        return [values.W_Symbol.make(str(x["symbol"])) for x in json], None
    elif "symbol" in json:
        return [], values.W_Symbol.make(str(json["symbol"]))
    assert 0


def to_bindings(json):
    def to_binding(j):
        fmls, rest = to_formals(j[0])
        assert not rest
        assert len (fmls) == 1
        return (fmls[0], to_ast(j[1])) # this is bad for multiple values
    l  = [to_binding(x) for x in json]
    return zip(*l)

def to_ast(json):
    if isinstance(json, list):
        if json[0] == {"symbol": "begin"}:
            return Begin([to_ast(x) for x in json[1:]])
        if json[0] == {"symbol": "#%app"}:
            return App(to_ast(json[1]), [to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "if"}:
            return If(to_ast(json[1]), to_ast(json[2]),  to_ast(json[3]))
        if json[0] == {"symbol": "quote"}:
            return Quote(to_value(json[1]))
        if json[0] == {"symbol": "lambda"}:
            fmls, rest = to_formals(json[1])
            return Lambda(fmls, rest, [to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "letrec-values"}:
            vars, rhss = to_bindings(json[1])
            return Letrec(list(vars), list(rhss), [to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "let-values"}:
            vars, rhss = to_bindings(json[1])
            return Let(vars, rhss, [to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "set!"}:
            return SetBang(values.W_Symbol.make(str(json[1]["symbol"])), to_ast(json[2]))
        if json[0] == {"symbol": "#%top"}:
            return Var(values.W_Symbol.make(str(json[1]["symbol"])))
        if json[0] == {"symbol": "define-values"}:
            fmls, rest = to_formals(json[1])
            assert not rest
            assert len(fmls) == 1
            return Define(fmls[0],to_ast(json[2]))
        if json[0] == {"symbol": "quote-syntax"}:
            raise Exception ("quote-syntax is unsupported")
        if json[0] == {"symbol": "begin0"}:
            raise Exception ("begin0 is unsupported")
        if json[0] == {"symbol": "with-continuation-mark"}:
            raise Exception ("with-continuation-mark is unsupported")
        if json[0] == {"symbol": "#%variable-reference"}:
            raise Exception ("#%variable-reference is unsupported")
        if json[0] == {"symbol": "case-lambda"}:
            raise Exception ("case-lambda is unsupported")
        assert 0
    if isinstance(json, dict):
        if "symbol" in json:
            return Var(values.W_Symbol.make(str(json["symbol"])))
        assert 0
    assert 0

def to_value(json):
    if json is False:
        return values.w_false
    if json is True:
        return values.w_true
    if isinstance (json, dict):
        if "vector" in json:
            return values.W_Vector(json["vector"])
        if "integer" in json:
            return values.W_Fixnum(int(json["integer"]))
        if "real" in json:
            return values.W_Flonum(float(json["real"]))
        else: assert 0
        

def interpret_one(ast, env=EmptyEnv()):
    frame = None
    #import pdb; pdb.set_trace()
    try:
        while True:
            ast, env, frame = ast.interpret(env, frame)
    except Done, e:
        return e.w_val

def interpret_toplevel(a, env):
    if isinstance(a, Begin):
        x = None
        for a2 in a.exprs:
            x = interpret_toplevel(a2, env)
        return x
    elif isinstance(a, Define):
        env.add(a.name, None)
        env.set(a.name, interpret_one(a.rhs, env))
        return values.w_void
    else:
        return interpret_one(a, env)
    

def interpret(asts):
    env = ConsEnv([], [], EmptyEnv())
    x = None
    for a in asts:
        x = interpret_toplevel(a, env)
    return x
            
    
