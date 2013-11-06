from pycket        import values
from pycket.prims  import prim_env
from rpython.rlib  import jit

class Env(object):
    _immutable_env_ = ["toplevel_env"]
    pass

class ToplevelEnv(object):
    def __init__(self):
        self.bindings = {}
    def lookup(self, sym):
        return self.bindings[sym]
    def set(self, sym, w_val):
        if sym in self.bindings:
            self.bindings[sym].value = w_val
        else:
            self.bindings[sym] = values.W_Cell(w_val)

class EmptyEnv(Env):
    def __init__ (self, toplevel):
        self.toplevel_env = toplevel
    def lookup(self, sym):
        raise Exception ("variable %s is unbound"%sym.value)

class ConsEnv(Env):
    _immutable_fields_ = ["args", "vals[*]", "prev"]
    @jit.unroll_safe
    def __init__ (self, args, vals, prev, toplevel):
        self.toplevel_env = toplevel
        for i in args.elems:
            assert isinstance (i, values.W_Symbol)
        self.args = args
        self.vals = vals
        self.prev = prev
    @jit.unroll_safe
    def lookup(self, sym):
        jit.promote(self.args)
        for i, s in enumerate(self.args.elems):
            if s is sym:
                v = self.vals[i]
                assert v is not None
                return v
        return self.prev.lookup(sym)
    def set(self, sym, val):
        jit.promote(self.args)
        for i, s in enumerate(self.args.elems):
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
    def __init__(self, args, rest, body, env, prev):
        self.args  = args
        self.rest = rest
        self.body = body
        self.env  = env
        self.prev = prev
    def plug_reduce(self, w_val):
        self.env.set(self.args.elems[- (len (self.rest) + 1)], w_val)
        if not self.rest:
            return make_begin(self.body, self.env, self.prev)
        else:
            return (self.rest[0], self.env, 
                    LetrecCont(self.args, self.rest[1:], 
                               self.body, self.env, self.prev))

class LetCont(Cont):
    def __init__(self, args, vals_w, rest, body, env, prev):
        self.args = args
        self.vals_w  = vals_w
        self.rest = rest
        self.body = body
        self.env  = env
        self.prev = prev
    def plug_reduce(self, w_val):
        if not self.rest:
            vals_w = self.vals_w + [w_val]
            env = ConsEnv(self.args, vals_w, self.env, self.env.toplevel_env)
            return make_begin(self.body, env, self.prev)
        else:
            return (self.rest[0], self.env, 
                    LetCont(self.args, self.vals_w + [w_val], self.rest[1:], 
                            self.body, self.env, self.prev))

class CellCont(Cont):
    def __init__(self, env, prev):
        self.env = env
        self.prev = prev
    def plug_reduce(self, w_val):
        return Value(values.W_Cell(w_val)), self.env, self.prev

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
        self.var._set(w_val, self.env)
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

class AST(object):
    pass


class Value (AST):
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret (self, env, frame):
        if frame is None: raise Done(self.w_val)
        return frame.plug_reduce(self.w_val)
    def assign_convert(self, vars):
        return self
    def mutated_vars(self):
        return {}
    def __repr__(self):
        return "V(%r)"%self.w_val

class Cell(AST):
    def __init__(self, expr):
        self.expr = expr
    def interpret(self, env, frame):
        return self.expr, env, CellCont(env, frame)
    def assign_convert(self, vars):
        return Cell(self.expr.assign_convert(vars))
    def mutated_vars(self):
        return self.expr.mutated_vars()
    def __repr__(self):
        return "Cell(%r)"%self.expr

class Quote (AST):
    _immutable_fields_ = ["w_val"]
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret (self, env, frame):
        return Value(self.w_val), env, frame
    def assign_convert(self, vars):
        return self
    def mutated_vars(self):
        return {}
    def __repr__(self):
        return "Quote(%r)"%self.w_val

class App(AST):
    _immutable_fields_ = ["rator", "rands[*]"]
    def __init__ (self, rator, rands):
        self.rator = rator
        self.rands = rands
    def assign_convert(self, vars):
        return App(self.rator.assign_convert(vars),
                   [e.assign_convert(vars) for e in self.rands])
    def mutated_vars(self):
        x = self.rator.mutated_vars()
        for r in self.rands:
            x.update(r.mutated_vars())
        return x
    def interpret (self, env, frame):
        return self.rator, env, Call([], self.rands, env, frame)
    def __repr__(self):
        return "(%r %r)"%(self.rator, self.rands)

class Begin(AST):
    _immutable_fields_ = ["exprs[*]"]
    def __init__(self, exprs):
        self.exprs = exprs
    def assign_convert(self, vars):
        return Begin(map(lambda e: e.assign_convert(vars), self.exprs))
    def mutated_vars(self):
        x = {}
        for r in self.exprs:
            x.update(r.mutated_vars())
        return x
    def interpret(self, env, frame):
        return make_begin(self.exprs, env, frame)
    def __repr__(self):
        return "(begin %r)" % self.exprs

class Var(AST):
    _immutable_fields_ = ["sym"]
    def __init__ (self, sym):
        self.sym = sym
    def interpret(self, env, frame):
        return Value(self._lookup(env)), env, frame
    def mutated_vars(self):
        return {}
    def __repr__(self):
        return "%s"%self.sym.value

class CellRef (Var):
    def assign_convert(self, vars):
        return self
    def _set(self, w_val, env): 
        v = env.lookup(self.sym)
        assert isinstance(v, values.W_Cell)
        v.value = w_val
    def _lookup(self, env):
        v = env.lookup(self.sym)
        assert isinstance(v, values.W_Cell)
        return v.value

class LexicalVar(Var):
    def _lookup(self, env):
        return env.lookup(self.sym)
    def _set(self, w_val, env): 
        assert 0
    def assign_convert(self, vars):
        if self.sym in vars:
            return CellRef(self.sym)
        else:
            return self

class ModuleVar(Var):
    def _lookup(self, env):
        return self._prim_lookup()

    @jit.elidable
    def _prim_lookup(self):
        return prim_env[self.sym]
    def assign_convert(self, vars):
        return self
    def _set(self, w_val, env): assert 0

class ToplevelVar(Var):
    def _lookup(self, env):
        return env.toplevel_env.lookup(self.sym).value
    def assign_convert(self, vars):
        return self
    def _set(self, w_val, env): 
        env.toplevel_env.set(self.sym, w_val)

class SymList(object):
    _immutable_fields_ = ["elems[*]"]
    def __init__(self, elems):
        assert isinstance(elems, list)
        self.elems = elems

class SetBang (AST):
    _immutable_fields_ = ["sym", "rhs"]
    def __init__(self, var, rhs):
        self.var = var
        self.rhs = rhs
    def interpret (self, env, frame):
        return self.rhs, env, SetBangCont(self.var, env, frame)
    def assign_convert(self, vars):
        return SetBang(self.var, self.rhs.assign_convert(vars))
    def mutated_vars(self):
        x = self.rhs.mutated_vars()
        x[self.var.sym] = None
        return x
    def __repr__(self):
        return "(set! %r %r)"%(self.var.sym.value, self.rhs)

class If (AST):
    _immutable_fields_ = ["tst", "thn", "els"]
    def __init__ (self, tst, thn, els):
        self.tst = tst
        self.thn = thn
        self.els = els
    def interpret(self, env, frame):
        return self.tst, env, IfCont(self.thn, self.els, env, frame)
    def assign_convert(self, vars):
        return If(self.tst.assign_convert(vars),
                  self.thn.assign_convert(vars),
                  self.els.assign_convert(vars))
    def mutated_vars(self):
        x = {}
        for b in [self.tst, self.els, self.thn]:
            x.update(b.mutated_vars())
        return x
    def __repr__(self):
        return "(if %r %r %r)"%(self.tst, self.thn, self.els)


class Lambda (AST):
    _immutable_fields_ = ["formals[*]", "rest", "body[*]", "args"]
    def __init__ (self, formals, rest, body):
        self.formals = formals
        self.rest = rest
        self.body = body
        self.args = SymList(formals + ([rest] if rest else []))
    def interpret (self, env, frame):
        return Value(values.W_Closure (self, env)), env, frame
    def assign_convert(self, vars):
        local_muts = {}
        for b in self.body:
            local_muts.update(b.mutated_vars())
        new_lets = []
        new_vars = vars.copy()
        for i in self.formals:
            if i in vars:
                del vars[i]
            if i in local_muts:
                new_lets.append(i)
        if self.rest and self.rest in vars:
            del vars[self.rest]
        if self.rest and self.rest in local_muts:
            new_lets.append(self.rest)
        cells = [Cell(LexicalVar(v)) for v in new_lets]
        new_vars.update(local_muts)
        new_body = [Let(new_lets, cells, [b.assign_convert(new_vars) for b in self.body])]
        return Lambda(self.formals, self.rest, new_body)
    def mutated_vars(self):
        x = {}
        for b in self.body:
            x.update(b.mutated_vars())
        for v in self.formals:
            if v in x:
                del x[v]
        if self.rest and self.rest in x:
            del x[self.rest]
        return x
    def __repr__(self):
        if self.rest and (not self.formals):
            return "(lambda %r %r)"%(self.rest, self.body)
        if self.rest:
            return "(lambda (%r . %r) %r)"%(self.formals, self.rest, self.body)
        else:
            return "(lambda (%r) %r)"%(self.formals, self.body)


class Letrec(AST):
    _immutable_fields_ = ["vars[*]", "rhss[*]", "body[*]"]
    def __init__(self, vars, rhss, body):
        self.vars = vars
        self.rhss = rhss
        self.body = body
        self.args = SymList(vars)
    def interpret (self, env, frame):
        env_new = ConsEnv(self.args, [None]*len(self.vars), env, env.toplevel_env)
        return self.rhss[0], env_new, LetrecCont(self.args, self.rhss[1:], self.body, env_new, frame)
    def mutated_vars(self):
        x = {}
        for b in self.body + self.rhss:
            x.update(b.mutated_vars())
        for v in self.vars:
            if v in x:
                del x[v]
        return x
    def assign_convert(self, vars):
        local_muts = {}
        for b in self.body + self.rhss:
            local_muts.update(b.mutated_vars())
        new_vars = vars.copy()
        new_vars.update(local_muts)
        new_rhss = [Cell(rhs.assign_convert(new_vars)) 
                    if self.vars[i] in local_muts
                    else rhs.assign_convert(new_vars)
                    for i, rhs in enumerate(self.rhss)]
        new_body = [b.assign_convert(new_vars) for b in self.body]
        return Letrec(self.vars, new_rhss, new_body)
    def __repr__(self):
        return "(letrec (%r) %r)"%(zip(self.vars, self.rhss), self.body)

class Let(AST):
    _immutable_fields_ = ["vars[*]", "rhss[*]", "body[*]"]
    def __init__(self, vars, rhss, body):
        self.vars = vars
        self.rhss = rhss
        self.body = body
        self.args = SymList(vars)
    def interpret (self, env, frame):
        if not self.vars:
            return make_begin(self.body, env, frame)
        return self.rhss[0], env, LetCont(self.args, [], self.rhss[1:], self.body, env, frame)
    def mutated_vars(self):
        x = {}
        for b in self.body:
            x.update(b.mutated_vars())
        for v in self.vars:
            if v in x:
                del x[v]
        for b in self.rhss:
            x.update(b.mutated_vars())
        return x
    def assign_convert(self, vars):
        local_muts = {}
        for b in self.body:
            local_muts.update(b.mutated_vars())
        new_rhss = [Cell(rhs.assign_convert(vars)) 
                    if self.vars[i] in local_muts
                    else rhs.assign_convert(vars)
                    for i, rhs in enumerate(self.rhss)]
        new_vars = vars.copy()
        new_vars.update(local_muts)
        new_body = [b.assign_convert(new_vars) for b in self.body]
        return Let(self.vars, new_rhss, new_body)
    def __repr__(self):
        return "(let (%r) %r)"%(zip(self.vars, self.rhss), self.body)



class Define(AST):
    def __init__(self, name, rhs):
        self.name = name
        self.rhs = rhs
    def assign_convert(self, vars):
        return Define(self.name, self.rhs.assign_convert(vars))
    def mutated_vars(self): assert 0
    def __repr__(self):
        return "(define %r %r)"%(self.name, self.rhs)

driver = jit.JitDriver(reds=["ast", "env", "frame"], greens=["green_ast"])

def interpret_one(ast, env=None):
    frame = None
    if not env:
        env = EmptyEnv(ToplevelEnv())
    #import pdb; pdb.set_trace()
    green_ast = None
    try:
        while True:
            driver.jit_merge_point(ast=ast, env=env, frame=frame, green_ast=green_ast)
            if not isinstance(ast, Value):
                jit.promote(ast)
                green_ast = ast
            ast, env, frame = ast.interpret(env, frame)
            if isinstance(ast, App):
                driver.can_enter_jit(ast=ast, env=env, frame=frame, green_ast=green_ast)
    except Done, e:
        return e.w_val

def interpret_toplevel(a, env):
    if isinstance(a, Begin):
        x = None
        for a2 in a.exprs:
            x = interpret_toplevel(a2, env)
        return x
    elif isinstance(a, Define):
        env.toplevel_env.set(a.name, interpret_one(a.rhs, env))
        return values.w_void
    else:
        return interpret_one(a, env)
    

def interpret(asts):
    env = EmptyEnv(ToplevelEnv())
    x = None
    for a in asts:
        x = interpret_toplevel(a, env)
    return x
            
    
