from pycket        import values
from pycket.prims  import prim_env
from rpython.rlib  import jit

class Env(object):
    _immutable_fields_ = ["toplevel_env"]
    pass

class Version(object):
    pass

class ToplevelEnv(object):
    _immutable_fields_ = ["version"]
    def __init__(self):
        self.bindings = {}
        self.version = Version()
    def lookup(self, sym):
        jit.promote(self)
        w_res = self._lookup(sym, jit.promote(self.version))
        if isinstance(w_res, values.W_Cell):
            w_res = w_res.value
        return w_res

    @jit.elidable
    def _lookup(self, sym, version):
        try:
            return self.bindings[sym]
        except:
            print ">>>> toplevel err %s"%sym.value
            raise

    def set(self, sym, w_val):
        if sym in self.bindings:
            self.bindings[sym].value = w_val
        else:
            self.bindings[sym] = values.W_Cell(w_val)
            self.version = Version()

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

class ContMeta(type):
    def __new__(cls, name, bases, dct):
        dct["green_key"] = name
        return type(name, bases, dct)

class Cont(object):
    __metaclass__ = ContMeta

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
    def __init__(self, args, ast, i, body, env, prev):
        self.args  = args
        self.ast = ast
        self.i = i
        self.body = body
        self.env  = env
        self.prev = prev
    def plug_reduce(self, w_val):
        #import pdb; pdb.set_trace()
        v = self.env.lookup(self.args.elems[self.i])
        assert isinstance(v, values.W_Cell)
        v.value = w_val
        if self.i >= (len(self.ast.rhss) - 1):
            return make_begin(self.body, self.env, self.prev)
        else:
            return (self.ast.rhss[self.i + 1], self.env, 
                    LetrecCont(self.args, self.ast, self.i + 1,
                               self.body, self.env, self.prev))

class LetCont(Cont):
    def __init__(self, args, vals_w, ast, i, body, env, prev):
        self.args = args
        self.vals_w  = vals_w
        self.ast = ast
        self.i = i
        self.body = body
        self.env  = env
        self.prev = prev
    def plug_reduce(self, w_val):
        jit.promote(len(self.vals_w))
        if self.i >= (len(self.ast.rhss) - 1):
            vals_w = self.vals_w + [w_val]
            env = ConsEnv(self.args, vals_w, self.env, self.env.toplevel_env)
            return make_begin(self.body, env, self.prev)
        else:
            return (self.ast.rhss[self.i + 1], self.env, 
                    LetCont(self.args, self.vals_w + [w_val], self.ast, self.i + 1,
                            self.body, self.env, self.prev))

class CellCont(Cont):
    def __init__(self, env, prev):
        self.env = env
        self.prev = prev
    def plug_reduce(self, w_val):
        return Value(values.W_Cell(w_val)), self.env, self.prev

class Call(Cont):
    # prev is the parent continuation
    def __init__ (self, vals_w, callast, i, env, prev):
        self.vals_w = vals_w
        self.callast = callast
        self.i = i
        self.env = env
        self.prev = prev
    def plug_reduce(self, w_val):
        if self.i == len(self.callast.rands):
            vals_w = self.vals_w + [w_val]
            #print vals_w[0]
            return vals_w[0].call(vals_w[1:], self.env, self.prev)
        else:
            return self.callast.rands[self.i], self.env, Call(self.vals_w + [w_val], self.callast, self.i + 1,
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

# Perhaps we should add an 'i' attribute to BeginCont. -Jeremy
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
    def anorm(self):
        return self, []
    def free_vars(self):
        return {}


class Value (AST):
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret (self, env, frame):
        if frame is None: raise Done(self.w_val)
        return frame.plug_reduce(self.w_val)
    def anorm (self): 
        assert 0
    def assign_convert(self, vars):
        return self
    def mutated_vars(self):
        return {}
    def tostring(self):
        return "V(%s)"%self.w_val.tostring()

class Cell(AST):
    def __init__(self, expr):
        self.expr = expr
    def interpret(self, env, frame):
        return self.expr, env, CellCont(env, frame)
    def anorm (self): 
        assert 0
    def assign_convert(self, vars):
        return Cell(self.expr.assign_convert(vars))
    def mutated_vars(self):
        return self.expr.mutated_vars()
    def free_vars (self):
        return self.expr.free_vars()
    def tostring(self):
        return "Cell(%s)"%self.expr

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
    def tostring(self):
        return "'%s"%self.w_val.tostring()

class App(AST):
    _immutable_fields_ = ["rator", "rands[*]"]
    def anorm(self):
        rator_simple, rator_lets = self.rator.anorm()
        results = [rand.anorm() for rand in self.rands]
        rand_simples = [simple for (simple, let) in results]
        rand_lets = [let for (simple, lets) in results for let in lets]
        fresh = LexicalVar.gensym()
        return LexicalVar(fresh), rator_lets + rand_lets + [(fresh, App(rator_simple, rand_simples))]
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
    def free_vars(self):
        x = self.rator.free_vars()
        for r in self.rands:
            x.update(r.free_vars())
        return x
    def interpret (self, env, frame):
        return self.rator, env, Call([], self, 0, env, frame)
    def tostring(self):
        return "(%s %s)"%(self.rator.tostring(), [r.tostring() for r in self.rands])

class Begin(AST):
    _immutable_fields_ = ["exprs[*]"]
    def __init__(self, exprs):
        assert isinstance(exprs[0], AST)
        self.exprs = exprs
    def anorm (self): 
        results = [e.anorm() for e in self.exprs]
        simples = [s for s,l in results]
        lets = [l for s,lets in results for l in lets]
        return simples[-1], lets
    def assign_convert(self, vars):
        return Begin(map(lambda e: e.assign_convert(vars), self.exprs))
    def mutated_vars(self):
        x = {}
        for r in self.exprs:
            x.update(r.mutated_vars())
        return x
    def free_vars(self):
        x = {}
        for r in self.exprs:
            x.update(r.free_vars())
        return x
    def interpret(self, env, frame):
        return make_begin(self.exprs, env, frame)
    def tostring(self):
        return "(begin %s)" % [e.tostring() for e in self.exprs]

class Var(AST):
    _immutable_fields_ = ["sym"]
    def __init__ (self, sym):
        self.sym = sym
    def interpret(self, env, frame):
        return Value(self._lookup(env)), env, frame
    def mutated_vars(self):
        return {}
    def free_vars (self): 
        return {self.sym: None}
    def tostring(self):
        return "%s"%self.sym.value

class CellRef (Var):
    def assign_convert(self, vars):
        return self
    def tostring(self):
        return "CellRef(%s)"%self.sym.value
    def _set(self, w_val, env): 
        v = env.lookup(self.sym)
        assert isinstance(v, values.W_Cell)
        v.value = w_val
    def _lookup(self, env):
        #import pdb; pdb.set_trace()
        v = env.lookup(self.sym)
        assert isinstance(v, values.W_Cell)
        return v.value

class LexicalVar(Var):
    _counter = 0
    @staticmethod
    def gensym():
        LexicalVar._counter += 1
        # not using `make` so that it's really gensym
        return values.W_Symbol("fresh_" + str(LexicalVar._counter))
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
    def free_vars(self): return {}
    @jit.elidable
    def _prim_lookup(self):
        return prim_env[self.sym]
    def assign_convert(self, vars):
        return self
    def _set(self, w_val, env): assert 0

class ToplevelVar(Var):
    def _lookup(self, env):
        return env.toplevel_env.lookup(self.sym)
    def free_vars(self): return {}
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
    def anorm(self):
        simple, lets = self.rhs.anorm()
        fresh = LexicalVar.gensym()
        return Quote(values.w_void), lets + [(fresh, SetBang(self.var, simple))]
    def interpret (self, env, frame):
        return self.rhs, env, SetBangCont(self.var, env, frame)
    def assign_convert(self, vars):
        return SetBang(self.var, self.rhs.assign_convert(vars))
    def mutated_vars(self):
        x = self.rhs.mutated_vars()
        x[self.var.sym] = None
        return x
    def free_vars(self):
        x = self.rhs.free_vars()
        x[self.var.sym] = None
        return x
    def tostring(self):
        return "(set! %s %s)"%(self.var.sym.value, self.rhs)

class If (AST):
    _immutable_fields_ = ["tst", "thn", "els"]
    def __init__ (self, tst, thn, els):
        self.tst = tst
        self.thn = thn
        self.els = els
    def anorm(self):
        simple, lets = self.tst.anorm()
        thn = anorm_and_bind(self.thn)
        els = anorm_and_bind(self.els)
        fresh = LexicalVar.gensym()
        return LexicalVar(fresh), lets + [(fresh, If(simple, thn, els))]
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
    def free_vars(self):
        x = {}
        for b in [self.tst, self.els, self.thn]:
            x.update(b.free_vars())
        return x
    def tostring(self):
        return "(if %s %s %s)"%(self.tst.tostring(), self.thn.tostring(), self.els.tostring())

class RecLambda(AST):
    _immutable_fields_ = ["name", "lam"]
    def __init__(self, name, lam):
        self.name= name
        self.lam = lam
    def anorm(self):
        return RecLambda(self.name, self.lam.do_anorm()), []
    def assign_convert(self, vars):
        v = vars.copy()
        if self.name in v:
            del v[self.name]
        return RecLambda(self.name, self.lam.assign_convert(v))
    def mutated_vars(self):
        v = self.lam.mutated_vars()
        if self.name in v:
            del v[self.name]
        return v
    def free_vars(self):
        v = self.lam.free_vars()
        if self.name in v:
            del v[self.name]
        return v
    def interpret(self, env, frame):
        e = ConsEnv(SymList([self.name]), [values.w_void], env, env.toplevel_env)
        Vcl, e, f = self.lam.interpret(e, frame)
        cl = Vcl.w_val
        assert isinstance(cl, values.W_Closure)
        cl.env.set(self.name, cl)
        return Vcl, env, frame
    def tostring(self):
        if self.lam.rest and (not self.lam.formals):
            return "(rec %s %s %s)"%(self.name, self.lam.rest, self.lam.body)
        if self.lam.rest:
            return "(rec %s (%s . %s) %s)"%(self.name, self.lam.formals, self.lam.rest, self.lam.body)
        else:
            return "(rec %s (%s) %s)"%(self.name, self.lam.formals, self.lam.body)


class Lambda (AST):
    _immutable_fields_ = ["formals[*]", "rest", "body[*]", "args", "frees[*]"]
    def do_anorm(self):
        return Lambda(self.formals, self.rest, [anorm_and_bind(Begin(self.body))])
    def anorm(self):
        return self.do_anorm(), []
    def __init__ (self, formals, rest, body):
        self.formals = formals
        self.rest = rest
        self.body = body
        self.args = SymList(formals + ([rest] if rest else []))
        self.frees = SymList(self.free_vars().keys())
    def interpret (self, env, frame):
        return Value(values.W_Closure (self, env)), env, frame
    def assign_convert(self, vars):
        local_muts = {}
        for b in self.body:
            local_muts.update(b.mutated_vars())
        new_lets = []
        new_vars = vars.copy()
        for i in self.args.elems:
            if i in vars:
                del vars[i]
            if i in local_muts:
                new_lets.append(i)
        cells = [Cell(LexicalVar(v)) for v in new_lets]
        new_vars.update(local_muts)
        new_body = [make_let(new_lets, cells, [b.assign_convert(new_vars) for b in self.body])]
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
    def free_vars(self):
        x = {}
        for b in self.body:
            x.update(b.free_vars())
        for v in self.formals:
            if v in x:
                del x[v]
        if self.rest and self.rest in x:
            del x[self.rest]
        return x
    def tostring(self):
        if self.rest and (not self.formals):
            return "(lambda %s %s)"%(self.rest, [b.tostring() for b in self.body])
        if self.rest:
            return "(lambda (%s . %s) %s)"%(self.formals, self.rest, [b.tostring() for b in self.body])
        else:
            return "(lambda (%s) %s)"%(self.formals, [b.tostring() for b in self.body])


class Letrec(AST):
    _immutable_fields_ = ["vars[*]", "rhss[*]", "body[*]"]
    def __init__(self, vars, rhss, body):
        self.vars = vars
        self.rhss = rhss
        self.body = body
        self.args = SymList(vars)
    def anorm(self):
        rhss = [anorm_and_bind(rhs) for rhs in self.rhss]
        fresh = LexicalVar.gensym()
        body = anorm_and_bind(Begin(self.body))
        return LexicalVar(fresh), [(fresh, Letrec(self.vars, rhss, [body]))]
    def interpret (self, env, frame):
        env_new = ConsEnv(self.args, [values.W_Cell(None) for var in self.vars], env, env.toplevel_env)
        return self.rhss[0], env_new, LetrecCont(self.args, self, 0, self.body, env_new, frame)
    def mutated_vars(self):
        x = {}
        for b in self.body + self.rhss:
            x.update(b.mutated_vars())
        for v in self.vars:
            x[v] = None
        return x
    def free_vars(self):
        x = {}
        for b in self.body + self.rhss:
            x.update(b.free_vars())
        for v in self.vars:
            if v in x:
                del x[v]
        return x
    def assign_convert(self, vars):
        local_muts = {}
        for b in self.body + self.rhss:
            local_muts.update(b.mutated_vars())
        for v in self.vars:
            local_muts[v] = None
        new_vars = vars.copy()
        new_vars.update(local_muts)
        new_rhss = [rhs.assign_convert(new_vars) for rhs in self.rhss]
        #import pdb; pdb.set_trace()
        new_body = [b.assign_convert(new_vars) for b in self.body]
        return Letrec(self.vars, new_rhss, new_body)
    def tostring(self):
        return "(letrec (%s) %s)"%([(v.tostring(),self.rhss[i].tostring()) for i, v in enumerate(self.vars)], 
                                   [b.tostring() for b in self.body])

def anorm_and_bind(ast):
    simple, lets = ast.anorm()
    return make_let_star(lets, [simple])

def make_let_star(bindings, body):
    if not bindings:
        return Begin(body)
    var, rhs = bindings[0]
    return Let([var], [rhs], [make_let_star(bindings[1:], body)])

def make_let(vars, rhss, body):
    if not vars:
        return Begin(body)
    else:
        return Let(vars, rhss, body)

def make_letrec(vars, rhss, body):
    if (1 == len(vars)):
        if (1 == len(body)):
            if isinstance(rhss[0], Lambda):
                if isinstance(body[0], LexicalVar) and vars[0] is body[0].sym:
                    return RecLambda(vars[0], rhss[0])
    return Letrec(vars, rhss, body)

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
        return self.rhss[0], env, LetCont(self.args, [], self, 0, self.body, env, frame)
    def anorm(self):
        new_lets = []
        new_rhss = []
        for i, v in enumerate(self.vars):
            rhs = self.rhss[i]
            simp, lets = rhs.anorm()
            new_lets += lets
            new_rhss += [simp]
        body = anorm_and_bind(Begin(self.body))
        fresh = LexicalVar.gensym()
        return LexicalVar(fresh), new_lets + [(fresh, Let(self.vars, new_rhss, [body]))]
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
    def free_vars(self):
        x = {}
        for b in self.body:
            x.update(b.free_vars())
        for v in self.vars:
            if v in x:
                del x[v]
        for b in self.rhss:
            x.update(b.free_vars())
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
    def tostring(self):
        return "(let (%s) %s)"%([(v.tostring(),self.rhss[i].tostring()) for i, v in enumerate(self.vars)], 
                                [b.tostring() for b in self.body])



class Define(AST):
    def __init__(self, name, rhs):
        self.name = name
        self.rhs = rhs
    def assign_convert(self, vars):
        return Define(self.name, self.rhs.assign_convert(vars))
    def mutated_vars(self): assert 0
    def free_vars(self): assert 0
    def tostring(self):
        return "(define %s %s)"%(self.name, self.rhs.tostring())

def get_printable_location(green_ast):
    return green_ast.tostring()
driver = jit.JitDriver(reds=["ast", "env", "frame"],
                       greens=["green_ast"],
                       get_printable_location=get_printable_location)

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
            #print ast.tostring()
            #print frame
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
            
    
