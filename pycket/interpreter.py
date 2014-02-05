from pycket        import values
from pycket        import vector
from pycket.prims  import prim_env
from pycket.error import SchemeException
from rpython.rlib  import jit, debug
from small_list import *

class Env(object):
    _immutable_fields_ = ["toplevel_env"]
    pass

class Version(object):
    pass

class ToplevelEnv(Env):
    _immutable_fields_ = ["version?"]
    def __init__(self):
        self.bindings = {}
        self.version = Version()
        self.toplevel_env = self # bit silly
    def lookup(self, sym, env_structure):
        raise SchemeException("variable %s is unbound"%sym.value)

    def toplevel_lookup(self, sym):
        jit.promote(self)
        w_res = self._lookup(sym, jit.promote(self.version))
        if isinstance(w_res, values.W_Cell):
            w_res = w_res.value
        return w_res

    @jit.elidable
    def _lookup(self, sym, version):
        try:
            return self.bindings[sym]
        except KeyError:
            raise SchemeException("toplevel variable %s not found" % sym.value)

    def toplevel_set(self, sym, w_val):
        if sym in self.bindings:
            self.bindings[sym].value = w_val
        else:
            self.bindings[sym] = values.W_Cell(w_val)
            self.version = Version()

class ConsEnv(Env):
    _immutable_fields_ = ["args", "prev"]
    @jit.unroll_safe
    def __init__ (self, args, prev, toplevel):
        if isinstance(prev, ConsEnv):
            assert args.prev is not None
        self.toplevel_env = toplevel
        self.args = args
        self.prev = prev
    @jit.unroll_safe
    def lookup(self, sym, env_structure):
        assert self.args is env_structure
        args = jit.promote(env_structure)
        for i, s in enumerate(args.elems):
            if s is sym:
                v = self._get_list(i)
                assert v is not None
                return v
        return self.prev.lookup(sym, args.prev)
    def set(self, sym, val, env_structure):
        if env_structure is not None:
            assert self.args is env_structure
        args = jit.promote(env_structure)
        for i, s in enumerate(args.elems):
            if s is sym:
                self._set_list(i, val)
                return
        return self.prev.set(sym, val, args.prev)
inline_small_list(ConsEnv, immutable=True, attrname="vals")

class Cont(object):
    def tostring(self):
        if self.prev:
            return "%s(%s)"%(self.__class__.__name__,self.prev.tostring())
        else:
            return "%s()"%(self.__class__.__name__)

def check_one_val(vals):
    if vals._get_size_list() != 1:
        raise SchemeException("expected 1 value but got %s"%(vals._get_size_list()))
    w_val = vals._get_list(0)
    return w_val

class CWVCont(Cont):
    _immutable_fields_ = ["consumer", "env", "prev"] # env is pointless
    def __init__(self, consumer, env, prev):
        self.consumer = consumer
        self.prev = prev
        self.env = env
    def plug_reduce (self, vals):
        val_list = vals._get_full_list()
        return self.consumer.call(val_list, self.env, self.prev)

class IfCont(Cont):
    _immutable_fields_ = ["ast", "env", "prev"]
    def __init__(self, ast, env, prev):
        self.ast = ast
        self.env = env
        self.prev = prev
    def plug_reduce(self, vals):
        ast = jit.promote(self.ast)
        env = self.env
        if ast.remove_env:
            # remove the env created by the let introduced by let_convert
            # it's no longer needed nor accessible
            assert env._get_size_list() == 1
            assert isinstance(env, ConsEnv)
            env = env.prev
        w_val = check_one_val(vals)
        if w_val is values.w_false:
            return ast.els, env, self.prev
        else:
            return ast.thn, env, self.prev

class LetrecCont(Cont):
    _immutable_fields_ = ["ast", "env", "prev", "i"]
    def __init__(self, ast, i, env, prev):
        self.ast = ast
        self.i = i
        self.env  = env
        self.prev = prev
    def plug_reduce(self, vals):
        #import pdb; pdb.set_trace()
        w_val = check_one_val(vals)
        v = self.env.lookup(self.ast.args.elems[self.i], self.ast.args)
        assert isinstance(v, values.W_Cell)
        v.value = w_val
        if self.i >= (len(self.ast.rhss) - 1):
            return self.ast.make_begin_cont(self.env, self.prev)
        else:
            return (self.ast.rhss[self.i + 1], self.env,
                    LetrecCont(self.ast, self.i + 1,
                               self.env, self.prev))

class LetCont(Cont):
    _immutable_fields_ = ["ast", "env", "prev"]
    def __init__(self, ast, env, prev):
        self.ast  = ast
        self.env  = env
        self.prev = prev
    def plug_reduce(self, vals):
        w_val = check_one_val(vals)
        ast = jit.promote(self.ast)
        if self._get_size_list() == (len(ast.rhss) - 1):
            vals_w = self._get_full_list() + [w_val]
            env = ConsEnv.make(vals_w, ast.args, self.env, self.env.toplevel_env)
            return ast.make_begin_cont(env, self.prev)
        else:
            return (ast.rhss[self._get_size_list() + 1], self.env,
                    LetCont.make(self._get_full_list() + [w_val], ast,
                                 self.env, self.prev))
inline_small_list(LetCont, attrname="vals_w", immutable=True)

class CellCont(Cont):
    _immutable_fields_ = ["env", "prev"]
    def __init__(self, env, prev):
        self.env = env
        self.prev = prev
    def plug_reduce(self, vals):
        w_val = check_one_val(vals)
        return return_value(values.W_Cell(w_val), self.env, self.prev)

class Call(Cont):
    _immutable_fields_ = ["ast", "env", "prev"]
    # prev is the parent continuation
    def __init__ (self, ast, env, prev):
        self.ast = ast
        self.env = env
        self.prev = prev
    def plug_reduce(self, vals):
        ast = jit.promote(self.ast)
        w_val = check_one_val(vals)
        if self._get_size_list() == len(ast.rands):
            vals_w = self._get_full_list() + [w_val]
            #print vals_w[0]
            env = self.env
            if ast.remove_env:
                # remove the env created by the let introduced by let_convert
                # it's no longer needed nor accessible
                assert isinstance(env, ConsEnv)
                assert len(vals_w) == len(ast.rands) + 1
                env = env.prev
            return vals_w[0].call(vals_w[1:], env, self.prev)
        else:
            return ast.rands[self._get_size_list()], self.env, Call.make(self._get_full_list() + [w_val], ast,
                                                          self.env, self.prev)
inline_small_list(Call, attrname="vals_w", immutable=True)

class SetBangCont(Cont):
    _immutable_fields_ = ["var", "env", "prev"]
    def __init__(self, var, env, prev):
        self.var = var
        self.env = env
        self.prev = prev
    def plug_reduce(self, vals):
        w_val = check_one_val(vals)
        self.var._set(w_val, self.env)
        return return_value(values.w_void, self.env, self.prev)

class BeginCont(Cont):
    _immutable_fields_ = ["ast", "env", "prev", "i"]
    def __init__(self, ast, i, env, prev):
        self.ast = ast
        self.i = i
        self.env = env
        self.prev = prev
    def plug_reduce(self, vals):
        return self.ast.make_begin_cont(self.env, self.prev, self.i)

class Done(Exception):
    def __init__(self, vals):
        self.values = vals

class AST(object):
    _attrs_ = []
    _settled_ = True

    should_enter = False

    simple = False

    def let_convert(self):
        return self
    def free_vars(self):
        return {}
    def assign_convert(self, vars, env_structure):
        raise NotImplementedError("abstract base class")
    def mutated_vars(self):
        raise NotImplementedError("abstract base class")
    def interpret(self, env, cont):
        raise NotImplementedError("abstract base class")
    def tostring(self):
        raise NotImplementedError("abstract base class")

def return_value(w_val, env, cont):
    return return_multi_vals(values.Values.make([w_val]), env, cont)

def return_multi_vals(vals, env, cont):
    if cont is None: 
        raise Done(vals)
    return cont.plug_reduce(vals)

class Cell(AST):
    _immutable_fields_ = ["expr"]
    def __init__(self, expr):
        self.expr = expr
    def interpret(self, env, cont):
        return self.expr, env, CellCont(env, cont)
    def let_convert(self):
        assert 0
    def assign_convert(self, vars, env_structure):
        return Cell(self.expr.assign_convert(vars, env_structure))
    def mutated_vars(self):
        return self.expr.mutated_vars()
    def free_vars(self):
        return self.expr.free_vars()
    def tostring(self):
        return "Cell(%s)"%self.expr.tostring()

class Quote(AST):
    _immutable_fields_ = ["w_val"]
    simple = True
    def __init__ (self, w_val):
        self.w_val = w_val
    def interpret(self, env, cont):
        return return_value(self.w_val, env, cont)
    def assign_convert(self, vars, env_structure):
        return self
    def mutated_vars(self):
        return {}
    def tostring(self):
        return "'%s"%self.w_val.tostring()

class App(AST):
    _immutable_fields_ = ["rator", "rands[*]", "remove_env"]

    should_enter = True

    def __init__ (self, rator, rands, remove_env=False):
        self.rator = rator
        self.rands = rands
        self.remove_env = remove_env
    def let_convert(self):
        fresh_vars = []
        fresh_rhss = []
        new_rator = self.rator
        new_rands = []

        if not self.rator.simple:
            fresh_rator = LexicalVar.gensym("AppRator_")
            fresh_rator_var = LexicalVar(fresh_rator)
            fresh_rhss.append(self.rator)
            fresh_vars.append(fresh_rator)
            new_rator = fresh_rator_var

        for i, rand in enumerate(self.rands):
            if rand.simple:
                new_rands.append(rand)
            else:
                fresh_rand = LexicalVar.gensym("AppRand%s_"%i)
                fresh_rand_var = LexicalVar(fresh_rand)
                fresh_rhss.append(rand)
                fresh_vars.append(fresh_rand)
                new_rands.append(fresh_rand_var)
        # The body is an App operating on the freshly bound symbols
        if fresh_vars:
            fresh_body = [App(new_rator, new_rands[:], remove_env=True)]
            return Let(SymList(fresh_vars[:]), fresh_rhss[:], fresh_body)
        else:
            return self
    def assign_convert(self, vars, env_structure):
        return App(self.rator.assign_convert(vars, env_structure),
                   [e.assign_convert(vars, env_structure) for e in self.rands],
                   remove_env=self.remove_env)
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
    def interpret(self, env, cont):
        return self.rator, env, Call.make([], self, env, cont)
    def tostring(self):
        return "(%s %s)"%(self.rator.tostring(), " ".join([r.tostring() for r in self.rands]))

class SequencedBodyAST(AST):
    _immutable_fields_ = ["body[*]"]
    def __init__(self, body):
        assert isinstance(body, list)
        assert len(body) > 0
        self.body = body

    def make_begin_cont(self, env, prev, i=0):
        if i == len(self.body) - 1:
            return self.body[i], env, prev
        else:
            return self.body[i], env, BeginCont(self, i + 1, env, prev)


class Begin(SequencedBodyAST):
    @staticmethod
    def make(body):
        if len(body) == 1:
            return body[0]
        else:
            return Begin(body)
    def assign_convert(self, vars, env_structure):
        return Begin.make([e.assign_convert(vars, env_structure) for e in self.body])
    def mutated_vars(self):
        x = {}
        for r in self.body:
            x.update(r.mutated_vars())
        return x
    def free_vars(self):
        x = {}
        for r in self.body:
            x.update(r.free_vars())
        return x
    def interpret(self, env, cont):
        return self.make_begin_cont(env, cont)
    def tostring(self):
        return "(begin %s)" % (" ".join([e.tostring() for e in self.body]))

class Var(AST):
    _immutable_fields_ = ["sym", "env_structure"]
    simple = True
    def __init__ (self, sym, env_structure=None):
        self.sym = sym
        self.env_structure = env_structure
    def interpret(self, env, cont):
        return return_value(self._lookup(env), env, cont)
    def mutated_vars(self):
        return {}
    def free_vars(self):
        return {self.sym: None}
    def tostring(self):
        return "%s"%self.sym.value

class CellRef(Var):
    def assign_convert(self, vars, env_structure):
        return CellRef(self.sym, env_structure)
    def tostring(self):
        return "CellRef(%s)"%self.sym.value
    def _set(self, w_val, env):
        v = env.lookup(self.sym, self.env_structure)
        assert isinstance(v, values.W_Cell)
        v.value = w_val
    def _lookup(self, env):
        #import pdb; pdb.set_trace()
        v = env.lookup(self.sym, self.env_structure)
        assert isinstance(v, values.W_Cell)
        return v.value

# Using this in rpython to have a mutable global variable
class Counter(object):
    value = 0


class LexicalVar(Var):
    _counter = Counter()
    @staticmethod
    def gensym(hint=""):
        LexicalVar._counter.value += 1
        # not using `make` so that it's really gensym
        return values.W_Symbol(hint + "fresh_" + str(LexicalVar._counter.value))
    def _lookup(self, env):
        return env.lookup(self.sym, self.env_structure)
    def _set(self, w_val, env):
        assert 0
    def assign_convert(self, vars, env_structure):
        if self.sym in vars:
            return CellRef(self.sym, env_structure)
        else:
            return LexicalVar(self.sym, env_structure)

class ModuleVar(Var):
    def _lookup(self, env):
        return self._prim_lookup()
    def free_vars(self): return {}
    @jit.elidable
    def _prim_lookup(self):
        return prim_env[self.sym]
    def assign_convert(self, vars, env_structure):
        return self
    def _set(self, w_val, env): assert 0

class ToplevelVar(Var):
    def _lookup(self, env):
        return env.toplevel_env.toplevel_lookup(self.sym)
    def free_vars(self): return {}
    def assign_convert(self, vars, env_structure):
        return self
    def _set(self, w_val, env):
        env.toplevel_env.toplevel_set(self.sym, w_val)

class SymList(object):
    _immutable_fields_ = ["elems[*]", "prev"]
    def __init__(self, elems, prev=None):
        assert isinstance(elems, list)
        self.elems = elems
        self.prev = prev

class SetBang(AST):
    _immutable_fields_ = ["sym", "rhs"]
    def __init__(self, var, rhs):
        self.var = var
        self.rhs = rhs
    def interpret(self, env, cont):
        return self.rhs, env, SetBangCont(self.var, env, cont)
    def assign_convert(self, vars, env_structure):
        return SetBang(self.var.assign_convert(vars, env_structure),
                       self.rhs.assign_convert(vars, env_structure))
    def mutated_vars(self):
        x = self.rhs.mutated_vars()
        x[self.var.sym] = None
        return x
    def free_vars(self):
        x = self.rhs.free_vars()
        x[self.var.sym] = None
        return x
    def tostring(self):
        return "(set! %s %s)"%(self.var.sym.value, self.rhs.tostring())

class If(AST):
    _immutable_fields_ = ["tst", "thn", "els", "remove_env"]
    def __init__ (self, tst, thn, els, remove_env=False):
        self.tst = tst
        self.thn = thn
        self.els = els
        self.remove_env = remove_env
    def let_convert(self):
        if self.tst.simple:
            return self
        else:
            fresh = LexicalVar.gensym("if_")
            return Let(SymList([fresh]), [self.tst], [If(LexicalVar(fresh), self.thn, self.els, remove_env=True)])
    def interpret(self, env, cont):
        return self.tst, env, IfCont(self, env, cont)
    def assign_convert(self, vars, env_structure):
        if self.remove_env:
            sub_env_structure = env_structure.prev
        else:
            sub_env_structure = env_structure
        return If(self.tst.assign_convert(vars, env_structure),
                  self.thn.assign_convert(vars, sub_env_structure),
                  self.els.assign_convert(vars, sub_env_structure),
                  remove_env=self.remove_env)
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
    _immutable_fields_ = ["name", "lam", "env_structure"]
    def __init__(self, name, lam, env_structure):
        assert isinstance(lam, Lambda)
        self.name = name
        self.lam  = lam
        self.env_structure = env_structure
    def assign_convert(self, vars, env_structure):
        v = vars.copy()
        if self.name in v:
            del v[self.name]
        env_structure = SymList([self.name], env_structure)
        return RecLambda(self.name, self.lam.assign_convert(v, env_structure), env_structure)
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
    def interpret(self, env, cont):
        e = ConsEnv.make([values.w_void], self.env_structure, env, env.toplevel_env)
        try:
            Vcl, e, f = self.lam.interpret(e, None)
            assert 0
        except Done, e:
            vals = e.values
            cl = check_one_val(vals)
        assert isinstance(cl, values.W_Closure)
        cl.env.set(self.name, cl, self.lam.frees)
        return return_value(cl, env, cont)
    def tostring(self):
        if self.lam.rest and (not self.lam.formals):
            return "(rec %s %s %s)"%(self.name, self.lam.rest, self.lam.body)
        if self.lam.rest:
            return "(rec %s (%s . %s) %s)"%(self.name, self.lam.formals, self.lam.rest, self.lam.body)
        else:
            return "(rec %s (%s) %s)"%(self.name, self.lam.formals, self.lam.body)


def make_lambda(formals, rest, body):
    args = SymList(formals + ([rest] if rest else []))
    frees = SymList(free_vars_lambda(body, args).keys())
    args = SymList(args.elems, frees)
    return Lambda(formals, rest, args, frees, body)

def free_vars_lambda(body, args):
    x = {}
    for b in body:
        x.update(b.free_vars())
    for v in args.elems:
        if v in x:
            del x[v]
    return x

class Lambda(SequencedBodyAST):
    _immutable_fields_ = ["formals[*]", "rest", "args", "frees", "enclosing_env_structure"]
    def __init__ (self, formals, rest, args, frees, body, enclosing_env_structure=None):
        SequencedBodyAST.__init__(self, body)
        self.formals = formals
        self.rest = rest
        self.args = args
        self.frees = frees
        self.enclosing_env_structure = enclosing_env_structure
    def interpret(self, env, cont):
        return return_value(values.W_Closure(self, env), env, cont)
    def assign_convert(self, vars, env_structure):
        local_muts = {}
        for b in self.body:
            local_muts.update(b.mutated_vars())
        new_lets = []
        new_vars = vars.copy()
        for i in self.args.elems:
            if i in new_vars:
                del new_vars[i]
            if i in local_muts:
                new_lets.append(i)
        new_vars.update(local_muts)
        if new_lets:
            sub_env_structure = SymList(new_lets, self.args)
        else:
            sub_env_structure = self.args
        new_body = [b.assign_convert(new_vars, sub_env_structure) for b in self.body]
        if new_lets:
            cells = [Cell(LexicalVar(v, self.args)) for v in new_lets]
            new_body = [Let(sub_env_structure, cells, new_body)]
        return Lambda(self.formals, self.rest, self.args, self.frees, new_body, env_structure)
    def mutated_vars(self):
        x = {}
        for b in self.body:
            x.update(b.mutated_vars())
        for v in self.args.elems:
            if v in x:
                del x[v]
        return x
    def free_vars(self):
        return free_vars_lambda(self.body, self.args)

    def tostring(self):
        if self.rest and (not self.formals):
            return "(lambda %s %s)"%(self.rest, [b.tostring() for b in self.body])
        if self.rest:
            return "(lambda (%s . %s) %s)"%(self.formals, self.rest, [b.tostring() for b in self.body])
        else:
            return "(lambda (%s) %s)"%(self.formals, [b.tostring() for b in self.body])


class Letrec(SequencedBodyAST):
    _immutable_fields_ = ["args", "rhss[*]"]
    def __init__(self, args, rhss, body):
        SequencedBodyAST.__init__(self, body)
        self.rhss = rhss
        self.args = args
    def interpret(self, env, cont):
        env_new = ConsEnv.make([values.W_Cell(None) for var in self.args.elems], self.args, env, env.toplevel_env)
        return self.rhss[0], env_new, LetrecCont(self, 0, env_new, cont)
    def mutated_vars(self):
        x = {}
        for b in self.body + self.rhss:
            x.update(b.mutated_vars())
        for v in self.args.elems:
            x[v] = None
        return x
    def free_vars(self):
        x = {}
        for b in self.body + self.rhss:
            x.update(b.free_vars())
        for v in self.args.elems:
            if v in x:
                del x[v]
        return x
    def assign_convert(self, vars, env_structure):
        local_muts = {}
        for b in self.body + self.rhss:
            local_muts.update(b.mutated_vars())
        for v in self.args.elems:
            local_muts[v] = None
        new_vars = vars.copy()
        new_vars.update(local_muts)
        sub_env_structure = SymList(self.args.elems, env_structure)
        new_rhss = [rhs.assign_convert(new_vars, sub_env_structure) for rhs in self.rhss]
        new_body = [b.assign_convert(new_vars, sub_env_structure) for b in self.body]
        return Letrec(sub_env_structure, new_rhss, new_body)
    def tostring(self):
        return "(letrec (%s) %s)"%([(v.tostring(),self.rhss[i].tostring()) for i, v in enumerate(self.args.elems)],
                                   [b.tostring() for b in self.body])

def make_let_star(bindings, body):
    if not bindings:
        return Begin.make(body)
    var, rhs = bindings[0]
    if len(body) == 1:
        bod = body[0]
        if isinstance(bod, LexicalVar) and (bod.sym is var):
            return rhs
    return Let(SymList([var]), [rhs], [make_let_star(bindings[1:], body)])

def make_let(vars, rhss, body):
    if not vars:
        return Begin.make(body)
    else:
        return Let(SymList(vars), rhss, body)

def make_letrec(vars, rhss, body):
    if (1 == len(vars)):
        if (1 == len(body)):
            if isinstance(rhss[0], Lambda):
                b = body[0]
                if isinstance(b, LexicalVar) and vars[0] is b.sym:
                    return RecLambda(vars[0], rhss[0], SymList([vars[0]]))
    return Letrec(SymList(vars), rhss, body)

class Let(SequencedBodyAST):
    # Not sure why, but rpython keeps telling me that vars is resized...
    _immutable_fields_ = ["vars[*]", "rhss[*]", "args"]
    def __init__(self, args, rhss, body):
        SequencedBodyAST.__init__(self, body)
        assert len(args.elems) > 0 # otherwise just use a begin
        self.rhss = rhss
        self.args = args
    def interpret(self, env, cont):
        return self.rhss[0], env, LetCont.make([], self, env, cont)
    def mutated_vars(self):
        x = {}
        for b in self.body:
            x.update(b.mutated_vars())
        x2 = {}
        for v in x:
            if v not in self.args.elems:
                x2[v] = x[v]
        for b in self.rhss:
            x2.update(b.mutated_vars())
        return x2
    def free_vars(self):
        x = {}
        for b in self.body:
            x.update(b.free_vars())
        for v in self.args.elems:
            if v in x:
                del x[v]
        for b in self.rhss:
            x.update(b.free_vars())
        return x
    def assign_convert(self, vars, env_structure):
        local_muts = {}
        for b in self.body:
            local_muts.update(b.mutated_vars())
        new_rhss = [Cell(rhs.assign_convert(vars, env_structure))
                    if self.args.elems[i] in local_muts
                    else rhs.assign_convert(vars, env_structure)
                    for i, rhs in enumerate(self.rhss)]
        new_vars = vars.copy()
        new_vars.update(local_muts)
        sub_env_structure = SymList(self.args.elems, env_structure)
        new_body = [b.assign_convert(new_vars, sub_env_structure) for b in self.body]
        return Let(sub_env_structure, new_rhss, new_body)
    def tostring(self):
        return "(let (%s) %s)"%(" ".join(["[%s %s]" % (v.tostring(),self.rhss[i].tostring()) for i, v in enumerate(self.args.elems)]), 
                                " ".join([b.tostring() for b in self.body]))


class Define(AST):
    _immutable_fields_ = ["name", "rhs"]
    name = values.W_Symbol('fake')
    rhs = Quote(values.w_null)

    def __init__(self, n, r):
        self.name = n
        self.rhs = r
    def assign_convert(self, vars, env_structure):
        return Define(self.name, self.rhs.assign_convert(vars, env_structure))
    def mutated_vars(self): assert 0
    def free_vars(self): assert 0
    def tostring(self):
        return "(define %s %s)"%(self.name, self.rhs.tostring())

def get_printable_location(green_ast):
    if green_ast is None:
        return 'Green_Ast is None'
    return green_ast.tostring()
driver = jit.JitDriver(reds=["env", "cont"],
                       greens=["ast"],
                       get_printable_location=get_printable_location)

def interpret_one(ast, env=None):
    import pdb
    #pdb.set_trace()
    cont = None
    if not env:
        env = ToplevelEnv()
    try:
        while True:
            driver.jit_merge_point(ast=ast, env=env, cont=cont)
            ast, env, cont = ast.interpret(env, cont)
            if ast.should_enter:
                driver.can_enter_jit(ast=ast, env=env, cont=cont)
    except Done, e:
        return e.values

def interpret_toplevel(a, env):
    if isinstance(a, Begin):
        x = None
        for a2 in a.body:
            x = interpret_toplevel(a2, env)
        return x
    elif isinstance(a, Define):
        env.toplevel_env.toplevel_set(a.name, interpret_one(a.rhs, env))
        return values.Values.make([values.w_void])
    else:
        return interpret_one(a, env)
    

def interpret(asts):
    env = ToplevelEnv()
    x = None
    for a in asts:
        x = interpret_toplevel(a, env)
    return x


