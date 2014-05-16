from pycket        import values
from pycket        import vector
from pycket.prims  import prim_env
from pycket.error  import SchemeException
from pycket.cont   import Cont
from rpython.rlib  import jit, debug
from small_list    import inline_small_list


class ModuleEnv(object):
    def __init__(self):
        self.modules = {}
    
    def require(module_name):
        assert 0
        # load the file, evaluate it, register it in the table

    def add_module(name, module):
        # note that `name` and `module.name` are different!
        assert isinstance(module, Module)
        self.modules[name] = module

    def lookup(modvar):
        assert isinstance(modvar, ModuleVar)
        return self.modules[modvar]

class Env(object):
    _immutable_fields_ = ["toplevel_env", "module_env"]
    pass

class Version(object):
    pass

class ToplevelEnv(Env):
    _immutable_fields_ = ["version?"]
    def __init__(self):
        self.bindings = {}
        self.version = Version()
        self.toplevel_env = self # bit silly
        self.module_env = None # could put something else here?
        self.commandline_arguments = []

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
            self.bindings[sym].set_val(w_val)
        else:
            self.bindings[sym] = values.W_Cell(w_val)
            self.version = Version()

class ConsEnv(Env):
    _immutable_fields_ = ["prev", "toplevel_env", "module_env"]
    def __init__ (self, prev, toplevel, module):
        self.toplevel_env = toplevel
        self.prev = prev
        self.module_env = module

    @jit.unroll_safe
    def lookup(self, sym, env_structure):
        jit.promote(env_structure)
        for i, s in enumerate(env_structure.elems):
            if s is sym:
                v = self._get_list(i)
                assert v is not None
                return v
        return self.prev.lookup(sym, env_structure.prev)

    @jit.unroll_safe
    def set(self, sym, val, env_structure):
        jit.promote(env_structure)
        for i, s in enumerate(env_structure.elems):
            if s is sym:
                self._set_list(i, val)
                return
        return self.prev.set(sym, val, env_structure.prev)
inline_small_list(ConsEnv, immutable=True, attrname="vals")

def check_one_val(vals):
    if vals._get_size_list() != 1:
        raise SchemeException("expected 1 value but got %s"%(vals._get_size_list()))
    w_val = vals._get_list(0)
    return w_val

class LetrecCont(Cont):
    _immutable_fields_ = ["ast", "env", "prev", "i"]
    def __init__(self, ast, i, env, prev):
        self.ast = ast
        self.i = i
        self.env  = env
        self.prev = prev

    def plug_reduce(self, _vals):
        vals = _vals._get_full_list()
        ast = jit.promote(self.ast)
        if ast.counts[self.i] != _vals._get_size_list():
            raise SchemeException("wrong number of values")
        for j, w_val in enumerate(vals):
            v = self.env.lookup(ast.args.elems[ast.total_counts[self.i] + j], ast.args)
            assert isinstance(v, values.W_Cell)
            v.set_val(w_val)
        if self.i >= (len(ast.rhss) - 1):
            return ast.make_begin_cont(self.env, self.prev)
        else:
            return (ast.rhss[self.i + 1], self.env,
                    LetrecCont(ast, self.i + 1,
                               self.env, self.prev))

class LetCont(Cont):
    _immutable_fields_ = ["ast", "env", "prev"]

    def __init__(self, ast, env, prev, rhsindex):
        self.ast  = ast
        self.env  = env
        self.prev = prev
        self.rhsindex = rhsindex

    def plug_reduce(self, _vals):
        vals = _vals._get_full_list()
        ast = jit.promote(self.ast)
        rhsindex = jit.promote(self.rhsindex)
        if ast.counts[rhsindex] != len(vals):
            raise SchemeException("wrong number of values")
        if rhsindex == (len(ast.rhss) - 1):
            vals_w = self._get_full_list() + vals
            env = ConsEnv.make(vals_w, self.env, self.env.toplevel_env)
            return ast.make_begin_cont(env, self.prev)
        else:
            return (ast.rhss[rhsindex + 1], self.env,
                    LetCont.make(self._get_full_list() + vals, ast,
                                 self.env, self.prev, rhsindex + 1))

inline_small_list(LetCont, attrname="vals_w", immutable=True)


class CellCont(Cont):
    _immutable_fields_ = ["env", "prev"]

    def __init__(self, ast, env, prev):
        self.ast = ast
        self.env = env
        self.prev = prev

    @jit.unroll_safe
    def plug_reduce(self, vals):
        ast = jit.promote(self.ast)
        vals_w = []
        for i, needs_cell in enumerate(ast.need_cell_flags):
            w_val = vals._get_list(i)
            if needs_cell:
                w_val = values.W_Cell(w_val)
            vals_w.append(w_val)
        return return_multi_vals(values.Values.make(vals_w), self.env, self.prev)

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
    _attrs_ = ["should_enter"]
    _immutable_fields_ = ["should_enter"]
    _settled_ = True

    should_enter = False # default value

    simple = False

    def defined_vars(self): return {}

    def interpret(self, env, cont):
        # default implementation for simple AST forms
        assert self.simple
        return return_value(self.interpret_simple(env), env, cont)
    def interpret_simple(self, env):
        raise NotImplementedError("abstract base class")

    def let_convert(self):
        return self
    def free_vars(self):
        return {}
    def assign_convert(self, vars, env_structure):
        """ make a copy of the AST that converts all writable variables into
        using cells. In addition, compute the state of the environment for
        every AST node that needs to know.

        The vars argument contains the variables that need to use cells.
        The env_structure is an instance of SymList (or None) describing the
        environment at that AST node.
        """
        raise NotImplementedError("abstract base class")
    def mutated_vars(self):
        raise NotImplementedError("abstract base class")
    def tostring(self):
        raise NotImplementedError("abstract base class")

class Module(AST):
    _immutable_fields_ = ["name", "body"]
    def __init__(self, name, body):
        self.name = name
        self.body = body
        defs = {}
        for b in body:
            defs.update(b.defined_vars())
        self.defs = defs

    # these are both empty and irrelevant for modules
    def mutated_vars(self): return {}
    def free_vars(self): return {}

    # all the module-bound variables that are mutated
    def _mutated_vars(self):
        x = {}
        for r in self.body:
            x.update(r.mutated_vars())
        return x        

    def assign_convert(self, vars, env_structure):
        local_muts = self._mutated_vars()
        new_vars = vars.copy()
        new_vars.update(local_muts)
        new_body = [b.assign_convert(new_vars, env_structure) for b in self.body]
        return Module(self.name, new_body)
    def tostring(self):
        return "(module %s %s)"%(self.name," ".join([s.tostring() for s in self.body]))
        

def return_value(w_val, env, cont):
    return return_multi_vals(values.Values.make([w_val]), env, cont)

def return_multi_vals(vals, env, cont):
    if cont is None: 
        raise Done(vals)
    return cont.plug_reduce(vals)

class Cell(AST):
    _immutable_fields_ = ["expr", "need_cell_flags[*]"]
    def __init__(self, expr, need_cell_flags=None):
        if need_cell_flags is None:
            need_cell_flags = [True]
        self.expr = expr
        self.need_cell_flags = need_cell_flags

    def interpret(self, env, cont):
        return self.expr, env, CellCont(self, env, cont)

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
    def interpret_simple(self, env):
        return self.w_val
    def assign_convert(self, vars, env_structure):
        return self
    def mutated_vars(self):
        return {}
    def tostring(self):
        if isinstance(self.w_val, values.W_Bool) or isinstance(self.w_val, values.W_Number) or isinstance(self.w_val, values.W_String) or isinstance(self.w_val, values.W_Symbol):
            return "%s"%self.w_val.tostring()
        return "'%s"%self.w_val.tostring()

class App(AST):
    _immutable_fields_ = ["rator", "rands[*]", "remove_env"]


    def __init__ (self, rator, rands, remove_env=False):
        self.rator = rator
        self.rands = rands
        self.remove_env = remove_env
        self.should_enter = isinstance(rator, ModuleVar)

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
            return Let(SymList(fresh_vars[:]), [1] * len(fresh_vars), fresh_rhss[:], fresh_body)
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

    @jit.unroll_safe
    def interpret(self, env, cont):
        w_callable = self.rator.interpret_simple(env)
        args_w = [rand.interpret_simple(env) for rand in self.rands]
        if self.remove_env:
            # remove the env created by the let introduced by let_convert
            # it's no longer needed nor accessible
            # this whole stuff about the env seems useless in the App case,
            # because the callable will just ignore the passed in env. However,
            # we have a speculation in place in W_Procedure that checks whether
            # the closed over env is the same as the passed in one, which
            # breaks otherwise
            assert isinstance(env, ConsEnv)
            env = env.prev
        return w_callable.call(args_w, env, cont)

    def tostring(self):
        return "(%s %s)"%(self.rator.tostring(), " ".join([r.tostring() for r in self.rands]))

class SequencedBodyAST(AST):
    _immutable_fields_ = ["body[*]"]
    def __init__(self, body):
        assert isinstance(body, list)
        assert len(body) > 0
        self.body = body

    def make_begin_cont(self, env, prev, i=0):
        jit.promote(self)
        jit.promote(i)
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
    def interpret_simple(self, env):
        return self._lookup(env)
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
        v.set_val(w_val)
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
    def __init__(self, sym, srcmod, srcsym):
        self.sym = sym
        self.srcmod = srcmod
        self.srcsym = srcsym
    def _lookup(self, env):
        return self._prim_lookup()
    def free_vars(self): return {}
    @jit.elidable
    def _prim_lookup(self):
        try:
            return prim_env[self.sym]
        except KeyError:
            raise SchemeException("can't find primitive %s" % (self.sym.tostring(), ))
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
            return Let(SymList([fresh]),
                       [1], 
                       [self.tst],
                       [If(LexicalVar(fresh), self.thn, self.els, remove_env=True)])

    def interpret(self, env, cont):
        w_val = self.tst.interpret_simple(env)
        if self.remove_env:
            # remove the env created by the let introduced by let_convert
            # it's no longer needed nor accessible
            assert env._get_size_list() == 1
            assert isinstance(env, ConsEnv)
            env = env.prev
        if w_val is values.w_false:
            return self.els, env, cont
        else:
            return self.thn, env, cont

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
    simple = True
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

    def interpret_simple(self, env):
        e = ConsEnv.make([values.w_void], env, env.toplevel_env)
        try:
            Vcl, e, f = self.lam.interpret(e, None)
            assert 0
        except Done, e:
            vals = e.values
            cl = check_one_val(vals)
        assert isinstance(cl, values.W_Closure)
        cl.env.set(self.name, cl, self.lam.frees)
        return cl

    def tostring(self):
        b = " ".join([b.tostring() for b in self.lam.body])
        if self.lam.rest and (not self.lam.formals):
            return "(rec %s %s %s)"%(self.name, self.lam.rest, b)
        if self.lam.rest:
            return "(rec %s (%s . %s) %s)"%(self.name, " ".join([v.value for v in self.lam.formals]), self.lam.rest, b)
        else:
            return "(rec %s (%s) %s)"%(self.name, " ".join([v.value for v in self.lam.formals]), b)


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
    _immutable_fields_ = ["formals[*]", "rest", "args",
                          "frees", "enclosing_env_structure",
                          "w_closure_if_no_frees?",
                          ]
    simple = True
    def __init__ (self, formals, rest, args, frees, body, enclosing_env_structure=None):
        SequencedBodyAST.__init__(self, body)
        body[0].should_enter = True
        self.formals = formals
        self.rest = rest
        self.args = args
        self.frees = frees
        self.enclosing_env_structure = enclosing_env_structure
        self.w_closure_if_no_frees = None

    def interpret_simple(self, env):
        if not self.frees.elems:
            # cache closure if there are no free variables and the toplevel env
            # is the same as last time
            w_closure = self.w_closure_if_no_frees
            if w_closure is None or w_closure.env.toplevel_env is not env.toplevel_env:
                w_closure = values.W_PromotableClosure(self, env)
                self.w_closure_if_no_frees = w_closure
            return w_closure
        return values.W_Closure(self, env)

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
            new_body = [Let(sub_env_structure, [1] * len(new_lets), cells, new_body)]
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
            return "(lambda (%s) %s)"%(" ".join([v.value for v in self.formals]),
                                       self.body[0].tostring() if len(self.body) == 1 else
                                       [b.tostring() for b in self.body])


class Letrec(SequencedBodyAST):
    _immutable_fields_ = ["args", "rhss[*]", "counts[*]", "total_counts[*]"]
    def __init__(self, args, counts, rhss, body):
        assert len(counts) > 0 # otherwise just use a begin
        assert isinstance(args, SymList)
        SequencedBodyAST.__init__(self, body)
        self.counts = counts
        total_counts = []
        total_count = 0
        for i, count in enumerate(counts):
            total_counts.append(total_count)
            total_count += count
        self.total_counts = total_counts[:] # copy to make fixed-size
        self.rhss = rhss
        self.args = args
    def interpret(self, env, cont):
        env_new = ConsEnv.make([values.W_Cell(None) for var in self.args.elems], env, env.toplevel_env)
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
        return Letrec(sub_env_structure, self.counts, new_rhss, new_body)
    def tostring(self):
        return "(letrec (%s) %s)"%([(v.value,self.rhss[i].tostring()) for i, v in enumerate(self.args.elems)],
                                   [b.tostring() for b in self.body])

def make_let(varss, rhss, body):
    if not varss:
        return Begin.make(body)
    else:
        counts = []
        argsl = []
        for vars in varss:
            counts.append(len(vars))
            argsl += vars
        argsl = argsl[:] # copy to make fixed-size
        return Let(SymList(argsl), counts, rhss, body)

def make_letrec(varss, rhss, body):
    if (1 == len(varss) and
        1 == len(varss[0]) and
        1 == len(body) and
        isinstance(rhss[0], Lambda)):
        b = body[0]
        if isinstance(b, LexicalVar) and varss[0][0] is b.sym:
            return RecLambda(varss[0][0], rhss[0], SymList([varss[0][0]]))
    counts = []
    argsl = []
    for vars in varss:
        counts.append(len(vars))
        argsl = argsl + vars
    if not varss:
        return Begin.make(body)
    else:
        return Letrec(SymList(argsl), counts, rhss, body)

class Let(SequencedBodyAST):
    _immutable_fields_ = ["rhss[*]", "args", "counts[*]"]
    def __init__(self, args, counts, rhss, body):
        SequencedBodyAST.__init__(self, body)
        assert len(counts) > 0 # otherwise just use a begin
        assert isinstance(args, SymList)
        self.counts = counts
        self.rhss = rhss
        self.args = args

    def interpret(self, env, cont):
        return self.rhss[0], env, LetCont.make([], self, env, cont, 0)

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
        new_rhss = []
        for i, rhs in enumerate(self.rhss):
            new_rhs = rhs.assign_convert(vars, env_structure)
            need_cell_flags = [self.args.elems[i + j] in local_muts
                                   for j in range(self.counts[i])]
            if True in need_cell_flags:
                new_rhs = Cell(new_rhs, need_cell_flags)
            new_rhss.append(new_rhs)

        new_vars = vars.copy()
        new_vars.update(local_muts)
        sub_env_structure = SymList(self.args.elems, env_structure)
        new_body = [b.assign_convert(new_vars, sub_env_structure) for b in self.body]
        return Let(sub_env_structure, self.counts, new_rhss, new_body)

    def tostring(self):
        return "(let (%s) %s)"%(" ".join(["[%s %s]" % (v.value,self.rhss[i].tostring()) for i, v in enumerate(self.args.elems)]), 
                                " ".join([b.tostring() for b in self.body]))


class DefineValues(AST):
    _immutable_fields_ = ["names", "rhs"]
    names = []
    rhs = Quote(values.w_null)

    def __init__(self, ns, r):
        self.names = ns
        self.rhs = r

    def defined_vars(self):
        defs = {}
        for n in self.names:
            defs[n] = None
        return defs

    def interpret(self, env, cont):
        return self.rhs.interpret(env, cont)

    def assign_convert(self, vars, env_structure):
        return DefineValues(self.names, self.rhs.assign_convert(vars, env_structure))
    def mutated_vars(self): 
        return self.rhs.mutated_vars()
    def free_vars(self):
        vs = self.rhs.free_vars()
        for n in self.names:
            if n in vs:
                del vs[n]
        return vs
    def tostring(self):
        return "(define-values %s %s)"%(self.names, self.rhs.tostring())

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
    elif isinstance(a, DefineValues):
        assert 0 # FIXME
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


