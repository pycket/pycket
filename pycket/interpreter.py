from pycket.AST               import AST
from pycket                   import values, values_string
from pycket                   import vector
from pycket.prims.expose      import prim_env, make_call_method
from pycket.error             import SchemeException
from pycket.cont              import Cont, nil_continuation, label
from pycket.env               import SymList, ConsEnv, ToplevelEnv
from pycket.arity             import Arity
from pycket                   import config

from rpython.rlib             import jit, debug, objectmodel
from rpython.rlib.objectmodel import r_dict, compute_hash, specialize
from small_list               import inline_small_list

import sys

# imported for side effects
import pycket.prims.general


class Done(Exception):
    def __init__(self, vals):
        self.values = vals

def var_eq(a, b):
    if isinstance(a, LexicalVar) and isinstance(b, LexicalVar):
        return a.sym is b.sym
    elif isinstance(a, ModuleVar) and isinstance(b, ModuleVar):
        # two renamed variables can be the same
        return (a.srcmod == b.srcmod and a.srcsym is b.srcsym)
    return False

def var_hash(a):
    if isinstance(a, LexicalVar):
        return compute_hash(a.sym)
    elif isinstance(a, ModuleVar):
        return compute_hash( (a.srcsym, a.srcmod) )
    assert False

def variable_set():
    " new set-like structure for variables "
    return r_dict(var_eq, var_hash, force_non_null=True)

def variables_equal(a, b):
    if len(a) != len(b):
        return False
    for k, v in a.iteritems():
         if not k in b:
             return False
    return True

def check_one_val(vals):
    if not isinstance(vals, values.W_Object):
        raise SchemeException("expected 1 value but got %s"%(vals.num_values()))
    return vals

class LetrecCont(Cont):
    _immutable_fields_ = ["counting_ast", "env", "prev"]
    def __init__(self, counting_ast, env, prev):
        Cont.__init__(self, env, prev)
        self.counting_ast = counting_ast

    def get_ast(self):
        return self.counting_ast.ast

    def get_next_executed_ast(self):
        ast, rhsindex = self.counting_ast.unpack(Letrec)
        if rhsindex == (len(ast.rhss) - 1):
            return ast.body[0]
        return ast.rhss[rhsindex + 1]

    @jit.unroll_safe
    def plug_reduce(self, vals, env):
        ast, i = self.counting_ast.unpack(Letrec)
        if ast.counts[i] != vals.num_values():
            raise SchemeException("wrong number of values")
        for j in range(vals.num_values()):
            w_val = vals.get_value(j)
            v = self.env.lookup(ast.args.elems[ast.total_counts[i] + j], ast.args)
            assert isinstance(v, values.W_Cell)
            v.set_val(w_val)
        if i >= (len(ast.rhss) - 1):
            return ast.make_begin_cont(self.env, self.prev)
        else:
            return (ast.rhss[i + 1], self.env,
                    LetrecCont(ast.counting_asts[i + 1],
                               self.env, self.prev))


@inline_small_list(immutable=True, attrname="vals_w",
                   unbox_num=True, factoryname="_make")
class LetCont(Cont):
    _immutable_fields_ = ["counting_ast", "env", "prev"]

    def __init__(self, counting_ast, env, prev):
        Cont.__init__(self, env, prev)
        self.counting_ast  = counting_ast

    def get_ast(self):
        return self.counting_ast.ast

    def get_next_executed_ast(self):
        ast, rhsindex = self.counting_ast.unpack(Let)
        if rhsindex == (len(ast.rhss) - 1):
            return ast.body[0]
        return ast.rhss[rhsindex + 1]

    @staticmethod
    @jit.unroll_safe
    def make(vals_w, ast, rhsindex, env, prev, fuse=True, pruning_done=False):
        if not env.pycketconfig().fuse_conts:
            fuse = False
        counting_ast = ast.counting_asts[rhsindex]

        # try to fuse the two Conts
        if fuse and not vals_w:
            if isinstance(prev, LetCont) and prev._get_size_list() == 0:
                prev_counting_ast = prev.counting_ast
                prev_ast, _ = prev_counting_ast.unpack(Let)
                # check whether envs are the same:
                if prev_ast.args.prev is ast.args.prev and env is prev.env:
                    combined_ast = counting_ast.combine(prev_counting_ast)
                    return FusedLet0Let0Cont(combined_ast, env, prev.prev)
            elif isinstance(prev, BeginCont):
                prev_counting_ast = prev.counting_ast
                prev_ast, _ = prev_counting_ast.unpack(SequencedBodyAST)
                # check whether envs are the same:
                if env is prev.env: # XXX could use structure to check plausibility
                    combined_ast = counting_ast.combine(prev_counting_ast)
                    return FusedLet0BeginCont(combined_ast, env, prev.prev)

        if not pruning_done:
            env = ast._prune_env(env, rhsindex + 1)
        return LetCont._make(vals_w, counting_ast, env, prev)

    @jit.unroll_safe
    def plug_reduce(self, vals, _env):
        len_vals = vals.num_values()
        jit.promote(len_vals)
        len_self = self._get_size_list()
        jit.promote(len_self)
        new_length = len_self + len_vals
        ast, rhsindex = self.counting_ast.unpack(Let)
        assert isinstance(ast, Let)
        if ast.counts[rhsindex] != len_vals:
            raise SchemeException("wrong number of values")
        if rhsindex == (len(ast.rhss) - 1):
            prev = self.env
            if ast.env_speculation_works:
                # speculate moar!
                if _env is self.env:
                    prev = _env
                else:
                    if not jit.we_are_jitted():
                        ast.env_speculation_works = False
            env = self._construct_env(len_self, vals, len_vals, new_length, prev)
            return ast.make_begin_cont(env, self.prev)
        else:
            # XXX remove copy
            vals_w = [None] * new_length
            i = 0
            for j in range(len_self):
                vals_w[i] = self._get_list(j)
                i += 1
            for j in range(len_vals):
                vals_w[i] = vals.get_value(j)
                i += 1
            return (ast.rhss[rhsindex + 1], self.env,
                    LetCont.make(vals_w, ast, rhsindex + 1,
                                 self.env, self.prev))

    @jit.unroll_safe
    def _construct_env(self, len_self, vals, len_vals, new_length, prev):
        # this is a complete mess. however, it really helps warmup a lot
        if new_length == 0:
            return ConsEnv.make0(prev)
        if new_length == 1:
            if len_self == 1:
                elem = self._get_list(0)
            else:
                assert len_self == 0 and len_vals == 1
                elem = vals.get_value(0)
            return ConsEnv.make1(elem, prev)
        if new_length == 2:
            if len_self == 0:
                assert len_vals == 2
                elem1 = vals.get_value(0)
                elem2 = vals.get_value(1)
            elif len_self == 1:
                assert len_vals == 1
                elem1 = self._get_list(0)
                elem2 = vals.get_value(0)
            else:
                assert len_self == 2 and len_vals == 0
                elem1 = self._get_list(0)
                elem2 = self._get_list(1)
            return ConsEnv.make2(elem1, elem2, prev)
        env = ConsEnv.make_n(new_length, prev)
        i = 0
        for j in range(len_self):
            env._set_list(i, self._get_list(j))
            i += 1
        for j in range(len_vals):
            env._set_list(i, vals.get_value(j))
            i += 1
        return env



class FusedLet0Let0Cont(Cont):
    _immutable_fields_ = ["combined_ast"]
    def __init__(self, combined_ast, env, prev):
        Cont.__init__(self, env, prev)
        self.combined_ast = combined_ast

    def get_ast(self):
        return self.combined_ast.ast1.ast

    def plug_reduce(self, vals, env):
        ast1, ast2 = self.combined_ast.unpack()
        ast1, index1 = ast1.unpack(Let)
        ast2, index2 = ast2.unpack(Let)
        actual_cont = LetCont.make(
                None, ast1, index1, self.env,
                LetCont.make(
                    None, ast2, index2, self.env, self.prev, fuse=False,
                    pruning_done=True),
                fuse=False)
        return actual_cont.plug_reduce(vals, env)


class FusedLet0BeginCont(Cont):
    _immutable_fields_ = ["combined_ast"]
    def __init__(self, combined_ast, env, prev):
        Cont.__init__(self, env, prev)
        self.combined_ast = combined_ast

    def get_ast(self):
        return self.combined_ast.ast1.ast

    def plug_reduce(self, vals, env):
        ast1, ast2 = self.combined_ast.unpack()
        ast1, index1 = ast1.unpack(Let)
        actual_cont = LetCont.make(
                None, ast1, index1, self.env,
                BeginCont(ast2, self.env, self.prev),
                fuse=False)
        return actual_cont.plug_reduce(vals, env)


class CellCont(Cont):
    _immutable_fields_ = ["env", "prev"]

    def __init__(self, ast, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast

    def get_ast(self):
        return self.ast

    @jit.unroll_safe
    def plug_reduce(self, vals, env):
        ast = jit.promote(self.ast)
        vals_w = []
        for i, needs_cell in enumerate(ast.need_cell_flags):
            w_val = vals.get_value(i)
            if needs_cell:
                w_val = values.W_Cell(w_val)
            vals_w.append(w_val)
        return return_multi_vals(values.Values.make(vals_w), self.env, self.prev)

class SetBangCont(Cont):
    _immutable_fields_ = ["ast", "env", "prev"]
    def __init__(self, ast, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast

    def get_ast(self):
        return self.ast

    def plug_reduce(self, vals, env):
        w_val = check_one_val(vals)
        self.ast.var._set(w_val, self.env)
        return return_value(values.w_void, self.env, self.prev)

class BeginCont(Cont):
    _immutable_fields_ = ["counting_ast", "env", "prev"]
    def __init__(self, counting_ast, env, prev):
        Cont.__init__(self, env, prev)
        self.counting_ast = counting_ast

    def get_ast(self):
        return self.counting_ast.ast

    def get_next_executed_ast(self):
        ast, i = self.counting_ast.unpack(SequencedBodyAST)
        return ast.body[i]

    def plug_reduce(self, vals, env):
        ast, i = self.counting_ast.unpack(SequencedBodyAST)
        return ast.make_begin_cont(self.env, self.prev, i)

# FIXME: it would be nice to not need two continuation types here
class Begin0Cont(Cont):
    _immutable_fields_ = ["ast", "env", "prev"]
    def __init__(self, ast, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast

    def get_ast(self):
        return self.ast

    def get_next_executed_ast(self):
        return self.ast

    def plug_reduce(self, vals, env):
        return self.ast.body, self.env, Begin0FinishCont(self.ast, vals, self.env, self.prev)

class Begin0FinishCont(Cont):
    _immutable_fields_ = ["ast", "vals", "env", "prev"]
    def __init__(self, ast, vals, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast
        self.vals = vals
    def plug_reduce(self, vals, env):
        return return_multi_vals(self.vals, self.env, self.prev)

class WCMKeyCont(Cont):
    _immutable_fields_ = ["ast", "env", "prev"]
    def __init__(self, ast, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast

    def get_ast(self):
        return self.ast

    def get_next_executed_ast(self):
        return self.ast.value

    def plug_reduce(self, vals, env):
        key = check_one_val(vals)
        return self.ast.value, self.env, WCMValCont(self.ast, key, self.env, self.prev)

class WCMValCont(Cont):
    _immutable_fields_ = ["ast", "env", "prev", "key"]
    def __init__(self, ast, key, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast
        self.key = key

    def get_ast(self):
        return self.ast

    def get_next_executed_ast(self):
        return self.ast.body

    def plug_reduce(self, vals, env):
        val = check_one_val(vals)
        key = self.key
        if isinstance(key, values.W_ContinuationMarkKey):
            body = values.W_ThunkBodyCMK(self.ast.body)
            return key.set_cmk(body, val, self.prev, env, self.prev)
        self.prev.update_cm(key, val)
        return self.ast.body, self.env, self.prev

class Module(object):
    _immutable_fields_ = ["name", "body"]
    def __init__(self, name, body, config):
        self.name = name
        self.body = body
        self.env = None
        self.config = config
        defs = {}
        for b in body:
            defs.update(b.defined_vars())
        self.defs = defs

    @jit.elidable
    def lookup(self, sym):
        if sym not in self.defs:
            raise SchemeException("unknown module variable %s" % (sym.tostring()))
        v = self.defs[sym]
        if not v:
            raise SchemeException("use of module variable before definition %s" % (sym.tostring()))
        return v

    # all the module-bound variables that are mutated
    def mod_mutated_vars(self):
        x = variable_set()
        for r in self.body:
            x.update(r.mutated_vars())
        return x

    def assign_convert_module(self):
        local_muts = self.mod_mutated_vars()
        new_body = [b.assign_convert(local_muts, None) for b in self.body]
        return Module(self.name, new_body, self.config)

    def tostring(self):
        return "(module %s %s)"%(self.name," ".join([s.tostring() for s in self.body]))

    def interpret_mod(self, env):
        try:
            return self._interpret_mod(env)
        except SchemeException, e:
            if e.context_module is None:
                e.context_module = self
            raise

    def _interpret_mod(self, env):
        self.env = env
        module_env = env.toplevel_env().module_env
        old = module_env.current_module
        module_env.current_module = self
        for f in self.body:
            # FIXME: this is wrong -- the continuation barrier here is around the RHS,
            # whereas in Racket it's around the whole `define-values`
            if isinstance(f, DefineValues):
                e = f.rhs
                vs = interpret_one(e, self.env).get_all_values()
                if len(f.names) == len(vs):
                    for n in range(len(vs)):
                        self.defs[f.names[n]] = vs[n]
                else:
                    raise SchemeException("wrong number of values for define-values")
            else: # FIXME modules can have other things, assuming expression
                vs = interpret_one(f, self.env)
                continue
        module_env.current_module = old

class Require(AST):
    _immutable_fields_ = ["modname", "module"]
    simple = True

    def __init__(self, modname, module):
        self.modname = modname
        self.module  = module

    def _mutated_vars(self):
        return variable_set()

    def assign_convert(self, vars, env_structure):
        return self

    # Interpret the module and add it to the module environment
    def interpret_simple(self, env):
        top = env.toplevel_env()
        top.module_env.add_module(self.modname, self.module)
        self.module.interpret_mod(top)
        return values.w_void

    def _tostring(self):
        return "(require %s)"%self.modname

empty_vals = values.Values.make([])

def jump(env, cont):
    return return_multi_vals(empty_vals, env, cont)

def return_value(w_val, env, cont):
    return return_multi_vals(values.Values.make1(w_val), env, cont)

def return_value_direct(w_val, env, cont):
    """ like return_value, but without using a label. only safe to use in
    AST.interpret and (automatically) by simple primitives """
    val = values.Values.make1(w_val)
    return cont.plug_reduce(val, env)

@label
def return_multi_vals(vals, env, cont):
    return cont.plug_reduce(vals, env)

def return_multi_vals_direct(vals, env, cont):
    return cont.plug_reduce(vals, env)

class Cell(AST):
    _immutable_fields_ = ["expr", "need_cell_flags[*]"]
    def __init__(self, expr, need_cell_flags=None):
        if need_cell_flags is None:
            need_cell_flags = [True]
        self.expr = expr
        self.need_cell_flags = need_cell_flags

    def interpret(self, env, cont):
        return self.expr, env, CellCont(self, env, cont)

    def assign_convert(self, vars, env_structure):
        return Cell(self.expr.assign_convert(vars, env_structure))

    def direct_children(self):
        return [self.expr]

    def _mutated_vars(self):
        return self.expr.mutated_vars()

    def _tostring(self):
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

    def direct_children(self):
        return []

    def _mutated_vars(self):
        return variable_set()

    def _tostring(self):
        if (isinstance(self.w_val, values.W_Bool) or
                isinstance(self.w_val, values.W_Number) or
                isinstance(self.w_val, values_string.W_String) or
                isinstance(self.w_val, values.W_Symbol)):
            return "%s" % self.w_val.tostring()
        return "'%s" % self.w_val.tostring()

class QuoteSyntax(AST):
    _immutable_fields_ = ["w_val"]
    simple = True
    def __init__ (self, w_val):
        self.w_val = w_val

    def interpret_simple(self, env):
        return values.W_Syntax(self.w_val)

    def assign_convert(self, vars, env_structure):
        return self

    def direct_children(self):
        return []

    def _mutated_vars(self):
        return variable_set()

    def _tostring(self):
        return "#'%s" % self.w_val.tostring()

class VariableReference(AST):
    _immutable_fields_ = ["var", "is_mut", "path"]
    simple = True
    def __init__ (self, var, path, is_mut=False):
        self.var = var
        self.path = path
        self.is_mut = is_mut

    def is_mutable(self, env):
        if self.is_mut:
            return True
        var = self.var
        if isinstance(var, ModuleVar):
            return var.is_mutable(env)
        else:
            return False

    def interpret_simple(self, env):
        return values.W_VariableReference(self)

    def assign_convert(self, vars, env_structure):
        v = self.var
        if isinstance(v, LexicalVar) and v in vars:
            return VariableReference(v, self.path, True)
        # top-level variables are always mutable
        if isinstance(v, ToplevelVar):
            return VariableReference(v, self.path, True)
        else:
            return self

    def direct_children(self):
        return []

    def _mutated_vars(self):
        return variable_set()

    def _tostring(self):
        return "#<#%variable-reference>"


class WithContinuationMark(AST):
    _immutable_fields_ = ["key", "value", "body"]

    def __init__(self, key, value, body):
        self.key = key
        self.value = value
        self.body = body

    def _tostring(self):
        return "(with-continuation-mark %s %s %s)"%(self.key.tostring(),
                                                    self.value.tostring(),
                                                    self.body.tostring())

    def assign_convert(self, vars, env_structure):
        return WithContinuationMark(self.key.assign_convert(vars, env_structure),
                                    self.value.assign_convert(vars, env_structure),
                                    self.body.assign_convert(vars, env_structure))

    def direct_children(self):
        return [self.key, self.value, self.body]

    def _mutated_vars(self):
        x = self.key.mutated_vars()
        for r in [self.value, self.body]:
            x.update(r.mutated_vars())
        return x

    def interpret(self, env, cont):
        return self.key, env, WCMKeyCont(self, env, cont)

class App(AST):
    _immutable_fields_ = ["rator", "rands[*]", "env_structure"]

    def __init__ (self, rator, rands, env_structure=None):
        assert rator.simple
        for r in rands:
            assert r.simple
        self.rator = rator
        self.rands = rands
        self.env_structure = env_structure

    @staticmethod
    def make(rator, rands, env_structure=None):
        if isinstance(rator, ModuleVar) and rator.is_primitive():
            try:
                w_prim = rator._lookup_primitive()
            except SchemeException:
                pass
            else:
                if isinstance(w_prim, values.W_Prim):
                    if w_prim.simple1 and len(rands) == 1:
                        return SimplePrimApp1(rator, rands, env_structure, w_prim)
                    if w_prim.simple2 and len(rands) == 2:
                        return SimplePrimApp2(rator, rands, env_structure, w_prim)
        return App(rator, rands, env_structure)

    @staticmethod
    def make_let_converted(rator, rands):
        all_args = [rator] + rands
        fresh_vars = []
        fresh_rhss = []

        name = "AppRator_"
        for i, rand in enumerate(all_args):
            if not rand.simple:
                fresh_rand = Gensym.gensym(name)
                fresh_rand_var = LexicalVar(fresh_rand)
                if isinstance(rand, Let) and len(rand.body) == 1:
                    # this is quadratic for now :-(
                    if not fresh_vars:
                        all_args[i] = fresh_rand_var
                        return rand.replace_innermost_with_app(fresh_rand, all_args[0], all_args[1:])
                    else:
                        fresh_body = [App.make_let_converted(all_args[0], all_args[1:])]
                        return Let(SymList(fresh_vars[:]), [1] * len(fresh_vars), fresh_rhss[:], fresh_body)
                all_args[i] = fresh_rand_var
                fresh_rhss.append(rand)
                fresh_vars.append(fresh_rand)
            name = "AppRand%s_"%i
        # The body is an App operating on the freshly bound symbols
        if fresh_vars:
            fresh_body = [App.make(all_args[0], all_args[1:])]
            return Let(SymList(fresh_vars[:]), [1] * len(fresh_vars), fresh_rhss[:], fresh_body)
        else:
            return App.make(rator, rands)

    def assign_convert(self, vars, env_structure):
        return App.make(self.rator.assign_convert(vars, env_structure),
                   [e.assign_convert(vars, env_structure) for e in self.rands],
                   env_structure=env_structure)

    def direct_children(self):
        return [self.rator] + self.rands

    def _mutated_vars(self):
        x = self.rator.mutated_vars()
        for r in self.rands:
            x.update(r.mutated_vars())
        return x

    # Let conversion ensures that all the participants in an application
    # are simple.
    @jit.unroll_safe
    def interpret(self, env, cont):
        rator = self.rator
        if (not env.pycketconfig().callgraph and
                isinstance(rator, ModuleVar) and
                rator.is_primitive()):
            self.set_should_enter() # to jit downrecursion
        w_callable = rator.interpret_simple(env)
        args_w = [None] * len(self.rands)
        for i, rand in enumerate(self.rands):
            args_w[i] = rand.interpret_simple(env)
        if isinstance(w_callable, values.W_PromotableClosure):
            # fast path
            jit.promote(w_callable)
            w_callable = w_callable.closure
        return w_callable.call_with_extra_info(args_w, env, cont, self)

    def _tostring(self):
        return "(%s %s)"%(self.rator.tostring(), " ".join([r.tostring() for r in self.rands]))


class SimplePrimApp1(App):
    _immutable_fields_ = ['w_prim', 'rand1']
    simple = True

    def __init__(self, rator, rands, env_structure, w_prim):
        App.__init__(self, rator, rands, env_structure)
        assert len(rands) == 1
        self.rand1, = rands
        self.w_prim = w_prim

    def run(self, env):
        result = self.w_prim.simple1(self.rand1.interpret_simple(env))
        if result is None:
            result = values.w_void
        return result

    def interpret_simple(self, env):
        return check_one_val(self.run(env))

    def interpret(self, env, cont):
        if not env.pycketconfig().callgraph:
            self.set_should_enter() # to jit downrecursion
        result = self.run(env)
        return return_multi_vals_direct(result, env, cont)


class SimplePrimApp2(App):
    _immutable_fields_ = ['w_prim', 'rand1', 'rand2']
    simple = True

    def __init__(self, rator, rands, env_structure, w_prim):
        App.__init__(self, rator, rands, env_structure)
        assert len(rands) == 2
        self.rand1, self.rand2 = rands
        self.w_prim = w_prim

    def run(self, env):
        arg1 = self.rand1.interpret_simple(env)
        arg2 = self.rand2.interpret_simple(env)
        result = self.w_prim.simple2(arg1, arg2)
        if result is None:
            result = values.w_void
        return result

    def interpret_simple(self, env):
        return check_one_val(self.run(env))

    def interpret(self, env, cont):
        if not env.pycketconfig().callgraph:
            self.set_should_enter() # to jit downrecursion
        result = self.run(env)
        return return_multi_vals_direct(result, env, cont)

class SequencedBodyAST(AST):
    _immutable_fields_ = ["body[*]", "counting_asts[*]"]
    def __init__(self, body, counts_needed=-1):
        assert isinstance(body, list)
        assert len(body) > 0
        self.body = body
        if counts_needed < len(self.body) + 1:
            counts_needed = len(self.body) + 1
        self.counting_asts = [
            CombinedAstAndIndex(self, i)
                for i in range(counts_needed)]

    @objectmodel.always_inline
    def make_begin_cont(self, env, prev, i=0):
        jit.promote(self)
        jit.promote(i)
        if i == len(self.body) - 1:
            return self.body[i], env, prev
        else:
            return self.body[i], env, BeginCont(
                    self.counting_asts[i + 1], env, prev)


class Begin0(AST):
    _immutable_fields_ = ["first", "body"]

    @staticmethod
    def make(fst, rst):
        if rst:
            return Begin0(fst, Begin.make(rst))
        return fst

    def __init__(self, fst, rst):
        assert isinstance(rst, AST)
        self.first = fst
        self.body = rst

    def assign_convert(self, vars, env_structure):
        return Begin0(self.first.assign_convert(vars, env_structure),
                      self.body.assign_convert(vars, env_structure))

    def direct_children(self):
        return [self.first, self.body]

    def _mutated_vars(self):
        x = variable_set()
        for r in [self.first, self.body]:
            x.update(r.mutated_vars())
        return x

    def _tostring(self):
        return "(begin0 %s %s)" % (self.first.tostring(), self.body.tostring())

    def interpret(self, env, cont):
        return self.first, env, Begin0Cont(self, env, cont)


class Begin(SequencedBodyAST):
    @staticmethod
    def make(body):
        if len(body) == 1:
            return body[0]
        else:
            return Begin(body)

    def assign_convert(self, vars, env_structure):
        return Begin.make([e.assign_convert(vars, env_structure) for e in self.body])

    def direct_children(self):
        return self.body

    def _mutated_vars(self):
        x = variable_set()
        for r in self.body:
            x.update(r.mutated_vars())
        return x

    @objectmodel.always_inline
    def interpret(self, env, cont):
        return self.make_begin_cont(env, cont)

    def _tostring(self):
        return "(begin %s)" % (" ".join([e.tostring() for e in self.body]))

class Var(AST):
    _immutable_fields_ = ["sym", "env_structure"]
    simple = True

    def __init__ (self, sym, env_structure=None):
        assert isinstance(sym, values.W_Symbol)
        self.sym = sym
        self.env_structure = env_structure

    def interpret_simple(self, env):
        val = self._lookup(env)
        if val is None:
            raise SchemeException("%s: undefined" % self.sym.utf8value)
        return val

    def direct_children(self):
        return []

    def _mutated_vars(self):
        return variable_set()

    def free_vars(self):
        return {self.sym: None}

    def _tostring(self):
        return "%s" % self.sym.variable_name()


class CellRef(Var):
    def assign_convert(self, vars, env_structure):
        return CellRef(self.sym, env_structure)

    def _tostring(self):
        return "CellRef(%s)" % Var._tostring(self)

    def _set(self, w_val, env):
        v = env.lookup(self.sym, self.env_structure)
        assert isinstance(v, values.W_Cell)
        v.set_val(w_val)

    def _lookup(self, env):
        v = env.lookup(self.sym, self.env_structure)
        assert isinstance(v, values.W_Cell)
        return v.get_val()

class Gensym(object):
    _counter = {}

    @staticmethod
    def gensym(hint="g"):
        count = Gensym._counter[hint] = Gensym._counter.get(hint, -1) +  1
        # not using `make` so that it's really gensym
        return values.W_Symbol(unicode(hint + str(count)))


class LexicalVar(Var):
    def _lookup(self, env):
        if not objectmodel.we_are_translated():
            self.env_structure.check_plausibility(env)
        return env.lookup(self.sym, self.env_structure)

    def _set(self, w_val, env):
        assert 0

    def assign_convert(self, vars, env_structure):
        #assert isinstance(vars, r_dict)
        if self in vars:
            return CellRef(self.sym, env_structure)
        else:
            return LexicalVar(self.sym, env_structure)

class ModuleVar(Var):
    _immutable_fields_ = ["modenv?", "sym", "srcmod", "srcsym", "w_value?"]
    def __init__(self, sym, srcmod, srcsym):
        Var.__init__(self, sym)
        self.srcmod = srcmod
        self.srcsym = srcsym
        self.modenv = None
        self.w_value = None

    def free_vars(self):
        return {}

    def _lookup(self, env):
        w_res = self.w_value
        if w_res is None:
            if self.modenv is None:
                self.modenv = env.toplevel_env().module_env
            self.w_value = w_res = self._elidable_lookup()

        if type(w_res) is values.W_Cell:
            return w_res.get_val()
        else:
            return w_res

    def is_mutable(self, env):
        if self.modenv is None:
            self.modenv = env.toplevel_env().module_env
        v = self._elidable_lookup()
        return isinstance(v, values.W_Cell)

    @jit.elidable
    def is_primitive(self):
        return self.srcmod in ["#%kernel", "#%unsafe", "#%paramz", "#%flfxnum", "#%utils", "#%place"]

    @jit.elidable
    def _elidable_lookup(self):
        assert self.modenv
        modenv = self.modenv
        if self.srcmod is None:
            mod = modenv.current_module
        elif self.is_primitive():
            return self._lookup_primitive()
        else:
            mod = modenv._find_module(self.srcmod)
            if mod is None:
                raise SchemeException("can't find module %s for %s" % (self.srcmod, self.srcsym.tostring()))
        return mod.lookup(self.srcsym)

    def _lookup_primitive(self):
        # we don't separate these the way racket does
        # but maybe we should
        try:
            return prim_env[self.srcsym]
        except KeyError:
            raise SchemeException("can't find primitive %s" % (self.srcsym.tostring()))

    def assign_convert(self, vars, env_structure):
        return self
        # # we use None here for hashing because we don't have the module name in the
        # # define-values when we need to look this up.
        # if ModuleVar(self.sym, None, self.srcsym) in vars:
        #     return ModCellRef(self.sym, self.srcmod, self.srcsym)
        # else:
        #     return self

    def _set(self, w_val, env):
        if self.modenv is None:
            self.modenv = env.toplevel_env().module_env
        v = self._elidable_lookup()
        assert isinstance(v, values.W_Cell)
        v.set_val(w_val)


# class ModCellRef(Var):
#     _immutable_fields_ = ["sym", "srcmod", "srcsym", "modvar"]

#     def __init__(self, sym, srcmod, srcsym, env_structure=None):
#         self.sym = sym
#         self.srcmod = srcmod
#         self.srcsym = srcsym
#         self.modvar = ModuleVar(self.sym, self.srcmod, self.srcsym)
#     def assign_convert(self, vars, env_structure):
#         return ModCellRef(self.sym, self.srcmod, self.srcsym)
#     def _tostring(self):
#         return "ModCellRef(%s)" %variable_name(self.sym)
#     def _set(self, w_val, env):
#         w_res = self.modvar._lookup(env)
#         assert isinstance(w_res, values.W_Cell)
#         w_res.set_val(w_val)
#     def _lookup(self, env):
#         w_res = self.modvar._lookup(env)
#         assert isinstance(w_res, values.W_Cell)
#         return w_res.get_val()
#     def to_modvar(self):
#         # we use None here for hashing because we don't have the module name in the
#         # define-values when we need to look this up.
#         return ModuleVar(self.sym, None, self.srcsym)


class ToplevelVar(Var):
    def _lookup(self, env):
        return env.toplevel_env().toplevel_lookup(self.sym)

    def assign_convert(self, vars, env_structure):
        return self

    def _set(self, w_val, env):
        env.toplevel_env().toplevel_set(self.sym, w_val)

# rewritten version for caching
def to_modvar(m):
    return ModuleVar(m.sym, None, m.srcsym)

class SetBang(AST):
    _immutable_fields_ = ["var", "rhs"]
    def __init__(self, var, rhs):
        self.var = var
        self.rhs = rhs

    def interpret(self, env, cont):
        return self.rhs, env, SetBangCont(self, env, cont)

    def assign_convert(self, vars, env_structure):
        return SetBang(self.var.assign_convert(vars, env_structure),
                       self.rhs.assign_convert(vars, env_structure))

    def _mutated_vars(self):
        x = self.rhs.mutated_vars()
        var = self.var
        if isinstance(var, CellRef):
            x[LexicalVar(self.var.sym)] = None
        # even though we don't change these to cell refs, we still
        # have to convert the definitions
        elif isinstance(var, ModuleVar):
            x[to_modvar(var)] = None
        # do nothing for top-level vars, they're all mutated
        return x

    def direct_children(self):
        return [self.var, self.rhs]

    def _tostring(self):
        return "(set! %s %s)" % (self.var.sym.variable_name(), self.rhs.tostring())

class If(AST):
    _immutable_fields_ = ["tst", "thn", "els"]
    def __init__ (self, tst, thn, els):
        assert tst.simple
        self.tst = tst
        self.thn = thn
        self.els = els

    @staticmethod
    def make_let_converted(tst, thn, els):
        if tst.simple:
            return If(tst, thn, els)
        else:
            fresh = Gensym.gensym("if_")
            return Let(SymList([fresh]),
                       [1],
                       [tst],
                       [If(LexicalVar(fresh), thn, els)])

    @objectmodel.always_inline
    def interpret(self, env, cont):
        w_val = self.tst.interpret_simple(env)
        if w_val is values.w_false:
            return self.els, env, cont
        else:
            return self.thn, env, cont

    def assign_convert(self, vars, env_structure):
        sub_env_structure = env_structure
        return If(self.tst.assign_convert(vars, env_structure),
                  self.thn.assign_convert(vars, sub_env_structure),
                  self.els.assign_convert(vars, sub_env_structure))

    def direct_children(self):
        return [self.tst, self.thn, self.els]

    def _mutated_vars(self):
        x = variable_set()
        for b in [self.tst, self.els, self.thn]:
            x.update(b.mutated_vars())
        return x

    def _tostring(self):
        return "(if %s %s %s)" % (self.tst.tostring(), self.thn.tostring(), self.els.tostring())

def make_lambda(formals, rest, body, srcpos, srcfile):
    args = SymList(formals + ([rest] if rest else []))
    frees = SymList(free_vars_lambda(body, args).keys())
    args = SymList(args.elems, frees)
    return Lambda(formals, rest, args, frees, body, srcpos, srcfile)


def free_vars_lambda(body, args):
    x = {}
    for b in body:
        x.update(b.free_vars())
    for v in args.elems:
        if v in x:
            del x[v]
    return x

class CaseLambda(AST):
    _immutable_fields_ = ["lams[*]", "any_frees", "recursive_sym", "w_closure_if_no_frees?"]
    simple = True

    def __init__(self, lams, recursive_sym=None):
        ## TODO: drop lams whose arity is redundant
        ## (case-lambda [x 0] [(y) 1]) == (lambda x 0)
        self.lams = lams
        self.any_frees = False
        for l in lams:
            if l.frees.elems:
                self.any_frees = True
                break
        self.w_closure_if_no_frees = None
        self.recursive_sym = recursive_sym
        self._arity = None

    @jit.unroll_safe
    def enable_jitting(self):
        for l in self.lams:
            l.enable_jitting()

    def make_recursive_copy(self, sym):
        return CaseLambda(self.lams, sym)

    def interpret_simple(self, env):
        if not env.pycketconfig().callgraph:
            self.enable_jitting() # XXX not perfectly pretty
        if not self.any_frees:
            # cache closure if there are no free variables and the toplevel env
            # is the same as last time
            w_closure = self.w_closure_if_no_frees
            if w_closure is None:
                w_closure = values.W_PromotableClosure(self, env.toplevel_env())
                self.w_closure_if_no_frees = w_closure
            else:
                if not jit.we_are_jitted():
                    assert w_closure.closure._get_list(0).toplevel_env() is env.toplevel_env()
            return w_closure
        return values.W_Closure.make(self, env)

    def free_vars(self):
        result = AST.free_vars(self)
        if self.recursive_sym in result:
            del result[self.recursive_sym]
        return result

    def direct_children(self):
        # the copy is needed for weird annotator reasons that I don't understand :-(
        return [l for l in self.lams]

    def _mutated_vars(self):
        x = variable_set()
        for l in self.lams:
            x.update(l.mutated_vars())
        return x

    def assign_convert(self, vars, env_structure):
        ls = [l.assign_convert(vars, env_structure) for l in self.lams]
        return CaseLambda(ls, recursive_sym=self.recursive_sym)

    def _tostring(self):
        if len(self.lams) == 1:
            return self.lams[0].tostring()
        return "(case-lambda %s)" % (" ".join([l.tostring() for l in self.lams]))

    def tostring_as_closure(self):
        if len(self.lams) == 0:
            return "#<procedure>"
        lam = self.lams[0]
        file, pos = lam.srcfile, lam.srcpos
        if file and (pos >= 0):
            return "#<procedure:%s:%s>" % (lam.srcfile, lam.srcpos)
        if file:
            return "#<procedure:%s>" % (lam.srcfile)
        return "#<procedure>"

    @jit.elidable
    def get_arity(self):
        if self._arity is not None:
            return self._arity
        arities = []
        rest = -1
        for l in self.lams:
            n = l.get_arity()
            if n < 0:
                r = (-n - 1)
                if rest >= 0:
                    rest = min(r, rest)
                else:
                    rest = r
            else:
                arities = arities + [n]
        self._arity = Arity(arities[:], rest)
        return self._arity

class Lambda(SequencedBodyAST):
    _immutable_fields_ = ["formals[*]", "rest", "args",
                          "frees", "enclosing_env_structure", 'env_structure'
                          ]
    simple = True
    def __init__ (self, formals, rest, args, frees, body, srcpos, srcfile, enclosing_env_structure=None, env_structure=None):
        SequencedBodyAST.__init__(self, body)
        self.srcpos = srcpos
        self.srcfile = srcfile
        self.formals = formals
        self.rest = rest
        self.args = args
        self.frees = frees
        self.enclosing_env_structure = enclosing_env_structure
        self.env_structure = env_structure
        for b in self.body:
            b.set_surrounding_lambda(self)

    def enable_jitting(self):
        self.body[0].set_should_enter()

    # returns n for fixed arity, -(n+1) for arity-at-least n
    # my kingdom for Either
    def get_arity(self):
        if self.rest:
            return -(len(self.formals)+1)
        else:
            return len(self.formals)

    def interpret_simple(self, env):
        assert False # unreachable

    def assign_convert(self, vars, env_structure):
        local_muts = variable_set()
        for b in self.body:
            local_muts.update(b.mutated_vars())
        new_lets = []
        new_vars = vars.copy()
        for i in self.args.elems:
            li = LexicalVar(i)
            if li in new_vars:
                del new_vars[li]
            if li in local_muts:
                new_lets.append(i)
        for k, v in local_muts.iteritems():
            new_vars[k] = v
        if new_lets:
            sub_env_structure = SymList(new_lets, self.args)
        else:
            sub_env_structure = self.args
        new_body = [b.assign_convert(new_vars, sub_env_structure) for b in self.body]
        if new_lets:
            cells = [Cell(LexicalVar(v, self.args)) for v in new_lets]
            new_body = [Let(sub_env_structure, [1] * len(new_lets), cells, new_body)]
        return Lambda(self.formals, self.rest, self.args, self.frees, new_body,
                      self.srcpos, self.srcfile, env_structure, sub_env_structure)

    def direct_children(self):
        return self.body[:]

    def set_surrounding_lambda(self, lam):
        self.surrounding_lambda = lam
        # don't recurse

    def _mutated_vars(self):
        x = variable_set()
        for b in self.body:
            x.update(b.mutated_vars())
        for v in self.args.elems:
            lv = LexicalVar(v)
            if lv in x:
                del x[lv]
        return x

    def free_vars(self):
        result = free_vars_lambda(self.body, self.args)
        return result

    def match_args(self, args):
        fmls_len = len(self.formals)
        args_len = len(args)
        if fmls_len != args_len and not self.rest:
            # don't format errors here, this is often caught and discarded
            raise SchemeException("wrong args")
        if fmls_len > args_len:
            raise SchemeException("not enough args")
        if self.rest:
            actuals = args[0:fmls_len] + [values.to_list(args[fmls_len:])]
        else:
            actuals = args
        return actuals

    def raise_nice_error(self, args):
        fmls_len = len(self.formals)
        args_len = len(args)
        if fmls_len != args_len and not self.rest:
            raise SchemeException(
                "wrong number of arguments to %s, expected %s but got %s" % (
                    self.tostring(), fmls_len,args_len))
        if fmls_len > args_len:
            raise SchemeException(
                "wrong number of arguments to %s, expected at least %s but got %s" % (
                    self.tostring(), fmls_len,args_len))

    @jit.unroll_safe
    def collect_frees(self, recursive_sym, env, closure):
        for s in self.frees.elems:
            assert isinstance(s, values.W_Symbol)
        vals = [None] * len(self.frees.elems)
        for j, v in enumerate(self.frees.elems):
            if v is recursive_sym:
                vals[j] = closure
            else:
                vals[j] = env.lookup(v, self.enclosing_env_structure)
        return vals

    @jit.unroll_safe
    def collect_frees_without_recursive(self, recursive_sym, env):
        num_vals = len(self.frees.elems)
        if recursive_sym is not None:
            num_vals -= 1
        vals = [None] * num_vals
        i = 0
        for v in self.frees.elems:
            if v is not recursive_sym:
                vals[i] = env.lookup(v, self.enclosing_env_structure)
                i += 1
        return vals

    def _tostring(self):
        if self.rest and not self.formals:
            return "(lambda %s %s)" % (self.rest.tostring(), [b.tostring() for b in self.body])
        if self.rest:
            fmls = " ".join([v.variable_name() for v in self.formals])
            return "(lambda (%s . %s) %s)" % (fmls, self.rest.tostring(), [b.tostring() for b in self.body])
        else:
            return "(lambda (%s) %s)" % (
                " ".join([v.variable_name() for v in self.formals]),
                self.body[0].tostring() if len(self.body) == 1 else
                " ".join([b.tostring() for b in self.body]))

class CombinedAstAndIndex(AST):
    _immutable_fields_ = ["ast", "index"]

    def __init__(self, ast, index):
        self.ast = ast
        self.index = index
        self.combinations = None

    @specialize.arg(1)
    def unpack(self, cls):
        jit.promote(self)
        ast = self.ast
        assert isinstance(ast, cls)
        return ast, self.index

    @jit.elidable
    def combine(self, other):
        key = (self, other)
        if self.combinations is None:
            self.combinations = {}
        result = self.combinations.get(key, None)
        if result is None:
            result = CombinedAstAndAst(self, other)
            self.combinations[key] = result
        return result

    def _tostring(self):
        return "<%s of %s>" % (self.index, self.ast.tostring())

class CombinedAstAndAst(AST):
    _immutable_fields_ = ["ast1", "ast2"]

    def __init__(self, ast1, ast2):
        self.ast1 = ast1
        self.ast2 = ast2

    def unpack(self):
        jit.promote(self)
        ast1 = self.ast1
        ast2 = self.ast2
        return ast1, ast2

class Letrec(SequencedBodyAST):
    _immutable_fields_ = ["args", "rhss[*]", "counts[*]", "total_counts[*]"]
    def __init__(self, args, counts, rhss, body):
        assert len(counts) > 0 # otherwise just use a begin
        assert isinstance(args, SymList)
        SequencedBodyAST.__init__(self, body, counts_needed=len(rhss))
        self.counts = counts
        total_counts = []
        total_count = 0
        for i, count in enumerate(counts):
            total_counts.append(total_count)
            total_count += count
        self.total_counts = total_counts[:] # copy to make fixed-size
        self.rhss = rhss
        self.args = args

    @jit.unroll_safe
    def interpret(self, env, cont):
        n_elems = len(self.args.elems)
        env_new = ConsEnv.make_n(n_elems, env)
        if n_elems:
            assert isinstance(env_new, ConsEnv)
            for i in range(n_elems):
                env_new._set_list(i, values.W_Cell(None))
        return self.rhss[0], env_new, LetrecCont(self.counting_asts[0], env_new, cont)

    def direct_children(self):
        return self.rhss + self.body

    def _mutated_vars(self):
        x = variable_set()
        for b in self.body + self.rhss:
            x.update(b.mutated_vars())
        for v in self.args.elems:
            lv = LexicalVar(v)
            x[lv] = None
        return x

    def free_vars(self):
        x = AST.free_vars(self)
        for v in self.args.elems:
            if v in x:
                del x[v]
        return x

    def assign_convert(self, vars, env_structure):
        local_muts = variable_set()
        for b in self.body + self.rhss:
            local_muts.update(b.mutated_vars())
        for v in self.args.elems:
            lv = LexicalVar(v)
            local_muts[lv] = None
        new_vars = vars.copy()
        for k, v in local_muts.iteritems():
            new_vars[k] = v
        sub_env_structure = SymList(self.args.elems, env_structure)
        new_rhss = [rhs.assign_convert(new_vars, sub_env_structure) for rhs in self.rhss]
        new_body = [b.assign_convert(new_vars, sub_env_structure) for b in self.body]
        return Letrec(sub_env_structure, self.counts, new_rhss, new_body)

    def _tostring(self):
        vars = []
        len = 0
        for i in self.counts:
            vars.append(self.args.elems[len:len+i])
        return "(letrec (%s) %s)" % (
            [([v.variable_name() for v in vs],
              self.rhss[i].tostring()) for i, vs in enumerate(vars)],
            [b.tostring() for b in self.body])

def _make_symlist_counts(varss):
    counts = []
    argsl = []
    for vars in varss:
        counts.append(len(vars))
        argsl += vars
    argsl = argsl[:] # copy to make fixed-size
    return SymList(argsl), counts

def make_let(varss, rhss, body):
    if not varss:
        return Begin.make(body)
    if 1 == len(varss) and 1 == len(varss[0]):
        return make_let_singlevar(varss[0][0], rhss[0], body)
    symlist, counts = _make_symlist_counts(varss)
    return Let(symlist, counts, rhss, body)

def make_let_singlevar(sym, rhs, body):
    if 1 == len(body):
        b, = body
        if isinstance(b, LexicalVar) and sym is b.sym:
            return rhs
        elif isinstance(b, App):
            rator = b.rator
            x = {}
            for rand in b.rands:
                x.update(rand.free_vars())
            if (isinstance(rator, LexicalVar) and
                    sym is rator.sym and
                    rator.sym not in x):
                return App.make_let_converted(rhs, b.rands)
        elif isinstance(b, If):
            tst = b.tst
            if (isinstance(tst, LexicalVar) and tst.sym is sym and
                    sym not in b.thn.free_vars() and
                    sym not in b.els.free_vars() and
                    rhs.simple):
                return If(rhs, b.thn, b.els)
    return Let(SymList([sym]), [1], [rhs], body)

def make_letrec(varss, rhss, body):
    if not varss:
        return Begin.make(body)
    if 1 == len(varss) and 1 == len(varss[0]):
        rhs = rhss[0]
        sym = varss[0][0]
        if isinstance(rhs, CaseLambda) and LexicalVar(sym) not in rhs.mutated_vars():
            reclambda = rhs.make_recursive_copy(sym)
            return make_let_singlevar(sym, reclambda, body)

    symlist, counts = _make_symlist_counts(varss)
    return Letrec(symlist, counts, rhss, body)

class Let(SequencedBodyAST):
    _immutable_fields_ = ["rhss[*]", "args", "counts[*]", "env_speculation_works?", "remove_num_envs[*]"]

    def __init__(self, args, counts, rhss, body, remove_num_envs=None):
        SequencedBodyAST.__init__(self, body, counts_needed=len(rhss))
        assert len(counts) > 0 # otherwise just use a begin
        assert isinstance(args, SymList)
        self.counts = counts
        self.rhss = rhss
        self.args = args
        self.env_speculation_works = True
        if remove_num_envs is None:
            remove_num_envs = [0] * (len(rhss) + 1)
        self.remove_num_envs = remove_num_envs

    def replace_innermost_with_app(self, newsym, rator, rands):
        assert len(self.body) == 1
        body = self.body[0]
        if isinstance(body, Let) and len(body.body) == 1:
            new_body = body.replace_innermost_with_app(newsym, rator, rands)
        else:
            app_body = [App.make_let_converted(rator, rands)]
            new_body = Let(SymList([newsym]), [1], [body], app_body)
        return Let(self.args, self.counts, self.rhss, [new_body])

    @jit.unroll_safe
    def _prune_env(self, env, i):
        env_structure = self.args.prev
        if i:
            # that many were pruned already:
            already_pruned = self.remove_num_envs[i - 1]
            for j in range(already_pruned):
                env_structure = env_structure.prev
        else:
            already_pruned = 0
        if not objectmodel.we_are_translated():
            if env_structure is None:
                assert isinstance(env, ToplevelEnv)
            else:
                env_structure.check_plausibility(env)
        for i in range(self.remove_num_envs[i] - already_pruned):
            env = env.get_prev(env_structure)
            env_structure = env_structure.prev
        return env

    @objectmodel.always_inline
    def interpret(self, env, cont):
        env = self._prune_env(env, 0)
        return self.rhss[0], env, LetCont.make(
                None, self, 0, env, cont)

    def direct_children(self):
        return self.rhss + self.body
        #return self.body + self.rhss

    def _mutated_vars(self):
        x = variable_set()
        for b in self.body:
            x.update(b.mutated_vars())
        for v in self.args.elems:
            lv = LexicalVar(v)
            if lv in x:
                del x[lv]
        for b in self.rhss:
            x.update(b.mutated_vars())
        return x

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
        sub_env_structure = SymList(self.args.elems, env_structure)
        local_muts = variable_set()
        for b in self.body:
            local_muts.update(b.mutated_vars())
        new_vars = vars.copy()
        for k, v in local_muts.iteritems():
            new_vars[k] = v
        self, sub_env_structure, env_structures, remove_num_envs = self._compute_remove_num_envs(
            new_vars, sub_env_structure)

        new_rhss = []
        for i, rhs in enumerate(self.rhss):
            new_rhs = rhs.assign_convert(vars, env_structures[i])
            need_cell_flags = [(LexicalVar(self.args.elems[i + j]) in local_muts)
                               for j in range(self.counts[i])]
            if True in need_cell_flags:
                new_rhs = Cell(new_rhs, need_cell_flags)
            new_rhss.append(new_rhs)

        body_env_structure = env_structures[len(self.rhss)]

        new_body = [b.assign_convert(new_vars, body_env_structure) for b in self.body]
        result = Let(sub_env_structure, self.counts, new_rhss, new_body, remove_num_envs)
        return result

    def _compute_remove_num_envs(self, new_vars, sub_env_structure):
        if not config.prune_env:
            remove_num_envs = [0] * (len(self.rhss) + 1)
            env_structures = [sub_env_structure.prev] * len(self.rhss)
            env_structures.append(sub_env_structure)
            return self, sub_env_structure, env_structures, remove_num_envs
        # find out whether a smaller environment is sufficient for the body
        free_vars_not_from_let = {}
        for b in self.body:
            free_vars_not_from_let.update(b.free_vars())
        for x in self.args.elems:
            try:
                del free_vars_not_from_let[x]
            except KeyError:
                pass
        # at most, we can remove all envs, apart from the one introduced by let
        curr_remove = max_depth = sub_env_structure.depth_and_size()[0] - 1
        max_needed = 0
        free_vars_not_mutated = True
        for v in free_vars_not_from_let:
            depth = sub_env_structure.depth_of_var(v)[1] - 1
            curr_remove = min(curr_remove, depth)
            max_needed = max(max_needed, depth)
            if LexicalVar(v) in new_vars:
                free_vars_not_mutated = False
        remove_num_envs = [curr_remove]
        if not curr_remove:
            body_env_structure = sub_env_structure
        else:
            next_structure = sub_env_structure.prev
            for i in range(curr_remove):
                next_structure = next_structure.prev
            body_env_structure = SymList(self.args.elems, next_structure)
        if (free_vars_not_mutated and max_needed == curr_remove and
                max_depth > max_needed):
            before_max_needed = sub_env_structure.prev.prev
            for i in range(max_needed):
                before_max_needed = before_max_needed.prev
            body = self.body[0]
            if before_max_needed and before_max_needed.depth_and_size()[1]:
                # there is unneeded local env storage that we will never need
                # in the body. thus, make a copy of all local variables into
                # the current let, *before* the last rhs is evaluated
                # we can reuse the var names
                copied_vars = free_vars_not_from_let.keys()
                new_rhss = self.rhss[:-1] + [LexicalVar(v) for v in copied_vars] + [self.rhss[-1]]
                new_lhs_vars = body_env_structure.elems[:-1] + copied_vars + [body_env_structure.elems[-1]]
                counts = self.counts[:-1] + [1] * len(copied_vars) + [self.counts[-1]]
                body_env_structure = SymList(new_lhs_vars)
                sub_env_structure = SymList(new_lhs_vars, sub_env_structure.prev)
                self = Let(body_env_structure, counts, new_rhss, self.body)
                return self._compute_remove_num_envs(new_vars, sub_env_structure)

        env_structures = [body_env_structure]
        for i in range(len(self.rhss) - 1, -1, -1):
            rhs = self.rhss[i]
            free_vars = rhs.free_vars()
            for v in free_vars:
                curr_remove = min(curr_remove, sub_env_structure.prev.depth_of_var(v)[1])
            next_structure = sub_env_structure.prev
            for i in range(curr_remove):
                next_structure = next_structure.prev
            env_structures.append(next_structure)
            remove_num_envs.append(curr_remove)
        env_structures.reverse()
        remove_num_envs.reverse()
        return self, sub_env_structure, env_structures, remove_num_envs[:]

    def _tostring(self):
        result = ["(let ("]
        j = 0
        for i, count in enumerate(self.counts):
            result.append("[")
            if count > 1:
                result.append("(")
            for _ in range(count):
                result.append(self.args.elems[j].variable_name())
                j += 1
            if count > 1:
                result.append(")")
            result.append(" ")
            result.append(self.rhss[i].tostring())
            result.append("]")
        result.append(") ")
        result.append(" ".join([b.tostring() for b in self.body]))
        result.append(")")
        return "".join(result)

class DefineValues(AST):
    _immutable_fields_ = ["names", "rhs", "display_names"]
    names = []
    rhs = Quote(values.w_null)

    def __init__(self, ns, r, display_names):
        self.names = ns
        self.rhs = r
        self.display_names = display_names

    def defined_vars(self):
        defs = {} # a dictionary, contains symbols
        for n in self.names:
            defs[n] = None
        return defs

    def interpret(self, env, cont):
        return self.rhs.interpret(env, cont)

    def assign_convert(self, vars, env_structure):
        mut = False
        need_cell_flags = [(ModuleVar(i, None, i) in vars) for i in self.names]
        if True in need_cell_flags:
            return DefineValues(self.names,
                                Cell(self.rhs.assign_convert(vars, env_structure),
                                     need_cell_flags),
                                self.display_names)
        else:
            return DefineValues(self.names,
                                self.rhs.assign_convert(vars, env_structure),
                                self.display_names)

    def direct_children(self):
        return [self.rhs]

    def _mutated_vars(self):
        return self.rhs.mutated_vars()

    def free_vars(self):
        # free_vars doesn't contain module-bound variables
        # which is the only thing defined by define-values
        return self.rhs.free_vars()

    def _tostring(self):
        return "(define-values %s %s)" % (
            self.display_names, self.rhs.tostring())


def get_printable_location_two_state(green_ast, came_from):
    if green_ast is None:
        return 'Green_Ast is None'
    surrounding = green_ast.surrounding_lambda
    if surrounding is not None and green_ast is surrounding.body[0]:
        return green_ast.tostring() + ' from ' + came_from.tostring()
    return green_ast.tostring()

driver_two_state = jit.JitDriver(reds=["env", "cont"],
                                 greens=["ast", "came_from"],
                                 get_printable_location=get_printable_location_two_state)

def inner_interpret_two_state(ast, env, cont):
    came_from = ast
    config = env.pycketconfig()
    while True:
        driver_two_state.jit_merge_point(ast=ast, came_from=came_from, env=env, cont=cont)
        if config.track_header:
            came_from = ast if ast.should_enter else came_from
        else:
            came_from = ast if isinstance(ast, App) else came_from
        t = type(ast)
        if t is Let:
            ast, env, cont = ast.interpret(env, cont)
        elif t is If:
            ast, env, cont = ast.interpret(env, cont)
        elif t is Begin:
            ast, env, cont = ast.interpret(env, cont)
        else:
            ast, env, cont = ast.interpret(env, cont)
        if ast.should_enter:
            driver_two_state.can_enter_jit(ast=ast, came_from=came_from, env=env, cont=cont)

def get_printable_location_one_state(green_ast ):
    if green_ast is None:
        return 'Green_Ast is None'
    return green_ast.tostring()
driver_one_state = jit.JitDriver(reds=["env", "cont"],
                       greens=["ast"],
                       get_printable_location=get_printable_location_one_state)
def inner_interpret_one_state(ast, env, cont):
    while True:
        driver_one_state.jit_merge_point(ast=ast, env=env, cont=cont)
        ast, env, cont = ast.interpret(env, cont)
        if ast.should_enter:
            driver_one_state.can_enter_jit(ast=ast, env=env, cont=cont)

def interpret_one(ast, env=None):
    if env is None:
        env = ToplevelEnv()
    if env.pycketconfig().two_state:
        inner_interpret = inner_interpret_two_state
    else:
        inner_interpret = inner_interpret_two_state
    cont = nil_continuation
    cont.update_cm(values.parameterization_key, values.top_level_config)
    try:
        inner_interpret(ast, env, cont)
    except Done, e:
        return e.values
    except SchemeException, e:
        if e.context_ast is None:
            e.context_ast = ast
        raise

def interpret_toplevel(a, env):
    if isinstance(a, Begin):
        x = None
        for a2 in a.body:
            x = interpret_toplevel(a2, env)
        return x
    elif isinstance(a, DefineValues):
        assert 0 # FIXME
        env.toplevel_env().toplevel_set(a.name, interpret_one(a.rhs, env))
        return values.Values.make([values.w_void])
    else:
        return interpret_one(a, env)

def interpret_module(m, env=None):
    env = env if env else ToplevelEnv()
    m.interpret_mod(env)
    return m

def interpret(asts):
    env = ToplevelEnv()
    x = None
    for a in asts:
        x = interpret_toplevel(a, env)
    return x
