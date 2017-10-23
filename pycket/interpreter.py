#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket                   import config
from pycket                   import values, values_string, values_parameter
from pycket                   import vector
from pycket.AST               import AST
from pycket.arity             import Arity
from pycket.cont              import Cont, NilCont, label
from pycket.env               import SymList, ConsEnv, ToplevelEnv
from pycket.error             import SchemeException
from pycket.prims.expose      import prim_env, make_call_method

from pycket.hash.persistent_hash_map import make_persistent_hash_type

from rpython.rlib             import jit, debug, objectmodel
from rpython.rlib.objectmodel import import_from_mixin
from rpython.rlib.objectmodel import r_dict, compute_hash, specialize
from rpython.rlib.rarithmetic import r_uint
from rpython.tool.pairtype    import extendabletype
from small_list               import inline_small_list

import inspect
import sys

# imported for side effects
import pycket.prims.general

BUILTIN_MODULES = [
    "#%kernel",
    "#%unsafe",
    "#%paramz",
    "#%flfxnum",
    "#%utils",
    "#%place",
    "#%foreign",
    "#%builtin",
    "#%extfl",
    "#%futures",
    "#%core",
    "#%linklet",
    "#%network" ]

class BindingFormMixin(object):
    _immutable_fields_ = ['_mutable_var_flags[*]']
    _mutable_var_flags = None

    def init_mutable_var_flags(self, flags):
        if True in flags:
            self._mutable_var_flags = flags
        else:
            self._mutable_var_flags = None

    @jit.unroll_safe
    def binds_mutable_var(self):
        if self._mutable_var_flags is None:
            return False
        for flag in self._mutable_var_flags:
            if flag:
                return True
        return False

    @objectmodel.always_inline
    def is_mutable_var(self, i):
        return self._mutable_var_flags is not None and self._mutable_var_flags[i]

    @objectmodel.always_inline
    def wrap_value(self, val, i):
        if self.is_mutable_var(i):
            val = values.W_Cell(val)
        return val

class Context(object):

    __metaclass__ = extendabletype
    _attrs_ = []

    def plug(self, ast):
        raise NotImplementedError("absract base class")

class __extend__(Context):

    # Below are the set of defunctionalized continuations used in the
    # paper "The Essence of Compiling with Continuations"
    # https://dl.acm.org/citation.cfm?id=989393.989443&coll=DL&dl=GUIDE

    def context(func):
        argspec = inspect.getargspec(func)
        assert argspec.varargs  is None
        assert argspec.keywords is None
        assert argspec.defaults is None
        argnames = argspec.args[:-1]

        class PrimContext(Context):
            _attrs_ = _immutable_fields_ = ["args"]

            def __init__(self, *args):
                Context.__init__(self)
                self.args = args

            def plug_direct(self, ast):
                args = self.args + (ast,)
                return func(*args)

            def plug(self, ast):
                return the_ast, TrampolineContext(ast, self)

            __getitem__ = plug

        class TrampolineAST(AST):

            _attrs_ = _immutable_fields_ = []

            def normalize(self, context):
                assert type(context) is TrampolineContext
                ast, context = context.ast, context.prev
                return context.plug_direct(ast)

        class TrampolineContext(Context):

            _attrs_ = _immutable_fields_ = ["ast", "prev"]

            def __init__(self, ast, prev):
                assert type(prev) is PrimContext
                self.ast  = ast
                self.prev = prev

            def plug_direct(self, ast):
                return self.prev.plug_direct(ast)

            plug = plug_direct

        the_ast = TrampolineAST()

        @objectmodel.always_inline
        def make_context(*args):
            return PrimContext(*args)
        make_context.__name__ = "%sContext" % func.__name__.replace("_", "")

        return make_context

    class Done(Exception):
        def __init__(self, ast):
            self.ast = ast

    class AstList(AST):
        _attrs_ = ["nodes"]
        def __init__(self, nodes):
            self.nodes = nodes

    EmptyList = AstList([])

    @staticmethod
    @objectmodel.always_inline
    def yields(ast):
        raise Context.Done(ast)

    @context
    def Nil(ast):
        Context.yields(ast)

    Nil = Nil()

    @staticmethod
    @specialize.arg(2)
    def normalize_term(expr, context=Nil, expect=AST):
        """
        This will perform A-normalization on the given expression. The given
        context is a value of type Context which contains the surrounding binding
        context of the current expression. The context is needed to properly
        re-build the expression inside out, as the given expression is traversed
        from outside inward.
        The transformation is trampolined in order to overcome Python's
        improverished call stack.
        """
        try:
            while True:
                expr, context = expr.normalize(context)
        except Context.Done as e:
            expr = e.ast
        assert isinstance(expr, expect)
        return expr

    @staticmethod
    def normalize_name(expr, context, hint="g"):
        context = Context.Name(context, hint)
        return expr, context

    @staticmethod
    def normalize_names(exprs, context, i=0):
        if i >= len(exprs):
            return context.plug(Context.EmptyList)
        expr = exprs[i]
        context = Context.Names(exprs, i, context)
        return Context.normalize_name(expr, context, hint="AppRand")

    @staticmethod
    @context
    def Name(context, hint, ast):
        if ast.simple:
            return context.plug(ast)
        sym  = Gensym.gensym(hint=hint)
        var  = LexicalVar(sym)
        body = Context.normalize_term(var, context)
        Context.yields(make_let_singlevar(sym, ast, [body]))

    @staticmethod
    @context
    def Names(exprs, i, context, ast):
        context = Context.Append(ast, context)
        return Context.normalize_names(exprs, context, i+1)

    @staticmethod
    def Let(xs, Ms, body, context):
        return Context._Let(xs, Ms, body, 0, context)

    @staticmethod
    @context
    def _Let(xs, Ms, body, i, context, ast):
        assert len(xs) == len(Ms)
        if i == len(Ms) - 1:
            body = Context.normalize_term(body, context)
            # Body may have been wrapped in a begin for convenience
            body = body.body if isinstance(body, Begin) else [body]
            Context.yields(make_let([xs[i]], [ast], body))
        X = xs[i]
        i += 1
        x_, M = xs[i], Ms[i]
        context  = Context._Let(xs, Ms, body, i, context)
        body  = Context.normalize_term(M, context)
        Context.yields(make_let([X], [ast], [body]))

    @staticmethod
    @context
    def If(thn, els, context, tst):
        thn = Context.normalize_term(thn)
        els = Context.normalize_term(els)
        result = If.make(tst, thn, els)
        return context.plug(result)

    @staticmethod
    @context
    def AppRator(args, context, ast):
        context = Context.AppRand(ast, context)
        return Context.normalize_names(args, context)

    @staticmethod
    @context
    def AppRand(rator, context, ast):
        assert isinstance(ast, Context.AstList)
        rands  = ast.nodes
        result = App.make(rator, rands)
        return context.plug(result)

    @staticmethod
    @context
    def Append(expr, context, ast):
        assert isinstance(ast, Context.AstList)
        ast = Context.AstList([expr] + ast.nodes)
        return context.plug(ast)

    @staticmethod
    @context
    def SetBang(var, context, ast):
        ast = SetBang(var, ast)
        return context.plug(ast)

@objectmodel.always_inline
def equal(a, b):
    assert a is None or isinstance(a, values.W_Symbol)
    assert b is None or isinstance(b, values.W_Symbol)
    return a is b

@objectmodel.always_inline
def hashfun(v):
    assert v is None or isinstance(v, values.W_Symbol)
    return r_uint(compute_hash(v))

SymbolSet = make_persistent_hash_type(
    super=values.W_ProtoObject,
    base=object,
    keytype=values.W_Symbol,
    valtype=values.W_Symbol,
    name="SymbolSet",
    hashfun=hashfun,
    equal=equal)

def is_builtin_module(mod):
    return (mod in BUILTIN_MODULES) or (0 <= mod.find("pycket-lang/extra-prims"))

class Done(Exception):
    _attrs_ = ["values"]
    _immutable_ = True
    def __init__(self, vals):
        self.values = vals

def var_eq(a, b):
    if isinstance(a, LexicalVar) and isinstance(b, LexicalVar):
        return a.sym is b.sym
    elif isinstance(a, ModuleVar) and isinstance(b, ModuleVar):
        # two renamed variables can be the same
        return a.srcsym is b.srcsym
    return False

def var_hash(a):
    if isinstance(a, LexicalVar):
        return compute_hash(a.sym)
    elif isinstance(a, ModuleVar):
        return compute_hash(a.srcsym)
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
    _immutable_fields_ = ["counting_ast"]
    def __init__(self, counting_ast, env, prev):
        Cont.__init__(self, env, prev)
        self.counting_ast = counting_ast

    def _clone(self):
        return LetrecCont(self.counting_ast, self.env, self.prev)

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
    _immutable_fields_ = ["counting_ast"]

    return_safe = True

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
    def make(vals_w, ast, rhsindex, env, prev):
        counting_ast = ast.counting_asts[rhsindex]
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
                elif not jit.we_are_jitted():
                    ast.env_speculation_works = False
            env = self._construct_env(ast, len_self, vals, len_vals, new_length, prev)
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
    def _construct_env(self, ast, len_self, vals, len_vals, new_length, prev):
        assert isinstance(ast, Let)
        # this is a complete mess. however, it really helps warmup a lot
        l_inst = self.env.get_current_linklet_instance()
        if new_length == 0:
            return ConsEnv.make0(prev)
        if new_length == 1:
            if len_self == 1:
                elem = self._get_list(0)
            else:
                assert len_self == 0 and len_vals == 1
                elem = vals.get_value(0)
            elem = ast.wrap_value(elem, 0)
            return ConsEnv.make1(elem, prev, l_inst)
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
            elem1 = ast.wrap_value(elem1, 0)
            elem2 = ast.wrap_value(elem2, 1)
            return ConsEnv.make2(elem1, elem2, prev, l_inst)
        env = ConsEnv.make_n(new_length, prev, l_inst)
        i = 0
        for j in range(len_self):
            val = self._get_list(j)
            val = ast.wrap_value(val, i)
            env._set_list(i, val)
            i += 1
        for j in range(len_vals):
            val = vals.get_value(j)
            val = ast.wrap_value(val, i)
            env._set_list(i, val)
            i += 1
        return env

class CellCont(Cont):
    _immutable_fields_ = ['ast']

    def __init__(self, ast, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast

    def _clone(self):
        return CellCont(self.ast, self.env, self.prev)

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

class BeginCont(Cont):
    _immutable_fields_ = ["counting_ast"]
    return_safe = True
    def __init__(self, counting_ast, env, prev):
        Cont.__init__(self, env, prev)
        self.counting_ast = counting_ast

    def _clone(self):
        return BeginCont(self.counting_ast, self.env, self.prev)

    def get_ast(self):
        return self.counting_ast.ast

    def get_next_executed_ast(self):
        ast, i = self.counting_ast.unpack(SequencedBodyAST)
        return ast.body[i]

    def plug_reduce(self, vals, env):
        ast, i = self.counting_ast.unpack(SequencedBodyAST)
        return ast.make_begin_cont(self.env, self.prev, i)

@inline_small_list(immutable=True, attrname="vals_w",
                   unbox_num=True, factoryname="_make")
class Begin0BodyCont(Cont):
    _immutable_fields_ = ["counting_ast"]
    return_safe = True

    def __init__(self, ast, env, prev):
        Cont.__init__(self, env, prev)
        self.counting_ast = ast

    @staticmethod
    def make(vals, ast, index, env, prev):
        counting_ast = ast.counting_asts[index]
        return Begin0BodyCont._make(vals, counting_ast, env, prev)

    def get_ast(self):
        return self.counting_ast.ast

    def get_next_executed_ast(self):
        ast, i = self.counting_ast.unpack(Begin0)
        return ast.body[i]

    def plug_reduce(self, vals, env):
        ast, index = self.counting_ast.unpack(Begin0)
        vals = self._get_full_list()
        if index == len(ast.body) - 1:
            return return_multi_vals(values.Values.make(vals), env, self.prev)
        return (ast.body[index + 1], self.env,
                Begin0BodyCont.make(vals, ast, index + 1, self.env, self.prev))

# FIXME: it would be nice to not need two continuation types here
class Begin0Cont(Cont):
    _immutable_fields_ = ["ast"]
    return_safe = True
    def __init__(self, ast, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast

    def _clone(self):
        return Begin0Cont(self.ast, self.env, self.prev)

    def get_ast(self):
        return self.ast

    def get_next_executed_ast(self):
        return self.ast

    def plug_reduce(self, vals, env):
        ast = jit.promote(self.ast)
        vals_w = vals.get_all_values()
        return ast.body[0], self.env, Begin0BodyCont.make(vals_w, ast, 0, self.env, self.prev)

class WCMKeyCont(Cont):
    _immutable_fields_ = ["ast"]
    return_safe = True
    def __init__(self, ast, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast

    def _clone(self):
        return WCMKeyCont(self.ast, self.env, self.prev)

    def get_ast(self):
        return self.ast

    def get_next_executed_ast(self):
        return self.ast.value

    def plug_reduce(self, vals, env):
        key = check_one_val(vals)
        return self.ast.value, self.env, WCMValCont(self.ast, key, self.env, self.prev)

class WCMValCont(Cont):
    _immutable_fields_ = ["ast", "key"]
    return_safe = True
    def __init__(self, ast, key, env, prev):
        Cont.__init__(self, env, prev)
        self.ast = ast
        self.key = key

    def _clone(self):
        return WCMValCont(self.ast, self.key, self.env, self.prev)

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

        # Perform a shallow copying of the continuation to ensure any marks
        # captured by call/cc and family are not affected by the mutation of
        # the mark set.
        cont = self.prev.clone()
        cont.update_cm(key, val)

        return self.ast.body, self.env, cont

class ModuleInfo(object):
    def __init__(self, current_module):
        self.current_module = current_module
        self.submodules = []
        self.requires   = []

class Module(AST):
    _immutable_fields_ = ["name", "body[*]", "requires[*]", "parent", "submodules[*]", "interpreted?", "lang"]
    visitable = True
    simple = True

    def __init__(self, name, body, config, lang=None):
        self.parent = None
        self.lang = lang
        self.name = name

        info = ModuleInfo(self)
        todo = body[:]
        while todo:
            curr = todo.pop()
            rest = curr.collect_module_info(info)
            todo.extend(rest)
        self.submodules = info.submodules[:]
        self.requires   = info.requires[:]
        self.body       = [b for b in body if not isinstance(b, Require)]

        self.env = None
        self.interpreted = False
        self.config = config

        defs = {}
        for b in self.body:
            b.defined_vars(defs)
        self.defs = defs

    def rebuild_body(self):
        return self.requires + self.body

    def set_parent_module(self, parent):
        assert isinstance(parent, Module)
        self.parent = parent

    def collect_module_info(self, info):
        info.submodules.append(self)
        self.set_parent_module(info.current_module)
        return []

    def full_module_path(self):
        if self.parent is None:
            return self.name
        path = []
        while self is not None:
            path.append(self.name)
            self = self.parent
        return "/".join([i for i in reversed(path)])

    @jit.elidable
    def lookup(self, sym):
        if sym not in self.defs:
            path = self.full_module_path()
            raise SchemeException("unknown module variable %s in module %s" % (sym.tostring(), path))
        v = self.defs[sym]
        if v is None:
            raise SchemeException("use of module variable before definition %s" % (sym.tostring()))
        return v

    # all the module-bound variables that are mutated
    def mod_mutated_vars(self):
        x = variable_set()
        for r in self.body:
            x.update(r.mutated_vars())
        return x

    def direct_children(self):
        return self.rebuild_body()

    def _tostring(self):
        return "(module %s %s)"%(self.name," ".join([s.tostring() for s in self.body]))

    def interpret_simple(self, env):
        """ Interpretation of a module is a no-op from the outer module.
            Modules must be executed explicitly by |interpret_mod|, usually via
            a require statement.  """
        return values.w_void

    def interpret_mod(self, env):
        if self.interpreted:
            return values.w_void
        try:
            self.interpreted = True
            return self._interpret_mod(env)
        except SchemeException, e:
            if e.context_module is None:
                e.context_module = self
            raise

    @jit.unroll_safe
    def root_module(self):
        while self.parent is not None:
            self = self.parent
        return self

    @jit.unroll_safe
    def find_submodule(self, name):
        if name == ".":
            return self
        if name == "..":
            return self.parent
        for s in self.submodules:
            assert isinstance(s, Module)
            if s.name == name:
                return s
        return None

    @jit.unroll_safe
    def resolve_submodule_path(self, path):
        if path is None:
            return self
        for p in path:
            self = self.find_submodule(p)
            assert self is not None
        return self

    def normalize(self, context):
        # Return the current module, as it is not safe to duplicate module forms
        for i, b in enumerate(self.body):
            self.body[i] = Context.normalize_term(b)
        return context.plug(self)

    def _interpret_mod(self, env):
        self.env = env
        module_env = env.toplevel_env().module_env
        old = module_env.current_module
        module_env.current_module = self
        if self.lang is not None:
            interpret_one(self.lang, self.env)
        elif self.parent is not None:
            self.parent.interpret_mod(self.env)

        for r in self.requires:
            interpret_one(r, self.env)
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
    _immutable_fields_ = ["fname", "loader", "path[*]"]
    visitable = True
    simple = True

    def __init__(self, fname, loader, path=None):
        self.fname = fname
        self.path = path
        self.loader = loader

    def find_module(self, env):
        assert not jit.we_are_jitted()
        if self.loader is not None:
            module = self.loader.lazy_load(self.fname)
        else:
            module = env.toplevel_env().module_env.current_module
        assert module is not None
        module = module.resolve_submodule_path(self.path)
        return module

    # Interpret the module and add it to the module environment
    def interpret_simple(self, env):
        module = self.find_module(env)
        top = env.toplevel_env()
        top.module_env.add_module(self.fname, module.root_module())
        module.interpret_mod(top)
        return values.w_void

    def collect_module_info(self, info):
        for r in info.requires:
            assert isinstance(r, Require)
            if (self.fname == r.fname and
                self.path == r.path and
                self.loader is r.loader):
                break
        else:
            info.requires.append(self)
        return []

    def _tostring(self):
        return "(require %s)" % self.fname

def return_value(w_val, env, cont):
    return return_multi_vals(values.Values.make1(w_val), env, cont)

def return_value_direct(w_val, env, cont):
    """ like return_value, but without using a label. only safe to use in
    AST.interpret and (automatically) by simple primitives """
    val = values.Values.make1(w_val)
    return cont.plug_reduce(val, env)

def return_multi_vals(vals, env, cont):
    if cont.return_safe:
        return cont.plug_reduce(vals, env)
    return safe_return_multi_vals(vals, env, cont)

# A safe variant which returns ensures control is handed back to
# the CEK loop before applying the continuation.
@label
def safe_return_multi_vals(vals, env, cont):
    return cont.plug_reduce(vals, env)

def return_multi_vals_direct(vals, env, cont):
    return cont.plug_reduce(vals, env)

def return_void(env, cont):
    return return_value(values.w_void, env, cont)

class Cell(AST):
    _immutable_fields_ = ["expr", "need_cell_flags[*]"]
    visitable = True
    def __init__(self, expr, need_cell_flags=None):
        if need_cell_flags is None:
            need_cell_flags = [True]
        self.expr = expr
        self.need_cell_flags = need_cell_flags

    def interpret(self, env, cont):
        return self.expr, env, CellCont(self, env, cont)

    def direct_children(self):
        return [self.expr]

    def _tostring(self):
        return "Cell(%s)"%self.expr.tostring()

class Quote(AST):
    _immutable_fields_ = ["w_val"]

    visitable = True
    simple = True
    ispure = True

    def __init__ (self, w_val):
        self.w_val = w_val

    def interpret_simple(self, env):
        return self.w_val

    def direct_children(self):
        return []

    def _tostring(self):
        if (isinstance(self.w_val, values.W_Bool) or
            isinstance(self.w_val, values.W_Number) or
            isinstance(self.w_val, values_string.W_String) or
            isinstance(self.w_val, values.W_Symbol)):
            return "%s" % self.w_val.tostring()
        return "'%s" % self.w_val.tostring()

class QuoteSyntax(AST):
    _immutable_fields_ = ["w_val"]
    visitable = True
    simple = True
    ispure = True

    def __init__ (self, w_val):
        self.w_val = w_val

    def interpret_simple(self, env):
        return values.W_Syntax(self.w_val)

    def direct_children(self):
        return []

    def _tostring(self):
        return "#'%s" % self.w_val.tostring()

class VariableReference(AST):
    _immutable_fields_ = ["var", "is_mut", "path"]
    visitable = True
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

    def direct_children(self):
        return []

    def _tostring(self):
        return "#<#%variable-reference>"

class WithContinuationMark(AST):
    _immutable_fields_ = ["key", "value", "body"]
    visitable = True

    def __init__(self, key, value, body):
        self.key = key
        self.value = value
        self.body = body

    def _tostring(self):
        return "(with-continuation-mark %s %s %s)"%(self.key.tostring(),
                                                    self.value.tostring(),
                                                    self.body.tostring())

    def direct_children(self):
        return [self.key, self.value, self.body]

    def interpret(self, env, cont):
        return self.key, env, WCMKeyCont(self, env, cont)

    def normalize(self, context):
        key    = Context.normalize_term(self.key)
        value  = Context.normalize_term(self.value)
        body   = Context.normalize_term(self.body)
        result = WithContinuationMark(key, value, body)
        return context.plug(result)

class App(AST):
    _immutable_fields_ = ["rator", "rands[*]", "env_structure"]
    visitable = True

    def __init__ (self, rator, rands, env_structure=None):
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

    def direct_children(self):
        return [self.rator] + self.rands

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

    def normalize(self, context):
        context = Context.AppRator(self.rands, context)
        return Context.normalize_name(self.rator, context, hint="AppRator")

    def _tostring(self):
        elements = [self.rator] + self.rands
        return "(%s)" % " ".join([r.tostring() for r in elements])

class SimplePrimApp1(App):
    _immutable_fields_ = ['w_prim', 'rand1']
    simple = True
    visitable = False

    def __init__(self, rator, rands, env_structure, w_prim):
        App.__init__(self, rator, rands, env_structure)
        assert len(rands) == 1
        self.rand1, = rands
        self.w_prim = w_prim

    def normalize(self, context):
        context = Context.AppRand(self.rator, context)
        return Context.normalize_names(self.rands, context)

    def run(self, env):
        result = self.w_prim.simple1(self.rand1.interpret_simple(env))
        if result is None:
            result = values.w_void
        return result

    def interpret_simple(self, env):
        return check_one_val(self.run(env))

    def interpret(self, env, cont):
        from pycket.prims.control import convert_runtime_exception
        if not env.pycketconfig().callgraph:
            self.set_should_enter() # to jit downrecursion
        try:
            result = self.run(env)
        except SchemeException, exn:
            return convert_runtime_exception(exn, env, cont)
        return return_multi_vals_direct(result, env, cont)

class SimplePrimApp2(App):
    _immutable_fields_ = ['w_prim', 'rand1', 'rand2']
    simple = True
    visitable = False

    def __init__(self, rator, rands, env_structure, w_prim):
        App.__init__(self, rator, rands, env_structure)
        assert len(rands) == 2
        self.rand1, self.rand2 = rands
        self.w_prim = w_prim

    def normalize(self, context):
        context = Context.AppRand(self.rator, context)
        return Context.normalize_names(self.rands, context)

    def run(self, env):
        from pycket.prims.control import convert_runtime_exception
        arg1 = self.rand1.interpret_simple(env)
        arg2 = self.rand2.interpret_simple(env)
        result = self.w_prim.simple2(arg1, arg2)
        if result is None:
            result = values.w_void
        return result

    def interpret_simple(self, env):
        return check_one_val(self.run(env))

    def interpret(self, env, cont):
        from pycket.prims.control import convert_runtime_exception
        if not env.pycketconfig().callgraph:
            self.set_should_enter() # to jit downrecursion
        try:
            result = self.run(env)
        except SchemeException, exn:
            return convert_runtime_exception(exn, env, cont)
        return return_multi_vals_direct(result, env, cont)

class SequencedBodyAST(AST):
    _immutable_fields_ = ["body[*]", "counting_asts[*]",
                          "_sequenced_env_structure",
                          "_sequenced_remove_num_envs[*]"]

    visitable = False
    _sequenced_env_structure = None
    _sequenced_remove_num_envs = None

    def __init__(self, body, counts_needed=-1, sequenced_env_structure=None, sequenced_remove_num_envs=None):
        from rpython.rlib.debug import make_sure_not_resized
        assert isinstance(body, list)
        assert len(body) > 0
        self.body = body
        make_sure_not_resized(self.body)
        if counts_needed < len(self.body) + 1:
            counts_needed = len(self.body) + 1
        self.counting_asts = [
            CombinedAstAndIndex(self, i)
                for i in range(counts_needed)]

    def init_body_pruning(self, env_structure, remove_num_envs):
        self._sequenced_env_structure = env_structure
        self._sequenced_remove_num_envs = remove_num_envs

    def copy_body_pruning(self, other):
        assert isinstance(other, SequencedBodyAST)
        self._sequenced_env_structure   = other._sequenced_env_structure
        self._sequenced_remove_num_envs = other._sequenced_remove_num_envs

    @staticmethod
    def _check_environment_consistency(env, env_structure):
        if objectmodel.we_are_translated():
            return
        if env_structure is None:
            assert isinstance(env, ToplevelEnv)
        else:
            env_structure.check_plausibility(env)

    @jit.unroll_safe
    def _prune_sequenced_envs(self, env, i=0):
        env_structure = self._sequenced_env_structure
        if env_structure is None:
            return env
        if i:
            already_pruned = self._sequenced_remove_num_envs[i - 1]
            for j in range(already_pruned):
                env_structure = env_structure.prev
        else:
            already_pruned = 0
        self._check_environment_consistency(env, env_structure)
        for i in range(self._sequenced_remove_num_envs[i] - already_pruned):
            env = env.get_prev(env_structure)
            env_structure = env_structure.prev
        return env

    @objectmodel.always_inline
    def make_begin_cont(self, env, prev, i=0):
        jit.promote(self)
        jit.promote(i)
        if not i:
            env = self._prune_sequenced_envs(env, 0)
        if i == len(self.body) - 1:
            return self.body[i], env, prev
        else:
            new_env = self._prune_sequenced_envs(env, i + 1)
            return self.body[i], env, BeginCont(
                    self.counting_asts[i + 1], new_env, prev)

class Begin0(SequencedBodyAST):
    _immutable_fields_ = ["first"]
    visitable = True

    @staticmethod
    def make(first, rest):
        rest = remove_pure_ops(rest, always_last=False)
        if rest:
            return Begin0(first, rest)
        return first

    def __init__(self, fst, rst):
        assert isinstance(rst, list)
        SequencedBodyAST.__init__(self, rst)
        self.first = fst

    def direct_children(self):
        return [self.first] + self.body

    def _tostring(self):
        body = [self.first.tostring()] + [b.tostring() for b in self.body]
        return "(begin0 %s)" % " ".join(body)

    def normalize(self, context):
        first = Context.normalize_term(self.first)
        body  = [Context.normalize_term(b) for b in self.body]
        result = Begin0(first, body)
        return context.plug(result)

    def interpret(self, env, cont):
        return self.first, env, Begin0Cont(self, env, cont)

@specialize.call_location()
def remove_pure_ops(ops, always_last=True):
    """ The specialize annotation is to allow handling of resizable and non-resizable
        lists as arguments. """
    if always_last:
        last = len(ops) - 1
        return [op for i, op in enumerate(ops) if not op.ispure or i == last]
    else:
        return [op for i, op in enumerate(ops) if not op.ispure]

class Begin(SequencedBodyAST):
    visitable = True

    @staticmethod
    def make(body):
        body = remove_pure_ops(body)

        if len(body) == 1:
            return body[0]

        # Flatten nested begin expressions
        flattened = []
        for b in body:
            if isinstance(b, Begin):
                for inner in b.body:
                    flattened.append(inner)
            else:
                flattened.append(b)
        body = remove_pure_ops(flattened)

        # Convert (begin (let ([...]) letbody) rest ...) =>
        #         (let ([...]) letbody ... rest ...)
        b0 = body[0]
        if isinstance(b0, Let):
            rest    = body[1:]
            letbody = b0.body
            letargs = b0._rebuild_args()
            letrhss = b0.rhss
            return make_let(letargs, letrhss, letbody + rest)

        return Begin(body)

    def direct_children(self):
        return self.body

    @objectmodel.always_inline
    def interpret(self, env, cont):
        return self.make_begin_cont(env, cont)

    def normalize(self, context):
        body = [Context.normalize_term(b) for b in self.body]
        result = Begin.make(body)
        return context.plug(result)

    def _tostring(self):
        return "(begin %s)" % (" ".join([e.tostring() for e in self.body]))

class BeginForSyntax(AST):
    _immutable_fields_ = ["body[*]"]
    visitable = True
    simple = True

    def __init__(self, body):
        self.body = body

    def direct_children(self):
        return self.body[:]

    def interpret_simple(self, env):
        return values.w_void

    def _tostring(self):
        return "(begin-for-syntax %s)" % " ".join([b.tostring() for b in self.body])

class Var(AST):
    _immutable_fields_ = ["sym", "env_structure"]
    simple = True
    ispure = True

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

    def _free_vars(self):
        return SymbolSet.singleton(self.sym)

    def _tostring(self):
        return "%s" % self.sym.variable_name()

class CellRef(Var):
    simple = True
    visitable = True

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

class GensymCounter(object):
    _attrs_ = ['_val']

    def __init__(self, val=0):
        self._val = val

    def next_value(self):
        val = self._val
        self._val += 1
        return val

class Gensym(object):
    _counters = {}

    @staticmethod
    @jit.elidable
    def get_counter(hint):
        result = Gensym._counters.get(hint, None)
        if result is not None:
            return result
        result = GensymCounter()
        Gensym._counters[hint] = result
        return result

    @staticmethod
    def gensym(hint="g"):
        counter = Gensym.get_counter(hint)
        count = counter.next_value()
        return values.W_Symbol(hint + str(count))

class LinkletVar(Var):
    visitable = True

    def __init__(self, sym, srcinstance_num):
        self.sym = sym
        self.srcinstance_number = srcinstance_num

    def _free_vars(self):
        return SymbolSet.EMPTY

    def _set(self, w_val, env):
        instance = env.get_current_linklet_instance()

        instance.set_bang_def(self.sym, w_val)

    def _lookup(self, env):
        instance = env.get_current_linklet_instance()
        return instance.lookup_linkl(self.sym, self.srcinstance_number)
    
class LexicalVar(Var):
    visitable = True
    def _lookup(self, env):
        return env.lookup(self.sym, self.env_structure)

    def _set(self, w_val, env):
        assert 0

class ModuleVar(Var):
    _immutable_fields_ = ["modenv?", "sym", "srcmod", "srcsym", "w_value?", "path[*]"]
    visitable = True

    def __init__(self, sym, srcmod, srcsym, path=None):
        Var.__init__(self, sym)
        self.srcmod = srcmod
        self.srcsym = srcsym
        self.path = path
        self.modenv = None
        self.w_value = None

    def _free_vars(self):
        return SymbolSet.EMPTY

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
        return self.srcmod is not None and is_builtin_module(self.srcmod)

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
        return mod.resolve_submodule_path(self.path).lookup(self.srcsym)

    def _lookup_primitive(self):
        # we don't separate these the way racket does
        # but maybe we should
        try:
            return prim_env[self.srcsym]
        except KeyError:
            raise SchemeException("can't find primitive %s" % (self.srcsym.tostring()))

    def _set(self, w_val, env):
        if self.modenv is None:
            self.modenv = env.toplevel_env().module_env
        v = self._elidable_lookup()
        assert isinstance(v, values.W_Cell)
        v.set_val(w_val)

class ToplevelVar(Var):
    visitable = True

    def _lookup(self, env):
        return env.toplevel_env().toplevel_lookup(self.sym)

    def _set(self, w_val, env):
        env.toplevel_env().toplevel_set(self.sym, w_val)

class SetBang(AST):
    _immutable_fields_ = ["var", "rhs"]
    visitable = True
    simple = True

    def __init__(self, var, rhs):
        self.var = var
        self.rhs = rhs

    def interpret_simple(self, env):
        w_val = self.rhs.interpret_simple(env)
        self.var._set(w_val, env)
        return values.w_void

    def _mutated_vars(self):
        x = self.rhs.mutated_vars()
        var = self.var
        if isinstance(var, CellRef):
            x[LexicalVar(self.var.sym)] = None
        # even though we don't change these to cell refs, we still
        # have to convert the definitions
        elif isinstance(var, ModuleVar):
            x[var] = None
        # do nothing for top-level vars, they're all mutated
        return x

    def direct_children(self):
        return [self.var, self.rhs]

    def normalize(self, context):
        context = Context.SetBang(self.var, context)
        return Context.normalize_name(self.rhs, context, hint="SetBang")

    def _tostring(self):
        return "(set! %s %s)" % (self.var.sym.variable_name(), self.rhs.tostring())

class If(AST):
    _immutable_fields_ = ["tst", "thn", "els"]
    visitable = True

    def __init__(self, tst, thn, els):
        self.tst = tst
        self.thn = thn
        self.els = els

    @staticmethod
    def make(tst, thn, els):
        if isinstance(tst, Quote):
            if tst.w_val is values.w_false:
                return els
            else:
                return thn
        return If(tst, thn, els)

    @objectmodel.always_inline
    def interpret(self, env, cont):
        w_val = self.tst.interpret_simple(env)
        if w_val is values.w_false:
            return self.els, env, cont
        else:
            return self.thn, env, cont

    def direct_children(self):
        return [self.tst, self.thn, self.els]

    def normalize(self, context):
        context = Context.If(self.thn, self.els, context)
        return Context.normalize_name(self.tst, context, hint="if")

    def _tostring(self):
        return "(if %s %s %s)" % (self.tst.tostring(), self.thn.tostring(), self.els.tostring())

def make_lambda(formals, rest, body, sourceinfo=None):
    """
    Create a λ-node after computing information about the free variables
    in the body. The 'args' field stores both the function arguments as well
    as the free variables in a SymList.
    The 'args' SymList hold the expected environment structure for the body of
    the λ-expression.
    """
    body = remove_pure_ops(body)
    args = SymList(formals + ([rest] if rest else []))
    frees = SymList(free_vars_lambda(body, args).keys())
    args = SymList(args.elems, frees)
    return Lambda(formals, rest, args, frees, body, sourceinfo=sourceinfo)

def free_vars_lambda(body, args):
    x = SymbolSet.EMPTY
    for b in body:
        x = x.union(b.free_vars())
    x = x.without_many(args.elems)
    return x

class CaseLambda(AST):
    _immutable_fields_ = ["lams[*]", "any_frees", "recursive_sym", "w_closure_if_no_frees?", "_arity"]
    visitable = True
    simple = True
    ispure = True

    def __init__(self, lams, recursive_sym=None, arity=None):
        ## TODO: drop lams whose arity is redundant
        ## (case-lambda [x 0] [(y) 1]) == (lambda x 0)
        self.lams = lams
        self.any_frees = False
        for l in lams:
            frees = l.frees.elems
            if frees and frees != [recursive_sym]:
                self.any_frees = True
                break
        self._closurerepr = None
        self.w_closure_if_no_frees = None
        self.recursive_sym = recursive_sym
        self._arity = arity
        self.compute_arity()

    @jit.unroll_safe
    def enable_jitting(self):
        for l in self.lams:
            l.enable_jitting()

    def make_recursive_copy(self, sym):
        return CaseLambda(self.lams, sym, self._arity)

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

    def _free_vars(self):
        # call _free_vars() to avoid populating the free vars cache
        result = AST._free_vars(self)
        if self.recursive_sym is not None:
            result = result.without(self.recursive_sym)
        return result

    def direct_children(self):
        # the copy is needed for weird annotator reasons that I don't understand :-(
        return [l for l in self.lams]

    def _tostring(self):
        if len(self.lams) == 1:
            return self.lams[0].tostring()
        return "(case-lambda %s)" % (" ".join([l.tostring() for l in self.lams]))

    @jit.elidable_promote('all')
    def tostring_as_closure(self):
        _closurerepr = self._closurerepr
        if _closurerepr is None:
            _closurerepr = self._closurerepr = self._tostring_as_closure()
        return _closurerepr

    def _tostring_as_closure(self):
        if len(self.lams) == 0:
            return "#<procedure>"
        lam = self.lams[0]
        assert isinstance(lam, Lambda)
        info = lam.sourceinfo
        file, pos = info.sourcefile, info.position
        if file and pos >= 0:
            return "#<procedure:%s:%s>" % (file, pos)
        if file is not None:
            return "#<procedure:%s>" % file
        return "#<procedure>"

    def get_arity(self):
        return self._arity

    def compute_arity(self):
        if self._arity is not None:
            return
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

    def normalize(self, context):
        lams   = [Context.normalize_term(lam, expect=Lambda) for lam in self.lams]
        result = CaseLambda(lams, recursive_sym=self.recursive_sym, arity=self._arity)
        return context.plug(result)

class Lambda(SequencedBodyAST):
    _immutable_fields_ = ["formals[*]", "rest", "args",
                          "frees", "enclosing_env_structure", "env_structure",
                          "sourceinfo"]
    visitable = True
    simple = True
    ispure = True

    import_from_mixin(BindingFormMixin)

    def __init__ (self, formals, rest, args, frees, body, sourceinfo=None, enclosing_env_structure=None, env_structure=None):
        SequencedBodyAST.__init__(self, body)
        self.sourceinfo = sourceinfo
        self.formals = formals
        self.rest = rest
        self.args = args
        self.frees = frees
        self.enclosing_env_structure = enclosing_env_structure
        self.env_structure = env_structure
        for b in self.body:
            b.set_surrounding_lambda(self)

    def init_arg_cell_flags(self, args_need_cell_flags):
        if True in args_need_cell_flags:
            self.args_need_cell_flags = args_need_cell_flags

    def enable_jitting(self):
        self.body[0].set_should_enter()

    def can_enter(self):
        return self.body[0].should_enter

    # returns n for fixed arity, -(n+1) for arity-at-least n
    # my kingdom for Either
    def get_arity(self):
        if self.rest:
            return -(len(self.formals)+1)
        else:
            return len(self.formals)

    def interpret_simple(self, env):
        assert False # unreachable

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

    def _free_vars(self):
        return free_vars_lambda(self.body, self.args)

    @jit.unroll_safe
    def _has_mutable_args(self):
        if self.args_need_cell_flags is None:
            return False
        for flag in self.args_need_cell_flags:
            if flag:
                return True
        return False

    def _is_mutable_arg(self, i):
        return self.args_need_cell_flags is not None and self.args_need_cell_flags[i]

    @jit.unroll_safe
    def match_args(self, args):
        fmls_len = len(self.formals)
        args_len = len(args)
        if fmls_len != args_len and self.rest is None:
            return None
        if fmls_len > args_len:
            return None
        if self.rest is None:
            if not self.binds_mutable_var():
                return args
            numargs = fmls_len
        else:
            numargs = fmls_len + 1
        actuals = [None] * numargs
        for i in range(fmls_len):
            actuals[i] = self.wrap_value(args[i], i)
        if self.rest is None:
            return actuals
        rest = values.to_list(args, start=fmls_len)
        actuals[-1] = self.wrap_value(rest, -1)
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
        if recursive_sym is not None and self.frees.contains_sym(recursive_sym):
            num_vals -= 1
        vals = [None] * num_vals
        i = 0
        for v in self.frees.elems:
            if v is not recursive_sym:
                vals[i] = env.lookup(v, self.enclosing_env_structure)
                i += 1
        return vals

    def normalize(self, context):
        body = [Context.normalize_term(b) for b in self.body]
        result = Lambda(self.formals, self.rest, self.args, self.frees, body,
                        sourceinfo=self.sourceinfo,
                        enclosing_env_structure=self.enclosing_env_structure,
                        env_structure=self.env_structure)
        return context.plug(result)

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
    visitable = True
    def __init__(self, args, counts, rhss, body):
        assert len(counts) > 0 # otherwise just use a begin
        assert isinstance(args, SymList)
        SequencedBodyAST.__init__(self, body, counts_needed=len(rhss))
        self.counts = counts
        total_counts = [0] * len(counts)
        total_count = 0
        for i, count in enumerate(counts):
            total_counts[i] = total_count
            total_count += count
        self.total_counts = total_counts
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

    def _free_vars(self):
        x = AST._free_vars(self)
        x = x.without_many(self.args.elems)
        return x

    def normalize(self, context):
        # XXX could we do something smarter here?
        args = self._rebuild_args()
        rhss = [Context.normalize_term(rhs) for rhs in self.rhss]
        body = [Context.normalize_term(b)   for b   in self.body]
        result = make_letrec(args, rhss, body)
        return context.plug(result)

    def _rebuild_args(self):
        start = 0
        result = [None] * len(self.counts)
        for i, c in enumerate(self.counts):
            result[i] = self.args.elems[start:start+c]
            start += c
        return result

    def _tostring(self):
        varss = self._rebuild_args()
        bindings = [None] * len(varss)
        for i, vars in enumerate(varss):
            lhs = "(%s)" % " ".join([v.variable_name() for v in vars])
            rhs = self.rhss[i].tostring()
            bindings[i] = "[%s %s]" % (lhs, rhs)
        bindings = " ".join(bindings)
        body = " ".join([b.tostring() for b in self.body])
        return "(letrec (%s) %s)" % (bindings, body)

def _make_symlist_counts(varss):
    counts = []
    argsl = []
    for vars in varss:
        counts.append(len(vars))
        argsl.extend(vars)
    argsl = argsl[:] # copy to make fixed-size
    return SymList(argsl), counts

def make_let(varss, rhss, body):
    if not varss:
        return Begin.make(body)

    body = remove_pure_ops(body)
    if len(body) != 1 or not isinstance(body[0], Let):
        return _make_let_direct(varss, rhss, body)

    body = body[0]
    assert isinstance(body, Let)
    for rhs in body.rhss:
        frees = rhs.free_vars()
        for vars in varss:
            for var in vars:
                if frees.haskey(var):
                    return _make_let_direct(varss, rhss, [body])
    # At this point, we know the inner let does not
    # reference vars in the outer let
    varss = varss + body._rebuild_args()
    rhss  = rhss  + body.rhss
    body  = body.body
    return make_let(varss, rhss, body)

def make_let_singlevar(sym, rhs, body):
    # Try to convert nested lets into a single let e.g.
    # (let ([v1 e1]) (let ([v2 e2]) e3)) => (let ([v1 e1] [v2 e2]) e3)
    # This improves the performance of some of the AST anaylsis/transformation
    # passes and flattens the environment, reducing allocation and pointer hopping.
    if len(body) == 1:
        b = body[0]
        if isinstance(b, Let):
            for r in b.rhss:
                if r.free_vars().haskey(sym):
                    break
            else:
                varss = [[sym]] + b._rebuild_args()
                rhss  = [rhs] + b.rhss
                body  = b.body
                return make_let(varss, rhss, body)
    body = remove_pure_ops(body)
    return Let(SymList([sym]), [1], [rhs], body)

def _make_let_direct(varss, rhss, body):
    symlist, counts = _make_symlist_counts(varss)
    if len(body) == 1:
        b = body[0]
        if isinstance(b, Begin):
            body = b.body
    return Let(symlist, counts, rhss, body)

def make_letrec(varss, rhss, body):
    if not varss:
        return Begin.make(body)
    if len(varss) == 1 and len(varss[0]) == 1:
        rhs = rhss[0]
        sym = varss[0][0]
        if isinstance(rhs, CaseLambda) and LexicalVar(sym) not in rhs.mutated_vars():
            reclambda = rhs.make_recursive_copy(sym)
            return make_let_singlevar(sym, reclambda, body)

    # Convert letrec binding no values to a let since the interpreter optimizes
    # them better
    for vars in varss:
        if vars:
            break
    else:
        return make_let(varss, rhss, body)

    symlist, counts = _make_symlist_counts(varss)
    return Letrec(symlist, counts, rhss, body)

class Let(SequencedBodyAST):
    _immutable_fields_ = ["rhss[*]", "args", "counts[*]", "env_speculation_works?", "remove_num_envs[*]"]
    visitable = True

    import_from_mixin(BindingFormMixin)

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
        self._check_environment_consistency(env, env_structure)
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

    def _free_vars(self):
        x = SymbolSet.EMPTY
        for b in self.body:
            x = x.union(b.free_vars())
        x = x.without_many(self.args.elems)
        for b in self.rhss:
            x = x.union(b.free_vars())
        return x

    def normalize(self, context):
        args = self._rebuild_args()
        body = Begin.make(self.body)
        context = Context.Let(args, self.rhss, body, context)
        return self.rhss[0], context

    def _rebuild_args(self):
        start = 0
        result = [None] * len(self.counts)
        for i, c in enumerate(self.counts):
            result[i] = self.args.elems[start:start+c]
            start += c
        return result

    def _tostring(self):
        result = ["(let ("]
        j = 0
        for i, count in enumerate(self.counts):
            result.append("[")
            if count > 1:
                result.append("(")
            for k in range(count):
                if k > 0:
                    result.append(" ")
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
    visitable = True

    def __init__(self, ns, r, display_names):
        self.names = ns
        self.rhs = r
        self.display_names = display_names

    def defined_vars(self, defs):
        for n in self.names:
            defs[n] = None

    def interpret(self, env, cont):
        return self.rhs.interpret(env, cont)

    def direct_children(self):
        return [self.rhs]

    def normalize(self, context):
        rhs    = Context.normalize_term(self.rhs)
        result = DefineValues(self.names, rhs, self.display_names)
        return context.plug(result)

    def _tostring(self):
        return "(define-values %s %s)" % (
            self.display_names, self.rhs.tostring())


def get_printable_location_two_state(green_ast, came_from):
    if green_ast is None:
        return 'Green_Ast is None'
    surrounding = green_ast.surrounding_lambda
    if green_ast.should_enter:
        return green_ast.tostring() + ' from ' + came_from.tostring()
    return green_ast.tostring()

driver_two_state = jit.JitDriver(reds=["env", "cont"],
                                 greens=["ast", "came_from"],
                                 get_printable_location=get_printable_location_two_state,
                                 should_unroll_one_iteration=lambda *args : True,
                                 is_recursive=True)

def inner_interpret_two_state(ast, env, cont):
    came_from = ast
    config = env.pycketconfig()
    while True:
        driver_two_state.jit_merge_point(ast=ast, came_from=came_from, env=env, cont=cont)
        came_from = ast if isinstance(ast, App) else came_from
        t = type(ast)
        # Manual conditionals to force specialization in translation
        # This (or a slight variant) is known as "The Trick" in the partial evaluation literature
        # (see Jones, Gomard, Sestof 1993)
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
                       get_printable_location=get_printable_location_one_state,
                       should_unroll_one_iteration=lambda *args : True,
                       is_recursive=True)

def inner_interpret_one_state(ast, env, cont):
    while True:
        driver_one_state.jit_merge_point(ast=ast, env=env, cont=cont)
        ast, env, cont = ast.interpret(env, cont)
        if ast.should_enter:
            driver_one_state.can_enter_jit(ast=ast, env=env, cont=cont)

def interpret_one(ast, env=None, cont=None):
    if env is None:
        env = ToplevelEnv()
    if env.pycketconfig().two_state:
        inner_interpret = inner_interpret_two_state
    else:
        inner_interpret = inner_interpret_one_state
        
    if cont is None:
        cont = NilCont()
    
    cont.update_cm(values.parameterization_key, values_parameter.top_level_config)
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

def interpret_module(m, env):
    env = env if env else ToplevelEnv()
    m.interpret_mod(env)
    return m

def interpret(asts):
    env = ToplevelEnv()
    x = None
    for a in asts:
        x = interpret_toplevel(a, env)
    return x
