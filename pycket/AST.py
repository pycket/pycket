from pycket.util  import snake_case
from rpython.rlib import jit, objectmodel
from rpython.rlib.rstack import stack_almost_full

class Visitable(type):
    def __new__(cls, name, bases, dct):
        visit_method_name = "visit_" + snake_case(name)
        @objectmodel.specialize.argtype(1)
        def dispatch_visitor(self, visitor, *args):
            return getattr(visitor, visit_method_name)(self, *args)
        if dct.get('visitable', False):
            dct['visit'] = dispatch_visitor
        result = type.__new__(cls, name, bases, dct)
        return result

class AST(object):
    __metaclass__ = Visitable

    _attrs_ = ["should_enter", "surrounding_lambda", "_stringrepr"]
    _immutable_fields_ = ["should_enter", "surrounding_lambda"]
    _settled_ = True

    should_enter = False # default value
    _stringrepr = None # default value
    surrounding_lambda = None

    simple = False
    ispure = False

    def defined_vars(self, defs):
        pass

    def interpret(self, env, cont):
        from pycket.interpreter import return_value_direct
        from pycket.prims.control import convert_runtime_exception, convert_os_error
        from pycket.error import SchemeException
        # default implementation for simple AST forms
        assert self.simple
        # interpret should only be called from interpret_one, therefore it's
        # safe to not use the Label implementation of return_value here
        try:
            val = self.interpret_simple(env)
        except SchemeException, exn:
            return convert_runtime_exception(exn, env, cont)
        except OSError, exn:
            return convert_os_error(exn, env, cont)
        return return_value_direct(val, env, cont)

    def interpret_simple(self, env):
        raise NotImplementedError("abstract base class")

    def set_surrounding_lambda(self, lam):
        from pycket.interpreter import Lambda
        assert isinstance(lam, Lambda)
        self.surrounding_lambda = lam
        for child in self.direct_children():
            child.set_surrounding_lambda(lam)

    def set_should_enter(self):
        """ Set the should_enter field and returns whether or not the field was
        already set. This looks potentially dangerous: the field is marked
        immutable above. It works however, because should_enter MUST only be
        used for deciding whether to use can_enter_jit or not. As long as that
        is the case, mutating it without informing the JIT is fine: We don't
        want the existing JIT code to be thrown out just because we set a flag
        on an AST somewhere that was already traced. The interpreter is not
        affected and will see the change, thus potentially newly tracing the
        AST.
        """
        if not self.should_enter:
            self.should_enter = True
            return True
        return False

    def direct_children(self):
        return []

    def collect_module_info(self, info):
        return self.direct_children()

    def free_vars(self, cache=None):
        if cache is None:
            cache = {}
        else:
            try:
                return cache[self]
            except KeyError:
                pass
        fvars = self._free_vars(cache)
        cache[self] = fvars
        return fvars

    def _free_vars(self, cache):
        from pycket.interpreter import SymbolSet
        free_vars = SymbolSet.EMPTY()
        for child in self.direct_children():
            free_vars = free_vars.union(child.free_vars(cache))
        return free_vars

    def mutated_vars(self, cache=None):
        if cache is None:
            cache = {}
        else:
            try:
                return cache[self]
            except KeyError:
                pass
        mvars = self._mutated_vars(cache)
        cache[self] = mvars
        return mvars

    def _mutated_vars(self, cache):
        from pycket.interpreter import variable_set
        x = variable_set()
        for b in self.direct_children():
            x.update(b.mutated_vars(cache))
        return x

    def normalize(self, ctxt):
        return ctxt.plug(self)

    def tostring(self):
        _stringrepr = self._stringrepr
        if _stringrepr is None:
            _stringrepr = self._stringrepr = self._tostring()
        return _stringrepr

    def constant_prop(self, constants):
        for child in self.direct_children():
            child.constant_prop(constants)

    def _tostring(self):
        return "UNKNOWN AST: "

    def write(self, port, env):
        port.write(self.tostring())

    def __str__(self):
        return self.tostring()

    # stackfull mode support methods

    def switch_to_interpret_stack(self, env, cont):
        from pycket.util import console_log
        try:
            console_log("CEK -> STACKFUL : ast ---- %s" % (self), 1)
            w_val = self.interpret_stack(env)
        except ConvertStack, cv:
            cv.chain(cont)
            console_log("STACKFUL -> CEK : ast ---- %s" % (cv.ast.tostring()), 1)
            return cv.ast, cv.env, cv.topcont
        return cont.plug_reduce(w_val, env)

    def interpret_stack(self, env):
        from pycket.interpreter import App
        from pycket.values import W_Prim
        from pycket.values_parameter import W_Parameter

        if stack_almost_full():
            #console_log("************* ABOUT TO OVERFLOW **************", 1)

            # this is not the correct way to handle the stack
            # overflow, as it switches back to the CEK and continue
            # there until it gets back here again, which is extra
            # allocation+GC

            # coming up (to stay in the stackful):
            # i)   raise another type of exception (e.g. BounceException)
            # ii) make some continuations for the stackful and collect
            #      them instead of CEK continuations (e.g. LetCont)
            #      (i.e. anybody calls to interpret_stack catches the
            #      BounceException and attaches it's own stackful
            #      continuation to it)
            # iii) place the trampoline in switch_to_interpret_stack
            #      above and continue with the current ast in
            #      BounceException, and 'plug_reduce_stack' the value
            #      whenever we manage to get one
            raise ConvertStack(self, env)

        from pycket.base import W_StackTrampoline
        while 1:
            stackfull_driver.jit_merge_point(ast=self, env=env)
            if isinstance(self, App):
                w_callable, args_w = self.get_callable_and_args(env)
                if type(w_callable) is W_Prim or isinstance(w_callable, W_Parameter):
                    raise ConvertStack(self, env)

                w_val = self._interpret_stack_app(w_callable, args_w)
            else:
                w_val = self._interpret_stack(env)

            if isinstance(w_val, W_StackTrampoline):
                self = w_val.ast
                assert self is not None
                env = w_val.env
                if self.should_enter:
                    stackfull_driver.can_enter_jit(ast=self, env=env)
            else:
                return w_val

    def _interpret_stack(self, env):
        if self.simple:
            return self.interpret_simple(env)
        raise ConvertStack(self, env)

def get_printable_location_stackfull(green_ast):
    return "stackfull: %s" % (green_ast.tostring(), )

stackfull_driver = jit.JitDriver(
        reds=["env"],
        greens=["ast"],
        get_printable_location=get_printable_location_stackfull,
        should_unroll_one_iteration=lambda *args : True)


class ConvertStack(Exception):
    def __init__(self, ast, env):
        self.ast = ast
        self.topcont = None
        self.patchcont = None
        self.env = env

    def chain(self, newcont):
        from pycket.cont import Cont, Cont
        if self.topcont is None:
            self.topcont = newcont
        if self.patchcont is not None:
            patchcont = self.patchcont
            assert isinstance(patchcont, Cont)
            patchcont.prev = newcont
        self.patchcont = newcont
