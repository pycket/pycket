from pycket.util  import snake_case
from rpython.rlib import jit, objectmodel

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
        except SchemeException as exn:
            return convert_runtime_exception(exn, env, cont)
        except OSError as exn:
            return convert_os_error(exn, env, cont)
        return return_value_direct(val, env, cont)

    def interpret_simple(self, env):
        raise NotImplementedError("possible issue in A-normalization: %s is not simple interpretable" % self.tostring())

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

