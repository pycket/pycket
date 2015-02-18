from rpython.rlib import jit

class AST(object):
    _attrs_ = ["should_enter", "mvars", "surrounding_lambda", "_stringrepr", "app_like", "count"]
    _immutable_fields_ = ["should_enter?", "surrounding_lambda", "app_like"]
    _settled_ = True

    should_enter = False # default value
    _stringrepr = None # default value
    mvars = None
    surrounding_lambda = None
    app_like = False

    simple = False

    is_label = False

    in_cycle = False

    count = 0

    def defined_vars(self): return {}

    def interpret(self, env, cont):
        from pycket.interpreter import return_value_direct
        # default implementation for simple AST forms
        assert self.simple
        # interpret should only be called from interpret_one, therefore it's
        # safe to not use the Label implementation of return_value here
        return return_value_direct(self.interpret_simple(env), env, cont)

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
        already set. The field is only actually mutated when it was originally
        false, as the should_enter field is marked as quasi-mutable.
        """
        if not self.should_enter:
            self.should_enter = True
            return True
        return False

    def direct_children(self):
        return []

    def free_vars(self):
        free_vars = {}
        for child in self.direct_children():
            free_vars.update(child.free_vars())
        return free_vars

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
        if self.mvars is not None:
            return self.mvars
        self.mvars = self._mutated_vars()
        return self.mvars

    def _mutated_vars(self):
        raise NotImplementedError("abstract base class")

    def tostring(self):
        _stringrepr = self._stringrepr
        if _stringrepr is None:
            _stringrepr = self._stringrepr = self._tostring()
        return _stringrepr

    def _tostring(self):
        return "UNKNOWN AST: "

    def __str__(self):
        return self.tostring()

