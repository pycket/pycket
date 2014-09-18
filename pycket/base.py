from pycket.error             import SchemeException
from pycket.cont              import label
from rpython.tool.pairtype    import extendabletype

@label
def tailcall(func, args, env, cont):
    return func(args, env, cont)

class W_Object(object):
    __metaclass__ = extendabletype
    _attrs_ = []
    errorname = "%%%%unreachable%%%%"
    def __init__(self):
        raise NotImplementedError("abstract base class")

    def iscallable(self):
        return False

    # The general `call` method is setup to return control to the CEK machine
    # before executing the body of the function being called. Unless you know
    # what you are doing, please override the `_call` method. This is needed to
    # get around (R)Python's lack of tail call elimination.
    # `_call` should always be safe to override, but `call` is safe to override
    # if it implements a simple primitive.
    def _call(self, args, env, cont):
        raise NotImplementedError("abstract base class")

    def call(self, args, env, cont):
        if self.iscallable():
            return tailcall(self._call, args, env, cont)
        raise SchemeException("%s is not callable" % self.tostring())

    def call_with_extra_info(self, args, env, cont, calling_app):
        return self.call(args, env, cont)

    def mark_non_loop(self):
        pass

    # an arity is a pair of a list of numbers and either -1 or a non-negative integer
    def get_arity(self):
        if self.iscallable():
            return ([],0)
        else:
            raise SchemeException("%s does not have arity" % self.tostring())

    def is_impersonator(self):
        return self.is_chaperone()
    def is_chaperone(self):
        return False
    def is_proxy(self):
        return self.is_chaperone() or self.is_impersonator()
    def get_proxied(self):
        return self
    def get_properties(self):
        return {}

    def immutable(self):
        return False
    def equal(self, other):
        return self is other # default implementation
    def eqv(self, other):
        return self is other # default implementation
    def tostring(self):
        return str(self)
