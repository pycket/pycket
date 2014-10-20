from pycket.error             import SchemeException
from pycket.cont              import label
from rpython.tool.pairtype    import extendabletype
from rpython.rlib import jit, objectmodel



class W_Object(object):
    __metaclass__ = extendabletype
    _attrs_ = []
    errorname = "%%%%unreachable%%%%"
    def __init__(self):
        raise NotImplementedError("abstract base class")

    def iscallable(self):
        return False

    def call(self, args, env, cont):
        raise SchemeException("%s is not callable" % self.tostring())

    def call_with_extra_info(self, args, env, cont, calling_app):
        return self.call(args, env, cont)

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

    def hash_equal(self):
        return objectmodel.compute_hash(self) # default implementation
    hash_eqv = hash_equal

    def tostring(self):
        return str(self)


class SingletonMeta(type):
    def __new__(cls, name, bases, dct):
        result = type.__new__(cls, name, bases, dct)
        result.singleton = result()
        return result

