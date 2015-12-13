
import inspect

from pycket                   import values
from pycket.error             import SchemeException
from rpython.rlib             import jit, objectmodel, unroll

class Spec(object):

    def __init__(self):
        raise NotImplementedError("abstract base class")

    def conforms(self, value):
        raise NotImplementedError("abstract base class")

    def repr(self):
        raise NotImplementedError("abstract base class")

class Value(Spec):
    def __init__(self, value):
        self.value = value

    @objectmodel.always_inline
    def conforms(self, value):
        return self.value is value

    @objectmodel.always_inline
    def repr(self):
        return self.value.tostring()

class Instance(Spec):
    def __init__(self, cls):
        self.cls = cls

    @objectmodel.always_inline
    def conforms(self, value):
        return isinstance(value, self.cls)

    @objectmodel.always_inline
    def repr(self):
        return self.cls.errorname

CACHE = {}

class ArgParser(object):

    _immutable_fields_ = ['context', 'args']

    def __init__(self, context, args, start_at=0):
        self.context = context
        self.args    = args
        self.index   = start_at

    def __nonzero__(self):
        return 0 <= self.index < len(self.args)

    def has_more(self):
        return 0 <= self.index < len(self.args)

    def next(self):
        if not self.has_more():
            raise StopIteration
        index = self.index
        val = self.args[index]
        self.index = index + 1
        return val

    def ensure_arity(self):
        pass

def define_parser(name, *_specs):

    if _specs in CACHE:
        checker = CACHE[_specs]
        assert checker.__name__ == name
        return checker
    else:
        specs = []
        for spec in _specs:
            if inspect.isclass(spec):
                specs.append(Instance(spec))
            else:
                specs.append(Value(spec))
        specs = unroll.unrolling_iterable(specs)

        def checker(self):
            arg = self.next()
            for spec in specs:
                if spec.conforms(arg):
                    return arg
            names = " ".join([spec.repr() for spec in specs])
            if len(_specs) == 1:
                types = names[0]
            else:
                types = "(or/c %s)" % names
            msg = "%s: expected %s at argument %d got %s" % (self.context, types, self.index, arg.tostring())
            raise SchemeException(msg)
        checker.__name__ = name

        @jit.unroll_safe
        def _checker(self):
            length = len(self.args) - self.index
            results = [None] * length
            for i in range(length):
                results[i] = checker(self)
            return results

    CACHE[_specs] = checker
    setattr(ArgParser, name, checker)
    setattr(ArgParser, "_" + name, _checker)

    return checker

