
import inspect

from pycket       import values
from pycket.error import SchemeException
from rpython.rlib import jit, objectmodel, unroll
from rpython.rlib.objectmodel import specialize

class EndOfInput(Exception):
    pass

class ArgParser(object):

    _attrs_ = ['context', 'args', 'index']
    _immutable_fields_ = ['context', 'args']

    def __init__(self, context, args, start_at=0):
        assert start_at >= 0
        self.context = context
        self.args    = args
        self.index   = start_at

    def __nonzero__(self):
        return self.has_more()

    def has_more(self):
        return 0 <= self.index < len(self.args)

    def next(self):
        if not self.has_more():
            raise EndOfInput
        index = self.index
        val = self.args[index]
        return val

    @specialize.arg(1)
    def expect(self, *args):
        val = self.next()
        if validate_arg(val, *args):
            self.index += 1
            return val
        raise SchemeException(
                "%s: expected %s at argument %d got %s -- type: %s" %
                (self.context, errorname(*args), self.index, val.tostring(), type(val)))

    @specialize.arg(1)
    @jit.unroll_safe
    def expect_many(self, *args):
        length = len(self.args) - self.index
        results = [None] * length
        for i in range(length):
            results[i] = self.expect(*args)
        return results

def is_constant_class(cls):
    return inspect.isclass(cls)

@specialize.arg(1)
def validate_arg(value, arg, *args):
    if is_constant_class(arg):
        if isinstance(value, arg):
            return True
    elif value is arg:
        return True

    return bool(args) and validate_arg(value, *args)

@specialize.arg(0)
def _errorname(arg, *args):
    if is_constant_class(arg):
        retval = arg.errorname
    else:
        retval = arg.tostring()
    if not args:
        return retval
    else:
        return retval + " " + _errorname(*args)

@specialize.arg(0)
def errorname(arg, *args):
    if not args:
        return _errorname(arg)
    return "(or/c %s)" % _errorname(arg, *args)

from rpython.rtyper.extregistry import ExtRegistryEntry

class Entry(ExtRegistryEntry):
    _about_ = is_constant_class

    def compute_result_annotation(self, s_cls):
        from rpython.annotator.model import SomeBool, SomePBC, SomeInstance
        r = SomeBool()
        assert s_cls.is_constant()
        if isinstance(s_cls, SomePBC):
            r.const = inspect.isclass(s_cls.const)
        elif isinstance(s_cls, SomeInstance):
            r.const = False
        else:
            assert False
        return r

    def specialize_call(self, hop):
        from rpython.rtyper.lltypesystem import lltype
        hop.exception_cannot_occur()
        return hop.inputconst(lltype.Bool, hop.s_result.const)

