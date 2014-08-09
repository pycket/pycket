
from rpython.rlib import unroll

# these aren't methods so that can handle empty conts
def get_mark_first(cont, key):
    p = cont
    while p:
        v = p.find_cm(key)
        if v:
            return v
        elif p.prev:
            p = p.prev
        else:
            return None

def get_marks(cont, key):
    from pycket import values
    if not(cont):
        return values.w_null
    # FIXME: don't use recursion
    # it would be much more convenient to write this with mutable pairs
    v = cont.find_cm(key)
    if v:
        return values.W_Cons(v, get_marks(cont.prev, key))
    else:
        return get_marks(cont.prev, key)

class Link(object):
    def __init__(self, k, v, next):
        from pycket.values import W_Object
        assert isinstance(k, W_Object)
        assert isinstance(v, W_Object)
        assert (d is None) or isinstance(d, Link)
        self.key = k
        self.val = v
        self.next = next

class Cont(object):
    # Racket also keeps a separate stack for continuation marks
    # so that they can be saved without saving the whole continuation.
    _immutable_fields_ = ['env', 'prev', 'marks']
    def __init__(self, env, prev):
        self.env = env
        self.prev = prev
        self.marks = None

    def find_cm(self, k):
        l = self.marks
        from pycket.prims import eqp_logic
        while l:
            if eqp_logic(l.key, k):
                return l.val
            else:
                l = l.rest
        return None

    def update_cm(self, k, v):
        from pycket.prims import eqp_logic
        l = self.marks
        while l:
            if eqp_logic(l.key, k):
                l.val = v
            else:
                l = l.rest

    def tostring(self):
        "NOT_RPYTHON"
        if self.prev:
            return "%s(%s)"%(self.__class__.__name__,self.prev.tostring())
        else:
            return "%s()"%(self.__class__.__name__)

def continuation_wrapper(func, is_label=False):
    """ workaround for the lack of closures in RPython. use to decorate a
    function that is supposed to be usable as a continuation. When the
    continuation triggers, the original function is called with one extra
    argument, the computed vals. """

    import inspect
    argspec = inspect.getargspec(func)
    assert argspec.varargs is None
    assert argspec.keywords is None
    assert argspec.defaults is None
    argnames = argspec.args

    if not is_label:
        argnames = argnames[:-1]

    unroll_argnames = unroll.unrolling_iterable(enumerate(argnames))

    class PrimCont(Cont):
        _immutable_fields_ = argnames

        def __init__(self, *args):
            for i, name in unroll_argnames:
                setattr(self, name, args[i])

        def plug_reduce(self, vals, env):
            args = ()
            for i, name in unroll_argnames:
                args += (getattr(self, name), )
            if not is_label:
                args += (vals, )
            return func(*args)
    PrimCont.__name__ = func.func_name + "PrimCont"

    def make_continuation(*args):
        return PrimCont(*args)

    make_continuation.func_name = func.func_name + "_make_continuation"
    return make_continuation

def continuation(func):
    return continuation_wrapper(func, False)

def label(func):
    return continuation_wrapper(func, True)

# A useful continuation constructor. This invokes the given procedure with
# the enviroment and continuation when values are supplied.
# This is just a simple way to place a function call onto the continuation.
@continuation
def call_cont(proc, env, cont, vals):
    return proc.call(vals._get_full_list(), env, cont)

# A continuation that simply invokes the code given with the args, env, and
# continuation. Typically, the code corresponds to the `_call` method used
# for implementing function calls. This continuation is used to return control
# to the CEK machine's dispatch loop before actually invoking the code.
@label
def tailcall_cont(code, args, env, cont):
    return code(args, env, cont)
