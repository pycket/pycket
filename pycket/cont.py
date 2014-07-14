
from rpython.rlib import unroll

def get_mark_first(cont, key):
    p = cont
    while p:
        if key in p.marks:
            return p.marks[key]
        elif p.prev:
            p = p.prev
        else:
            return None

def get_marks(cont, key):
    from pycket import values
    if not(cont):
        return values.w_null
    # FIXME: don't use recursion
    if key in cont.marks:
        return values.W_Cons(cont.marks[key], get_marks(cont.prev, key))
    else:
        return get_marks(cont.prev, key)
    
        

class Cont(object):
    _immutable_fields_ = ['env', 'prev', 'marks']
    def __init__(self, env, prev):
        self.env = env
        self.prev = prev
        # FIXME
        # Note: Racket uses a list for this for speed in 
        # cases where there are few marks (which is basically always).
        self.marks = {}
        # Racket also keeps a separate stack for continuation marks
        # so that they can be saved without saving the whole continuation.

    def update_cm(self, key, val):
        self.marks[key] = val

    def tostring(self):
        "NOT_RPYTHON"
        if self.prev:
            return "%s(%s)"%(self.__class__.__name__,self.prev.tostring())
        else:
            return "%s()"%(self.__class__.__name__)

def continuation(func):
    """ workaround for the lack of closures in RPython. use to decorate a
    function that is supposed to be usable as a continuation. When the
    continuation triggers, the original function is called with one extra
    argument, the computed vals. """

    import inspect
    argspec = inspect.getargspec(func)
    assert argspec.varargs is None
    assert argspec.keywords is None
    assert argspec.defaults is None
    argnames = argspec.args[:-1]

    unroll_argnames = unroll.unrolling_iterable(enumerate(argnames))

    class PrimCont(Cont):
        _immutable_fields_ = argnames

        def __init__(self, *args):
            for i, name in unroll_argnames:
                setattr(self, name, args[i])

        def plug_reduce(self, vals):
            args = ()
            for i, name in unroll_argnames:
                args += (getattr(self, name), )
            args += (vals, )
            return func(*args)
    PrimCont.__name__ = func.func_name + "PrimCont"

    def make_continuation(*args):
        return PrimCont(*args)

    make_continuation.func_name = func.func_name + "_make_continuation"
    return make_continuation

# A useful continuation constructor. This invokes the given procedure with
# the enviroment and continuation when values are supplied.
# This is just a simple way to place a function call onto the continuation.
@continuation
def call_cont(proc, env, cont, vals):
    return proc.call(vals._get_full_list(), env, cont)

