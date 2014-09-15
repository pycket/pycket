
from rpython.rlib import jit, unroll

# these aren't methods so that can handle empty conts
def get_mark_first(cont, key):
    p = cont
    while isinstance(p, Cont):
        v = p.find_cm(key)
        if v:
            return v
        elif p.prev:
            p = p.prev
    return p.find_cm(key)

def get_marks(cont, key):
    from pycket import values
    if not cont:
        return values.w_null
    # FIXME: don't use recursion
    # it would be much more convenient to write this with mutable pairs
    v = cont.find_cm(key)
    if v:
        return values.W_Cons.make(v, get_marks(cont.prev, key))
    else:
        return get_marks(cont.prev, key)

class Link(object):
    def __init__(self, k, v, next):
        from pycket.values import W_Object
        assert isinstance(k, W_Object)
        assert isinstance(v, W_Object)
        assert (next is None) or isinstance(next, Link)
        self.key = k
        self.val = v
        self.next = next

class BaseCont(object):
    # Racket also keeps a separate stack for continuation marks
    # so that they can be saved without saving the whole continuation.
    def __init__(self):
        self.marks = None

    def find_cm(self, k):
        from pycket.prims.equal import eqp_logic
        l = self.marks
        while l is not None:
            if eqp_logic(l.key, k):
                return l.val
            l = l.next
        return None

    def update_cm(self, k, v):
        from pycket.prims.equal import eqp_logic
        l = self.marks
        while l is not None:
            if eqp_logic(l.key, k):
                l.val = v
                return
            l = l.next
        self.marks = Link(k, v, self.marks)

    def get_marks(self, key):
        from pycket import values
        v = self.find_cm(key)
        return values.W_Cons.make(v, values.w_null) if v is not None else values.w_null

    def plug_reduce(self, _vals, env):
        raise NotImplementedError("abstract method")

    def tostring(self):
        "NOT_RPYTHON"
        if not isinstance(self, NilCont) and self.prev:
            return "%s(%s)"%(self.__class__.__name__,self.prev.tostring())
        else:
            return "%s()"%(self.__class__.__name__)

# Continuation used to signal that the computation is done.
class NilCont(BaseCont):
    def plug_reduce(self, vals, env):
        from pycket.interpreter import Done
        raise Done(vals)

nil_continuation = NilCont()

class Cont(BaseCont):
    _immutable_fields_ = ['env', 'prev']
    def __init__(self, env, prev):
        # TODO: Consider using a dictionary to store the marks
        BaseCont.__init__(self)
        self.env = env
        self.prev = prev

    def get_marks(self, key):
        from pycket import values
        v = self.find_cm(key)
        if v is not None:
            return values.W_Cons.make(v, self.prev.get_marks(key))
        else:
            return self.prev.get_marks(key)

# Not used yet, but maybe if there's a continuation whose prev isn't named `cont`
def continuation_named(name="cont"):
    def wrap(f):
        return continuation(f, prev_name=name)
    return wrap

def continuation(func, prev_name="cont"):
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
            self.marks = None
            for i, name in unroll_argnames:
                if name == prev_name:
                    self.prev = args[i]
                setattr(self, name, args[i])

        def plug_reduce(self, vals, env):
            args = ()
            for i, name in unroll_argnames:
                args += (getattr(self, name), )
            args += (vals,)
            return func(*args)
    PrimCont.__name__ = func.func_name + "PrimCont"

    def make_continuation(*args):
        return PrimCont(*args)

    make_continuation.func_name = func.func_name + "_make_continuation"
    return make_continuation

def make_label(func, enter=False):
    from pycket.AST import AST

    class Args(BaseCont):
        _immutable_fields_ = ["args"]
        def __init__(self, args):
            self.args = args

    func = jit.unroll_safe(func)

    # The @label decorator will produce a new Label per each use, as an @label can be
    # used to encode a loop (as they act in a manner similar to an assembly label).
    class Label(AST):
        should_enter = enter
        def interpret(self, env, cont):
            from pycket.interpreter import empty_vals
            assert type(cont) is Args
            return func(*cont.args)
        def tostring(self):
            if self.should_enter:
                return "LoopLabel(%s)" % func.__name__
            return "Label(%s)" % func.__name__

    the_label = Label()

    def make(*args):
        env = args[-2]
        return the_label, env, Args(args) #invoke_func(func, args)

    return make

def loop_label(func):
    return make_label(func, enter=True)

# A label is function wrapped by some extra logic to hand back control to the
# CEK loop before invocation.
def label(func):
    return make_label(func, enter=False)

# A useful continuation constructor. This invokes the given procedure with
# the enviroment and continuation when values are supplied.
# This is just a simple way to place a function call onto the continuation.
@continuation
def call_cont(proc, env, cont, vals):
    return proc.call(vals._get_full_list(), env, cont)

