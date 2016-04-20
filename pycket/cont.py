import inspect

from rpython.rlib import jit, objectmodel, unroll

class Link(object):
    _immutable_fields_ = ["key", "next"]
    def __init__(self, k, v, next):
        from pycket.values import W_Object
        assert isinstance(k, W_Object) and isinstance(v, W_Object)
        assert next is None or isinstance(next, Link)
        self.key = k
        self.val = v
        self.next = next

    @jit.unroll_safe
    def clone_links(self):
        marks = None
        while self is not None:
            marks = Link(self.key, self.val, marks)
            self = self.next
        return marks

class BaseCont(object):
    # Racket also keeps a separate stack for continuation marks
    # so that they can be saved without saving the whole continuation.
    _immutable_fields_ = []
    _attrs_ = ['marks']

    # This field denotes whether or not it is safe to directly invoke the
    # plug_reduce operation of the continuation.
    return_safe = False

    def __init__(self):
        self.marks = None

    def has_unwind(self):
        return False

    def has_rewind(self):
        return False

    def unwind(self, env, cont):
        from pycket.interpreter import return_void
        return return_void(env, cont)

    def clone(self):
        result = self._clone()
        if self.marks is not None:
            result.marks = self.marks.clone_links()
        else:
            result.marks = None
        return result

    def _clone(self):
        raise NotImplementedError("abstract method")

    def get_ast(self):
        return None # best effort

    def get_next_executed_ast(self):
        return None # best effort

    def get_previous_continuation(self, upto=[]):
        return None

    @jit.unroll_safe
    def find_cm(self, k, not_found=None):
        from pycket.prims.equal import eqp_logic
        l = self.marks
        while l is not None:
            if eqp_logic(l.key, k):
                return l.val
            l = l.next
        return not_found

    @jit.unroll_safe
    def update_cm(self, k, v):
        from pycket.prims.equal import eqp_logic
        l = self.marks
        while l is not None:
            if eqp_logic(l.key, k):
                l.val = v
                return
            l = l.next
        self.marks = Link(k, v, self.marks)

    def get_marks(self, key, upto=[]):
        from pycket import values
        v = self.find_cm(key)
        return values.W_Cons.make(v, values.w_null) if v is not None else values.w_null

    # XXX: why isn't this in Cont?
    @jit.unroll_safe
    def get_mark_first(self, key, upto=[]):
        p = self
        while p is not None:
            v = p.find_cm(key)
            if v is not None:
                return v
            p = p.get_previous_continuation(upto=upto)
        return None

    def append(self, tail, upto=None, stop=None):
        return tail

    def plug_reduce(self, _vals, env):
        raise NotImplementedError("abstract method")

    def tostring(self):
        "NOT_RPYTHON"
        if not isinstance(self, NilCont) and self.prev:
            return "%s(%s)" % (self.__class__.__name__,self.prev.tostring())
        else:
            return "%s()" % self.__class__.__name__

# Continuation used to signal that the computation is done.
class NilCont(BaseCont):

    def _clone(self):
        return NilCont()

    def plug_reduce(self, vals, env):
        from pycket.interpreter import Done
        raise Done(vals)

class Cont(BaseCont):

    _immutable_fields_ = ['env', 'prev']

    def __init__(self, env, prev):
        BaseCont.__init__(self)
        self.env = env
        self.prev = prev

    def get_previous_continuation(self, upto=[]):
        return self.prev

    def get_ast(self):
        return self.prev.get_ast()

    def get_next_executed_ast(self):
        return self.prev.get_next_executed_ast()

    def append(self, tail, upto=None, stop=None):
        if self is stop:
            return self.clone()
        rest = self.prev.append(tail, upto, stop)
        head = self.clone()
        assert isinstance(head, Cont)
        head.prev = rest
        return head

    def get_marks(self, key, upto=[]):
        from pycket import values
        while self is not None:
            v = self.find_cm(key)
            prev = self.get_previous_continuation(upto=upto)
            if v is not None:
                rest = prev.get_marks(key, upto=upto) if prev is not None else values.w_null
                return values.W_Cons.make(v, rest)
            self = prev
        return values.w_null

class Prompt(Cont):

    _immutable_fields_ = ['tag', 'handler']

    def __init__(self, tag, handler, env, prev):
        Cont.__init__(self, env, prev)
        self.tag     = tag
        self.handler = handler

    def _clone(self):
        return Prompt(self.tag, self.handler, self.env, self.prev)

    def get_previous_continuation(self, upto=[]):
        for tag in upto:
            if tag is self.tag:
                return None
        return self.prev

    def append(self, tail, upto=None, stop=None):
        if upto is self.tag or stop is tail:
            return tail
        return Cont.append(self, tail, upto, stop)

    def plug_reduce(self, _vals, env):
        return self.prev.plug_reduce(_vals, env)

class Barrier(Cont):

    def _clone(self):
        return Barrier(self.env, self.prev)

    def get_previous_continuation(self, upto=[]):
        return None

    def plug_reduce(self, _vals, env):
        return self.prev.plug_reduce(_vals, env)

def _make_args_class(base, argnames):
    unroll_argnames = unroll.unrolling_iterable(enumerate(argnames))

    class Args(base):
        _immutable_fields_ = getattr(base, '_immutable_fields_', []) + argnames
        def _init_args(self, *args):
            for i, name in unroll_argnames:
                setattr(self, name, args[i])

        def _copy_args(self, other):
            for _, name in unroll_argnames:
                val = getattr(self, name)
                setattr(other, name, val)

        def _get_args(self):
            args = ()
            for i, name in unroll_argnames:
                args += (getattr(self, name), )
            return args

        def tostring(self):
            return "%s%s" % (self.__class__.__name__, len(self._get_args()))

    return Args

def continuation(func):
    """ workaround for the lack of closures in RPython. use to decorate a
    function that is supposed to be usable as a continuation. When the
    continuation triggers, the original function is called with one extra
    argument, the computed vals. """

    argspec = inspect.getargspec(func)
    assert argspec.varargs is None
    assert argspec.keywords is None
    assert argspec.defaults is None
    argnames = argspec.args[:-1]
    assert argnames[-1] == "cont"
    assert argnames[-2] == "env"
    argnames = argnames[:-2]

    PrimCont = _make_args_class(Cont, argnames)
    def __init__(self, *args):
        env = args[-2]
        cont = args[-1]
        Cont.__init__(self, env, cont)
        args = args[:-2]
        self._init_args(*args)
    PrimCont.__init__ = __init__

    def _clone(self):
        result = objectmodel.instantiate(PrimCont)
        Cont.__init__(result, self.env, self.prev)
        self._copy_args(result)
        return result

    def plug_reduce(self, vals, env):
        args = self._get_args()
        args += (self.env, self.prev, vals,)
        return func(*args)

    PrimCont._clone = _clone
    PrimCont.plug_reduce = plug_reduce
    PrimCont.__name__ = func.func_name + "PrimCont"

    def make_continuation(*args):
        return PrimCont(*args)

    make_continuation.func_name = func.func_name + "_make_continuation"
    return make_continuation

def make_label(func, enter=False):
    import pycket.values
    from pycket.AST import AST

    func = jit.unroll_safe(func)
    func_argnames, varargs, varkw, defaults = inspect.getargspec(func)
    assert not varkw and not defaults
    if varargs: # grrr, bad
        class Args(Cont):
            _immutable_fields_ = ["args"]
            def __init__(self, *args):
                Cont.__init__(self, args[-2], args[-1])
                self.args = args[:-2]

            def _get_args(self):
                return self.args

            def tostring(self):
                return "%s%s" % (self.__class__.__name__, len(self._get_args()))

    else:
        assert func_argnames[-2] == "env", "next to last argument to %s must be named 'env', not %r" % (func.func_name, func_argnames[-2])

        Args = _make_args_class(Cont, func_argnames[:-2])
        def __init__(self, *args):
            env = args[-2]
            cont = args[-1]
            Cont.__init__(self, env, cont)
            args = args[:-2]
            self._init_args(*args)
        Args.__init__ = __init__

    # The @label decorator will produce a new Label per each use, as an @label can be
    # used to encode a loop (as they act in a manner similar to an assembly label).
    if enter:
        clsname = "LoopLabel"
    else:
        clsname = "Label"
    strrepr = "%s(%s:%s:%s)" % (clsname, func.func_name, func.__module__,
                                func.__code__.co_firstlineno)
    class Label(AST):
        should_enter = enter
        def interpret(self, env, cont):
            assert type(cont) is Args
            args = cont._get_args()
            args += (cont.env, cont.prev)
            return func(*args)
        def tostring(self):
            return strrepr
    Label.__name__ = clsname

    the_label = Label()

    def make(*args):
        env = args[-2]
        return the_label, env, Args(*args)

    return make

# Choose whether or not to use a loop label based on a given predicate
def guarded_loop(pred):
    def wrapper(func):
        loop   = make_label(func, enter=True)
        noloop = make_label(func, enter=False)
        return lambda *args: loop(*args) if pred(*args) else noloop(*args)
    return wrapper

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
    return proc.call(vals.get_all_values(), env, cont)

# A useful continuation constructor. This invokes the given procedure with
# the enviroment and continuation when values are supplied.
# This is just a simple way to place a function call onto the continuation.
@continuation
def call_extra_cont(proc, calling_app, env, cont, vals):
    return proc.call_with_extra_info(vals.get_all_values(), env, cont, calling_app)

