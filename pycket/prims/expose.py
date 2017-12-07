
from rpython.rlib import jit, unroll
from pycket.error import SchemeException
from pycket.arity import Arity

prim_env = {}

SAFE = 0
UNSAFE = 1
SUBCLASS_UNSAFE = 2

class unsafe(object):
    """ can be used in the argtypes part of an @expose call. The corresponding
    argument will be assumed to have the precise corresponding type (no
    subtypes!)."""

    def __init__(self, typ):
        assert not typ.__subclasses__()
        self.typ = typ

    def make_unwrapper(self):
        typ = self.typ
        def unwrapper(w_arg):
            assert w_arg is not None
            # the following precise type check is intentional.
            # record_exact_class records a precise class to the JIT,
            # excluding subclasses
            assert type(w_arg) is typ
            jit.record_exact_class(w_arg, typ)
            return w_arg
        return unwrapper, typ.errorname

class subclass_unsafe(object):
    """ can be used in the argtypes part of an @expose call. The corresponding
    argument will be assumed to have a subtype of the corresponding type.
    This tends to be less efficient than unsafe, so use only if unsafe doesn't
    work.
    """

    def __init__(self, typ):
        self.typ = typ

    def make_unwrapper(self):
        typ = self.typ
        def unwrapper(w_arg):
            # this is not really communicating much to the jit
            # but at least type inference knows the unsafe type
            # (and if we come up with better ideas when know the place to
            # use it)
            assert isinstance(w_arg, typ)
            return w_arg
        return unwrapper, typ.errorname


class default(object):
    """ can be used in the argtypes part of an @expose call. If the argument is
    missing, the default value is passed to the function. """

    def __init__(self, typ, default=None):
        self.typ = typ
        self.default = default

class procedure(object):
    errorname = "procedure"
    def make_unwrapper(self):
        return unwrap_procedure, "procedure"

def unwrap_procedure(w_arg):
    if w_arg.iscallable():
        return w_arg
    return None

procedure = procedure()

def _make_arg_unwrapper(func, argstypes, funcname, has_self=False, simple=False):
    argtype_tuples = []
    min_arg = 0
    isdefault = False
    for i, typ in enumerate(argstypes):
        default_value = None
        if isinstance(typ, default):
            isdefault = True
            default_value = typ.default
            typ = typ.typ
        else:
            assert not isdefault, "non-default argument %s after default argument" % typ
            min_arg += 1
        unwrapper, errorname = typ.make_unwrapper()
        type_errormsg = "expected %s as argument %s to %s, got " % (
                            errorname, i, funcname)
        argtype_tuples.append((i, unwrapper, isdefault, default_value, type_errormsg))
    unroll_argtypes = unroll.unrolling_iterable(argtype_tuples)
    max_arity = len(argstypes)
    if min_arg == max_arity:
        aritystring = max_arity
    else:
        aritystring = "%s to %s" % (min_arg, max_arity)
    errormsg_arity = "expected %s arguments to %s, got " % (
        aritystring, funcname)
    if min_arg == max_arity and not has_self and min_arg in (1, 2) and simple:
        func_arg_unwrap, call1, call2 = make_direct_arg_unwrapper(
            func, min_arg, unroll_argtypes, errormsg_arity)
    else:
        func_arg_unwrap = make_list_arg_unwrapper(
            func, has_self, min_arg, max_arity, unroll_argtypes, errormsg_arity)
        call1 = call2 = None
    _arity = Arity.oneof(*range(min_arg, max_arity+1))
    return func_arg_unwrap, _arity, call1, call2

def make_direct_arg_unwrapper(func, num_args, unroll_argtypes, errormsg_arity):
    # fast paths that allow the calling without constructing an args list
    def func_arg_unwrap(*allargs):
        from pycket import values
        args = allargs[0]
        rest = allargs[1:]
        typed_args = ()
        lenargs = len(args)
        if lenargs != num_args:
            raise SchemeException(errormsg_arity + str(lenargs))
        if num_args == 1:
            return func_direct_unwrap(args[0], *rest)
        else:
            assert num_args == 2
            return func_direct_unwrap(args[0], args[1], *rest)
    func_arg_unwrap.func_name = "%s_arg_unwrap%s" % (func.func_name, num_args)
    if num_args == 1:
        (i, unwrapper, default, default_value, type_errormsg), = list(unroll_argtypes)
        assert i == 0
        assert not default
        def func_direct_unwrap(arg1, *rest):
            typed_arg1 = unwrapper(arg1)
            if typed_arg1 is None:
                raise SchemeException(type_errormsg + arg1.tostring())
            return func(typed_arg1, *rest)
        func_direct_unwrap.func_name = "%s_fast1" % (func.func_name, )
        return func_arg_unwrap, func_direct_unwrap, None
    else:
        assert num_args == 2
        ((i1, unwrapper1, default1, default_value1, type_errormsg1),
         (i2, unwrapper2, default2, default_value2, type_errormsg2)
            ) = list(unroll_argtypes)
        assert i1 == 0 and i2 == 1
        assert not default1 and not default2
        def func_direct_unwrap(arg1, arg2, *rest):
            typed_arg1 = unwrapper1(arg1)
            if typed_arg1 is not None:
                typed_arg2 = unwrapper2(arg2)
                if typed_arg2 is not None:
                    return func(typed_arg1, typed_arg2, *rest)
                else:
                    type_errormsg = type_errormsg2
                    arg = arg2
            else:
                type_errormsg = type_errormsg1
                arg = arg1
            raise SchemeException(type_errormsg + arg.tostring())
        func_direct_unwrap.func_name = "%s_fast2" % (func.func_name, )
        return func_arg_unwrap, None, func_direct_unwrap


def make_list_arg_unwrapper(func, has_self, min_arg, max_arity, unroll_argtypes, errormsg_arity):
    def func_arg_unwrap(*allargs):
        from pycket import values
        if has_self:
            self = allargs[0]
            args = allargs[1]
            rest = allargs[2:]
            typed_args = (self, )
        else:
            args = allargs[0]
            rest = allargs[1:]
            typed_args = ()
        lenargs = len(args)
        if not min_arg <= lenargs <= max_arity:
            raise SchemeException(errormsg_arity + str(lenargs))
        type_errormsg = None
        for i, unwrapper, default, default_value, type_errormsg in unroll_argtypes:
            if i >= min_arg and i >= lenargs:
                assert default
                typed_args += (default_value, )
                continue
            arg = args[i]
            typed_arg = unwrapper(arg)
            if typed_arg is None:
                break
            typed_args += (typed_arg, )
        else:
            typed_args += rest
            return func(*typed_args)
        # reachable only by break when the type check fails
        assert type_errormsg is not None
        raise SchemeException(type_errormsg + arg.tostring())
    func_arg_unwrap.func_name = "%s_arg_unwrap" % (func.func_name, )
    return func_arg_unwrap

def _make_result_handling_func(func_arg_unwrap, simple):
    if simple:
        def func_result_handling(*args):
            from pycket.interpreter   import return_multi_vals, return_value_direct
            from pycket.prims.control import convert_runtime_exception
            from pycket               import values
            env = args[-2]
            cont = args[-1]
            args = args[:-2]
            try:
                result = func_arg_unwrap(*args)
            except SchemeException, exn:
                return convert_runtime_exception(exn, env, cont)
            if result is None:
                result = values.w_void
            if isinstance(result, values.Values):
                return return_multi_vals(result, env, cont)
            else:
                return return_value_direct(result, env, cont)
        return func_result_handling
    else:
        return func_arg_unwrap

# FIXME: Abstract away the common operations between this and expose
def make_procedure(n="<procedure>", argstypes=None, simple=True, arity=None):
    def wrapper(func):
        from pycket import values
        names = [n] if isinstance(n, str) else n
        name = names[0]
        if argstypes is not None:
            func_arg_unwrap, _arity, _, _ = _make_arg_unwrapper(func, argstypes, name, simple=simple)
            if arity is not None:
                _arity = arity
        else:
            func_arg_unwrap = func
            _arity = arity or Arity.unknown
            assert isinstance(_arity, Arity)
        func_result_handling = _make_result_handling_func(func_arg_unwrap, simple)
        result_arity = Arity.oneof(1) if simple else None
        return values.W_Prim(name, make_remove_extra_info(func_result_handling),
                             arity=_arity, result_arity=result_arity)
    return wrapper

def make_remove_extra_info(func):
    def remove_extra_info(*args):
        args = args[:-1]
        return func(*args)
    remove_extra_info.__name__ += func.__name__
    return remove_extra_info

def expose(n, argstypes=None, simple=True, arity=None, nyi=False, extra_info=False):
    """
    n:          names that the function should be exposed under
    argstypes:  if None, the list of args is passed directly to the function
                otherwise the function arguments are typechecked and passed
                individually
    simple:     if the function does not need access to env and cont and just
                returns a value
    nyi:        the function is not yet implemented and will always raise
    extra_info: the function gets an extra argument extra_call_info, which
                contains information about the caller. The only useful thing to
                do with it is to pass it into a w_value.call_with_extra_info as
                the last argument. This will ensure that the call graph
                information stays correct.
    """
    def wrapper(func):
        from pycket import values
        names = [n] if isinstance(n, str) else n
        name = names[0]
        if extra_info:
            assert not simple
        call1 = call2 = None
        if nyi:
            def func_arg_unwrap(*args):
                raise SchemeException(
                    "primitive %s is not yet implemented" % name)
            _arity = arity or Arity.unknown
        elif argstypes is not None:
            func_arg_unwrap, _arity, call1, call2 = _make_arg_unwrapper(
                    func, argstypes, name, simple=simple)
            if arity is not None:
                _arity = arity
        else:
            func_arg_unwrap = func
            _arity = arity or Arity.unknown
        func_result_handling = _make_result_handling_func(func_arg_unwrap, simple)
        if not extra_info:
            func_result_handling = make_remove_extra_info(func_result_handling)
        result_arity = Arity.ONE if simple else None
        p = values.W_Prim(name, func_result_handling,
                          arity=_arity, result_arity=result_arity,
                          simple1=call1, simple2=call2, is_nyi=nyi)
        for nam in names:
            sym = values.W_Symbol.make(nam)
            if sym in prim_env and prim_env[sym].is_implemented():
                raise SchemeException("name %s already defined" % nam)
            prim_env[sym] = p
        func_arg_unwrap.w_prim = p
        return func_arg_unwrap
    return wrapper

def make_call_method(argstypes=None, arity=None, simple=True, name="<method>"):
    def wrapper(func):
        if argstypes is not None:
            func_arg_unwrap, _, _, _ = _make_arg_unwrapper(
                func, argstypes, name, has_self=True)
        else:
            func_arg_unwrap = func
        return _make_result_handling_func(func_arg_unwrap, simple)
    return wrapper

# Facility to convert a label into a callable object
def make_callable_label(argstypes=None, arity=None, name="<label>"):
    from pycket import values
    class LabelFunction(values.W_Procedure):
        _immutable_fields_ = ["label"]
        def __init__(self, label):
            self.label = label

        @make_call_method(argstypes=argstypes, arity=arity, simple=False)
        def call(self, *rest):
            return self.label(*rest)

        def tostring(self):
            return "#<procedure>"

    def wrapper(label):
        return LabelFunction(label)

    return wrapper


def expose_val(name, w_v):
    from pycket import values
    sym = values.W_Symbol.make(name)
    if sym in prim_env and prim_env[sym].is_implemented():
        raise Error("name %s already defined" % name)
    prim_env[sym] = w_v

def define_nyi(name, bail=True, prim_args=None, *args, **kwargs):
    from pycket import values
    if values.W_Symbol.make(name) not in prim_env:
        if bail:
            @expose(name, prim_args, nyi=True, *args, **kwargs)
            def nyi(a):
                pass
        else:
            @expose(name, prim_args, nyi=False, *args, **kwargs)
            def nyi(a):
                from pycket import values
                print "NOT YET IMPLEMENTED: %s" % name
                return values.w_false
