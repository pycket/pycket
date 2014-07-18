from rpython.rlib  import jit, unroll
from pycket.error import SchemeException
from pycket import values

class unsafe(object):
    """ can be used in the argtypes part of an @expose call. The corresponding
    argument will be assumed to have the precise corresponding type (no
    subtypes!)."""

    def __init__(self, typ):
        self.typ = typ

class default(object):
    """ can be used in the argtypes part of an @expose call. If the argument is
    missing, the default value is passed to the function. """

    def __init__(self, typ, default=None):
        self.typ = typ
        self.default = default

def _make_arg_unwrapper(func, argstypes, funcname):
    argtype_tuples = []
    min_arg = 0
    isdefault = False
    for i, typ in enumerate(argstypes):
        isunsafe = False
        default_value = None
        if isinstance(typ, default):
            isdefault = True
            default_value = typ.default
            typ = typ.typ
        else:
            assert not isdefault, "non-default argument %s after default argument" % typ
            min_arg += 1
        if isinstance(typ, unsafe):
            typ = typ.typ
            isunsafe = True
        argtype_tuples.append((i, typ, isunsafe, isdefault, default_value))
    unroll_argtypes = unroll.unrolling_iterable(argtype_tuples)
    max_arity = len(argstypes)
    if min_arg == max_arity:
        aritystring = max_arity
    else:
        aritystring = "%s to %s" % (min_arg, max_arity)
    errormsg_arity = "expected %s arguments to %s, got %%s" % (aritystring, funcname)
    for _, typ, _, _, _ in argtype_tuples:
        assert typ.__dict__.get("errorname"), str(typ)
    _arity = range(min_arg, max_arity+1), -1
    def func_arg_unwrap(args, *rest):
        if not min_arg <= len(args) <= max_arity:
            raise SchemeException(errormsg_arity % len(args))
        typed_args = ()
        lenargs = len(args)
        for i, typ, unsafe, default, default_value in unroll_argtypes:
            if i >= min_arg and i >= lenargs:
                assert default
                typed_args += (default_value, )
                continue
            arg = args[i]

            if not unsafe:
                if typ is not values.W_Object and not isinstance(arg, typ):
                    raise SchemeException("expected %s as argument to %s, got %s" % (typ.errorname, funcname, args[i].tostring()))
            else:
                assert arg is not None
                assert type(arg) is typ
                jit.record_known_class(arg, typ)
            typed_args += (arg, )
        typed_args += rest
        return func(*typed_args)
    func_arg_unwrap.func_name = "%s_arg_unwrap" % (func.func_name, )
    return func_arg_unwrap, _arity

def _make_result_handling_func(func_arg_unwrap, simple):
    if simple:
        def func_result_handling(*args):
            from pycket.interpreter import return_multi_vals, return_value
            env = args[-2]
            cont = args[-1]
            args = args[:-2]
            result = func_arg_unwrap(*args)
            if result is None:
                result = values.w_void
            if isinstance(result, values.Values):
                return return_multi_vals(result, env, cont)
            else:
                return return_value(result, env, cont)
        return func_result_handling
    else:
        return func_arg_unwrap

def expose(name, argstypes=None, simple=True, arity=None, nyi=False):
    from pycket.prims import prim_env
    def wrapper(func):
        if nyi:
            def func_arg_unwrap(*args):
                raise SchemeException("primitive %s is not yet implemented"%name)
            _arity = arity or ([], 0)
        elif argstypes is not None:
            func_arg_unwrap, _arity = _make_arg_unwrapper(func, argstypes, name)
            if arity is not None:
                _arity = arity
        else:
            func_arg_unwrap = func
            _arity = arity or ([], 0)
        func_result_handling = _make_result_handling_func(func_arg_unwrap, simple)
        cls = values.W_Prim
        prim_env[values.W_Symbol.make(name)] = cls(name, func_result_handling, _arity)
        return func_arg_unwrap
    return wrapper

def expose_val(name, w_v):
    from pycket.prims import prim_env
    prim_env[values.W_Symbol.make(name)] = w_v
