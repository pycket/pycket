#! /usr/bin/env python
# -*- coding: utf-8 -*-
import operator
import os
import time
import math
from pycket import values
from pycket.cont import Cont
from pycket import vector as values_vector
from pycket import arithmetic # imported for side effect
from pycket.error import SchemeException
from rpython.rlib  import jit, unroll

prim_env = {}

class unsafe(object):
    """ can be used in the argtypes part of an @expose call. The corresponding
    argument will be assumed to have the precise corresponding type (no
    subtypes!)."""

    def __init__(self, typ):
        self.typ = typ

def expose(name, argstypes=None, simple=True):
    def wrapper(func):
        if argstypes is not None:
            argtype_tuples = []
            for i, typ in enumerate(argstypes):
                if isinstance(typ, unsafe):
                    argtype_tuples.append((i, typ.typ, True))
                else:
                    argtype_tuples.append((i, typ, False))
            unroll_argtypes = unroll.unrolling_iterable(argtype_tuples)
            arity = len(argstypes)
            errormsg_arity = "expected %s arguments to %s, got %%s" % (arity, name)
            for _, typ, _ in argtype_tuples:
                assert typ.__dict__.get("errorname"), str(typ)
            def wrap_func(args, *rest):
                if len(args) != arity:
                    raise SchemeException(errormsg_arity % len(args))
                typed_args = ()
                for i, typ, unsafe in unroll_argtypes:
                    arg = args[i]
                    if not unsafe:
                        if typ is not values.W_Object and not isinstance(arg, typ):
                            raise SchemeException("expected %s as argument to %s, got %s" % (typ.errorname, name, args[i].tostring()))
                    else:
                        assert arg is not None
                        assert type(arg) is typ
                        jit.record_known_class(arg, typ)
                    typed_args += (arg, )
                typed_args += rest
                result = func(*typed_args)
                if result is None:
                    return values.w_void
                return result
        else:
            def wrap_func(*args):
                result = func(*args)
                if result is None:
                    return values.w_void
                return result
        wrap_func.func_name = "wrap_%s" % (func.func_name, )
        if simple:
            cls = values.W_SimplePrim
        else:
            cls = values.W_Prim
        prim_env[values.W_Symbol.make(name)] = cls(name, wrap_func)
        return wrap_func
    return wrapper


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


def val(name, v):
    prim_env[values.W_Symbol.make(name)] = v

def make_cmp(name, op, con):
    from values import W_Number, W_Fixnum, W_Flonum, W_Bignum
    from rpython.rlib.rbigint import rbigint
    @expose(name, [W_Number, W_Number], simple=True)
    def do(w_a, w_b):
        if isinstance(w_a, W_Fixnum) and isinstance(w_b, W_Fixnum):
            return con(getattr(operator, op)(w_a.value, w_b.value))
        if isinstance(w_a, W_Bignum) and isinstance(w_b, W_Bignum):
            return con(getattr(w_a.value, op)(w_b.value))
        if isinstance(w_a, W_Flonum) and isinstance(w_b, W_Flonum):
            return con(getattr(operator, op)(w_a.value, w_b.value))

        # Upcast float
        if isinstance(w_a, W_Fixnum) and isinstance(w_b, W_Flonum):
            a = float(w_a.value)
            return con(getattr(operator, op)(a, w_b.value))
        if isinstance(w_a, W_Flonum) and isinstance(w_b, W_Fixnum):
            b = float(w_b.value)
            return con(getattr(operator, op)(w_a.value, b))

        # Upcast bignum
        if isinstance(w_a, W_Bignum) and isinstance(w_b, W_Fixnum):
            b = rbigint.fromint(w_b.value)
            return con(getattr(w_a.value, op)(b))
        if isinstance(w_a, W_Fixnum) and isinstance(w_b, W_Bignum):
            a = rbigint.fromint(w_a.value)
            return con(getattr(a, op)(w_b.value))

        # Upcast bignum/float
        if isinstance(w_a, W_Bignum) and isinstance(w_b, W_Flonum):
            b = rbigint.fromfloat(w_b.value)
            return con(getattr(w_a.value, op)(b))
        if isinstance(w_a, W_Flonum) and isinstance(w_b, W_Bignum):
            a = rbigint.fromfloat(w_a.value)
            return con(getattr(a, op)(w_b.value))

        raise SchemeException("unsupported operation %s on %s %s" % (
            name, w_a.tostring(), w_b.tostring()))

for args in [
        ("=", "eq", values.W_Bool.make),
        ("<", "lt", values.W_Bool.make),
        (">", "gt", values.W_Bool.make),
        ("<=", "le", values.W_Bool.make),
        (">=", "ge", values.W_Bool.make),
        ]:
    make_cmp(*args)


def make_pred(name, cls):
    @expose(name, [values.W_Object], simple=True)
    def do(a):
        return values.W_Bool.make(isinstance(a, cls))

def make_pred_eq(name, val):
    typ = type(val)
    @expose(name, [values.W_Object], simple=True)
    def do(a):
        return values.W_Bool.make(isinstance(a, typ) and a is val)


for args in [
        ("pair?", values.W_Cons),
        ("mpair?", values.W_MCons),
        ("number?", values.W_Number),
        ("fixnum?", values.W_Fixnum),
        ("flonum?", values.W_Flonum),
        ("vector?", values.W_MVector),
        ("string?", values.W_String),
        ("symbol?", values.W_Symbol),
        ("boolean?", values.W_Bool),
        ("procedure?", values.W_Procedure),
        ]:
    make_pred(*args)

for args in [
        ("void?", values.w_void),
        ("false?", values.w_false),
        ("null?", values.w_null),
        ]:
    make_pred_eq(*args)

@expose("integer?", [values.W_Object])
def integerp(n):
    return values.W_Bool.make(isinstance(n, values.W_Fixnum) or
                              isinstance(n, values.W_Bignum) or
                              isinstance(n, values.W_Flonum) and
                              math.floor(n.value) == n.value)

@expose("exact-integer?", [values.W_Object])
def exact_integerp(n):
    return values.W_Bool.make(isinstance(n, values.W_Fixnum) or
                              isinstance(n, values.W_Bignum))

@expose("real?", [values.W_Object])
def realp(n):
    return values.W_Bool.make(isinstance(n, values.W_Fixnum) or
                              isinstance(n, values.W_Bignum) or
                              isinstance(n, values.W_Flonum))

@expose("rational?", [values.W_Object])
def rationalp(n):
    if isinstance(n, values.W_Fixnum) or isinstance(n, values.W_Bignum):
        return values.w_true
    if isinstance(n, values.W_Flonum):
        v = n.value
        return values.W_Bool.make(not(math.isnan(v) or math.isinf(v)))

@expose("exact?", [values.W_Object])
def exactp(n):
    return values.W_Bool.make(isinstance(n, values.W_Fixnum) or
                              isinstance(n, values.W_Bignum))

@expose("inexact?", [values.W_Object])
def inexactp(n):
    return values.W_Bool.make(isinstance(n, values.W_Flonum))

@expose("quotient", [values.W_Integer, values.W_Integer], simple=True)
def quotient(a, b):
    return a.arith_quotient(b)

def make_binary_arith(name, methname):
    @expose(name, [values.W_Number, values.W_Number], simple=True)
    def do(a, b):
        return getattr(a, methname)(b)

for args in [
        ("modulo",   "arith_mod"),
        ("expt",     "arith_pow"),
        ]:
    make_binary_arith(*args)


def make_arith(name, neutral_element, methname, supports_zero_args):
    @expose(name, simple=True)
    @jit.unroll_safe
    def do(args):
        if not args:
            if not supports_zero_args:
                raise SchemeException("expected at least 1 argument to %s" % name)
            return neutral_element
        if len(args) == 1:
            return getattr(neutral_element, methname)(args[0])
        else:
            init = args[0]
            for i in range(1, jit.promote(len(args))):
                init = getattr(init, methname)(args[i])
            return init

for args in [
        ("+", values.W_Fixnum(0), "arith_add", True),
        ("-", values.W_Fixnum(0), "arith_sub", False),
        ("*", values.W_Fixnum(1), "arith_mul", True),
        ("/", values.W_Fixnum(1), "arith_div", False),
        ("bitwise-and", values.W_Fixnum(-1), "arith_and", True),
        ("bitwise-ior", values.W_Fixnum(0), "arith_or", True),
        ("bitwise-xor", values.W_Fixnum(0), "arith_xor", True),
        ]:
    make_arith(*args)

def make_unary_arith(name, methname):
    @expose(name, [values.W_Number], simple=True)
    def do(a):
        return getattr(a, methname)()

for args in [
        ("sin", "arith_sin"),
        ("cos", "arith_cos"),
        ("atan", "arith_atan"),
        ("sqrt", "arith_sqrt"),
        ("sub1", "arith_sub1"),
        ("exact->inexact", "arith_exact_inexact"),
        ]:
    make_unary_arith(*args)


val("null", values.w_null)
val("true", values.w_true)
val("false", values.w_false)

# FIXME: this implementation sucks
@expose("string-append")
def string_append(args):
    if not args:
        return values.W_String("")
    l = []
    for a in args:
        if not isinstance(a, values.W_String):
            raise SchemeException("string-append: expected a string")
        l.append(a.value)
    return values.W_String(''.join(l))

@expose("string-length", [values.W_String])
def string_length(s1):
    return values.W_Fixnum(len(s1.value))

@expose("substring")
def substring(args):
    """
    (substring str start [end]) → string?
        str : string?
        start : exact-nonnegative-integer?
        end : exact-nonnegative-integer? = (string-length str)
    """
    if not (len(args) == 2 or len(args) == 3):
        raise SchemeException("substring: expects 2 or 3 arguments")
    w_string = args[0]
    if not isinstance(w_string, values.W_String):
        raise SchemeException("substring: expects a string, got something else")
    string = w_string.value
    w_start = args[1]
    if not isinstance(w_start, values.W_Fixnum):
        raise SchemeException("substring: start index must be fixnum")
    start = w_start.value
    if start > len(string) or start < 0:
            raise SchemeException("substring: end index out of bounds")
    if len(args) == 3:
        w_end = args[2]
        if not isinstance(w_end, values.W_Fixnum):
            raise SchemeException("substring: end index must be fixnum")
        end = w_end.value
        if end > len(string) or end < 0:
            raise SchemeException("substring: end index out of bounds")
    else:
        end = len(string)
    if end < start:
        raise SchemeException(
            "substring: ending index is smaller than starting index")
    return values.W_String(string[start:end])


@expose("values", simple=False)
def do_values(vals, env, cont):
    from pycket.interpreter import return_multi_vals
    return return_multi_vals(values.Values.make(vals), env, cont)

@continuation
def call_consumer(consumer, env, cont, vals):
    return consumer.call(vals._get_full_list(), env, cont)

@expose("call-with-values", [values.W_Procedure] * 2, simple=False)
def call_with_values (producer, consumer, env, cont):
    # FIXME: check arity
    return producer.call([], env, call_consumer(consumer, env, cont))

@continuation
def time_apply_cont(initial, env, cont, vals):
    from pycket.interpreter import return_multi_vals
    final = time.clock()
    ms = values.W_Fixnum(int((final - initial) * 1000))
    vals_l = vals._get_full_list()
    results = values.Values.make([values.to_list(vals_l), ms, ms, values.W_Fixnum(0)])
    return return_multi_vals(results, env, cont)

@expose("call/cc", [values.W_Procedure], simple=False)
def callcc(a, env, cont):
    return a.call([values.W_Continuation(cont)], env, cont)

@expose("time-apply", [values.W_Procedure, values.W_List], simple=False)
def time_apply(a, args, env, cont):
    initial = time.clock()
    return a.call(values.from_list(args), env, time_apply_cont(initial, env, cont))


@expose("apply", simple=False)
def apply(args, env, cont):
    if not args:
        raise SchemeException("apply expected at least one argument, got 0")
    fn = args[0]
    if not isinstance(fn, values.W_Procedure):
        raise SchemeException("apply expected a procedure, got something else")
    lst = args[-1]
    if not listp_loop(lst):
        raise SchemeException("apply expected a list as the last argument, got something else")
    args_len = len(args)-1
    assert args_len >= 0
    others = args[1:args_len]
    new_args = others + values.from_list(lst)
    return fn.call(new_args, env, cont)


@expose("printf")
def printf(args):
    if not args:
        raise SchemeException("printf expected at least one argument, got 0")
    fmt = args[0]
    if not isinstance(fmt, values.W_String):
        raise SchemeException("printf expected a format string, got something else")
    fmt = fmt.value
    vals = args[1:]
    i = 0
    j = 0
    while i < len(fmt):
        if fmt[i] == '~':
            if i+1 == len(fmt):
                raise SchemeException("bad format string")
            s = fmt[i+1]
            if s == 'a' or s == 'v' or s == 's':
                # print a value
                # FIXME: different format chars
                if j >= len(vals):
                    raise SchemeException("not enough arguments for format string")
                os.write(1,vals[j].tostring()),
                i += 2
                j += 1
            elif s == 'n':
                os.write(1,"\n") # newline
            else:
                raise SchemeException("unexpected format character")
        else:
            os.write(1,fmt[i])
            i += 1

@expose("equal?", [values.W_Object] * 2)
def equalp(a, b):
    # this doesn't work for cycles
    return values.W_Bool.make(a.equal(b))

@expose("eqv?", [values.W_Object] * 2)
def eqvp(a, b):
    # this doesn't work for cycles
    return values.W_Bool.make(a.eqv(b))

@expose("eq?", [values.W_Object] * 2)
def eqp(a, b):
    # this doesn't work for cycles
    if a is b:
        return values.w_true
    elif isinstance(a, values.W_Fixnum) and isinstance(b, values.W_Fixnum):
        return values.W_Bool.make(a.value == b.value)
    elif isinstance(a, values.W_Character) and isinstance(b, values.W_Character):
        return values.W_Bool.make(a.value == b.value)
    else:
        return values.w_false

@expose("length", [values.W_List])
def length(a):
    n = 0
    while True:
        if isinstance(a, values.W_Null):
            return values.W_Fixnum(n)
        if isinstance(a, values.W_Cons):
            a = a.cdr()
            n = n+1
        else:
            raise SchemeException("length: not a list")

@expose("list")
def do_list(args):
    return values.to_list(args)

@expose("list*")
def do_liststar(args):
    a = len(args)-1
    if a < 0:
        raise SchemeException("list* expects at least one argument")
    return values.to_improper(args[:a], args[a])

@expose("assq", [values.W_Object] * 2)
def assq(a, b):
    if values.w_null is b:
        return values.w_false
    else:
        if eqp([a, do_car([do_car([b])])]):
            return do_car([b])
        else:
            return assq([a, do_cdr([b])])


@expose("cons", [values.W_Object, values.W_Object])
def do_cons(a, b):
    return values.W_Cons.make(a,b)

@expose("car", [values.W_Cons])
def do_car(a):
    return a.car()

@expose("cadr")
def do_cadr(args):
    return do_car([do_cdr(args)])

@expose("cddr")
def do_cddr(args):
    return do_cdr([do_cdr(args)])

@expose("caddr")
def do_caddr(args):
    return do_car([do_cdr([do_cdr(args)])])

@expose("cadddr")
def do_cadddr(args):
    return do_car([do_cdr([do_cdr([do_cdr(args)])])])

@expose("cdr", [values.W_Cons])
def do_cdr(a):
    return a.cdr()


@expose("mlist")
def do_mlist(args):
    return values.to_mlist(args)

@expose("mcons", [values.W_Object, values.W_Object])
def do_mcons(a, b):
    return values.W_MCons(a,b)

@expose("mcar", [values.W_MCons])
def do_mcar(a):
    return a.car()

@expose("mcdr", [values.W_MCons])
def do_mcdr(a):
    return a.cdr()

@expose("set-mcar!", [values.W_MCons, values.W_Object])
def do_set_mcar(a, b):
    a.set_car(b)

@expose("set-mcdr!", [values.W_MCons, values.W_Object])
def do_set_mcdr(a, b):
    a.set_cdr(b)

@expose("void")
def do_void(args): return values.w_void

@expose("number->string", [values.W_Number])
def num2str(a):
    return values.W_String(a.tostring())

@expose("vector-ref", [values.W_MVector, values.W_Fixnum], simple=False)
def vector_ref(v, i, env, cont):
    idx = i.value
    if not (0 <= idx < v.length()):
        raise SchemeException("vector-ref: index out of bounds")
    return do_vec_ref(v, i, env, cont)

@continuation
def imp_vec_ref_cont(f, i, v, env, cont, vals):
    from pycket.interpreter import check_one_val
    return f.call([v, i, check_one_val(vals)], env, cont)

def do_vec_ref(v, i, env, cont):
    from pycket.interpreter import return_value
    if isinstance(v, values_vector.W_Vector):
        return return_value(v.ref(i.value), env, cont)
    elif isinstance(v, values.W_ImpVector):
        uv = v.vec
        f = v.refh
        return do_vec_ref(uv, i, env, imp_vec_ref_cont(f, i, uv, env, cont))
    else:
        assert False


@expose("vector-set!", [values.W_MVector, values.W_Fixnum, values.W_Object], simple=False)
def vector_set(v, i, new, env, cont):
    idx = i.value
    if not (0 <= idx < v.length()):
        raise SchemeException("vector-set!: index out of bounds")
    return do_vec_set(v, i, new, env, cont)

@continuation
def imp_vec_set_cont(v, i, env, cont, vals):
    from pycket.interpreter import check_one_val
    return do_vec_set(v, i, check_one_val(vals), env, cont)

def do_vec_set(v, i, new, env, cont):
    from pycket.interpreter import return_value
    if isinstance(v, values_vector.W_Vector):
        v.set(i.value, new)
        return return_value(values.w_void, env, cont)
    elif isinstance(v, values.W_ImpVector):
        uv = v.vec
        f = v.seth
        return f.call([uv, i, new], env, imp_vec_set_cont(uv, i, env, cont))
    else:
        assert False

@expose("impersonate-vector", [values.W_MVector, values.W_Procedure, values.W_Procedure])
def impersonate_vector(v, refh, seth):
    return values.W_ImpVector(v, refh, seth)

@expose("vector")
def vector(args):
    return values_vector.W_Vector.fromelements(args)

@expose("make-vector")
def make_vector(args):
    if len(args) == 2:
        n, val = args
    elif len(args) == 1:
        n, = args
        val = values.W_Fixnum(0)
    else:
        raise SchemeException("make-vector: unexpected number of parameters")
    if not isinstance(n, values.W_Fixnum):
        raise SchemeException("make-vector: expected a fixnum")
    if not (n.value >= 0):
        raise SchemeException("make-vector: expected a positive fixnum")
    return values_vector.W_Vector.fromelement(val, n.value)

@expose("vector-length", [values_vector.W_MVector])
def vector_length(v):
    return values.W_Fixnum(v.length())

# my kingdom for a tail call
def listp_loop(v):
    while True:
        if v is values.w_null: return True
        if isinstance(v, values.W_Cons):
            v = v.cdr()
            continue
        return False

@expose("list?", [values.W_Object])
def consp(v):
    return values.W_Bool.make(listp_loop(v))


@expose("display", [values.W_Object])
def display(s):
    os.write(1, s.tostring())
    return values.w_void

@expose("write", [values.W_Object])
def write(s):
    os.write(1, s.tostring())
    return values.w_void

@expose("current-inexact-milliseconds", [])
def curr_millis():
    return values.W_Flonum(time.clock()*1000)

@expose("error", [values.W_Symbol, values.W_String])
def error(name, msg):
    raise SchemeException("%s: %s"%(name.tostring(), msg.tostring()))

@expose("list->vector", [values.W_List])
def list2vector(l):
    return values_vector.W_Vector.fromelements(values.from_list(l))

@expose("vector->list", [values_vector.W_Vector])
def vector2list(v):
    es = []
    for i in range(v.len):
        es.append(v.ref(i))
    return values.to_list(es)

# FIXME: make that a parameter
@expose("current-command-line-arguments", [], simple=False)
def current_command_line_arguments(env, cont):
    from pycket.interpreter import return_value
    w_v = values_vector.W_Vector.fromelements(
            env.toplevel_env.commandline_arguments)
    return return_value(w_v, env, cont)

# ____________________________________________________________

## Unsafe Fixnum ops
@expose("unsafe-fx+", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxplus(a, b):
    return values.W_Fixnum(a.value + b.value)

@expose("unsafe-fx-", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxminus(a, b):
    return values.W_Fixnum(a.value - b.value)

@expose("unsafe-fx*", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxtimes(a, b):
    return values.W_Fixnum(a.value * b.value)

@expose("unsafe-fx<", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxlt(a, b):
    return values.W_Bool.make(a.value < b.value)

@expose("unsafe-fx>", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxgt(a, b):
    return values.W_Bool.make(a.value > b.value)

@expose("unsafe-fx=", [unsafe(values.W_Fixnum)] * 2)
def unsafe_fxeq(a, b):
    return values.W_Bool.make(a.value == b.value)

@expose("unsafe-fx->fl", [unsafe(values.W_Fixnum)])
def unsafe_fxfl(a):
    return values.W_Flonum(float(a.value))

## Unsafe Flonum ops
@expose("unsafe-fl+", [unsafe(values.W_Flonum)] * 2)
def unsafe_flplus(a, b):
    return values.W_Flonum(a.value + b.value)

@expose("unsafe-fl-", [unsafe(values.W_Flonum)] * 2)
def unsafe_flminus(a, b):
    return values.W_Flonum(a.value - b.value)

@expose("unsafe-fl*", [unsafe(values.W_Flonum)] * 2)
def unsafe_fltimes(a, b):
    return values.W_Flonum(a.value * b.value)

@expose("unsafe-fl<", [unsafe(values.W_Flonum)] * 2)
def unsafe_fllt(a, b):
    return values.W_Bool.make(a.value < b.value)

@expose("unsafe-fl>", [unsafe(values.W_Flonum)] * 2)
def unsafe_flgt(a, b):
    return values.W_Bool.make(a.value > b.value)

@expose("unsafe-fl=", [unsafe(values.W_Flonum)] * 2)
def unsafe_fleq(a, b):
    return values.W_Bool.make(a.value == b.value)

## Unsafe vector ops

# FIXME: Chaperones
@expose("unsafe-vector-ref", [values.W_Object, unsafe(values.W_Fixnum)], simple=False)
def unsafe_vector_ref(v, i, env, cont):
    from pycket.interpreter import return_value
    if isinstance(v, values.W_ImpVector):
        return do_vec_ref(v, i, env, cont)
    else:
        assert type(v) is values_vector.W_Vector
        return return_value(v.ref(i.value), env, cont)

@expose("unsafe-vector*-ref", [unsafe(values_vector.W_Vector), unsafe(values.W_Fixnum)])
def unsafe_vector_star_ref(v, i):
    return v.ref(i.value)

# FIXME: Chaperones
@expose("unsafe-vector-set!", [values.W_Object, unsafe(values.W_Fixnum), values.W_Object], simple=False)
def unsafe_vector_set(v, i, new, env, cont):
    from pycket.interpreter import return_value
    if isinstance(v, values.W_ImpVector):
        return do_vec_set(v, i, new, env, cont)
    else:
        assert type(v) is values_vector.W_Vector
        return return_value(v.set(i.value, new), env, cont)

@expose("unsafe-vector*-set!",
        [unsafe(values_vector.W_Vector), unsafe(values.W_Fixnum), values.W_Object])
def unsafe_vector_star_set(v, i, new):
    return v.set(i.value, new)

@expose("unsafe-vector-length", [values.W_MVector])
def unsafe_vector_length(v):
    return values.W_Fixnum(v.length())

@expose("unsafe-vector*-length", [unsafe(values_vector.W_Vector)])
def unsafe_vector_star_length(v):
    return values.W_Fixnum(v.length())


@expose("symbol->string", [values.W_Symbol])
def symbol_to_string(v):
    return values.W_String(v.value)

@expose("string->symbol", [values.W_String])
def string_to_symbol(v):
    return values.W_Symbol(v.value)

@expose("integer->char", [values.W_Fixnum])
def integer_to_char(v):
    return values.W_Character(unichr(v.value))


# Loading

# FIXME: Proper semantics.
@expose("load", [values.W_String], simple=False)
def load(lib, env, cont):
    from pycket.expand import ensure_json_ast_load, load_json_ast_rpython
    lib_name = lib.tostring()
    json_ast = ensure_json_ast_load(lib_name)
    if json_ast is None:
        raise SchemeException(
            "can't gernerate load-file for %s "%(lib.tostring()))
    ast = load_json_ast_rpython(json_ast)
    return ast, env, cont
