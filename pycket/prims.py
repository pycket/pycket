import operator
import os
from pycket import values
from pycket import arithmetic # imported for side effect
from rpython.rlib  import jit

prim_env = {}

def expose(name, simple=True):
    def wrapper(func):
        if simple:
            cls = values.W_SimplePrim
        else:
            cls = values.W_Prim
        prim_env[values.W_Symbol.make(name)] = cls(name, func)
        return func
    return wrapper

def val(name, v):
    prim_env[values.W_Symbol.make(name)] = v

def make_cmp(name, func, con):
    @expose(name, simple=True)
    def do(args):
        a,b = args
        if isinstance(a, values.W_Fixnum) and isinstance(b, values.W_Fixnum):
            return con(func(a.value, b.value))
        if isinstance(a, values.W_Bignum) and isinstance(b, values.W_Bignum):
            assert 0
        if isinstance(a, values.W_Flonum) and isinstance(b, values.W_Flonum):
            return con(func(a.value, b.value))
        assert 0

for args in [
        ("=", operator.eq,  values.W_Bool.make),
        ("<", operator.lt,  values.W_Bool.make),
        (">", operator.gt,  values.W_Bool.make),
        ("<=", operator.le,  values.W_Bool.make),
        (">=", operator.ge,  values.W_Bool.make),
        ]:
    make_cmp(*args)


def make_pred(name, cls):
    @expose(name, simple=True)
    def do(args):
        a, = args
        return values.W_Bool.make(isinstance(a, cls))

def make_pred_eq(name, val):
    @expose(name, simple=True)
    def do(args):
        a, = args
        return values.W_Bool.make(a is val)


for args in [
        ("pair?", values.W_Cons),
        ("number?", values.W_Number),
        ("vector?", values.W_Vector),
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

def make_arith(name, zero, methname):
    @expose(name, simple=True)
    @jit.unroll_safe
    def do(args):
        if not args and zero:
            return zero
        else:
            init = args[0]
            for i in args[1:]:
                init = getattr(init, methname)(i)
            return init

for args in [
        ("+", values.W_Fixnum(0), "arith_add"),
        ("-", None, "arith_sub"),
        ("/", None, "arith_div"),
        ("*", values.W_Fixnum(1), "arith_mul"),
        ]:
    make_arith(*args)

def make_unary_arith(name, methname):
    @expose(name, simple=True)
    def do(args):
        a, = args
        return getattr(a, methname)()

for args in [
        ("sin", "arith_sin"),
        ("cos", "arith_cos"),
        ("atan", "arith_atan"),
        ("sqrt", "arith_sqrt"),
        ]:
    make_unary_arith(*args)
    
val ("null", values.w_null)
val ("true", values.w_true)
val ("false", values.w_false)

def equal_loop (a,b):
    if a is b:
        return True
    if isinstance (a, values.W_Fixnum) and isinstance (b, values.W_Fixnum):
        return a.value == b.value
    if a is values.w_void:
        return False
    if a is values.w_null:
        return False
    if isinstance(a, values.W_Symbol): 
        return False
    if isinstance(a, values.W_Cons) and isinstance (b, values.W_Cons):
        return equal_loop(a.car, b.car) and equal_loop(a.cdr, b.cdr)
    if isinstance(a, values.W_Vector) and isinstance (b, values.W_Vector):
        if len(a.elems) != len(b.elems): return False
        for i, v in enumerate(a.elems):
            if not equal_loop(v, b.elems[i]):
                return False
        return True
    return False

@expose("call/cc", simple=False)
def callcc(args, env, frame):
    a, = args
    return a.call([values.W_Continuation(frame)], env, frame)

@expose("equal?")
def equalp(args):
    # this doesn't work for cycles
    a,b = args
    return values.W_Bool.make(equal_loop (a,b))

@expose("eq?")
def eqp(args):
    # this doesn't work for cycles
    a,b = args
    if a is b:
        return values.w_true
    if isinstance(a, values.W_Fixnum) and isinstance(b, values.W_Fixnum):
        return values.W_Bool.make(a.value == b.value)
    else:
        return values.w_false
    


@expose("list")
def do_list(args):
    return values.to_list(args)

@expose("list*")
def do_liststar(args):
    a = len(args)-1
    if a < 0:
        raise Exception("list* expects at least one argument")
    return values.to_improper(args[:a], args[a])

@expose("cons")
def do_cons(args):
    a,b = args
    return values.W_Cons(a,b)

@expose("car")
def do_car(args):
    a, = args
    assert isinstance (a,values.W_Cons)
    return a.car

@expose("cdr")
def do_cdr(args):
    a, = args
    assert isinstance (a,values.W_Cons)
    return a.cdr

@expose("set-car!")
def do_car(args):
    a,b = args
    assert isinstance (a,values.W_Cons)
    a.car = b
    return values.w_void

@expose("set-cdr!")
def do_cdr(args):
    a,b = args
    assert isinstance (a,values.W_Cons)
    a.cdr = b
    return values.w_void

@expose("void")
def do_void(args): return values.w_void

@expose("number->string")
def num2str(args):
    a, = args
    if not isinstance(a, values.W_Number):
        raise Exception("number->string: expected a number")
    return values.W_String(a.tostring())

@expose("vector-ref")
def vector_ref(args):
    v, i = args
    if not isinstance(v, values.W_Vector):
        raise Exception("vector-ref: expected a vector")
    if not isinstance(i, values.W_Fixnum):
        raise Exception("vector-ref: expected a fixnum")
    idx = i.value
    if not (0 <= idx < len(v.elems)):
        raise Exception("vector-ref: index out of bounds")
    return v.ref(i.value)

@expose("vector-set!")
def vector_set(args):
    v, i, new = args
    if not isinstance(v, values.W_Vector):
        raise Exception("vector-set!: expected a vector")
    if not isinstance(i, values.W_Fixnum):
        raise Exception("vector-set!: expected a fixnum")
    idx = i.value
    if not (0 <= idx < len(v.elems)):
        raise Exception("vector-set!: index out of bounds")
    return v.set(i.value, new)

@expose("vector")
def vector(args):
    return values.W_Vector(args)

@expose("make-vector")
def make_vector(args):
    if len(args) == 2:
        n, val = args
    elif len(args) == 1:
        n, = args
        val = values.W_Fixnum(0)
    else:
        assert 0
    if not isinstance(n, values.W_Fixnum):
        raise Exception("make-vector: expected a fixnum")
    if not (n.value >= 0):
        raise Exception("make-vector: expected a positive fixnum")
    return values.W_Vector([val] * n.value)

@expose("vector-length")
def vector_length(args):
    v, = args
    if not isinstance(v, values.W_Vector):
        raise Exception("vector-length: expected a vector")
    return values.W_Fixnum(len(v.elems))

# my kingdom for a tail call
def listp_loop(v):
    while True:
        if v is values.w_null: return True
        if isinstance(v, values.W_Cons):
            v = v.cdr
            continue
        return False

@expose("list?")
def consp(args):
    v, = args
    return values.W_Bool.make(listp_loop(v))


@expose("display")
def display(args):
    s, = args
    os.write(1, s.tostring())
    return values.w_void

@expose("write")
def write(args):
    s, = args
    os.write(1, s.tostring())
    return values.w_void
