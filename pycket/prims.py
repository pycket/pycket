import operator
from pycket import values

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

def make_arith(name, func, con):
    @expose(name, simple=True)
    def do(args):
        a,b = args
        assert isinstance (a, values.W_Fixnum)
        assert isinstance (b, values.W_Fixnum)
        return con(func(a.value, b.value))


for args in [
        ("+", operator.add, values.W_Fixnum),
        ("-", operator.sub, values.W_Fixnum),
        ("*", operator.mul, values.W_Fixnum),
        ("=", operator.eq,  values.W_Bool.make),
        ("<", operator.lt,  values.W_Bool.make),
        (">", operator.gt,  values.W_Bool.make),
        ]:
    make_arith(*args)

val ("null", values.w_null)
val ("true", values.w_true)
val ("false", values.w_false)

def equal_loop (a,b):
    if a is b: 
        return values.w_true
    if isinstance (a, values.W_Fixnum) and isinstance (b, values.W_Fixnum):
        values.W_Bool.make(a.value == b.value)
    if a is values.w_void:
        return values.w_false
    if a is values.w_null:
        return values.w_false
    if isinstance(a, values.W_Cons) and isinstance (b, values.W_Cons):
        return values.W_Bool.make(equal_loop(a.car, b.car) and
                                  equal_loop(a.cdr, b.cdr))

@expose("call/cc", simple=False)
def callcc(args, env, frame):
    a, = args
    return a.call([values.W_Continuation(frame)], env, frame)

@expose("equal?")
def equalp(args):
    # this doesn't work for cycles
    a,b = args
    equal_loop (a,b)
    


@expose("list")
def do_list(args):
    return values.to_list(args)

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
