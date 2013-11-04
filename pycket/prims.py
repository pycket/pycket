import operator
from pycket import values

prim_env = {}

def expose(name):
    def wrapper(func):
        prim_env[values.W_Symbol.make(name)] = values.W_Prim(name, func)
        return func
    return wrapper

def make_arith(name, func, con):
    @expose(name)
    def do(args):
        a,b = args
        assert isinstance (a, values.W_Fixnum)
        assert isinstance (b, values.W_Fixnum)
        return con (func(a.value, b.value))


for args in [
        ("+", operator.add, values.W_Fixnum),
        ("-", operator.sub, values.W_Fixnum),
        ("*", operator.mul, values.W_Fixnum),
        ("=", operator.eq,  values.W_Bool.make),
        ("<", operator.lt,  values.W_Bool.make),
        (">", operator.gt,  values.W_Bool.make),
        ]:
    make_arith(*args)




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
