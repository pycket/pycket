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
        ]:
    make_arith(*args)



