from pycket import values

def do_plus(args):
    a,b = args
    assert isinstance (a, values.W_Fixnum)
    assert isinstance (b, values.W_Fixnum)
    return values.W_Fixnum (a.value + b.value)

prim_env = {}

prim_env[values.W_Symbol.make("+")] = values.W_Prim ("+", do_plus)
