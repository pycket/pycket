from pycket.expand import expand
from pycket.interpreter import *

def test_toplevel():
    prog = "1"
    val = interpret(to_ast(expand(prog)))
    assert isinstance(val, W_Fixnum)
    assert val.value == 1
