from pycket.expand import expand
from pycket.interpreter import *

def test_toplevel():
    prog = "1"
    core = expand(prog)
    val = interpret(core)
    assert isinstance(val, W_Fixnum)
    assert val.value == 1
