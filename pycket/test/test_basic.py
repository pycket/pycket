from pycket.expand import expand
from pycket.interpreter import *

def test_constant():
    prog = "1"
    val = interpret(to_ast(expand(prog)))
    assert isinstance(val, W_Fixnum)
    assert val.value == 1


def test_plus():
    prog = "(+ 2 3)"
    val = interpret(to_ast(expand(prog)))
    assert isinstance(val, W_Fixnum)
    assert val.value == 5
