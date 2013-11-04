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

def test_thunk():
    prog = "((lambda () 1))"
    val = interpret(to_ast(expand(prog)))
    assert isinstance(val, W_Fixnum)
    assert val.value == 1

def test_thunk2():
    prog = "((lambda () 1 2))"
    val = interpret(to_ast(expand(prog)))
    assert isinstance(val, W_Fixnum)
    assert val.value == 2


def test_call():
    prog = "((lambda (x) (+ x 1)) 2)"
    val = interpret(to_ast(expand(prog)))
    assert isinstance(val, W_Fixnum)
    assert val.value == 3
