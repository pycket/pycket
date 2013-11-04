from pycket.expand import expand
from pycket.interpreter import *

def run_fix(p,v):
    val = interpret(to_ast(expand(p)))
    assert isinstance(val, W_Fixnum)
    assert val.value == v


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

def test_curry():
    prog = "(((lambda (y) (lambda (x) (+ x y))) 2) 3)"
    val = interpret(to_ast(expand(prog)))
    assert isinstance(val, W_Fixnum)
    assert val.value == 5

def test_arith():
    run_fix("(+ 1 2)", 3)
    run_fix("(* 1 2)", 2)
    run_fix("(- 1 2)", -1)
    run_fix("(* -1 2)", -2)

def test_letrec():
    run_fix("(letrec ([x 1]) x)", 1)
    run_fix("(letrec ([x 1] [y 2]) y)", 2)
    run_fix("(letrec ([x 1] [y 2]) (+ x y))", 3)

# def test_fib():
#     Y = """
#   (lambda (f)
#     ((lambda (x) (x x))
#      (lambda (g)
#        (f (lambda (z) ((g g) z)))))))
# """
#     fac = """
#     (lambda (f)
#       (lambda (x)
#         (if (< x 2)
#             1
#             (* x (f (- x 1))))))
#  """

#     fib = """
#     (lambda (f)
#       (lambda (x)
#         (if (< x 2)
#             x
#             (+ (f (- x 1)) (f (- x 2))))))
# """
#     run_fix("((%s %s) 2)"%(Y,fib), 1)
#     run_fix("((%s %s) 2)"%(Y,fac), 2)

