import pytest
import os
from pycket.expand import expand, to_ast
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *

def execute(p, stdlib=False):
    e = expand(p, stdlib=stdlib)
    ast = to_ast(e)
    val = interpret_one(ast)
    return val

def run_fix(p, v, stdlib=False):
    val = execute(p, stdlib=stdlib)
    ov = check_one_val(val)
    assert isinstance(ov, W_Fixnum)
    assert ov.value == v
    return ov.value

def run_flo(p, v, stdlib=False):
    val = execute(p, stdlib=stdlib)
    ov = check_one_val(val)
    assert isinstance(ov, W_Flonum)
    assert ov.value == v
    return ov.value

def run(p, v=None, stdlib=False):
    val = execute(p, stdlib=stdlib)
    ov = check_one_val(val)
    if v is not None:
        assert ov.equal(v)
    return ov

def run_top(p, v=None, stdlib=False):
    e = expand(p, wrap=True, stdlib=stdlib)
    ast = to_ast(e)
    val = interpret([ast])
    ov = check_one_val(val)
    if v:
        assert ov.equal(v)
    return ov


def test_constant():
    prog = "1"
    val = interpret_one(to_ast(expand(prog)))
    ov = check_one_val(val)
    assert isinstance(ov, W_Fixnum)
    assert ov.value == 1


def test_read_err ():
    with pytest.raises(Exception):
        expand ("(")

def test_arithmetic():
    run_fix("(+ )", 0)
    run_fix("(+ 1)", 1)
    run_fix("(+ 2 3)", 5)
    run_fix("(+ 2 3 4)", 9)

    with pytest.raises(SchemeException):
        run_fix("(- )", 0)
    run_fix("(- 1)", -1)
    run_fix("(- 2 3)", -1)
    run_fix("(- 2 3 4)", -5)

    run_fix("(* )", 1)
    run_fix("(* 2)", 2)
    run_fix("(* 2 3)", 6)
    run_fix("(* 2 3 4)", 24)

    with pytest.raises(SchemeException):
        run_flo("(/ )", 0)
    run_flo("(/ 2.0)", 0.5)
    run_flo("(/ 2. 3.)", 2. / 3.)
    run_flo("(/ 2. 3. 4.)", 2. / 3. / 4.)

def test_thunk():
    prog = "((lambda () 1))"
    run_fix(prog, 1)

def test_thunk2():
    prog = "((lambda () 1 2))"
    run_fix(prog, 2)


def test_call():
    prog = "((lambda (x) (+ x 1)) 2)"
    run_fix(prog, 3)

def test_curry():
    prog = "(((lambda (y) (lambda (x) (+ x y))) 2) 3)"
    run_fix(prog, 5)

def test_arith():
    run_fix("(+ 1 2)", 3)
    run_fix("(* 1 2)", 2)
    run_fix("(- 1 2)", -1)
    run_fix("(* -1 2)", -2)

def test_arith_minus_one_arg_bug():
    run_fix("(- 1)", -1)

def test_letrec():
    run_fix("(letrec ([x 1]) x)", 1)
    run_fix("(letrec ([x 1] [y 2]) y)", 2)
    run_fix("(letrec ([x 1] [y 2]) (+ x y))", 3)
    run_fix("(let ([x 0]) (letrec ([x 1] [y x]) (+ x y)))", 2)
    run_fix("(letrec ([x (lambda (z) x)]) 2)", 2)

def test_reclambda():
    run_fix("((letrec ([c (lambda (n) (if (< n 0) 1 (c (- n 1))))]) c) 10)", 1)
    run_fix("""
        ((letrec ([c (lambda (n) (let ([ind (lambda (n) (display n) (if (< n 0) 1 (c (- n 1))))]) (ind n)))]) c) 10)""", 1)
    run_fix("""
(let ()
  (define (nested n)
    (let countdown ([i n]) (if (< i 0) 1 (countdown (- i 1))))
    (if (< n 0) 1 (nested (- n 1))))
  (nested 10))""", 1)

def test_let():
    run_fix("(let () 1)", 1)
    run_fix("(let ([x 1]) x)", 1)
    run_fix("(let ([x 1] [y 2]) y)", 2)
    run_fix("(let ([x 1] [y 2]) (+ x y))", 3)
    run_fix("(let ([x 0]) (let ([x 1] [y x]) (+ x y)))", 1)

def test_let_values():
    run_fix("(let-values ([(a b c) (values 1 2 3)]) (+ a b c))", 6)
    run_fix("(let-values ([(a b c) (values 1 2 3)] [(d) 1] [(e f g h) (values 1 2 1 1)]) (+ a b c d e f g h))", 12)
    run_fix("(let-values ([(a b c) (values 1 2 3)]) (set! a (+ a 5)) (+ a b c))", 11)
    run_fix("(let-values ([() (values )]) 1)", 1)

def test_letrec_values():
    run_fix("(letrec-values ([(a b c) (values 1 2 3)]) (+ a b c))", 6)
    run_fix("(letrec-values ([(a b c) (values 1 2 3)] [(d) 1] [(e f g h) (values 1 2 1 1)]) (+ a b c d e f g h))", 12)

def test_fac():
    run_fix("(letrec ([fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))]) (fac 5))", 120)

def test_fib():
    run_fix("(letrec ([fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))]) (fib 2))", 2)
    run_fix("(letrec ([fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))]) (fib 3))", 3)

def test_void():
    run ("(void)", w_void)
    run ("(void 1)", w_void)
    run ("(void 2 3 #true)", w_void)

def test_mcons():
    run_fix ("(mcar (mcons 1 2))", 1)
    run_fix ("(mcdr (mcons 1 2))", 2)
    with pytest.raises(SchemeException):
        run("(mcar 1)", None)
    with pytest.raises(SchemeException):
        run("(mcar 1 2)", None)

def test_cons():
    run_fix ("(car (cons 1 2))", 1)
    run_fix ("(cdr (cons 1 2))", 2)
    with pytest.raises(SchemeException):
        run("(car 1)", None)
    with pytest.raises(SchemeException):
        run("(car 1 2)", None)

def test_set_mcar_car():
    run_fix ("(letrec ([x (mcons 1 2)]) (set-mcar! x 3) (mcar x))", 3)
    run_fix ("(letrec ([x (mcons 1 2)]) (set-mcdr! x 3) (mcdr x))", 3)
    with pytest.raises(SchemeException):
        run_fix ("(letrec ([x (cons 1 2)]) (set-car! x 3) (car x))", 3)
    with pytest.raises(SchemeException):
        run_fix ("(letrec ([x (cons 1 2)]) (set-cdr! x 3) (cdr x))", 3)

def test_set_bang():
    run("((lambda (x) (set! x #t) x) 1)", w_true)
    run("(letrec([x 0]) ((lambda (x) (set! x #t) x) 1))", w_true)

def test_bools():
    run ("#t", w_true)
    run ("#true", w_true)
    run ("#T", w_true)
    run ("#f", w_false)
    run ("#false", w_false)
    run ("#F", w_false)
    run ("true", w_true)
    run ("false", w_false)

def test_lists():
    run ("null", w_null)
    run ("(list)", w_null)
    run ("(list #t)", to_list([w_true]))

def test_fib():
    Y = """
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (g)
       (f (lambda (z) ((g g) z))))))
"""
    fac = """
    (lambda (f)
      (lambda (x)
        (if (< x 2)
            1
            (* x (f (- x 1))))))
 """

    fib = """
    (lambda (f)
      (lambda (x)
        (if (< x 2)
            x
            (+ (f (- x 1)) (f (- x 2))))))
"""
    run_fix("((%s %s) 2)"%(Y,fib), 1)
    run_fix("((%s %s) 2)"%(Y,fac), 2)

def test_vararg():
    run_fix ("((lambda x (car x)) 1)", 1)
    run_fix ("((lambda (a . x) a) 1)", 1)
    run ("((lambda (a . x) x) 1)", w_null)

def test_callcc():
    run_fix ("(call/cc (lambda (k) 1))", 1)
    run_fix ("(+ 1 (call/cc (lambda (k) 1)))", 2)
    run_fix ("(+ 1 (call/cc (lambda (k) (k 1))))", 2)
    run_fix ("(+ 1 (call/cc (lambda (k) (+ 5 (k 1)))))", 2)


def test_values():
    run_fix("(values 1)", 1)
    run_fix("(let () (values 1 2) (values 3))", 3)
    v = execute("(values #t #f)")
    prog = """
(let () (call/cc (lambda (k) (k 1 2))) 3)
"""
    run_fix(prog, 3)
    assert [w_true, w_false] == v._get_full_list()
    run_fix("(call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b)))", 3)
    run_fix("(call-with-values (lambda () (values 1 2)) +)", 3)
    run_fix("(call-with-values (lambda () (values)) (lambda () 0))", 0)
    run_fix("(call-with-values (lambda () (values 1)) (lambda (x) x))", 1)
    run_fix("(call-with-values (lambda () (values 1)) values)", 1)
    run_fix("(call-with-values (lambda () 1) values)", 1)
    run_fix("""
(call-with-values (lambda () (time-apply (lambda () (+ 1 2)) '()))
                  (lambda (result t r gc) (and (fixnum? t) (fixnum? r) (fixnum? gc)
                                               (car result))))
""", 3)

def test_define():
    run_top("(define x 1) x", W_Fixnum(1))

def test_time():
    run_fix("(time 1)", 1)

def test_apply():
    run_fix("(apply + (list 1 2 3))", 6)
    run_fix("(apply + 1 2 (list 3))", 6)
    run_fix("(apply + 1 2 3 (list))", 6)

def test_setbang_recursive_lambda():
    run_fix("((letrec ([f (lambda (a) (set! f (lambda (a) 1)) (f a))]) f) 6)", 1)

def test_keyword():
    run("'#:foo", W_Keyword.make("foo"))
