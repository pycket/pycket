#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest
from pycket.expand import expand
from pycket.interpreter import *
from pycket.values import *
from pycket import values_string
from pycket.prims import *

from pycket.test.testhelper import (run, run_fix, run_flo, run_top, execute,
        run_values, check_equal, run_mod)


def test_constant():
    ov = run("1", stdlib=False)
    assert isinstance(ov, W_Fixnum)
    assert ov.value == 1


def test_read_err ():
    with pytest.raises(Exception):
        expand_string("(", result=False)

def test_arithmetic():
    run_fix("(+ )", 0)
    run_fix("(+ 1)", 1)
    run_fix("(+ 2 3)", 5)
    run_fix("(+ 2 3 4)", 9)

    with pytest.raises(Exception):
        run_fix("(- )", 0)
    run_fix("(- 1)", -1)
    run_fix("(- 2 3)", -1)
    run_fix("(- 2 3 4)", -5)

    run_fix("(* )", 1)
    run_fix("(* 2)", 2)
    run_fix("(* 2 3)", 6)
    run_fix("(* 2 3 4)", 24)

    with pytest.raises(Exception):
        run_flo("(/ )", 0)
    run_flo("(/ 2.0)", 0.5)
    run_flo("(/ 2. 3.)", 2. / 3.)
    run_flo("(/ 2. 3. 4.)", 2. / 3. / 4.)

def test_thunk():
    prog = "((lambda () 1))"
    run_fix(prog, 1)

@pytest.mark.skipif(pytest.config.new_pycket, reason="Racket core lambda has only one expression in the body")
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
    run_fix("(letrec-values ([(x) 1]) x)", 1)
    run_fix("(letrec-values ([(x) 1] [(y) 2]) y)", 2)
    run_fix("(letrec-values ([(x) 1] [(y) 2]) (+ x y))", 3)
    run_fix("(letrec-values ([(x) (lambda (z) x)]) 2)", 2)

def test_reclambda():
    run_fix("((letrec-values ([(c) (lambda (n) (if (< n 0) 1 (c (- n 1))))]) c) 10)", 1)
    run_fix("""
        ((letrec-values ([(c) (lambda (n) (let-values ([(ind) (lambda (n) (begin (display n) (if (< n 0) 1 (c (- n 1)))))]) (ind n)))]) c) 10)""", 1)
    run_fix("""
  (letrec-values ([(nested) (lambda (n)
    (begin
      (letrec-values ([(countdown) (lambda (i) (if (< i 0) 1 (countdown (- i 1))))]) (countdown n))
      (if (< n 0) 1 (nested (- n 1)))))])
    (nested 10))""", 1)

def test_let():
    run_fix("(let-values () 1)", 1)
    run_fix("(let-values ([(x) 1]) x)", 1)
    run_fix("(let-values ([(x) 1] [(y) 2]) y)", 2)
    run_fix("(let-values ([(x) 1] [(y) 2]) (+ x y))", 3)

def test_run_pruning_let():
    run_fix("(let-values ([(c) 7]) (let-values ([(b) (+ c 1)]) (let-values ([(a) (+ b 1)] [(d) (- c 5)]) (+ a d))))", 11)

def test_let_values():
    run_fix("(let-values ([(a b c) (values 1 2 3)]) (+ a b c))", 6)
    run_fix("(let-values ([(a b c) (values 1 2 3)] [(d) 1] [(e f g h) (values 1 2 1 1)]) (+ a b c d e f g h))", 12)
    run_fix("(let-values ([(a b c) (values 1 2 3)]) (set! a (+ a 5)) (+ a b c))", 11)
    run_fix("(let-values ([() (values )]) 1)", 1)

def test_letrec_values():
    run_fix("(letrec-values ([(a b c) (values 1 2 3)]) (+ a b c))", 6)
    run_fix("(letrec-values ([(a b c) (values 1 2 3)] [(d) 1] [(e f g h) (values 1 2 1 1)]) (+ a b c d e f g h))", 12)

def test_fac():
    run_fix("(letrec-values ([(fac) (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))]) (fac 5))", 120)

def test_fib():
    run_fix("(letrec-values ([(fib) (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))]) (fib 2))", 2)
    run_fix("(letrec-values ([(fib) (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))]) (fib 3))", 3)

def test_void():
    run ("(void)", w_void)
    run ("(void 1)", w_void)
    run ("(void 2 3 #true)", w_void)

def test_mcons():
    run_fix ("(mcar (mcons 1 2))", 1)
    run_fix ("(mcdr (mcons 1 2))", 2)
    if pytest.config.load_expander:
        run_fix ("(begin (#%require (quote #%unsafe)) (unsafe-mcar (mcons 1 2)))", 1)
        run_fix ("(begin (#%require (quote #%unsafe)) (unsafe-mcdr (mcons 1 2)))", 2)
    else:
        run_fix ("(unsafe-mcar (mcons 1 2))", 1)
        run_fix ("(unsafe-mcdr (mcons 1 2))", 2)
    with pytest.raises(Exception):
        run("(mcar 1)", None, expect_to_fail=True)
    with pytest.raises(Exception):
        run("(mcar 1 2)", None, expect_to_fail=True)

def test_mcons_equal(doctest):
    """
    > (equal? (mcons 1 2) (mcons 1 2))
    #t
    > (equal? (mcons 1 2) (mcons 1 4))
    #f
    > (equal? (mcons 1 2) (cons 1 2))
    #f
    """

def test_cons():
    run_fix ("(car (cons 1 2))", 1)
    run_fix ("(cdr (cons 1 2))", 2)
    with pytest.raises(Exception):
        run("(car 1)", None, expect_to_fail=True)
    with pytest.raises(Exception):
        run("(car 1 2)", None, expect_to_fail=True)

def test_set_mcar_car():
    run_fix ("(letrec-values ([(x) (mcons 1 2)]) (set-mcar! x 3) (mcar x))", 3)
    run_fix ("(letrec-values ([(x) (mcons 1 2)]) (set-mcdr! x 3) (mcdr x))", 3)
    # These raise static errors now
    # with pytest.raises(SchemeException):
    #     run_fix ("(letrec ([x (cons 1 2)]) (set-car! x 3) (car x))", 3)
    # with pytest.raises(SchemeException):
    #     run_fix ("(letrec ([x (cons 1 2)]) (set-cdr! x 3) (cdr x))", 3)

def test_cell():
    cell = W_Cell(W_Fixnum(9))
    val1 = cell.w_value
    assert cell.get_val().value == 9
    cell.set_val(W_Fixnum(10))
    assert val1 is cell.w_value
    assert cell.get_val().value == 10
    cell.set_val(W_Fixnum(12))
    assert val1 is cell.w_value
    assert cell.get_val().value == 12

    cell = W_Cell(None)
    cell.set_val(W_Fixnum(10))
    val1 = cell.w_value
    assert cell.get_val().value == 10
    cell.set_val(W_Fixnum(12))
    assert val1 is cell.w_value
    assert cell.get_val().value == 12

def test_set_bang():
    run("((lambda (x) (begin (set! x #t) x)) 1)", w_true)
    run("(letrec-values ([(x) 0]) ((lambda (x) (begin (set! x #t) x)) 1))", w_true)

def test_bools():
    run ("#t", w_true)
    run ("#true", w_true)
    run ("#T", w_true)
    run ("#f", w_false)
    run ("#false", w_false)
    run ("#F", w_false)
    run ("(not #t)", w_false)
    run ("(not #f)", w_true)
    run ("(not 5)", w_false)

@pytest.mark.skipif(pytest.config.load_expander, reason="they're in racket/bool")
def test_racket_bool():
    run("true", w_true, extra="(require racket/bool)")
    run("false", w_false, extra="(require racket/bool)")

def test_lists():
    run ("null", w_null)
    run ("(list)", w_null)
    run ("(list #t)", to_list([w_true]))
    run ("(list-tail (list #f #f #t #t) 2)", to_list([w_true, w_true]))

def test_box():
    run("(unbox (box #t))", w_true)
    run("(unbox (box-immutable #f))", w_false)
    run("(unsafe-unbox (box #t))", w_true)
    run("(let-values ([(b) (box 5)]) (begin (set-box! b #f) (unbox b)))", w_false)
    # "and" is not a primitive
    #run("(let-values ([(b) (box 5)]) (let-values ([(r) (box-cas! b 5 6)]) (and r (eqv? (unbox b) 6))))", w_true)

def test_fib_ycombinator():
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

def test_callwithcurrentcontinuation():
    run_fix ("(call-with-current-continuation (lambda (k) 1))", 1)
    run_fix ("(+ 1 (call-with-current-continuation (lambda (k) 1)))", 2)
    run_fix ("(+ 1 (call-with-current-continuation (lambda (k) (k 1))))", 2)
    run_fix ("(+ 1 (call-with-current-continuation (lambda (k) (+ 5 (k 1)))))", 2)


def test_values():
    run_fix("(values 1)", 1)
    run_fix("(let-values () (values 1 2) (values 3))", 3)
    prog = "(let-values () (call-with-current-continuation (lambda (k) (k 1 2))) 3)"
    run_fix(prog, 3)
    v = run_values("(values #t #f)")
    assert [w_true, w_false] == v
    run_fix("(call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b)))", 3)
    run_fix("(call-with-values (lambda () (values 1 2)) +)", 3)
    run_fix("(call-with-values (lambda () (values)) (lambda () 0))", 0)
    run_fix("(call-with-values (lambda () (values 1)) (lambda (x) x))", 1)
    run_fix("(call-with-values (lambda () (values 1)) values)", 1)
    run_fix("(call-with-values (lambda () 1) values)", 1)

@pytest.mark.skipif(pytest.config.new_pycket, reason="-and- is not in kernel")
def test_call_with_values():
    run_fix("""
    (call-with-values (lambda () (time-apply (lambda () (+ 1 2)) (quote ())))
                  (lambda (result t r gc) (and (fixnum? t) (fixnum? r) (fixnum? gc)
                                               (car result))))
""", 3)

def test_define():
    run_top("(define x 1) x", W_Fixnum(1))

@pytest.mark.skipif(pytest.config.new_pycket, reason="time is not in kernel")
def test_time():
    run_fix("(time 1)", 1)

def test_apply():
    run_fix("(apply + (list 1 2 3))", 6)
    run_fix("(apply + 1 2 (list 3))", 6)
    run_fix("(apply + 1 2 3 (list))", 6)

def test_setbang_recursive_lambda():
    run_fix("((letrec-values ([(f) (lambda (a) (begin (set! f (lambda (a) 1)) (f a)))]) f) 6)", 1)

def test_keyword():
    run("(quote #:foo)", W_Keyword.make("foo"))


#
# From http://people.csail.mit.edu/jaffer/r5rs_8.html
# And the racket docs
#
def test_eq():
    run("(eq? (quote yes) (quote yes))", w_true)
    run("(eq? (quote yes) (quote no))", w_false)
    run("(let-values ([(v) (mcons 1 2)]) (eq? v v))", w_true)
    run("(eq? (mcons 1 2) (mcons 1 2))", w_false)
    #run_top("(eq? (make-string 3 #\z) (make-string 3 #\z))", w_false, stdlib=True)

    run("(eq? (quote a) (quote a))", w_true)
    run("(eq? (quote (a)) (quote (a)))", w_false) #racket
    run("(eq? (list (quote a)) (list (quote a)))", w_false)
    # run('(eq? "a" "a")', w_true) #racket
    # run('(eq? "" "")', w_true) #racket
    run("(eq? (quote ()) (quote ()))", w_true)
    run("(eq? 2 2)",  w_true) #racket
    run("(eq? #\A #\A)", w_true) #racket
    run("(eq? car car)", w_true)
    run("(let-values (((n) (+ 2 3))) (eq? n n))", w_true) #racket
    run("(let-values (((x) (quote (a)))) (eq? x x))", w_true)
    #run("(let-values (((x) (quote #()))) (eq? x x))", w_true)
    run("(let-values (((p) (lambda (x) x))) (eq? p p))", w_true)

def test_equal():
    run("(equal? (quote a) (quote a))", w_true)
    run("(equal? (quote a) (quote a))", w_true)
    run("(equal? (quote (a (b) c)) (quote (a (b) c)))", w_true)
    run('(equal? "abc" "abc")', w_true)
    run("(equal? 2 2)", w_true)
    run("(equal? (make-vector 5 (quote a)) (make-vector 5 (quote a)))", w_true)
    run("(equal? (lambda (x) x) (lambda (y) y))", w_false) #racket

def test_eqv():
    check_equal(
        "(eqv? 'yes 'yes)", "#t",
        "(eqv? 'yes 'no)", "#f",
        "(eqv? (expt 2 100) (expt 2 100))", "#t",
        "(eqv? 2 2.0)", "#f",
        "(eqv? (integer->char 955) (integer->char 955))", "#t",
    #run_top("(eqv? (make-string 3 #\z) (make-string 3 #\z))", "#f", stdlib=True)
        "(eqv? +nan.0 +nan.0)", "#t",

        "(eqv? 'a 'a)", "#t",
        "(eqv? 'a 'b)", "#f",
        "(eqv? 2 2)", "#t",
        "(eqv? '() '())", "#t",
        "(eqv? 100000000 100000000)", "#t",
        "(eqv? 1.0+2i 1.0+2i)", "#t",
        "(eqv? (cons 1 2) (cons 1 2))", "#f",
        """(eqv? (lambda () 1)
                 (lambda () 2))""", "#f",
        "(eqv? #f 'nil)", "#f",
        """(let ((p (lambda (x) x)))
           (eqv? p p))""", "#t",
    # run('(eqv? "" "")', "#t") #racket
        "(eqv? '#() '#())", "#f", #racket
        """(eqv? (lambda (x) x)
                 (lambda (x) x))""", "#f", #racket
        """(eqv? (lambda (x) x)
                 (lambda (y) y))""", "#f", #racket
    )
    run_top("""(define gen-counter
             (lambda ()
               (let ((n 0))
                 (lambda () (set! n (+ n 1)) n))))
           (let ((g (gen-counter)))
             (eqv? g g))""",
        w_true)
    run_top("""(define gen-counter
             (lambda ()
               (let ((n 0))
                 (lambda () (set! n (+ n 1)) n))))
           (eqv? (gen-counter) (gen-counter))""",
        w_false)
    run_top("""(define gen-loser
             (lambda ()
               (let ((n 0))
                 (lambda () (set! n (+ n 1)) 27))))
           (let ((g (gen-loser)))
             (eqv? g g))""",
        w_true)
    run_top("""(define gen-loser
             (lambda ()
               (let ((n 0))
                 (lambda () (set! n (+ n 1)) 27))))
           (eqv? (gen-loser) (gen-loser))""",
        w_false) #racket

    run("""(letrec-values (((f) (lambda () (if (eqv? f g) 'both 'f)))
                           ((g) (lambda () (if (eqv? f g) 'both 'g))))
             (eqv? f g))""",
        w_false) #racket

    run("""(letrec-values (((f) (lambda () (if (eqv? f g) 'f 'both)))
                           ((g) (lambda () (if (eqv? f g) 'g 'both))))
             (eqv? f g))""",
        w_false)
    run("(eqv? (quote a) (quote a))", w_true) #racket
    # run('(eqv? "a" "a")', w_true) #racket
    run("(eqv? (quote b) (cdr (quote (a b))))", w_false) #racket
    run("""(let-values (((x) (quote a)))
           (eqv? x x))""", w_true)

def test_eqv_doc(doctest):
    """
    > (eqv? 0.0 -0.0)
    #f
    """

@pytest.mark.skipif(pytest.config.new_pycket and not pytest.config.load_expander, reason="normalizer issues")
def test_caselambda():
    run("(case-lambda [(x) 1])")
    run("(case-lambda [(x) x])")
    run("(case-lambda [() 0])")
    run("(case-lambda [() 0] [(x) x])")
    run("(case-lambda [x 0])")
    run("((case-lambda [() #f]))", w_false)
    run("((case-lambda [(x) #f]) 0)", w_false)
    run("((case-lambda [() 0] [(x) x]) #f)", w_false)
    run("((case-lambda [x #t] [(x) x]) #f)", w_true)
    run("((case-lambda [x (car x)] [(x) x]) #f #t 17)", w_false)
    run("((case-lambda [(x) x] [x (car x)]) #f #t 17)", w_false)

def test_begin0():
    run_fix("(begin0 1 2)", 1)
    run_fix("(begin0 1)", 1)
    run_fix("(begin0 1 2 3)", 1)
    v = run_values("(begin0 (values #t #f) 2 3)")
    assert v == [w_true, w_false]
    run_fix("(let-values ([(x) 1]) (begin0 x (set! x 2)))", 1)
    run_fix("(let-values ([(x) 10]) (begin0 (set! x 0) (set! x (+ x 1))) x)", 1)

def test_varref():
    run("(#%variable-reference)")
    run("(#%variable-reference add1)")
    run("(let-values ([(x) 0]) (#%variable-reference x))")
    run("(let-values ([(x) 0]) (variable-reference-constant? (#%variable-reference x)))", w_true)
    run("(let-values ([(x) 0]) (set! x 1) (variable-reference-constant? (#%variable-reference x)))", w_false)

def test_with_continuation_mark():
    m = run_mod(
    """
    #lang pycket
    (define key (make-continuation-mark-key))
    (define result
      (with-continuation-mark key "quiche"
        (with-continuation-mark key "ham"
           (continuation-mark-set-first
            (current-continuation-marks)
            key))))
    """)
    sym = W_Symbol.make("result")
    assert isinstance(m.defs[sym], values_string.W_String)
    assert m.defs[sym].as_str_utf8() == "ham"

def test_with_continuation_mark2(doctest):
    u"""
    > (with-continuation-mark 'hello 'bye (let ([x (call-with-immediate-continuation-mark 'hello (λ (x) x) #f)]) x))
    #f
    """

@pytest.mark.skipif(pytest.config.new_pycket, reason="require")
def test_with_continuation_mark3():
    m = run_mod(
    """
    #lang racket/base
    (define a 1)
    (set! a 2)
    (define result
        (let* ([extract
	        (lambda (k) (continuation-mark-set->list
	           (continuation-marks k)
	           'x))]
           [go
	        (lambda (in?)
              (lambda ()
	        	      (let ([k (with-continuation-mark 'x 10
	        			 (begin0
	        			  (with-continuation-mark 'x 11
	        			    (call/cc
	        			     (lambda (k )
	        			       (with-continuation-mark 'x 12
	        				 (if in?
	        				     (extract k)
	        				     k)))))
	        			  (+ a 3)))])
	        		(if in?
	        		    k
	        		    (extract k)))))])
        ((go #t))))
    (define valid (equal? result '(11 10)))
    """)
    sym = W_Symbol.make("valid")
    assert m.defs[sym] is w_true

def test_with_continuation_mark_impersonator():
    m = run_mod(
    """
    #lang pycket
    (define mark-key
      (impersonate-continuation-mark-key
       (make-continuation-mark-key)
       (lambda (l) (car l))
       (lambda (s) (string->list s))))
    (define result
      (with-continuation-mark mark-key "quiche"
        (continuation-mark-set-first
         (current-continuation-marks)
         mark-key)))
    """)
    sym = W_Symbol.make("result")
    assert isinstance(m.defs[sym], W_Character)
    assert m.defs[sym].value == 'q'

def test_impersonator_application_mark():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)
    (define key (make-continuation-mark-key))
    (define proc
      (lambda ()
        (continuation-mark-set-first
          (current-continuation-marks)
          key)))
    (define wrapped
      (impersonate-procedure proc (lambda () (values))
                             impersonator-prop:application-mark (cons key 42)))
    (define result (wrapped))
    """)
    sym = W_Symbol.make("result")
    assert isinstance(m.defs[sym], W_Fixnum)
    assert m.defs[sym].value == 42

def test_arity():
    run("(procedure-arity-includes? add1 1)", w_true)
    run("(procedure-arity-includes? add1 2)", w_false)
    run("(procedure-arity-includes? make-vector 1)", w_true)
    run("(procedure-arity-includes? make-vector 2)", w_true)
    run("(procedure-arity-includes? (lambda (x) 0) 1)", w_true)
    run("(procedure-arity-includes? (lambda (x) 0) 2)", w_false)
    run("(procedure-arity-includes? (lambda (x y) 0) 2)", w_true)
    run("(procedure-arity-includes? (lambda (x . y) 0) 1)", w_true)
    run("(procedure-arity-includes? (lambda (x . y) 0) 2)", w_true)
    run("(procedure-arity-includes? (lambda (x . y) 0) 200000)", w_true)
    run("(procedure-arity-includes? (lambda x 1) 1)", w_true)
    run("(procedure-arity-includes? (lambda x 1) 0)", w_true)

def test_arity_kw(doctest):
    """
    ! (require racket/private/kw)
    > (procedure-arity-includes? cons 2)
    #t
    > (procedure-arity-includes? display 3)
    #f
    > (procedure-arity-includes? (lambda (x #:y y) x) 1)
    #f
    > (procedure-arity-includes? (lambda (x #:y y) x) 1 #t)
    #t
    """
    assert doctest

def test_tostring_of_list():
    l = to_list([W_Fixnum(0), W_Fixnum(1), W_Fixnum(5)])
    assert l.tostring() == "(0 1 5)"
    l = to_improper([W_Fixnum(0), W_Fixnum(1)], W_Fixnum(5))
    assert l.tostring() == "(0 1 . 5)"

def test_callgraph_reconstruction():
    from pycket.expand    import expand_string, parse_module
    from pycket           import config
    from pycket.callgraph import LOOP_PARTICIPANT, LOOP_HEADER
    from pycket.env       import w_global_config as conf
    str = """
        #lang pycket
        (define (f x) (g (+ x 1)))
        (define (g x) (if (= x 0) (g 5) (h x)))
        (define (h x) x)
        (f 5)
        (f -1)
        """
    conf.reset_callgraph()
    ast = parse_module(expand_string(str))
    env = ToplevelEnv(config.get_testing_config(**{"pycket.callgraph":True}))
    m = interpret_module(ast, env)
    f = m.defs[W_Symbol.make("f")].closure.caselam.lams[0]
    g = m.defs[W_Symbol.make("g")].closure.caselam.lams[0]
    h = m.defs[W_Symbol.make("h")].closure.caselam.lams[0]

    assert conf.callgraph.calls == {f: {g: None}, g: {h: None, g: None}}
    assert g.body[0].should_enter

    str = """
        #lang pycket
        (define (f x) (g (+ x 1)))
        (define (g x) (if (= x 0) (f 5) (h x)))
        (define (h x) x)
        (g 0)
        """

    conf.reset_callgraph()

    ast = parse_module(expand_string(str))
    env = ToplevelEnv(config.get_testing_config(**{"pycket.callgraph":True}))
    m = interpret_module(ast, env)
    f = m.defs[W_Symbol.make("f")].closure.caselam.lams[0]
    g = m.defs[W_Symbol.make("g")].closure.caselam.lams[0]
    h = m.defs[W_Symbol.make("h")].closure.caselam.lams[0]

    assert conf.callgraph.calls == {f: {g: None}, g: {h: None, f: None}}
    assert (conf.callgraph.recursive == {f: LOOP_HEADER, g: LOOP_PARTICIPANT} or
            conf.callgraph.recursive == {f: LOOP_PARTICIPANT, g: LOOP_HEADER})
    assert g.body[0].should_enter or f.body[0].should_enter
    conf.reset_callgraph()

def test_callgraph_reconstruction_through_primitives():
    from pycket.expand import expand_string, parse_module
    from pycket        import config
    from pycket.env    import w_global_config as conf
    str = """
        #lang pycket
        (define (f k) (k (apply h '(5))))
        (define (g x) (+ (call-with-current-continuation f) 7))
        (define (h x) x)
        (g 5)
        """

    ast = parse_module(expand_string(str))
    env = ToplevelEnv(config.get_testing_config(**{"pycket.callgraph":True}))
    m = interpret_module(ast, env)
    f = m.defs[W_Symbol.make("f")].closure.caselam.lams[0]
    g = m.defs[W_Symbol.make("g")].closure.caselam.lams[0]
    h = m.defs[W_Symbol.make("h")].closure.caselam.lams[0]

    assert conf.callgraph.calls == {g: {f: None}, f: {h: None}}
    conf.reset_callgraph()

def test_should_enter_downrecursion():
    from pycket.expand import expand_string, parse_module
    from pycket        import config
    from pycket.env    import w_global_config as conf
    str = """
        #lang pycket

        (define (append a b)
          (if (null? a)
              b
              (cons (car a) (append (cdr a) b))))
        (append (list 1 2 3 5 6 6 7 7 8 3 4 5 3 5 4 3 5 3 5 3 3 5 4 3) (list 4 5 6))

        (define (n->f n)
          (cond
           [(zero? n) (lambda (f) (lambda (x) x))]
           [else
            (define n-1 (n->f (- n 1)))
            (lambda (f)
               (define fn-1 (n-1 f))
               (lambda (x) (f (fn-1 x))))]))
        (n->f 10)
    """
    conf.reset_callgraph()
    ast = parse_module(expand_string(str))
    env = ToplevelEnv(config.get_testing_config(**{"pycket.callgraph":True}))
    m = interpret_module(ast, env)
    append = m.defs[W_Symbol.make("append")].closure.caselam.lams[0]
    f = m.defs[W_Symbol.make("n->f")].closure.caselam.lams[0]

    assert conf.callgraph.calls == {append: {append: None}, f: {f: None}}

    assert append.body[0].should_enter
    # This is long to account for let conversion
    assert append.body[0].els.body[0].should_enter

    assert f.body[0].should_enter
    assert f.body[0].els.body[0].body[0].should_enter
    conf.reset_callgraph()

@pytest.mark.skipif(pytest.config.new_pycket, reason="require")
def test_reader_graph(doctest):
    """
    ! (require racket/shared)
    ! (define x (make-placeholder #f))
    ! (define y (list 1 2 x))
    > (pair? (shared ([x (cons x x)]) x))
    #t
    > (shared ([x (cons x x)]) (eq? (car x) (cdr x)))
    #t
    > (make-reader-graph 1)
    1
    > (make-reader-graph '(1 2))
    '(1 2)
    > (placeholder-set! x y)
    > (length (make-reader-graph y))
    3
    > (list? (caddr (make-reader-graph y)))
    #true
    > (placeholder? (caddr (make-reader-graph y)))
    #false
    """
    assert doctest

def test_wrap_values():
    assert isinstance(wrap(1), W_Fixnum)
    assert isinstance(wrap(1.0), W_Flonum)
    assert isinstance(wrap(W_Fixnum(1)), W_Fixnum)
    assert wrap(w_void) is w_void
    assert wrap(True) is w_true
    assert wrap(False) is w_false

    assert isinstance(wrap(1, 2), W_UnwrappedFixnumCons)
    assert isinstance(wrap(1, w_null), W_UnwrappedFixnumConsProper)
    assert isinstance(wrap(1.0, 2), W_UnwrappedFlonumCons)
    assert isinstance(wrap(1.0, w_null), W_UnwrappedFlonumConsProper)
    assert isinstance(wrap(True, 2), W_WrappedCons)
    assert isinstance(wrap(True, w_null), W_WrappedConsProper)

@pytest.mark.xfail
def test_let_too_many_values(doctest):
    """
    ! (require racket/private/more-scheme)
    > (with-handlers ([void (lambda (x) 1)]) (let ([x (values 1 2)]) x))
    1
    """
    assert doctest


def test_memq(doctest):
    """
    > (memq 'a '(a b c))
    '(a b c)
    > (memq 'b '(a b c))
    '(b c)
    > (memq 'x '(a b c))
    #f
    > (memq 2 (list 1 2 3 4))
    '(2 3 4)
    > (memq 9 (list 1 2 3 4))
    #f
    """
    assert doctest

def test_memv(doctest):
    """
    > (memv 2 (list 1 2 3 4))
    '(2 3 4)
    > (memv 9 (list 1 2 3 4))
    #f
    """