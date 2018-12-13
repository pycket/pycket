#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest
from pycket.values import W_Symbol, w_false, w_true
from pycket.error import SchemeException
from pycket.prims.linklet import W_LinkletInstance, w_uninitialized

from pycket.test.testhelper import (make_linklet, inst, get_val, defines, variables, get_var_val, eval_fixnum, eval_bool, empty_target, make_instance, check_val)


def test_instantiate_basic():
    l = make_instance("(linklet () (x) (define-values (x) 4))")
    assert isinstance(l, W_LinkletInstance)
    assert check_val(l, "x", 4)

def test_instantiate_target():
    l = make_linklet("(linklet () (x) (define-values (x) 4) (+ x x))")
    result, t = eval_fixnum(l, empty_target())
    assert result == 8
    assert check_val(t, "x", 4)

    # even if linklet doesn't export, def goes into target if it doesn't already have it
    l2 = make_linklet("(linklet () () (define-values (x) 4) (+ x x))")
    result2, t2 = eval_fixnum(l2, empty_target())
    assert result2 == 8
    assert check_val(t2, "x", 4)

def test_instantiate_target_transfer_set_banged():
    l2 = make_linklet("(linklet () (y) (define-values (y) 10) (set! y 50))", "l2")
    t1 = empty_target("t1")
    t2 = empty_target("t2")
    _, t1 = eval_fixnum(l2, t1, [])
    _, t2 = eval_fixnum(l2, t2, [])
    assert check_val(t2, "y", 50)

def test_instantiate_target_def_overwrite():
    l = make_linklet("(linklet () (x) (define-values (x) 4) (+ x x))")
    t = make_instance("(linklet () () (define-values (x) 1) (define-values (y) 2))")
    result, t = eval_fixnum(l, t)
    assert result == 8
    assert check_val(t, "x", 4)
    assert check_val(t, "y", 2)

def test_instantiate_target_always_overwrite():
    # if target doesn't have it, then it doesn't matter if linklet exports or not,
    # put the variable in the target
    l = make_linklet("(linklet () () (define-values (z) 4) z)")
    result, t = eval_fixnum(l, empty_target())
    assert result == 4
    assert check_val(t, "z", 4)

def test_instantiate_target_def_stays_the_same():
    # if linklet doesn't export, then target's def stay the same
    l = make_linklet("(linklet () () (define-values (x) 4) (+ x x))")
    t = make_instance("(linklet () () (define-values (x) 1) (define-values (y) 2))")
    result, t = eval_fixnum(l, t)
    assert result == 8
    assert check_val(t, "x", 1)
    assert check_val(t, "y", 2)

    # use the local var, don't change target's var if you don't export
    l = make_linklet("(linklet () () (define-values (x) 4) (+ x x))")
    t1 = make_instance("(linklet () () (define-values (x) 10))")
    t2 = make_instance("(linklet () (x) (define-values (x) 10))")
    result1, t1 = eval_fixnum(l, t1)
    result2, t2 = eval_fixnum(l, t2)
    assert result1 == 8 and result2 == 8
    assert check_val(t1, "x", 10)
    assert check_val(t2, "x", 10)

    # imported variables doesn't get into target at all ...
    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) () (+ x x))")
    result, t = eval_fixnum(l2, empty_target(), [l1])
    assert result == 8
    assert not defines(t, "x")

    # ... let alone overwrite any var inside the target
    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) () (+ x x))")
    t = make_instance("(linklet () () (define-values (x) 1))")
    result, t = eval_fixnum(l2, t, [l1])
    assert result == 8
    assert check_val(t, "x", 1)

def test_instantiate_defs_export_names():
    l = make_instance("(linklet () ((x x15)) (define-values (x) 4))")
    assert not defines(l, "x")
    assert defines(l, "x15")

    # LinkletVars will be referred by the external name (e.g. (+ x15 x15)
    l = make_linklet("(linklet () ((x x15)) (define-values (x) 4) (+ x x))")
    result, t = eval_fixnum(l, empty_target())
    assert result == 8
    assert not defines(t, "x")
    assert defines(t, "x15")

def test_instantiate_discarding_defs():
    l = make_instance("(linklet () ((x x15)) (define-values (x) 4) (define-values (x15) 75))")
    assert not defines(l, "x")
    assert check_val(l, "x15", 4) #### Not 75!
    k,v = get_var_val(l, "x15.1") # uninterned
    assert v.value == 75

    l = make_instance("(linklet () ((x x15) k) (define-values (x) 4) (define-values (x15) 75) (define-values (k) x15))")
    assert not defines(l, "x")
    assert check_val(l, "x15", 4) #### Not 75!
    assert check_val(l, "k", 75) #### Not 4!
    k,v = get_var_val(l, "x15.1")
    assert v.value == 75

def test_instantiate_use_targets_def():
    l = make_linklet("(linklet () (x) (+ x x))")
    t = make_instance("(linklet () () (define-values (x) 10))")
    result, _ = eval_fixnum(l, t)
    assert result == 20

    # use linklet's definition if both linklet and target have it
    l = make_linklet("(linklet () () (define-values (x) 4) (+ x x))") # doesn't export x
    t = make_instance("(linklet () () (define-values (x) 10))")
    result, t = eval_fixnum(l, t)
    assert result == 8
    assert check_val(t, "x", 10)

def test_instantiate_basic_import():
    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) () (+ x x))")
    result, _ = eval_fixnum(l2, empty_target(), [l1])
    assert result == 8

    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) (y) (define-values (y) (+ x x)) (+ y y))")
    result, t = eval_fixnum(l2, empty_target(), [l1])
    assert result == 16
    assert check_val(t, "y", 8)
    assert not defines(t, "x")

    # target's defs are overwritten only if the linklet has a definition
    # not with an imported variable
    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) () (+ x x))")
    t = make_instance("(linklet () () (define-values (x) 1000))")
    result, t = eval_fixnum(l2, t, [l1])
    assert result == 8
    assert check_val(t, "x", 1000)

    ## same thing with the import renaming
    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet (((x x2))) () (+ x2 x2))")
    t = make_instance("(linklet () () (define-values (x) 1000) (define-values (x2) 2000))")
    result, t = eval_fixnum(l2, t, [l1])
    assert result == 8
    assert check_val(t, "x", 1000)
    assert check_val(t, "x2", 2000)

    ## slightly trickier
    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet (((x x2))) () (define-values (x) 14) (+ x2 x))")
    t = make_instance("(linklet () () (define-values (x) 1000) (define-values (x2) 2000))")
    result, t = eval_fixnum(l2, t, [l1])
    assert result == 18
    assert check_val(t, "x", 1000)
    assert check_val(t, "x2", 2000)

def test_instantiate_basic_export():
    l1 = make_instance("(linklet () (a) (define-values (a) 4))")
    l2 = make_linklet("(linklet ((a)) () (+ a a))")
    result, _ = eval_fixnum(l2, empty_target(), [l1])
    assert result == 8

    l1 = make_instance("(linklet () ((a1 a)) (define-values (a1) 4))")
    l2 = make_linklet("(linklet ((a)) () (+ a a))")
    result, _ = eval_fixnum(l2, empty_target(), [l1])
    assert result == 8

def test_instantiate_uninitialize_undefined_exports():
    l = make_linklet("(linklet () (x))")
    _, t = eval_fixnum(l, empty_target())
    assert t.is_var_uninitialized(W_Symbol.make("x"))

    # don't touch if target has it
    l = make_linklet("(linklet () (x))")
    t = make_instance("(linklet () () (define-values (x) 10))")
    _, t = eval_fixnum(l, t)
    assert not t.is_var_uninitialized(W_Symbol.make("x"))

    # target exports the same var with another external name
    l = make_linklet("(linklet () (x2) (+ x2 x2))")
    t = make_instance("(linklet () ((x x2)) (define-values (x) 10))")
    result, t = eval_fixnum(l, t)
    assert result == 20
    assert check_val(t, "x2", 10)
    assert not defines(t, "x")

def test_instantiate_export_rename():
    l1 = make_instance("(linklet () ((x1 x)) (define-values (x1) 4))")
    l2 = make_linklet("(linklet ((x)) ((y1 y)) (define-values (y1) x) (+ x y1))")
    assert check_val(l1, "x", 4)
    result, t = eval_fixnum(l2, empty_target(), [l1])
    assert result == 8
    assert check_val(t, "y", 4)
    assert not defines(t, "x")

def test_instantiate_import_rename():
    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_instance("(linklet () (x) (define-values (x) 10))")
    l3 = make_linklet("(linklet (((x x1))((x x2))) () (+ x1 x2))")
    result, _ = eval_fixnum(l3, empty_target(), [l1,l2])
    assert result == 14

def test_instantiate_eval_define_values():
    l = make_linklet("(linklet () ((x x15)) (define-values (x) 4) (+ x x))")
    t = make_instance("(linklet () ((x x16)) (define-values (x) 1000))")
    _, t = eval_fixnum(l, t)
    assert defines(t, "x15") and defines(t, "x16") and not defines(t, "x")
    assert check_val(t, "x15", 4)
    assert check_val(t, "x16", 1000)

def test_instantiate_closures_and_variables():
    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_linklet("(linklet ((x)) (g) (define-values (g) (lambda (y) x)))")
    _, t = eval_fixnum(l2, empty_target(), [l1])
    assert defines(t, "g") and not defines(t, "x")

    # use the modified target
    l3 = make_linklet("(linklet () (g) (g 5))")
    result, t = eval_fixnum(l3, t)
    assert result == 4

    # import the closure
    l1 = make_instance("(linklet () (x) (define-values (x) 4))")
    l2 = make_instance("(linklet ((x)) (g) (define-values (g) (lambda (y) x)))", [l1])
    l4 = make_linklet("(linklet ((g)) () (g 3))")
    result, _ = eval_fixnum(l4, empty_target(), [l2])
    assert result == 4

def test_instantiate_cannot_mutate_imported():
    # mutating an imported variable is a *compilation* error
    with pytest.raises(SchemeException) as e:
        make_linklet("(linklet ((x)) () (set! x 5) (+ x x))")
    assert "cannot mutate imported variable" in str(e.value)

def test_instantiate_set_bang():
    l = make_linklet("(linklet () () (define-values (x) 3) (set! x 5) (+ x x))")
    t = make_instance("(linklet () () (define-values (x) 6))")
    result, t = eval_fixnum(l, t)
    assert defines(t, "x")
    assert check_val(t, "x", 6)
    assert result == 10

    l = make_linklet("(linklet () (x) (set! x 5) (+ x x))")
    t = make_instance("(linklet () () (define-values (x) 3))")
    result, t = eval_fixnum(l, t)
    assert check_val(t, "x", 5)
    assert result == 10

def test_instantiate_closure_capture_and_reset1():
    l1 = make_instance("(linklet () (x) (define-values (x) -1))")
    l2 = make_instance("(linklet ((x)) (g) (define-values (g) (lambda (p) x)))", [l1])
    l3 = make_linklet("(linklet ((g)) (x) (set! x 5) (g 1000))")
    t = make_instance("(linklet () () (define-values (x) 2000))")
    result, t = eval_fixnum(l3, t, [l2])
    assert check_val(t, "x", 5)
    assert result == -1

def test_instantiate_closure_capture_and_reset2():
    l1 = make_instance("(linklet () (x) (define-values (x) -11))")
    l2 = make_instance("(linklet ((x)) (g) (define-values (y) 131) (define-values (g) (lambda (p) (+ x y))) (set! y 71))", [l1])
    l3 = make_linklet("(linklet ((g)) () (g -1))")
    result, t = eval_fixnum(l3, empty_target(), [l2])
    assert result == 60


def test_instantiate_closure_capture_and_reset3():
    l2 = make_linklet("(linklet () (y) (define-values (y) 10) (set! y 50))", "l2")
    t2 = empty_target("t2")
    _, t2 = eval_fixnum(l2, t2, [])
    l4 = make_linklet("(linklet () (y) (define-values (z) (+ y y)) (set! y 200) (define-values (y) 90) z)")
    result, t2 = eval_fixnum(l4, t2)
    assert result == 100
    assert check_val(t2, "y", 90)

def test_instantiate_closure_capture_and_reset4():
    l2 = make_linklet("(linklet () (y g) (define-values (y) 10) (define-values (g) (lambda () y)) (set! y 50))", "l2")
    t2 = empty_target("t2")
    _, t2 = eval_fixnum(l2, t2, [])
    l4 = make_linklet("(linklet () (y g) (set! y 200) (define-values (y) 90) (g))")
    result, t2 = eval_fixnum(l4, t2)
    assert result == 90
    assert check_val(t2, "y", 90)

def test_instantiate_closure_capture_and_reset():
    l2 = make_linklet("(linklet () (y g) (define-values (y) 10) (define-values (g) (lambda () y)) (set! y 50))", "l2")
    t1 = empty_target("t1")
    t2 = empty_target("t2")
    t3 = empty_target("t3")

    _, t1 = eval_fixnum(l2, t1, [])
    _, t2 = eval_fixnum(l2, t2, [])
    _, t3 = eval_fixnum(l2, t3, [])

    assert check_val(t1, "y", 50)
    assert check_val(t2, "y", 50)
    assert check_val(t3, "y", 50)

    l3 = make_linklet("(linklet () (y g) (set! y 300) (g))", "l3")
    result, t1 = eval_fixnum(l3, t1)
    assert result == 300
    # here's an interesting one:
    assert check_val(t1, "y", 300)
    assert check_val(t2, "y", 50)
    assert check_val(t3, "y", 50)

    l4 = make_linklet("(linklet () (y g) (set! y 200) (define-values (y) 90) (g))", "l4")
    result, t2 = eval_fixnum(l4, t2)
    assert result == 90
    assert check_val(t1, "y", 300)
    assert check_val(t2, "y", 90)
    assert check_val(t3, "y", 50)

    l5 = make_linklet("(linklet () (g) (define-values (y) 90) (+ y (g)))", "l5")
    result, t3 = eval_fixnum(l5, t3)
    assert result == 140
    assert check_val(t1, "y", 300)
    assert check_val(t2, "y", 90)
    assert check_val(t3, "y", 50)

def test_instantiate_small_list():
    # boxed immutable hash table (small-list.rkt)
    l1 = make_instance("(linklet () (h) (define-values (h) (box (hasheq))))")
    l2 = make_linklet("(linklet ((h)) () (set-box! h (hash-set (unbox h) \"a\" 5)) (hash-ref (unbox h) \"a\" #f))")
    result, t = eval_fixnum(l2, empty_target(), [l1])
    assert result == 5
    result, _ = eval_fixnum(make_linklet("(linklet ((h)) () (hash-ref (unbox h) \"a\" #f))"), t, [l1])
    assert result == 5

def test_instantiate_hashes():
    l1 = make_instance("(linklet () (h) (define-values (h) (hasheq \"a\" 4 \"b\" 5)))")
    l2 = make_linklet("(linklet ((h)) (h2) (define-values (h2) (hash-copy h)) (hash-ref h2 \"b\"))")
    result, t = eval_fixnum(l2, empty_target(), [l1])
    assert result == 5
    l3 = make_linklet("(linklet ((h2)) () (hash-ref h2 \"a\"))")
    result, _ = eval_fixnum(l3, empty_target(), [t])
    assert result == 4

    # hash-set! to target
    t = make_instance("(linklet () () (define-values (h) (make-hasheq)))")
    l1 = make_linklet("(linklet () (h) (hash-set! h \"k\" 150) (hash-set! h \"y\" 29))")
    _, t = eval_fixnum(l1, t)
    l2 = make_linklet("(linklet () (h) (hash-set! h \"y\" 50) (hash-ref h \"k\"))")
    result, t = eval_fixnum(l2, t)
    assert result == 150
    l3 = make_linklet("(linklet () (h) (+ (hash-ref h \"k\") (hash-ref h \"y\")))")
    result, _ = eval_fixnum(l3, t)
    assert result == 200

    # hash-set! to imported instance
    l1_use_later = make_instance("(linklet () (h) (define-values (h) (make-hasheq)))")
    l2 = make_linklet("(linklet ((h)) () (hash-set! h \"a\" 5) (hash-set! h \"b\" 10) (hash-set! h \"c\" 20) (hash-ref h \"a\" #f))")
    result, t = eval_fixnum(l2, empty_target(), [l1_use_later])
    assert result == 5
    l3 = make_linklet("(linklet ((h)) () (hash-set! h \"c\" 200) (hash-ref h \"b\" #f))")
    result, t = eval_fixnum(l3, t, [l1_use_later])
    assert result == 10
    l4 = make_linklet("(linklet ((h)) () (hash-ref h \"c\" #f))")
    result, t = eval_fixnum(l4, t, [l1_use_later])
    assert result == 200

    # slightly more complicated
    # l1_use_later has a hash-table "h" contains {a:5, b:10, c:200}
    l10 = make_linklet("(linklet ((h)) (g) (define-values (g) (hash-copy h)) (hash-ref g \"c\"))")
    result, t = eval_fixnum(l10, empty_target(), [l1_use_later])
    assert result == 200
    l11 = make_linklet("(linklet () (g) (hash-set! g \"a\" -1) (hash-ref g \"b\"))")
    result, t = eval_fixnum(l11, t)
    assert result == 10
    l12 = make_linklet("(linklet () (g) (hash-ref g \"a\"))")
    result, _ = eval_fixnum(l12, t)
    assert result == -1

def test_instantiate_lets_and_scopes():
    l = make_linklet("(linklet () () (letrec-values (((fact) (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))) (fact 5)))")
    result, _ = eval_fixnum(l, empty_target())
    assert result == 120

    l1 = make_instance("(linklet () (add2) (define-values (add) (lambda (x) (lambda (y) (+ x y)))) (define-values (add2) (add 2)))")
    l2 = make_linklet("(linklet ((add2)) () (add2 6))")
    result, _ = eval_fixnum(l2, empty_target(), [l1])
    assert result == 8

    l = make_linklet("(linklet () () ((((lambda (x) (lambda (x) (lambda (y) (+ x y)))) 1) 2) 3))")
    result, _ = eval_fixnum(l, empty_target())
    assert result == 5

    l = make_linklet("(linklet () () (let-values (((x) (let-values (((x) 2)) (+ x x)))) (+ x x)))")
    result, _ = eval_fixnum(l, empty_target())
    assert result == 8

def test_instantiate_letrec_rhs_cells():
    l1 = make_linklet("(linklet () () (define-values (k) (lambda (stx_32) (letrec-values (((e) 1)((x) (+ stx_32 e))) x))) (k 5))")
    result1, _ = eval_fixnum(l1, empty_target())
    assert result1 == 6

    l2 = make_linklet("(linklet () () (define-values (k) (lambda (stx_32) (letrec-values (((e) 1)((x) (+ stx_32 e))((p) ((lambda (x) x) x))) p))) (k 5))")
    result2, _ = eval_fixnum(l2, empty_target())
    assert result2 == 6

    l3 = make_linklet("(linklet () () (define-values (k) (lambda (stx_32) (letrec-values (((e) 1)((x) (+ stx_32 e))((p) ((lambda (x) (letrec-values (((u) e)) u)) x))) p))) (k 5))")
    result3, _ = eval_fixnum(l3, empty_target())
    assert result3 == 1

    l4 = make_linklet("(linklet () () (let-values () (letrec-values (((f) (lambda (p) (g (- p 1)))) ((g) (lambda (k) (if (zero? k) 1 (f k))))) (g 100))))")
    result4, _ = eval_fixnum(l4, empty_target())
    assert result4 == 1

@pytest.mark.skip(reason="need to fix the context normalizer")
def test_compilation_context_normalize_term():
    # Context.normalize_term might be faulty
    l = make_linklet("(linklet () () (let-values (((x) 5)) (+ x (let-values (((x) 10)) x))))")
    result, _ = eval_fixnum(l, empty_target())
    assert result == 15

    l = make_linklet("(linklet () () (let-values (((x) 5)) (+ x (let-values (((x) 10)) (+ x (let-values (((x) 20) ((y) 21)) (+ x y)))))))")
    result, _ = eval_fixnum(l, empty_target())
    assert result == 56


def test_continuation_marks_across_linklets():
    l1 = make_instance("""(linklet () (f)
                            (define-values (f)
                              (lambda (c)
                                (if
                                 (eq? (continuation-mark-set-first #f parameterization-key) c)
                                 1 -1))))""")
    l2 = make_linklet("""(linklet ((f)) ()
                         (f (continuation-mark-set-first #f parameterization-key)))""")
    result, _ = eval_fixnum(l2, empty_target(), [l1])
    assert result == 1

    l1 = make_instance("""(linklet
     () (f)
     (define-values (f)
       (lambda (c)
         (if (eq? c (continuation-mark-set-first #f parameterization-key))
             1
             -1))))""")
    l2 = make_linklet("""(linklet
     ((f)) ()
     (define-values (caner) (make-parameter #f))
     (define-values (new-params)
       (extend-parameterization
        (continuation-mark-set-first #f parameterization-key)
        caner
        11))
     (with-continuation-mark
         parameterization-key
       new-params
       (car (list (f new-params))))
     )""")
    result, _ = eval_fixnum(l2, empty_target(), [l1])
    assert result == 1


def test_make_prefab_predicate():
    l = """
(linklet
 ()
 ()
 (define-values
  (struct:a make-a a? a-b a-c)
  (let-values (((struct: make- ? -ref -set!)
                (make-struct-type
                 (quote a)
                 #f
                 2
                 0
                 #f
                 null
                 (quote prefab)
                 #f
                 (quote (0 1))
                 #f
                 (quote a))))
    (values
     struct:
     make-
     ?
     (make-struct-field-accessor -ref 0 (quote b))
     (make-struct-field-accessor -ref 1 (quote c)))))
 (define-values (v0) (make-prefab-struct (quote a) 1 2))
 (a? v0))
"""
    r, t = eval_bool(make_linklet(l), empty_target())
    assert r

def test_make_prefab_predicate2():
    l2 = """
(linklet
 ()
 ()
 (define-values
  (struct:a make-a a? a-b a-c)
  (let-values (((struct: make- ? -ref -set!)
                (make-struct-type
                 (quote a)
                 #f
                 2
                 0
                 #f
                 null
                 (quote prefab)
                 #f
                 (quote (0 1))
                 #f
                 (quote a))))
    (values
     struct:
     make-
     ?
     (make-struct-field-accessor -ref 0 (quote b))
     (make-struct-field-accessor -ref 1 (quote c)))))
 (define-values
  (-struct:a -make-a -a? -a-b -a-c)
  (let-values (((struct: make- ? -ref -set!)
                (make-struct-type
                 (quote a)
                 #f
                 2
                 0
                 #f
                 null
                 (quote prefab)
                 #f
                 (quote (0 1))
                 #f
                 (quote a))))
    (values
     struct:
     make-
     ?
     (make-struct-field-accessor -ref 0 (quote b))
     (make-struct-field-accessor -ref 1 (quote c)))))
  (-a? (make-prefab-struct (quote a) 1 2)))
"""
    r2, t2 = eval_bool(make_linklet(l2), empty_target())
    print r2
    assert r2


def test_wcm_make_env1():
    l = """
(linklet
 ()
 ()
 (define-values (p) (lambda (x y) (+ x y)))
 (define-values
  (last)
  (lambda (l)
    ((letrec-values (((loop)
                      (lambda (l x) (if (pair? x) (loop x (cdr x)) (car l)))))
       loop)
     l
     (cdr l))))
 (define-values
  (h)
  (lambda (a b)
    (with-continuation-mark
     (make-continuation-mark-key (quote xxxxx))
     (last (list a b))
     (apply p (list a b)))))
 (h 1 2))
"""
    r2, t2 = eval_fixnum(make_linklet(l), empty_target())
    print r2
    assert r2 == 3


def test_wcm_make_env2():
    l = """
(linklet
 ()
 ()
 (define-values (p) (lambda (x y) (+ x y)))
 (define-values
  (last)
  (lambda (l)
    ((letrec-values (((loop)
                      (lambda (l x) (if (pair? x) (loop x (cdr x)) (car l)))))
       loop)
     l
     (cdr l))))
 (define-values
  (h)
  (lambda (a b)
    (with-continuation-mark
     (quote xxx)
     (last (list a b))
     (apply p (list a b)))))
 (h 1 2))
"""
    r2, t2 = eval_fixnum(make_linklet(l), empty_target())
    print r2
    assert r2 == 3

