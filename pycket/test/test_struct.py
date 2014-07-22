from pycket.test.testhelper import *
from pycket.values import *
import pytest

skip = pytest.mark.skipif("True")

def test_current_inspector(source):
    """
    (inspector? (current-inspector))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_make_struct_type(source):
    """
    (define-values (struct:p0 make-p0 p0? p0-ref p0-set!) (make-struct-type 'p0 #f 3 0 #f null 'prefab #f '(0 1 2)))
    (and 
      (struct-type? struct:p0) 
      (struct-constructor-procedure? make-p0) 
      (struct-predicate-procedure? p0?) 
      (struct-accessor-procedure? p0-ref) 
      (struct-mutator-procedure? p0-set!))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_make_struct_field_accessor(source):
    """
    (define-values (struct:p1 make-p1 p1? p1-ref p1-set!) (make-struct-type 'p1 #f 3 0 #f null 'prefab #f '(0 1 2)))
    (define accessor (make-struct-field-accessor p1-ref 0))
    (procedure? accessor)
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_main_functions(source):
    """
    (struct posn (x y))

    (let* ([p (posn 1 2)]
           [p? (posn? p)]
           [notp? (posn? 0)]
           [x (posn-x p)]
           [y (posn-y p)])
    (and p? (not notp?) (= x 1) (= y 2)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_copying_and_update(source):
    """
    (struct posn (x y))

    (let* ([p1 (posn 1 2)]
           [p2 (struct-copy posn p1 [x 3])]
           [x1 (posn-x p1)]
           [x2 (posn-x p2)]
           [y2 (posn-y p2)])
    (and (= x1 1) (= x2 3) (= y2 2)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_subtypes(source):
    """
    (struct posn (x y))
    (struct 3d-posn posn (z))

    (let* ([p (3d-posn 1 2 3)]
           [p? (posn? p)]
           [x (posn-x p)]
           [z (3d-posn-z p)])
    (and p? (= x 1) (= z 3)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_comparison(source):
    """
    (struct glass (width height) #:transparent)
    (struct lead (width height))
    (define slab (lead 1 2))

    (let* ([glass_test (equal? (glass 1 2) (glass 1 2))]
           [slab (lead 1 2)]
           [lead_test1 (equal? slab slab)]
           [lead_test2 (equal? slab (lead 1 2))])
    (and glass_test lead_test1 (not lead_test2)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_mutation(source):
    """
    (struct dot (x y) #:mutable)

    (let* ([d (dot 1 2)]
           [dx0 (dot-x d)]
           [m (set-dot-x! d 10)]
           [dx1 (dot-x d)])
    (and (= dx0 1) (= dx1 10)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_auto_values(source):
    """
    (struct p3 (x y [z #:auto]) #:transparent #:auto-value 0)
    (struct p4 p3 (t))

    (let* ([p (p3 1 2)]
           [4dp (p4 1 2 4)]
           [pz (p3-z p)]
           [4pdt (p4-t 4dp)])
    (and (= pz 0) (= 4pdt 4)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_guard():
    run("""((lambda (name) (struct thing (name) #:transparent #:guard 
      (lambda (name type-name) (cond 
        [(string? name) name] 
        [else (error type-name \"bad name: ~e\" name)])))
    (thing? (thing name))) \"apple\")""", w_true)
    e = pytest.raises(SchemeException, run,
        """((lambda (name) (struct thing (name) #:transparent #:guard 
      (lambda (name type-name) (cond 
        [(string? name) name] 
        [else (error type-name "bad name")])))
    (thing? (thing name))) 1)""")
    assert "bad name" in e.value.msg
