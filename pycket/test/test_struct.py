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

def test_struct_comparison2():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/generic-interfaces)

    (struct lead (width height)
      #:methods
      gen:equal+hash
      [(define (equal-proc a b equal?-recur)
         ; compare a and b
         (and (equal?-recur (lead-width a) (lead-width b))
              (equal?-recur (lead-height a) (lead-height b))))
       (define (hash-proc a hash-recur)
         ; compute primary hash code of a
         (+ (hash-recur (lead-width a))
            (* 3 (hash-recur (lead-height a)))))
       (define (hash2-proc a hash2-recur)
         ; compute secondary hash code of a
         (+ (hash2-recur (lead-width a))
                 (hash2-recur (lead-height a))))])

    (define result (equal? (lead 1 2) (lead 1 2)))
    """)
    assert m.defs[W_Symbol.make("result")] == w_true

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
    run(
    """
    ((lambda (name) (struct thing (name) #:transparent #:guard 
      (lambda (name type-name) (cond 
        [(string? name) name] 
        [else (error type-name \"bad name: ~e\" name)])))
    (thing? (thing name))) \"apple\")
    """, w_true)
    e = pytest.raises(SchemeException, run,
    """
    ((lambda (name) (struct thing (name) #:transparent #:guard 
      (lambda (name type-name) (cond 
        [(string? name) name] 
        [else (error type-name "bad name")])))
    (thing? (thing name))) 1)
    """)
    assert "bad name" in e.value.msg

def test_struct_prop_procedure():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)
    (require (prefix-in k: '#%kernel))

    (struct x() #:property prop:procedure (lambda _ 1))
    (struct y() #:property k:prop:procedure (lambda _ 2))

    (define xval ((x)))
    (define yval ((y)))
    """)
    assert m.defs[W_Symbol.make("xval")].value == 1
    assert m.defs[W_Symbol.make("yval")].value == 2

def test_struct_prop_procedure_fail():
    e = pytest.raises(SchemeException, run_mod,
    """
    #lang pycket
    (require racket/private/kw)
    (require (prefix-in k: '#%kernel))

    (struct x() #:property prop:procedure (lambda _ 1) #:property k:prop:procedure (lambda _ 2))
    """)
    assert "duplicate property binding" in e.value.msg

def test_struct_prop_procedure_with_self_arg():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)

    (struct greeter (name)
              #:property prop:procedure
                         (lambda (self other)
                           (string-append
                            "Hi " other
                            ", I'm " (greeter-name self))))
    (define joe-greet (greeter "Joe"))
    (define greeting (joe-greet "Mary"))
    """)
    ov = m.defs[W_Symbol.make("greeting")]
    assert ov.value == "Hi Mary, I'm Joe"

def test_struct_super_prop_procedure():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)
    (require (prefix-in k: '#%kernel))

    (struct x() #:property prop:procedure (lambda _ 1))
    (struct y x())

    (define yval ((y)))
    """)
    assert m.defs[W_Symbol.make("yval")].value == 1

def test_struct_prop_arity():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)

    (struct evens (proc)
    #:property prop:procedure (struct-field-index proc)
    #:property prop:arity-string
    (lambda (p)
      "an even number of arguments"))
    (define pairs
        (evens
         (case-lambda
          [() null]
          [(a b . more)
           (cons (cons a b)
                 (apply pairs more))])))
    (define x (pairs 1 2 3 4))
    """)
    ov = m.defs[W_Symbol.make("x")]
    assert isinstance(ov, W_Cons)
    e = pytest.raises(SchemeException, run_mod,
    """
    #lang pycket
    (require racket/private/kw)

    (struct evens (proc)
    #:property prop:procedure (struct-field-index proc)
    #:property prop:arity-string
    (lambda (p)
      "an even number of arguments"))
    (define pairs
        (evens
         (case-lambda
          [() null]
          [(a b . more)
           (cons (cons a b)
                 (apply pairs more))])))
    (pairs 5)
    """)
    assert "an even number of arguments" in e.value.msg

def test_checked_procedure_check_and_extract(source):
    """
    (define-values (prop prop? prop-accessor) (make-struct-type-property 'p #f (list (cons prop:checked-procedure sqrt)) #f))
    (define-values (struct:posn make-posn posn? posn-x posn-y) (make-struct-type 'a #f 2 1 'uninitialized (list (cons prop 0))))
    (define posn_instance (make-posn (lambda (a b) #t) 2))
    (define proc (lambda (a b c) (+ a b c)))

    (let* ([check_0 (checked-procedure-check-and-extract struct:posn posn_instance proc 1 2)]
           [check_1 (checked-procedure-check-and-extract struct:posn 3 proc 1 2)])
    (and (= check_0 2) (= check_1 6)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_super():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)

    (struct posn (x y))
        (define (raven-constructor super-type)
        (struct raven ()
                #:super super-type
                #:transparent
                #:property prop:procedure (lambda (self) 'nevermore)) raven)
    (define r ((raven-constructor struct:posn) 1 2))
    (define x (posn-x r))
    """)
    ov = m.defs[W_Symbol.make("x")]
    assert ov.value == 1

def test_struct_prefab():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)

    (define lunch '#s(sprout bean))
    (struct sprout (kind) #:prefab)
    (define t (sprout? lunch))
    (define f (sprout? #s(sprout bean #f 17)))

    (define result (and (not f) t))
    """)
    assert m.defs[W_Symbol.make("result")] == w_true

@skip
def test_procedure():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)

    (define ((f x) #:k [y 0])
      (+ x y))
    (define proc (procedure-rename (f 1) 'x))
    (define x (proc))
    """)
    ov = m.defs[W_Symbol.make("x")]
    assert ov.value == 1

def test_unsafe():
    m = run_mod(
    """
    #lang pycket

    (struct posn ([x #:mutable] [y #:mutable]) #:transparent)
    (struct 3dposn posn ([z #:mutable]))

    (define p (3dposn 1 2 3))
    (unsafe-struct*-set! p 2 4)
    (define x (unsafe-struct*-ref p 2))
    """)
    ov = m.defs[W_Symbol.make("x")]
    assert ov.value == 4

def test_unsafe_impersonators():
    m = run_mod(
    """
    #lang pycket

    (struct posn ([x #:mutable] [y #:mutable]) #:transparent)
    (define a (posn 1 1))
    (define b (impersonate-struct a))
    (unsafe-struct-set! b 1 2)
    (define x (unsafe-struct-ref b 1))
    """)
    ov = m.defs[W_Symbol.make("x")]
    assert ov.value == 2
