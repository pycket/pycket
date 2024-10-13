from pycket.test.testhelper import *
from pycket.values import *
import pytest

skip = pytest.mark.skipif("True")


# Creating Structure Types

def test_make_struct_type(doctest):
    """
    > (define-values (struct:a make-a a? a-ref a-set!)
        (make-struct-type 'a #f 2 1 'uninitialized))
    > (define an-a (make-a 'x 'y))
    > (a-ref an-a 1)
    'y
    > (a-ref an-a 2)
    'uninitialized
    > (define a-first (make-struct-field-accessor a-ref 0))
    > (a-first an-a)
    'x
    > (define-values (struct:b make-b b? b-ref b-set!)
        (make-struct-type 'b struct:a 1 2 'b-uninitialized))
    > (define a-b (make-b 'x 'y 'z))
    > (a-ref a-b 1)
    'y
    > (a-ref a-b 2)
    'uninitialized
    > (b-ref a-b 0)
    'z
    > (b-ref a-b 1)
    'b-uninitialized
    > (b-ref a-b 2)
    'b-uninitialized
    ;;;;;;;;;;;;;;;;
    > (define p1 #s(p a b c))
    > (define-values (struct:p make-p p? p-ref p-set!)
        (make-struct-type 'p #f 3 0 #f null 'prefab #f '(0 1 2)))
    > (p? p1)
    #t
    > (p-ref p1 0)
    'a
    > (make-p 'x 'y 'z)
    '#s(p x y z)
    """
    assert doctest

def test_make_struct_type2(doctest):
    """
    ! (require racket/private/generic-interfaces)
    > (struct color (r g b) #:constructor-name -color)
    > (struct rectangle (w h color) #:extra-constructor-name rect)
    > (rectangle 13 50 (-color 192 157 235))
    > (rect 50 37 (-color 35 183 252))
    > (struct circle (radius) #:reflection-name '<circle>)
    > (circle 15)
    ;#<|<circle>|>
    """
    assert doctest

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

def test_struct_inheritance(source):
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

def test_struct_inheritance2():
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

def test_struct_comparison(source):
    """
    (struct glass (width height) #:transparent #:mutable)
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

@pytest.mark.skipif(pytest.config.new_pycket, reason="normalizer issues")
def test_struct_guard():
    run(
    """
    ((lambda (name) (begin (struct thing (name) #:transparent #:guard
      (lambda (name type-name) (cond
        ((string? name) name)
        (else (error type-name \"bad name: ~e\" name)))))
    (thing? (thing name)))) \"apple\")
    """, w_true)
    e = pytest.raises(SchemeException, run,
    """
    ((lambda (name) (begin (struct thing (name) #:transparent #:guard
      (lambda (name type-name) (cond
        ((string? name) name)
        (else (error type-name "bad name")))))
    (thing? (thing name)))) 1)
    """)
    assert "bad name" in e.value.msg

def test_struct_guard2():
    m = run_mod(
    """
    #lang pycket

    (define-values (s:o make-o o? o-ref o-set!)
        (make-struct-type 'o #f 1 0 'odefault null (make-inspector) #f null (lambda (o n) (+ o 1))))

    (define x (o-ref (make-o 10) 0))
    """)
    ov = m.defs[W_Symbol.make("x")]
    assert ov.value == 11

def test_struct_guard3():
    m = run_mod(
    """
    #lang pycket

    (define got null)
    (define-values (s:a make-a a? a-ref a-set!)
        (make-struct-type 'a #f 2 1 'adefault null (make-inspector) #f null
            (lambda (a b n) (set! got (cons (list a b n) got)) (values 1 2))))
    (define-values (s:b make-b b? b-ref b-set!)
        (make-struct-type 'b s:a 1 2 'bdefault null (make-inspector) #f null
            (lambda (a b c n) (set! got (cons (list a b c n) got)) (values 10 20 30))))

    (define x (a-ref (make-b 'x 'y 'z) 0))
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


# Structure Type Properties

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

def test_struct_prop_procedure_inheritance():
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)
    (struct x (proc) #:property prop:procedure 0)
    (struct y x ())

    (define b (y (lambda (x) x)))
    (define val (b 10))
    """)
    assert m.defs[W_Symbol.make("val")].value == 10

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
    assert ov.as_str_utf8() == "Hi Mary, I'm Joe"

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
    (define-values (prop prop? prop-accessor) (make-struct-type-property 'p #f (list (cons prop:checked-procedure add1)) #f))
    (define-values (struct:posn make-posn posn? posn-x posn-y) (make-struct-type 'a #f 2 1 'uninitialized (list (cons prop 0))))
    (define posn_instance (make-posn (lambda (a b) #t) 2))
    (define proc (lambda (a b c) (+ a b c)))

    (let* ([check_0 (checked-procedure-check-and-extract struct:posn posn_instance proc 1 2)]
           [check_1 (checked-procedure-check-and-extract struct:posn 3 proc 1 2)])
    (and (= check_0 2) (= check_1 6)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

# This is 8.0+ only and I have no idea how to teach the toolchain that...
@skip
def test_struct_prop_sealed_fail(source):
    """
    (struct x () #:property prop:sealed #t)
    (struct y x ())
    (y)
    """
    with pytest.raises(SchemeException) as e:
      run_mod_expr(source, wrap=True)
    assert "cannot make a subtype of a sealed type" in str(e.value)


# Generic Interfaces

def test_current_inspector(source):
    """
    (inspector? (current-inspector))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true


# Copying and Updating Structures

def test_struct_copying_and_update(doctest):
    """
    > (struct fish (color weight) #:transparent)
    > (define marlin (fish 'orange-and-white 11))
    > (define dory (struct-copy fish marlin
                                [color 'blue]))
    > dory
    (fish 'blue 11)
    > (struct shark fish (weeks-since-eating-fish) #:transparent)
    > (define bruce (shark 'grey 110 3))
    > (define chum (struct-copy shark bruce
                                [weight #:parent fish 90]
                                [weeks-since-eating-fish 0]))
    > chum
    (shark 'grey 90 0)
    ; subtypes can be copied as if they were supertypes,
    ; but the result is an instance of the supertype
    > (define not-really-chum
        (struct-copy fish bruce
                     [weight 90]))
    > not-really-chum
    (fish 'grey 90)
    """
    assert doctest


# Structure Utilities

def test_struct2vector(source):
    """
    (struct posn (x y) #:transparent)
    (struct 3d-posn posn ([z #:mutable]))

    (let* ([d (3d-posn 1 2 3)]
           [v (struct->vector d)]
           [v_name (vector-ref v 0)]
           [v0 (vector-ref v 1)]
           [v2 (vector-ref v 3)])
    (and (eq? v_name 'struct:3d-posn) (= v0 1) (= v2 3)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_prefab_struct_key(doctest):
    """
    > (prefab-struct-key #s(cat "Garfield"))
    'cat
    > (struct cat (name) #:prefab)
    > (struct cute-cat cat (shipping-dest) #:prefab)
    > (cute-cat "Nermel" "Abu Dhabi")
    '#s((cute-cat cat 1) "Nermel" "Abu Dhabi")
    > (prefab-struct-key (cute-cat "Nermel" "Abu Dhabi"))
    '(cute-cat cat 1)
    """
    assert doctest

def test_is_prefab_key(source):
    """
    (let* ([k0 (prefab-key? 'cat)]
           [k1 (prefab-key? '(cute-cat cat 1))]
           [k2 (prefab-key? '(clown 1 (1 #f) #(0)))]
           [l (prefab-key? 0)])
    (and k0 k1 k2 (not l)))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_make_prefab_struct(doctest):
    """
    > (make-prefab-struct 'clown "Binky" "pie")
    '#s(clown "Binky" "pie")
    > (make-prefab-struct '(clown 2) "Binky" "pie")
    '#s(clown "Binky" "pie")
    > (make-prefab-struct '(clown 2 (0 #f) #()) "Binky" "pie")
    '#s(clown "Binky" "pie")
    ;> (make-prefab-struct '(clown 1 (1 #f) #()) "Binky" "pie")
    ;'#s((clown (1 #f)) "Binky" "pie")
    ;> (make-prefab-struct '(clown 1 (1 #f) #(0)) "Binky" "pie")
    ;'#s((clown (1 #f) #(0)) "Binky" "pie")
    """
    assert doctest


# Structure Type Transformer Binding

def test_struct(doctest):
    """
    ! (require racket/private/define-struct)
    > (struct x ())
    > (struct? (x))
    #f
    > (struct y () #:inspector (make-inspector))
    > (struct? (y))
    #t
    """
    assert doctest

def test_struct_info(doctest):
    """
    ! (require racket/private/define-struct)
    > (struct x ())
    > (define-values (struct-type skipped?) (struct-info (x)))
    > struct-type
    #f
    > skipped?
    #t
    > (struct y () #:inspector (make-inspector))
    > (define-values (struct-type skipped?) (struct-info (y)))
    > skipped?
    #f
    """
    assert doctest


# Other

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

def test_struct_immutable_boolean(source):
    """
    (struct struct-with-immu (a b c))
    (define-values (struct-i) (struct-with-immu 1 #f 2))

    (let-values ([(first-ok) (equal? (struct-with-immu-a struct-i)  1)]
                 [(immu-ok)  (equal? (struct-with-immu-b struct-i) #f)]
                 [(last-ok)  (equal? (struct-with-immu-c struct-i)  2)])
      (and first-ok immu-ok last-ok))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_immutable_boolean1(source):
    """
    (struct struct-with-immu (a b [c #:mutable]))
    (define struct-i (struct-with-immu 1 #f 2))
    (set-struct-with-immu-c! struct-i 3)

    (let-values ([(first-ok) (equal? (struct-with-immu-a struct-i)  1)]
                 [(immu-ok)  (equal? (struct-with-immu-b struct-i) #f)]
                 [(last-ok)  (equal? (struct-with-immu-c struct-i)  3)])
      (and first-ok immu-ok last-ok))
    """
    result = run_mod_expr(source, wrap=True)
    assert result == w_true

def test_struct_tostring(doctest):
    """
    ! (struct a (a))
    ! (struct b a (b) #:transparent)
    ! (struct b1 a (b))
    ! (struct a1 (a) #:transparent)
    ! (struct b2 a1 (b))
    ! (struct b3 a1 (b) #:transparent)
    > (format "~v" (a 1))
    "#<a>"
    > (format "~v" (b 1 2))
    "(b ... 2)"
    > (format "~v" (b1 1 2))
    "#<b1>"
    > (format "~v" (b2 1 2))
    "(b2 1 ...)"
    > (format "~v" (b3 1 2))
    "(b3 1 2)"
    """

def test_auto_values(doctest):
    """
    ! (struct posn (x y [z #:auto]) #:auto-value 0 #:transparent)
    ! (struct color-posn posn (hue) #:mutable)
    ! (define cp (color-posn 1 2 "blue"))
    > (format "~v" (posn 1 2))
    "(posn 1 2 0)"
    > (posn? (posn 1 2))
    #t
    > (posn-y (posn 1 2))
    2
    > (color-posn-hue cp)
    "blue"
    > (format "~v" cp)
    "(color-posn 1 2 0 ...)"
    """
    assert doctest

def test_struct_operations_arity(doctest):
    """
    ! (require racket/base)
    ! (struct posn (x [y #:mutable] [z #:auto]) #:auto-value 0 #:transparent)
    > (procedure-arity posn-x)
    1
    > (procedure-arity posn-y)
    1
    > (procedure-arity posn-z)
    1
    > (procedure-arity set-posn-y!)
    2
    > (procedure-arity posn)
    2
    """

@pytest.mark.caner
def test_struct_object_name_field(doctest):
    """
    ! (require racket/base)
    ! (struct posn (x y name) #:property prop:object-name 2)
    > (object-name (posn 1 2 'my-point))
    'my-point
    """

@skip
def test_serializable(source):
    """
    (= (point-x (deserialize (serialize (point 1 2)))) 1)
    """
    extra = """
    (require racket/serialize)
    (serializable-struct point (x y))
    """
    result = run_mod_expr(source, extra=extra, wrap=True)
    assert result == w_true

def test_inherited_auto_values(doctest):
    """
    ! (struct test1 ([a #:auto] [b #:auto] [c #:auto]) #:auto-value 0 #:transparent)
    ! (struct test2 test1 () #:transparent)
    ! (struct test3 test2 () #:transparent)
    > (test1? (test1))
    #t
    > (test2? (test2))
    #t
    > (test3? (test3))
    #t
    """

def test_struct_ports(doctest):
    """
    > (define-values (struct:struct-port make-struct-port struct-port?
                    struct-port-ref struct-port-set!)
        (make-struct-type 'struct-port #f 2 0 0
                        (list (cons prop:input-port 0)
                              (cons prop:output-port 1))
                        #f
                        #f
                        (list 0 1)))
    > (define asp (make-struct-port (open-input-string "akg cdef")
                                   (open-output-string)))
    > (read asp)
    'akg
    > (read-char asp)
    '#\space
    > (peek-char asp)
    '#\c
    > (close-input-port asp)
    > (write-string "0123" asp)
    4
    > (write-char #\c asp)
    > (write-byte 1 asp)
    > (get-output-string (struct-port-ref asp 1))
    "0123c\u0001"
    """

def test_procedure_extract_target(doctest):
    """
    ! (require racket/private/kw)
    ! (struct wrapper (proc) #:property prop:procedure 0)
    ! (define proc (wrapper +))
    > (procedure-extract-target proc)
    +
    """
