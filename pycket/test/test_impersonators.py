from pycket.test.testhelper import *
from pycket.values import *
from pycket.impersonators import *
from pycket.values_struct import *
import pytest

# This test ensures the new property based on this change to Racket:
# http://git.racket-lang.org/plt/commit/0b71b8481dcf0c8eb99edf5fef9bfdfeb4f92465
def test_chaperone_struct_self_arg():
    m = run_mod(
    """
    #lang pycket
    (struct point (x y))
    (define p (point 1 2))
    (define cell #f)
    (define p-chap
      (chaperone-struct p
        point-x (lambda (self val) (set! cell self) val)))
    (point-x p-chap)
    """)
    prox = m.defs[W_Symbol.make("p")]
    chap = m.defs[W_Symbol.make("p-chap")]
    cell = m.defs[W_Symbol.make("cell")]
    assert isinstance(prox, W_Struct)
    assert isinstance(cell, W_Cell)
    assert isinstance(chap, W_ChpStruct)
    self = cell.get_val()
    #assert self is not prox
    assert self is chap

def test_noninterposing_chaperone():
    m = run_mod(
    """
    #lang pycket
    (define-values (prop:blue blue? blue-ref) (make-impersonator-property 'blue))
    (define-values (prop:green green? green-ref) (make-struct-type-property 'green 'can-impersonate))
    (define a-equal+hash (list
                         (lambda (v1 v2 equal?)
                           (equal? (aa-y v1) (aa-y v2)))
                         (lambda (v1 hash)
                           (hash (aa-y v1)))
                         (lambda (v2 hash)
                           (hash (aa-y v2)))))
    (define (a-impersonator-of v) (a-x v))
    (define (aa-y v) (if (a? v) (a-y v) (pre-a-y v)))
      (define-struct pre-a (x y)
        #:property prop:equal+hash a-equal+hash
        #:property prop:green 'color)
      (define-struct a (x y)
        #:property prop:impersonator-of a-impersonator-of
        #:property prop:equal+hash a-equal+hash)
      (define-struct (a-more a) (z))
      (define-struct (a-new-impersonator a) ()
        #:property prop:impersonator-of a-impersonator-of)
      (define-struct (a-new-equal a) ()
        #:property prop:equal+hash a-equal+hash)
    (define a-pre-a (chaperone-struct (make-pre-a 17 1) pre-a-y (lambda (a v) v)))
    (define t1 (chaperone-of? a-pre-a a-pre-a))
    (define t2
      (chaperone-of?
        (make-pre-a 17 1)
        (chaperone-struct (make-pre-a 17 1) pre-a-y #f prop:blue 'color)))
    (define t3
      (chaperone-of?
        (make-pre-a 17 1)
        (chaperone-struct a-pre-a pre-a-y #f prop:blue 'color)))
    (define t4
      (chaperone-of? a-pre-a
        (chaperone-struct a-pre-a pre-a-y #f prop:blue 'color)))
    (define t5
      (chaperone-of?
        (chaperone-struct a-pre-a pre-a-y #f prop:blue 'color)
        a-pre-a))
    (define t6
      (chaperone-of?
        a-pre-a
        (chaperone-struct a-pre-a pre-a-y (lambda (a v) v) prop:blue 'color)))
    (define t7
      (chaperone-of? a-pre-a
        (chaperone-struct a-pre-a green-ref (lambda (a v) v))))
    """)
    assert m.defs[W_Symbol.make("t1")] is w_true
    assert m.defs[W_Symbol.make("t2")] is w_true
    assert m.defs[W_Symbol.make("t3")] is w_false
    assert m.defs[W_Symbol.make("t4")] is w_true
    assert m.defs[W_Symbol.make("t5")] is w_true
    assert m.defs[W_Symbol.make("t6")] is w_false
    assert m.defs[W_Symbol.make("t7")] is w_false

def test_noninterposing_chaperone_procedure():
    m = run_mod(
    """
    #lang racket/base
    (define-values (prop:blue blue? blue-ref) (make-impersonator-property 'blue))
    (define wrapper (lambda (x) x))
    (define f1 (lambda (a) a))
    (define f2 (lambda (b) b))
    (define f3 (lambda (c) c))
    (define g1 (chaperone-procedure f1 wrapper))
    (define g2 (chaperone-procedure f2 wrapper))
    (define g3 (chaperone-procedure f2 wrapper))
    (define t1 (chaperone-of? g1 (chaperone-procedure g1 #f prop:blue 'color)))
    (define t2 (chaperone-of? g2 (chaperone-procedure g2 #f prop:blue 'color)))
    (define t3 (chaperone-of? g3 (chaperone-procedure g3 #f prop:blue 'color)))
    (define t4 (chaperone-of? f3 (chaperone-procedure f3 #f prop:blue 'color)))
    (define t5 (chaperone-of? f3 (chaperone-procedure g3 #f prop:blue 'color)))
    """)
    assert m.defs[W_Symbol.make("t1")] is values.w_true
    assert m.defs[W_Symbol.make("t2")] is values.w_true
    assert m.defs[W_Symbol.make("t3")] is values.w_true
    assert m.defs[W_Symbol.make("t4")] is values.w_true
    assert m.defs[W_Symbol.make("t5")] is values.w_false

def test_chaperone_procedure_star():
    m = run_mod(
    """
    #lang racket/base
    (define val #f)
    (define proc (lambda (x) x))
    (define g
      (chaperone-procedure* proc (lambda (p x) (set! val p) x)))
    (g 1)
    """)
    proc = m.defs[W_Symbol.make("g")]
    val  = m.defs[W_Symbol.make("val")]
    assert isinstance(val, W_Cell)
    assert proc is val.get_val()

def test_chaperone_stack_exhaustion():
    m = run_mod(
    """
    #lang racket/base
    (define d
      (for/fold ([v (vector 1 2 3)])
          ([i 100000])
        (chaperone-vector v (lambda (x i val) val) (lambda (x i val) val))))
    (vector-ref d 0)
    """)

