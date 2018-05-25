import pytest
from pycket.expand import expand, parse_module, expand_string
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *

from pycket.test.testhelper import run, run_fix, run_mod_expr, run_mod_defs, run_mod

def test_empty_mod():
    run_mod_defs("")

def test_racket_mod():
    m = run_mod("#lang racket/base\n (define x 1)")
    ov = m.defs[W_Symbol.make("x")]
    assert ov.value == 1

def test_constant_mod():
    run_mod_expr("1")

def test_constant_mod_val():
    ov = run_mod_expr("1")
    assert isinstance(ov, W_Fixnum)
    assert ov.value == 1

# look ma, no modules!
def test_constant():
    run_fix("1", v=1)

def test_set_modvar():
    m = run_mod("""
#lang pycket

(define sum 0)

(define (tail-rec-aux i n)
  (if (< i n)
      (begin (set! sum (+ sum 1)) (tail-rec-aux (+ i 1) n))
      sum))

(tail-rec-aux 0 100)
""")
    ov = m.defs[W_Symbol.make("sum")].get_val()
    assert ov.value == 100

def test_set_mod2():
    m = run_mod("""
#lang pycket
(provide table)
(define table #f)
(set! table #f)
""")
    ov = m.defs[W_Symbol.make("table")]
    assert isinstance(ov, W_Cell)


def test_set_mod_other():
    m = run_mod("""
#lang pycket
    (require pycket/set-export)
(define y (not x))
""")
    assert m.defs[W_Symbol.make("y")]

def test_use_before_definition():
    with pytest.raises(SchemeException):
        m = run_mod("""
        #lang pycket
        x
        (define x 1)
    """)

    with pytest.raises(SchemeException):
        m = run_mod("""
        #lang pycket
        x
        (define x 1)
        (set! x 2)
    """)

def test_shadowing_macro():
    m = run_mod("""
#lang pycket

(define-syntax bind+
  (syntax-rules ()
    [(bind+ v e) (let ([x v]) (+ x e))]
    [(bind+ v0 v ... e) (let ([x v0]) (bind+ v ... (+ x e)))]))

(define x (bind+ 1 2 3))
""")
    ov = m.defs[W_Symbol.make("x")]
    assert ov.value == 6

def test_kw_submodule():
    # Just test to see if this runs.
    # It caused quite a few problems
    # when implementing submodules.
    m = run_mod(
    """
    #lang pycket
    (require racket/private/kw)
    procedure-arity-includes?
    """)

def test_submodule_operations():
    m = run_mod(
    """
    #lang pycket
    (module test-sub-module pycket
      (provide a)
      (define a 1))
    (require (submod "." test-sub-module))
    (define b (+ a a))
    """)
    assert len(m.submodules) == 1

    # properties concerning relationships between modules and submodules
    b   = m.defs[W_Symbol.make("b")]
    sub = m.resolve_submodule_path(["test-sub-module"])
    assert sub.parent is m
    assert sub in m.submodules
    assert m.root_module() is m
    assert sub.root_module() is m

    a   = sub.defs[W_Symbol.make("a")]
    assert isinstance(b, W_Integer) and b.value == 2
    assert isinstance(a, W_Integer) and a.value == 1

def test_module_star():
    m = run_mod(
    """
    #lang pycket
    (module outer pycket
      (define a 1)
      (module* inner #f
        (provide b)
        (define b (+ a 2))))
    (module snd pycket
      (require (submod ".." outer inner))
      (provide c)
      (define c (+ b 1)))
    (require (submod "." snd))
    (define d c)
    """)
    assert len(m.submodules) == 2
    outer = m.find_submodule("outer")
    snd   = m.find_submodule("snd")

    assert outer.parent is m
    assert snd.parent is m
    assert len(outer.submodules) == 1
    assert len(snd.submodules) == 0

    d = m.defs[W_Symbol.make("d")]
    assert isinstance(d, W_Integer) and d.value == 4

