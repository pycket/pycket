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
