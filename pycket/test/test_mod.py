import pytest
from pycket.expand import expand, to_ast, parse_module, expand_string
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *

from pycket.test.testhelper import run, run_fix, run_mod_expr, run_mod_defs, run_mod

def test_empty_mod():
    run_mod_defs("")

def test_racket_mod():
    m = run_mod("#lang racket\n (define x 1)")
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
    
