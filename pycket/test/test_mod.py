import pytest
from pycket.expand import expand, to_ast, parse_module, expand_string
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *

from pycket.test.testhelper import run, run_fix, run_flo, run_top, execute

def test_simplest_mod():
    mod = interpret_module(parse_module(expand_string("#lang racket\n")))
    

def test_constant_mod():
    val = interpret_module(parse_module(expand_string("#lang s-exp pycket-lang (define #%pycket-expr 1)")))

def run_expr_mod(e):
    str = "#lang s-exp pycket-lang (define #%%pycket-expr %s)"%e
    mod = interpret_module(parse_module(expand_string(str)))
    print mod.defs
    val = mod.defs[W_Symbol.make("#%pycket-expr")]
    return val

def test_constant_mod_val():
    ov = run_expr_mod("1")
    assert isinstance(ov, W_Fixnum)
    assert ov.value == 1
    
