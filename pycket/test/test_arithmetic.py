import pytest
from pycket.expand import expand
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *
from pycket.test.test_basic import run_fix, run


def test_mul_zero():
    import pdb; pdb.set_trace()
    run_fix("(* 0 1.2)", 0)
    run_fix("(* 1.2 0)", 0)

