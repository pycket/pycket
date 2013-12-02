import pytest
from pycket.expand import expand, to_ast
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *

from pycket.test.test_basic import run_top

current_dir = os.path.dirname(__file__)

def run_file(fname, *replacements):
    with file(os.path.join(current_dir, fname)) as f:
        s = f.read()
    for replace, with_ in replacements:
        assert s.count(replace) == 1
        s = s.replace(replace, with_)
    return run_top(s)

def test_puzzle():
    run_file("puzzle.sch", ("1048575", "460"), ("50", "1"))

def test_nqueens():
    run_file("nqueens.sch", ("10000", "1"))

def test_bubble():
    run_file("bubble.sch", ("10000", "100"))

#def test_pseudoknot():
#    run_file("nucleic2.sch")

def test_microkanren():
    run_file("microkanren.sch")

def test_minikanren():
    run_file("minikanren.sch")
