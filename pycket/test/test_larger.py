import pytest
from pycket.expand import expand, to_ast
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *

from pycket.test.test_basic import run_top

def run_file(fname):
    with file(fname) as f:
        s = f.read()
    return run_top(s)

def test_puzzle():
    run_file("puzzle.sch")

def test_nqueens():
    run_file("nqueens.sch")

def test_pseudoknot():
    run_file("nucleic2.sch")

def test_microkanren():
    run_file("microkanren.sch")

def test_minikanren():
    run_file("minikanren.sch")
