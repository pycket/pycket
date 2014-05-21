import pytest
from pycket.expand import expand, expand_string, to_ast
from pycket.json import loads
from pycket.interpreter import *
from pycket.values import *
from pycket.prims import *

from pycket.test.testhelper import run_file



def test_puzzle():
    run_file("puzzle.rkt", ("1048575", "460"), ("50", "1"))

def test_nqueens():
    run_file("nqueens.rkt", ("10000", "1"))

def test_bubble():
    run_file("bubble.rkt", ("10000", "100"))

def test_bubble():
    run_file("bubble-imp.rkt", ("10000", "100"))

def test_bubble_unsafe():
    run_file("bubble-unsafe.rkt", ("10000", "100"))

#def test_pseudoknot():
#    run_file("nucleic2.sch", ("(i 100)", "(i 1)"))

def test_microkanren():
    run_file("microkanren.rkt")

def test_earley():
    run_file("earley.rkt", ("(test 14)", "(test 3)"))

def test_triangle():
    run_file("triangle.rkt", ("1000000", "10"))

#def test_minikanren():
#    run_file("minikanren.sch")
