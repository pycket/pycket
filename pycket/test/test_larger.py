import pytest
from pycket.expand import expand, expand_string, to_ast
from pycket.pycket_json import loads
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

def test_bubble_unsafe2():
    run_file("bubble-unsafe2.rkt", ("10000", "100"))

#def test_pseudoknot():
#    run_file("nucleic2.sch", ("(i 100)", "(i 1)"))

def test_microkanren():
    run_file("microkanren.rkt")

def test_earley():
    run_file("earley.rkt", ("(test 14)", "(test 3)"))

def test_triangle():
    run_file("triangle.rkt", ("1000000", "10"))

def test_imp_predicates():
    run_file("imp-predicates.rkt")

def test_imp_procedures():
    run_file("imp-proc.rkt")

def test_imp_box():
    run_file("imp-box.rkt")

def test_imp_vector():
    run_file("imp-vector.rkt", ("my-len 100", "my-len 2"), ("max-val 100", "max-val 2"))

def test_imp_struct():
    run_file("imp-struct.rkt")

def test_thread_cells():
    run_file("test-thread-cell.rkt")

def test_impersonator_properties():
    run_file("impersonator-properties.rkt")

def test_reverse():
    # might be a regression
    run_file("test_reverse.rkt")

#def test_minikanren():
#    run_file("minikanren.sch")
