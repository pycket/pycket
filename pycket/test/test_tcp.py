import pytest

from pycket.values          import w_true, w_false
from pycket.test.testhelper import run
from pycket.prims.tcp       import *

def test_1():
    run("(tcp-listener? (cons 1 2))", w_false, False,
        '(require "pycket/pycket-lang/tcp.rkt")')
    run("(tcp-socket? (cons 3 4))", w_false, False,
        '(require "pycket/pycket-lang/tcp.rkt")')

def test_2(doctest):
    """
    ! (require "pycket/pycket-lang/tcp.rkt")
    > (tcp-listener? (cons 1 2))
    #f
    > (tcp-socket? (cons 3 4))
    #f
    """
