#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

from pycket.test.testhelper import run_expr
from pycket.test.utils import string_to_sexp

def test_doctest(doctest):
    r"""
    ! (define-values (a) 1)
    > (+ 3 1)
    4
    > (+ a a)
    2
    """

@pytest.mark.comp
def test_string_to_sexp_complex_numbers(doctest):
    r"""
    > (/ 2+3i 2)
    1+3/2i
    > (/ 2+3i 3-4i)
    -6/25+17/25i
    > (/ -2-3i -4+5i)
    -7/41+22/41i
    > (= 5+5i 5+5.0i)
    #t
    """

@pytest.mark.skipif(not pytest.config.load_expander, reason="quote issue")
def test_string_to_sexp(doctest):
    r"""
    > (car '(red round))
    'red
    """    
