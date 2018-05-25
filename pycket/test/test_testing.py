#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

from pycket.test.testhelper import run_expr
from pycket.test.utils import string_to_sexp

@pytest.mark.hey
def test_doctest(doctest):
    r"""
    ! (define-values (a) 1)
    > (+ 3 1)
    4
    > (+ a a)
    2
    """

#@pytest.mark.hey
@pytest.mark.skip(reason="not yet")
def test_string_to_sexp(doctest):
    r"""
    > (car '(red round))
    'red
    """    
