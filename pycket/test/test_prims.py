#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

from pycket.test.testhelper import run_expr

def test_format(doctest):
    r"""
    > (format "a")
    "a"
    ;E (format "a~a")
    ;E (format "a" 1)
    ;> (format "~~~n~%")
    ;"~\n\n"
    > (format "abc~adef~aghi" 1 2)
    "abc1def2ghi"
    ;> (format "in : ~.s" 4)
    ;"4"
    """
