#! /usr/bin/env python
# -*- coding: utf-8 -*-

import pytest

from pycket.linklet_test.testhelper import run_expr

def test_format():
    run_expr("""(format "a")""", "a")
    #run_expr("""(format "a~a")""", "a")
    #run_expr("""(format "a" 1)""", "a")
    run_expr("""(format "~~~n~%")""", "~\n\n")
    run_expr("""(format "abc~adef~aghi" 1 2)""", "abc1def2ghi")
    run_expr("""(format "in : ~.s" 4)""", "in : 4") # issue #214
    
    r"""
    > (format "a")
    "a"
    E (format "a~a")
    E (format "a" 1)
    > (format "~~~n~%")
    "~\n\n"
    > (format "abc~adef~aghi" 1 2)
    "abc1def2ghi"
    """
