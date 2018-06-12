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

def test_write(doctest):
    r"""
    > (let-values (((x) (open-output-string))) (begin (write 7 x) (get-output-string x)))
    "7"
    > (let-values (((x) (open-output-string))) (begin (write (cons 1 2) x) (get-output-string x)))
    "(1 . 2)"
    > (let-values (((x) (open-output-string))) (begin (write (quote x) x) (get-output-string x)))
    "x"
    > (let-values (((x) (open-output-string))) (begin (write (cons (quote x) null) x) (get-output-string x)))
    "(x)"
    """

def test_write_and_len():
    def run_write(s, expected, length=None):
        run_expr("(let-values (((x) (open-output-string))) (begin (write "+s+" x) (get-output-string x)))", expected)
        if length:
            run_expr("(let-values (((x) (open-output-string))) (begin (write "+s+" x) (string-length (get-output-string x))))", length)
    run_write('"hello"', '"hello"', 7)
    run_write('""', '""', 2)
    run_write('(hash 1 2)', '#hash((1 . 2))')
    run_write('(quote (1 2 (3 . 4)))', '(1 2 (3 . 4))')

    if pytest.config.use_expander:
        run_write('"\\n"', '"\\n"')
        run_write("0.0f0", "0.0f0")
        run_write("0.0s0", "0.0f0")
        run_write('#rx#"y"', '#rx#"y"')
        run_write('#px#"y"', '#px#"y"')
        run_write('#px"y"', '#px"y"')
        run_write('#rx#"y\\n"', '#rx#"y\\n"')
