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
    > (let-values (((x) (open-output-string))) (begin (write (string->symbol "x") x) (get-output-string x)))
    "x"
    > (let-values (((x) (open-output-string))) (begin (write (cons (string->symbol "x") null) x) (get-output-string x)))
    "(x)"
    """

def test_write_and_len():
    def run_write(s, expected, length=None):
        run_expr("(let-values (((x) (open-output-string))) (begin (write "+s+" x) (get-output-string x)))", expected)
        if length:
            run_expr("(let-values (((x) (open-output-string))) (begin (write "+s+" x) (string-length (get-output-string x))))", length)
    run_expr("\"hello\"", "hello", 7)
    run_expr("\"\"", "", 2)
    run_expr("(hash 1 2)", "#hash((1 . 2))")
