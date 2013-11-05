#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Test.
#

import sys
from rpython import conftest
class o:
    view = False
    viewloops = True
conftest.option = o
from rpython.jit.metainterp.test.test_ajit import LLJitMixin

import pytest
from pycket.expand import expand
from pycket.interpreter import *
from pycket.values import *


class TestLLtype(LLJitMixin):

    def test_reverse(self):

        ast = to_ast(expand("(letrec ([countdown (lambda (n) (if (< n 0) 1 (countdown (- n 1))))]) (countdown 1000))"))

        def interp_w():
            val = interpret_one(ast)
            assert isinstance(val, W_Fixnum)
            return val.value

        assert interp_w() == 1

        self.meta_interp(interp_w, [], listcomp=True, listops=True, backendopt=True)
