#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Testing environmental and external behavior
#
import pytest
from pycket.test.test_basic import run_fix, run
from pycket.targetpycket import main
from rpython.rlib import jit

def pytest_funcarg__json(request):
    tmpdir = request.getfuncargvalue('tmpdir')
    assert request.function.__doc__ is not None
    json = tmpdir / "ast.json"
    json.write(request.function.__doc__)
    return str(json)

def pytest_funcarg__empty_json(request):
    def make_filename():
        import inspect, py
        module_file = inspect.getmodule(request.function).__file__
        return str(py.path.local(module_file).dirpath("empty.json"))
    return request.cached_setup(setup=make_filename, scope="session")


class TestCommandline(object):

    def test_no_argv(self):
        assert main(['arg0']) == 3

    def test_one_arg(self, empty_json):
        assert main(['arg0', empty_json]) == 0

    def test_jitarg_fail(self, empty_json):
        with pytest.raises(ValueError):
            main(['arg0', '--jit', empty_json])
        assert main(['arg0', empty_json, '--jit']) == 2

    def test_jitarg_works(self, empty_json):
        assert main(['arg0', '--jit', 'trace_limit=30000',empty_json]) == 0
        assert main(['arg0', empty_json, '--jit', 'trace_limit=30000']) == 0
