#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# conftest - configuring pytest, especially funcargs
#

def _doctstring_tempfile_named(request, name):
    tmpdir = request.getfuncargvalue('tmpdir')
    assert request.function.__doc__ is not None
    file_name = tmpdir / name
    file_name.write(request.function.__doc__)
    return str(file_name)

def _make_ast(request, wrap=False, stdlib=False):
    from pycket.expand import expand, to_ast
    assert request.function.__doc__ is not None
    code = request.function.__doc__
    e = expand(code, wrap, stdlib)
    ast = to_ast(e)
    return ast


def pytest_funcarg__json(request):
    return _doctstring_tempfile_named(request, "ast.json")

def pytest_funcarg__racket_file(request):
    return _doctstring_tempfile_named(request, "prog.rkt")

def pytest_funcarg__empty_json(request):
    def make_filename():
        import inspect, py
        module_file = inspect.getmodule(request.function).__file__
        return str(py.path.local(module_file).dirpath("empty.json"))
    return request.cached_setup(setup=make_filename, scope="session")

def pytest_funcarg__ast(request):
    return _make_ast(request)
def pytest_funcarg__ast_wrap(request):
    return _make_ast(request, wrap=True)
def pytest_funcarg__ast_std(request):
    return _make_ast(request, wrap=True, stdlib=True)
