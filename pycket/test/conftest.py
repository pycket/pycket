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

def pytest_funcarg__doctest(request):
    from textwrap import dedent
    from pycket.test.testhelper import check_equal

    assert request.function.__doc__ is not None
    code = dedent(request.function.__doc__)
    lines = [s for s in code.splitlines() if s]
    exprs = []
    expect = []
    current_expr = ""
    for line in lines:
        if line[0] == ">":
            current_expr = line[2:]
        elif line[0] in " \t":
            current_expr += "\n" + line[2:]
        else:
            exprs.append(current_expr)
            current_expr = ""
            expect.append(line)
    pairs = []
    for pair in zip(exprs,expect):
        pairs.extend(pair)
    check_equal(*pairs)
    return True
