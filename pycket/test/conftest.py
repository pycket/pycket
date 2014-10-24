#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# conftest - configuring pytest, especially funcargs
#
import pytest

def _doctstring_tempfile_named(request, name):
    tmpdir = request.getfuncargvalue('tmpdir')
    assert request.function.__doc__ is not None
    file_name = tmpdir / name
    file_name.write(request.function.__doc__)
    return str(file_name)

def _make_ast(request, wrap=False, stdlib=False):
    from pycket.expand import expand, to_ast, _to_module
    assert request.function.__doc__ is not None
    code = request.function.__doc__
    e = expand(code, wrap, stdlib)
    ast = _to_module(e)
    return ast

def _make_source(request):
    assert request.function.__doc__ is not None
    code = request.function.__doc__
    return code

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

def pytest_funcarg__source(request):
    return _make_source(request)

def pytest_funcarg__ast(request):
    return _make_ast(request)
def pytest_funcarg__ast_wrap(request):
    return _make_ast(request, wrap=True)
def pytest_funcarg__ast_std(request):
    return _make_ast(request, wrap=True, stdlib=True)

def pytest_funcarg__doctest(request):
    from textwrap import dedent
    from pycket.test.testhelper import check_equal, execute
    from pycket.error import SchemeException

    assert request.function.__doc__ is not None
    code = dedent(request.function.__doc__)
    lines = [lin for lin in code.splitlines() if lin]
    setup = []
    exprs = []
    expect = []
    errors = []
    current_let = []
    setup_done = False
    for line in lines:
        if line[0] == ";":
            # skip comments
            continue
        if not line.strip():
            continue
        elif line[0] == "!":
            if setup_done:
                raise RuntimeError("Don't use ! in midst of other doctest cmds")
            setup.append(line[2:])
        elif line[0] == ">":
            setup_done = True
            current_let.append(line[2:])
        elif line[0] == "E":
            errors.append(line[1:])
        elif line[0] in " \t":
            current_let[-1] += "\n" + line[2:]
        else:
            exprs.append(current_let[0] if len(current_let) == 1 else current_let)
            current_let = []
            expect.append(line)
    pairs = []
    for pair in zip(exprs,expect):
        pairs.extend(pair)
    check_equal(*pairs, extra="\n".join(setup))
    for error in errors:
        with pytest.raises(SchemeException):
            execute(error, extra="\n".join(setup))
    return True
