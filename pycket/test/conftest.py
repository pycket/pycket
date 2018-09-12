#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# conftest - configuring pytest, especially funcargs
#
import pycket.config # to configure early
import random

import pytest
from rpython.rlib.objectmodel import specialize, we_are_translated

def replace(func):
    @specialize.call_location()
    def replacement(*args):
        if we_are_translated():
            return func(*args)
        return bool(random.getrandbits(1))
    replacement.__name__ = func.__name__ + "_replaced"
    return replacement

def pytest_addoption(parser):
    parser.addoption('--bytecode', action='store', default='', help='Run pycket with bytecode expansion')
    parser.addoption('--random', action='store_true', help='Override functions in rpython.rlib.jit.py to test special cases for the JIT')
    parser.addoption('--new', action='store_true', default=False, help='Use the entry point of the NEW Pycket')
    parser.addoption('--use-expander', action='store_true', default=False, help='Run the tests using the reader and evaluator from expander linklet')

def pytest_configure(config):
    if config.getvalue('random'):
        from rpython.rlib import jit
        jit.isconstant = replace(jit.isconstant)
        jit.isvirtual = replace(jit.isvirtual)
        # XXX: Being able to patch we_are_jitted would be nice as well,
        # but too much code depends on it behaving deterministically

    config.byte_option = False
    config.new_pycket = False
    config.load_expander = False

    if config.getvalue('--new'):
        print("\nTesting with NEW Pycket... ")
        config.new_pycket = True
        if config.getvalue('--use-expander'):
            print("using the expander linklet...\n")
            config.load_expander = True
        else:
            print("WITHOUT using the expander linklet...\n")
    else:
        print("\nTesting with OLD Pycket... ")

def pytest_funcarg__racket_file(request):
    tmpdir = request.getfuncargvalue('tmpdir')
    name = 'prog.rkt'
    assert request.function.__doc__ is not None
    file_name = tmpdir / name
    file_name.write(request.function.__doc__)
    return str(file_name)

def pytest_funcarg__empty_json(request):
    def make_filename():
        import inspect, py
        module_file = inspect.getmodule(request.function).__file__
        return str(py.path.local(module_file).dirpath("empty.json"))
    return request.cached_setup(setup=make_filename, scope="session")

def pytest_funcarg__source(request):
    assert request.function.__doc__ is not None
    code = request.function.__doc__
    return code

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
        with pytest.raises(Exception):
            execute(error, extra="\n".join(setup))
    return True
