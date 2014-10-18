#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
from pycket.expand import load_json_ast_rpython, expand_to_ast, PermException
from pycket.interpreter import interpret_one, ToplevelEnv, interpret_module, GlobalConfig
from pycket.error import SchemeException
from pycket.option_helper import parse_args, ensure_json_ast
from pycket.values_string import W_String

from rpython.rlib import jit

# _____ Define and setup target ___

def entry_point(argv):
    try:
        return actual_entry(argv)
    except SchemeException, e:
        print "ERROR:"
        print e.format_error()
        raise # to see interpreter-level traceback

def actual_entry(argv):
    jit.set_param(None, "trace_limit", 20000)

    config, names, args, retval = parse_args(argv)
    if retval != 0 or config is None:
        return retval
    args_w = [W_String.fromstr_utf8(arg) for arg in args]
    module_name, json_ast = ensure_json_ast(config, names)
    if json_ast is None:
        ast = expand_to_ast(module_name) 
    else:
        ast = load_json_ast_rpython(json_ast)
    GlobalConfig.load(ast)
    env = ToplevelEnv()
    env.commandline_arguments = args_w
    env.module_env.add_module(module_name, ast)
    val = interpret_module(ast, env)
    return 0

def target(driver, args):
    if "--with-branch" in args:
        import subprocess
        base_name = subprocess.check_output(["git", "rev-parse", "--abbrev-ref", "HEAD"]).strip()
    else:
        base_name = 'pycket'
    if driver.config.translation.jit:
        driver.exe_name = base_name + '-%(backend)s'
    else:
        driver.exe_name = base_name + '-%(backend)s-nojit'
    return entry_point, None
