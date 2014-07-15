#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
from pycket.expand import load_json_ast_rpython
from pycket.interpreter import interpret_one, ToplevelEnv, interpret_module, GlobalConfig
from pycket.error import SchemeException
from pycket.option_helper import parse_args, ensure_json_ast
from pycket.values import W_String

from rpython.rlib import jit

# _____ Define and setup target ___

def entry_point(argv):
    jit.set_param(None, "trace_limit", 20000)

    config, names, args, retval = parse_args(argv)
    if retval != 0 or config is None:
        return retval
    args_w = [W_String(arg) for arg in args]
    module_name, json_ast = ensure_json_ast(config, names)
    if json_ast is None:
        raise RuntimeError("can't generate json ast file %s "%(json_ast))

    ast = load_json_ast_rpython(json_ast)
    try:
        GlobalConfig.load(ast)
        env = ToplevelEnv()
        env.commandline_arguments = args_w
        env.module_env.add_module(module_name, ast)
        val = interpret_module(ast, env)
    except SchemeException, e:
        print "ERROR:", e.msg
        raise # to see interpreter-level traceback
    else:
        # print val.tostring()
        return 0

def target(driver, args):
    if driver.config.translation.jit:
        driver.exe_name = 'pycket-%(backend)s'
    else:
        driver.exe_name = 'pycket-%(backend)s-nojit'
    return entry_point, None
