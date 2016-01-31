#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# _____ Define and setup target ___

import os
from rpython.rlib          import jit, objectmodel
from rpython.rlib.nonconst import NonConstant

def make_entry_point(pycketconfig=None):
    from pycket.expand import load_json_ast_rpython, expand_to_ast, PermException, ModTable
    from pycket.interpreter import interpret_one, ToplevelEnv, interpret_module
    from pycket.error import SchemeException
    from pycket.option_helper import parse_args, ensure_json_ast
    from pycket.values_string import W_String


    def entry_point(argv):
        if not objectmodel.we_are_translated():
            import sys
            sys.setrecursionlimit(10000)
        try:
            return actual_entry(argv)
        except SchemeException, e:
            print "ERROR:"
            print e.format_error()
            raise # to see interpreter-level traceback

    def actual_entry(argv):
        jit.set_param(None, "trace_limit", 1000000)
        jit.set_param(None, "threshold", 131)
        jit.set_param(None, "trace_eagerness", 50)

        if NonConstant(False):
            # Hack to give os.open() the correct annotation
            os.open('foo', 1, 1)

        config, names, args, retval = parse_args(argv)
        if retval != 0 or config is None:
            return retval
        args_w = [W_String.fromstr_utf8(arg) for arg in args]
        module_name, json_ast = ensure_json_ast(config, names)

        modtable = ModTable()
        modtable.enter_module(module_name)
        if json_ast is None:
            if 'byteRecursive' in names:
                #print "json_ast is None, going for byteRecursive"
                ast = expand_to_ast(module_name, modtable, "-l pycket/zoTransform --")
            else:
                #print "json_ast is None, regular go"
                ast = expand_to_ast(module_name, modtable)
        else:
            if 'byteRecursive' in names:
                #print "going for byte recursive"
                ast = load_json_ast_rpython(json_ast, modtable, "-l pycket/zoTransform --")
            else:
                #print "regular go"
                ast = load_json_ast_rpython(json_ast, modtable)

        modtable.exit_module(module_name, ast)

        env = ToplevelEnv(pycketconfig)
        env.globalconfig.load(ast)
        env.commandline_arguments = args_w
        env.module_env.add_module(module_name, ast)
        try:
            val = interpret_module(ast, env)
        finally:
            from pycket.prims.input_output import shutdown
            if config.get('save-callgraph', False):
                with open('callgraph.dot', 'w') as outfile:
                    env.callgraph.write_dot_file(outfile)
            shutdown(env)
        return 0
    return entry_point

def target(driver, args): #pragma: no cover
    from rpython.config.config import to_optparse
    from pycket.config import expose_options, compute_executable_suffix
    config = driver.config
    parser = to_optparse(config, useoptions=["pycket.*"])
    parser.parse_args(args)
    if config.pycket.with_branch:
        import subprocess
        base_name = subprocess.check_output(["git", "rev-parse", "--abbrev-ref", "HEAD"]).strip()
    else:
        base_name = 'pycket'
    base_name += '-%(backend)s'
    if not config.translation.jit:
        base_name += '-%(backend)s-nojit'

    driver.exe_name = base_name + compute_executable_suffix(config)
    # it's important that the very first thing we do, before importing anything
    # else from pycket is call expose_options
    expose_options(config)
    entry_point = make_entry_point(config)
    return entry_point, None

def get_additional_config_options(): #pragma: no cover
    from pycket.config import pycketoption_descr
    return pycketoption_descr

take_options = True
