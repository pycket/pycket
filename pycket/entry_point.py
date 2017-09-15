#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# _____ Define and setup target ___

from rpython.rlib import jit, objectmodel

POST_RUN_CALLBACKS = []

def register_post_run_callback(callback):
    """
    Registers functions to be called after the user program terminates.
    This is mostly useful for defining debugging/logging hooks to print out
    runtime stats after the program is done.
    """
    POST_RUN_CALLBACKS.append(callback)
    return callback

@register_post_run_callback
def save_callgraph(config, env):
    if config.get('save-callgraph', False):
        with open('callgraph.dot', 'w') as outfile:
            env.callgraph.write_dot_file(outfile)

def make_entry_point(pycketconfig=None):
    from pycket.expand import JsonLoader, ModuleMap, PermException
    from pycket.prims.linklet import Linklet
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
        jit.set_param(None, "max_unroll_loops", 15)

        config, names, args, retval = parse_args(argv)
        if retval != 0 or config is None:
            return retval
        args_w = [W_String.fromstr_utf8(arg) for arg in args]
        module_name, json_ast = ensure_json_ast(config, names)

        entry_flag = 'byte-expand' in names
        multi_mod_flag = 'multiple-modules' in names

        multi_mod_map = ModuleMap(json_ast) if multi_mod_flag else None

        reader = JsonLoader(bytecode_expand=entry_flag,
                            multiple_modules=multi_mod_flag,
                            module_mapper=multi_mod_map)

        if json_ast is None:
            ast = reader.expand_to_ast(module_name)
        else:
            ast = reader.load_json_ast_rpython(module_name, json_ast)

        env = ToplevelEnv(pycketconfig)
        env.globalconfig.load(ast)
        env.commandline_arguments = args_w
        env.module_env.add_module(module_name, ast)
        
        expander_linkl = Linklet.load_linklet("expander.rktl", reader)
        expander_instance = expander_linkl.instantiate(env, [])
        expander_instance.provide_all_exports_to_prim_env()
        
        env.current_linklet_instance = expander_instance
        
        try:
            val = interpret_module(ast, env)
        finally:
            from pycket.prims.input_output import shutdown
            for callback in POST_RUN_CALLBACKS:
                callback(config, env)
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
