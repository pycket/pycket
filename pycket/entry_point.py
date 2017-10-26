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

def get_primitive(prim_name_str):
    from pycket.prims.expose import prim_env
    from pycket.values import W_Symbol
    from pycket.error import SchemeException

    prim_sym = W_Symbol.make(prim_name_str)
    if not prim_sym in prim_env:
        raise SchemeException("Primitive not found : %s" % prim_name_str)

    return prim_env[prim_sym]

def read_eval_print(expr_str, pycketconfig, sysconfig):
    from pycket.interpreter import check_one_val
    from pycket.values import W_Symbol, W_WrappedConsProper, w_null
    from pycket.values_string import W_String

    # First run (boot) to set things like (current-module-name-resolver) (o/w it's going to stay as the
    # "core-module-name-resolver" which can't recognize modules like 'racket/base (anything other than
    # things like '#%kernel really)

    boot = get_primitive("boot")
    boot.call_interpret([])

    flcl = get_primitive("find-library-collection-links")
    lib_coll_links = flcl.call_interpret([], pycketconfig, sysconfig)
    clcl = get_primitive("current-library-collection-links")
    clcl.call_interpret([lib_coll_links], pycketconfig, sysconfig)

    flcp = get_primitive("find-library-collection-paths")
    lib_coll_paths = flcp.call_interpret([], pycketconfig, sysconfig)
    clcp = get_primitive("current-library-collection-paths")
    clcp.call_interpret([lib_coll_paths], pycketconfig, sysconfig)

    # namespace-require the '#%kernel
    ns = get_primitive("namespace-require")
    ns.call_interpret([W_WrappedConsProper.make(W_Symbol.make("quote"),
                                                  W_WrappedConsProper.make(W_Symbol.make("#%kernel"),
                                                                           w_null))], pycketconfig, sysconfig)
    # get the read, eval, print, open-input-string primitives
    ev = get_primitive("eval")
    rd = get_primitive("read")
    ex = get_primitive("expand")
    pr = get_primitive("print")
    ois = get_primitive("open-input-string")

    # Start calling
    # open-input-string
    str_port = check_one_val(ois.call_interpret([W_String.make(expr_str)], pycketconfig, sysconfig))
    # read
    sexp = check_one_val(rd.call_interpret([str_port], pycketconfig, sysconfig))
    # expand
    #expanded = check_one_val(ex.call_interpret([sexp], pycketconfig, sysconfig))
    # eval
    results = check_one_val(ev.call_interpret([sexp], pycketconfig, sysconfig))  # FIXME handle multiple values

    
    # print
    pr.call_interpret([results], pycketconfig, sysconfig)
    pr.call_interpret([W_String.make("\n")], pycketconfig, sysconfig)
    
    return

def make_entry_point(pycketconfig=None):
    from pycket.expand import JsonLoader, ModuleMap, PermException
    from pycket.prims.linklet import W_Linklet
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

        env = ToplevelEnv(pycketconfig)
        env.commandline_arguments = args_w

        # load the expander
        expander_linkl, sys_config = W_Linklet.load_linklet("expander.rktl", reader)
        expander_instance = expander_linkl.instantiate([], config=pycketconfig)
        expander_instance.provide_all_exports_to_prim_env()
        
        env.current_linklet_instance = expander_instance

        if 'rep' in config:
            
            expr_str = names['expr']
            read_eval_print(expr_str, pycketconfig, sys_config)

            from pycket.prims.input_output import shutdown
            for callback in POST_RUN_CALLBACKS:
                callback(config, env)
            shutdown(env)
            return 0

        if json_ast is None:
            ast = reader.expand_to_ast(module_name)
        else:
            ast = reader.load_json_ast_rpython(module_name, json_ast)

        env.globalconfig.load(ast)
        env.module_env.add_module(module_name, ast)
        
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
