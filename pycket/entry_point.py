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
    from pycket.env import w_global_config
    if config.get('save-callgraph', False):
        with open('callgraph.dot', 'w') as outfile:
            w_global_config.callgraph.write_dot_file(outfile)

def make_entry_point(pycketconfig=None):
    from pycket.expand import JsonLoader, ModuleMap, PermException
    from pycket.interpreter import ToplevelEnv, interpret_one, interpret_module
    from pycket.error import SchemeException, ExitException
    from pycket.option_helper import parse_args, JUST_EXIT, RETURN_OK, MISSING_ARG
    from pycket.old_pycket_option_helper import parse_args as old_pycket_parse_args, ensure_json_ast
    from pycket.values_string import W_String
    from pycket.racket_entry import load_inst_linklet, racket_entry

    def entry_point(argv):
        if not objectmodel.we_are_translated():
            import sys
            sys.setrecursionlimit(10000)

            if pycketconfig is not None:
                from rpython.config.config import to_optparse
                if '--linklets' in argv:
                    to_optparse(pycketconfig).parse_args(['--linklets'])

        try:
            if pycketconfig is not None and pycketconfig.pycket.linklets:
                return actual_entry(argv)
            else:
                return old_pycket_actual_entry(argv)
        except SchemeException as e:
            print("ERROR:")
            print(e.format_error())
            raise # to see interpreter-level traceback

    def actual_entry(argv):
        jit.set_param(None, "trace_limit", 1000000)
        jit.set_param(None, "threshold", 131)
        jit.set_param(None, "trace_eagerness", 50)
        jit.set_param(None, "max_unroll_loops", 15)

        from pycket.env import w_global_config
        w_global_config.set_pycketconfig(pycketconfig)

        config, names, args, retval = parse_args(argv)

        if config['verbose']:
            level = int(names['verbosity_level'][0])
            w_global_config.set_config_val('verbose', level)

            if 'verbosity_keywords' in names:
                w_global_config.set_verbose_keywords(names['verbosity_keywords'])

            if 'not-implemented' in names:
                print("These flags are not implemented yet : %s" % names['not-implemented'])

        if retval == MISSING_ARG:
            print("Bad switch, or missing argument in : %s" % argv[1:])
            return 1

        if retval == JUST_EXIT or config is None:
            return RETURN_OK # exit normally

        if 'stdout_level' in names: # -O
            from pycket.prims.logging import w_main_logger
            w_main_logger.set_stdout_level(names['stdout_level'][0])

        if 'stderr_level' in names: # -W
            from pycket.prims.logging import w_main_logger
            w_main_logger.set_stderr_level(names['stderr_level'][0])

        if 'syslog_level' in names: # -L
            from pycket.prims.logging import w_main_logger
            w_main_logger.set_syslog_level(names['syslog_level'][0])

        current_cmd_args = [W_String.fromstr_utf8(arg) for arg in args]

        if 'json-linklets' in names:
            for linkl_json in names['json-linklets']:
                vvv = config['verbose']
                load_inst_linklet(linkl_json, debug=vvv)

        try:
            if not config['stop']:
                racket_entry(names, config, current_cmd_args)
        except ExitException as e:
            pass
        finally:
            from pycket.prims.input_output import shutdown
            env = ToplevelEnv(pycketconfig)
            #env.commandline_arguments = current_cmd_args
            for callback in POST_RUN_CALLBACKS:
                callback(config, env)
            shutdown(env)
        return 0

    def old_pycket_actual_entry(argv):
        jit.set_param(None, "trace_limit", 1000000)
        jit.set_param(None, "threshold", 131)
        jit.set_param(None, "trace_eagerness", 50)
        jit.set_param(None, "max_unroll_loops", 15)

        config, names, args, retval = old_pycket_parse_args(argv)
        if retval != 0 or config is None:
            return retval
        args_w = [W_String.fromstr_utf8(arg) for arg in args]
        module_name, json_ast = ensure_json_ast(config, names)

        from pycket.env import w_global_config
        from pycket.prims.general import make_stub_predicates_no_linklet
        w_global_config.set_linklet_mode_off()

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
