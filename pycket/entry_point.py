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
    from pycket.interpreter import ToplevelEnv
    from pycket.error import SchemeException
    from pycket.option_helper import parse_args
    from pycket.values_string import W_String
    from pycket.racket_entry import load_inst_linklet_json, racket_entry

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

        if config['version']:
            print "Welcome to Pycket\n"

        if retval != 0 or config is None:
            return retval

        current_cmd_args = [W_String.fromstr_utf8(arg) for arg in args]

        if 'json-linklets' in names:
            for linkl_json in names['json-linklets']:
                load_inst_linklet_json(linkl_json, pycketconfig)

        try:
            if not config['stop']:
                racket_entry(names, config, pycketconfig, current_cmd_args)
        finally:
            from pycket.prims.input_output import shutdown
            env = ToplevelEnv(pycketconfig)
            #env.commandline_arguments = current_cmd_args
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
