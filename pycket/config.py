#! /usr/bin/env python
# -*- coding: utf-8 -*-

from rpython.config.config import OptionDescription, BoolOption, IntOption, ArbitraryOption, FloatOption
from rpython.config.translationoption import get_combined_translation_config

pycketoption_descr = OptionDescription(
        "pycket", "Pycket Options", [
    BoolOption("two_state", "enable the two-state JIT driver",
               default=True, cmdline="--two-state"),
    BoolOption("callgraph", "enable dynamic callgraph reconstruction",
               default=True, cmdline="--callgraph"),
    BoolOption("log_callgraph", "log the callgraph decisions",
               default=False, cmdline="--log-callgraph",
               requires=[("pycket.callgraph", True)]),
    BoolOption("fuse_conts", "fuse the continuations",
               default=False, cmdline="--fuse-conts"),
    BoolOption("with_branch", "build the git branch name into the executable name",
               default=False, cmdline="--with-branch"),
    BoolOption("strategies", "strategies for data structures (vectors, cells, hashmaps, etc)",
               default=True, cmdline="--strategies"),
    BoolOption("type_size_specialization", "unbox small data structure fields and type specialize (environments, continuations, structs, cons cells, etc)",
               default=True, cmdline="--type-size-specialization"),
    BoolOption("prune_env", "prune environment",
               default=True, cmdline="--prune-env"),
    BoolOption("immutable_boolean_field_elision", "elide immutable boolean fields from structs",
               default=False, cmdline="--ibfe"),
])

def get_testing_config(**overrides):
    return get_combined_translation_config(
            pycketoption_descr,
            translating=False,
            overrides=overrides)


def compute_executable_suffix(config):
    config = config.pycket
    res = []
    if not config.callgraph:
        res.append("-no-callgraph")
    if not config.prune_env:
        res.append("-no-prune-env")
    if not config.two_state:
        res.append("-no-two-state")
    if not config.strategies:
        res.append("-no-strategies")
    if not config.type_size_specialization:
        res.append("-no-type-size-specialization")
    if config.fuse_conts:
        res.append("-fuse-conts")
    if config.log_callgraph:
        res.append("-log")
    if config.immutable_boolean_field_elision:
        res.append("-ibfe")
    return "".join(res)


# ______________________________________________________________________________
# XXX if at all possible, the config object (stored on the toplevel env) should
# be accessed directly. For very few config options this is not practical, so
# we expose them here. this code must not be changed, the flags are mutated
# from entry_point.

exposed_options = ['strategies',
                   'type_size_specialization',
                   'prune_env',
                   'immutable_boolean_field_elision',
]

def expose_options(config):
    import sys
    assert 'pycket.small_list' not in sys.modules, "pycket.config imported too late"
    for name in exposed_options:
        globals()[name] = getattr(config.pycket, name)

expose_options(get_testing_config()) # expose defaults
