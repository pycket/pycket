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
    BoolOption("with_branch", "but the git branch name into the executable name",
               default=False, cmdline="--with-branch"),
    BoolOption("track_header", "track loops headers instead of last AST element",
               default=False, cmdline="--track-header"),
    BoolOption("type_specialization", "type specialize data structures (vectors, cons cells, cells, hashmaps, environments, conts etc)",
               default=True, cmdline="--type-specialization"),
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
        if not config.two_state:
            res.append("-no-loop")
        else:
            res.append("-no-callgraph")
    elif not config.two_state:
        res.append("-no-two-state")
    if not config.type_specialization:
        res.append("-no-type-specialization")
    if config.fuse_conts:
        res.append("-fuse-conts")
    return "".join(res)


# ______________________________________________________________________________
# XXX if at all possible, the config object (stored on the toplevel env) should
# be accessed directly. For very few config options this is not practical, so
# we expose them here. this code must not be changed, the flags are mutated
# from entry_point.

exposed_options = ['type_specialization']

def expose_options(config):
    import sys
    assert 'pycket.small_list' not in sys.modules, "pycket.config imported too late"
    for name in exposed_options:
        globals()[name] = getattr(config.pycket, name)

expose_options(get_testing_config()) # expose defaults
