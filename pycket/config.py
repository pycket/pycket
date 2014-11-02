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
               default=True, cmdline="--fuse-conts"),
    BoolOption("with_branch", "but the git branch name into the executable name",
               default=False, cmdline="--with-branch"),
])

def get_testing_config():
    return get_combined_translation_config(
            pycketoption_descr,
            translating=False)
