#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.config import get_testing_config
from pycket.entry_point import make_entry_point, target, get_additional_config_options

from rpython.jit.metainterp.test.support import LLJitMixin
from rpython.jit.metainterp.warmspot import get_stats
from pycket.racket_entry import dev_mode_metainterp

# 1

# The following call fails :
# -- on PyPy with a TypeError: 'GcForwardReference' object is not hashable
# -- on CPython it passes everything (annotation, rtyper etc) and fails at runtime with
#      AttributeError: 'NoneType' object has no attribute '_getattr'

LLJitMixin().meta_interp(dev_mode_metainterp, [], listcomp=True, listops=True, backendopt=True)

# 2
from rpython.translator.driver import TranslationDriver
from rpython.config.translationoption import get_combined_translation_config

# pypy targetpycket.py --linklets --verbose 2 -I racket/kernel/init -e "1"
entry_flags_1 = ['--linklets', '--verbose', '2', '-I', 'racket/kernel/init', '-e', '1']

def interp_w_1():
    # driver = TranslationDriver()
    # optiondescr = get_additional_config_options()
    # config = get_combined_translation_config(
    #             optiondescr,
    #             existing_config=driver.config,
    #             translating=True)
    # driver.config = config
    # f, _1, _2  = target(driver, [])
    # f(['--linklets', '--verbose', '2', '-I', 'racket/kernel/init', '-e', '1'])

    ## sys.argv = ['--linklets', '--verbose', '2', '-I', 'racket/kernel/init', '-e', '1']
    # from pycket.__main__ import main
    # main()

    make_entry_point(get_testing_config())(entry_flags_1)

## LLJitMixin().meta_interp(interp_w_1, [], listcomp=True, listops=True, backendopt=True)


# 3

# pypy targetpycket.py --linklets --dev
entry_flags_2 = ['--linklets', '--dev']

def interp_w_2():
    make_entry_point(get_testing_config())(entry_flags_2)

## LLJitMixin().meta_interp(interp_w_2, [], listcomp=True, listops=True, backendopt=True)
