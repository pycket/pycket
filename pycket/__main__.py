#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
from rpython.rlib.objectmodel import we_are_translated
from rpython.translator.driver import TranslationDriver

from pycket.entry_point import target

import sys
import pdb, traceback

def main():
    assert not we_are_translated()
    tgt = target(TranslationDriver(), sys.argv)
    try:
        f, _1, _2  = tgt
    except TypeError:
        f = tgt
    except ValueError:
        f, _ =  tgt

    try:
        sys.exit(f(sys.argv))
    except SystemExit:
        raise
    except:
        _type, value, tb = sys.exc_info()
        traceback.print_exception(_type, value, tb)
        pdb.post_mortem(tb)

if __name__ == '__main__':
    main()
