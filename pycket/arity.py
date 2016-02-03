#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.util  import memoize
from rpython.rlib import jit

class Arity(object):
    _immutable_fields_ = ['arity_list[*]', 'at_least']

    def __init__(self, arity_list, at_least):
        self.arity_list = arity_list
        self.at_least = at_least

    @jit.elidable
    def list_includes(self, arity):
        return arity in self.arity_list

    @jit.elidable
    def arity_includes(self, arity):
        return ((self.at_least != -1 and arity >= self.at_least) or
                self.list_includes(arity))

    @jit.elidable
    def shift_arity(self, shift):
        arity_list = [i + shift for i in self.arity_list if i + shift > -1]
        at_least = max(self.at_least + shift, -1)
        return Arity(arity_list, at_least)

    @staticmethod
    @memoize
    def geq(n):
        return Arity([], n)

    @staticmethod
    @memoize
    def oneof(*lst):
        return Arity(list(lst), -1)

Arity.unknown = Arity.geq(0)
Arity.ZERO    = Arity.oneof(0)
Arity.ONE     = Arity.oneof(1)
Arity.TWO     = Arity.oneof(2)
Arity.THREE   = Arity.oneof(3)

