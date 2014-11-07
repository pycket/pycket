from rpython.rlib import jit

class Arity(object):
    _immutable_fields_ = ['arity_list[*]', 'at_least']

    def __init__(self, arity_list, at_least):
        self.arity_list = arity_list
        self.at_least = at_least

    @jit.elidable
    def list_includes(self, arity):
        return arity in self.arity_list

Arity.unknown = Arity([], 0)
