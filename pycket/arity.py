from rpython.rlib import jit

AT_LEAST_CACHE = {}

class Arity(object):
    _immutable_fields_ = ['arity_list[*]', 'at_least']

    def __init__(self, arity_list, at_least):
        self.arity_list = arity_list
        self.at_least = at_least

    @jit.elidable
    def list_includes(self, arity):
        return arity in self.arity_list

    @staticmethod
    def geq(n):
        lup = AT_LEAST_CACHE.get(n, None)
        if lup is None:
            AT_LEAST_CACHE[n] = lup = Arity([], n)
        return lup

    @staticmethod
    def fixed(*lst):
        return Arity(list(lst), -1)

Arity.unknown = Arity([], 0)

for i in range(10):
    setattr(Arity, "geq_%d" % i, Arity.geq(i))
