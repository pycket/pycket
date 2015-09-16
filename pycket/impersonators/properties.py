
from rpython.rlib import jit

class Map(object):

    _immutable_fields_ = ['indexes', 'other_maps']
    _attrs_ = ['indexes', 'other_maps']

    def __init__(self):
        self.indexes = {}
        self.other_maps = {}

    @jit.elidable
    def get_index(self, name):
        return self.indexes.get(name, -1)

    @jit.elidable
    def add_attribute(self, name):
        if name not in self.other_maps:
            newmap = Map()
            newmap.indexes.update(self.indexes)
            newmap.indexes[name] = len(self.indexes)
            self.other_maps[name] = newmap
        return self.other_maps[name]

    def storage_size(self):
        return len(self.indexes)

    @staticmethod
    def new_empty_map():
        return Map()

