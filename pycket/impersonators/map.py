
from rpython.rlib import jit

def make_map_type():

    class Map(object):
        """ A basic implementation of a map which assigns Racket values to an index
        based on the identity of the Racket value. A Map consists of

        * indexes: a map from objects to indicies for object described by the current map
        * other_maps: sub maps which are extensions the current map
        """

        _immutable_fields_ = ['indexes', 'other_maps']
        _attrs_ = ['indexes', 'other_maps']

        def __init__(self):
            self.indexes = {}
            self.other_maps = {}

        def __iter__(self):
            return self.indexes.iteritems()

        def iterkeys(self):
            return self.indexes.iterkeys()

        def itervalues(self):
            return self.indexes.itervalues()

        def iteritems(self):
            return self.indexes.iteritems()

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

    Map.EMPTY = Map()

    return Map

