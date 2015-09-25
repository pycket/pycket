
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

# TODO Find a beter name for this
class CachingMap(object):
    """ A map implementation which partitions its data into two groups, a collection
    of static data stored in the map itself, and a collection of indexes used to
    index into a corresponding data array.

    This partitioning allows structures such as impersonators to share not just
    their layout but common data as well.
    """
    _immutable_fields_ = ['indexes', 'static_data', 'static_submaps', 'dynamic_submaps']
    _attrs_ = ['indexes', 'static_data', 'static_submaps', 'dynamic_submaps']

    def __init__(self):
        self.indexes = {}
        self.static_data = {}
        self.dynamic_submaps = {}
        self.static_submaps = {}

    def iterkeys(self):
        for key in self.indexes.iterkeys():
            yield key
        for key in self.static_data.iterkeys():
            yield key

    @jit.elidable
    def get_dynamic_index(self, name):
        return self.indexes.get(name, -1)

    @jit.elidable
    def get_static_data(self, name, default=None):
        return self.static_data.get(name, default)

    def lookup(self, name, storage, default=None):
        idx = self.get_dynamic_index(name)
        if idx != -1:
            return storage[idx]
        return self.get_static_data(name, default)

    @jit.elidable
    def add_static_attribute(self, name, value):
        assert name not in self.indexes and name not in self.static_data
        key = (name, value)
        if key not in self.static_submaps:
            newmap = CachingMap()
            newmap.indexes.update(self.indexes)
            newmap.static_data.update(self.static_data)
            newmap.static_data[name] = value
            self.static_submaps[key] = newmap
        return self.static_submaps[key]

    @jit.elidable
    def add_dynamic_attribute(self, name):
        assert name not in self.indexes and name not in self.static_data
        if name not in self.dynamic_submaps:
            newmap = CachingMap()
            newmap.indexes.update(self.indexes)
            newmap.static_data.update(self.static_data)
            newmap.indexes[name] = len(self.indexes)
            self.dynamic_submaps[name] = newmap
        return self.dynamic_submaps[name]

CachingMap.EMPTY = CachingMap()
