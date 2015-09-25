
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

        def lookup(self, name, storage, default=None):
            idx = self.get_index(name)
            if idx == -1:
                return default
            return storage[idx]

        @jit.elidable
        def add_attribute(self, name):
            if name not in self.other_maps:
                newmap = Map()
                newmap.indexes.update(self.indexes)
                newmap.indexes[name] = len(self.indexes)
                self.other_maps[name] = newmap
            return self.other_maps[name]

        add_static_attribute  = 0
        add_dynamic_attribute = add_attribute

        def storage_size(self):
            return len(self.indexes)

    Map.EMPTY = Map()

    return Map

class Counter(object):
    def __init__(self, value=0):
        self._value = value
    def inc(self):
        self._value += 1

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

    COUNTER = Counter(-1)

    def __init__(self):
        self.indexes = {}
        self.static_data = {}
        self.dynamic_submaps = {}
        self.static_submaps = {}
        CachingMap.COUNTER.inc()

    def iterkeys(self):
        for key in self.indexes.iterkeys():
            yield key
        for key in self.static_data.iterkeys():
            yield key

    def iteritems(self):
        for item in self.indexes.iteritems():
            yield item
        for item in self.static_data.iteritems():
            yield item

    def itervalues(self):
        for val in self.indexes.itervalues():
            yield val
        for val in self.static_data.itervalues():
            yield val

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

    def is_leaf(self):
        return not self.indexes and not self.static_data

    def compute_path_data(self):
        count_data = []
        self._compute_path_data(count_data)
        return count_data[:]

    def _compute_path_data(self, count_data, static=0, dynamic=0):
        if self.is_leaf():
            count_data.append((static, dynamic))
            return
        for sub in self.static_submaps.itervalues():
            sub._compute_path_data(count_data, static + 1, dynamic)
        for sub in self.dynamic_submaps.itervalues():
            sub._compute_path_data(count_data, static, dynamic + 1)

CachingMap.EMPTY = CachingMap()
