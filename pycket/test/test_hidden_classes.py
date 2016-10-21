
import pytest

from pycket.hidden_classes                    import make_map_type, make_caching_map_type
from pycket.impersonators.hidden_classes.base import W_ImpPropertyDescriptor

def test_map():
    empty = make_map_type("__getitem__", int).EMPTY
    maps = []
    map  = empty
    for i in range(1, 4):
        map = map.add_attribute(i)
        maps.append(map)
    assert map.get_index(1) == 0
    assert map.get_index(2) == 1
    assert map.get_index(3) == 2
    assert map.get_index(4) == -1
    assert map.storage_size() == 3

    assert empty.other_maps.get(1) is not None
    assert empty.other_maps.get(1).other_maps.get(2) is not None
    assert empty.other_maps.get(1).other_maps.get(2).other_maps.get(3) is not None

    map_ = empty.add_attribute(2).add_attribute(1).add_attribute(3)
    assert map_ is not map
    assert map_.get_index(2) == 0
    assert map_.get_index(1) == 1
    assert map_.get_index(3) == 2

def test_map_descriptors():
    empty = make_map_type("__getitem__", W_ImpPropertyDescriptor).EMPTY
    a = W_ImpPropertyDescriptor("a")
    b = W_ImpPropertyDescriptor("b")
    c = W_ImpPropertyDescriptor("c")
    d = W_ImpPropertyDescriptor("d")

    map  = empty
    maps = []
    for i in [a, b, c]:
        map = map.add_attribute(i)
        maps.append(map)

    assert map.get_index(a) == 0
    assert map.get_index(b) == 1
    assert map.get_index(c) == 2
    assert map.get_index(d) == -1
    assert map.storage_size() == 3

    assert empty.other_maps.get(a) is not None
    assert empty.other_maps.get(a).other_maps.get(b) is not None
    assert empty.other_maps.get(a).other_maps.get(b).other_maps.get(c) is not None

    map_ = empty.add_attribute(b).add_attribute(a).add_attribute(c)
    assert map_ is not map
    assert map_.get_index(b) == 0
    assert map_.get_index(a) == 1
    assert map_.get_index(c) == 2
    assert map_.get_index(d) == -1

def test_caching_map():
    import gc
    empty = make_caching_map_type("__getitem__", int).EMPTY
    map = empty
    for i in range(1, 4):
        gc.collect()
        map = map.add_dynamic_attribute(i)
    assert map.get_dynamic_index(1) == 0
    assert map.get_dynamic_index(2) == 1
    assert map.get_dynamic_index(3) == 2
    assert map.get_dynamic_index(4) == -1
    assert map.storage_size() == 3

    # Check to ensure we get the same hidden class
    # by adding the same attributes.
    # GC aggressively to ensure we are not messing
    # up due to use of weak hash tables
    map2 = empty
    for i in range(1, 4):
        gc.collect()
        map2 = map2.add_dynamic_attribute(i)
    assert map is map2

    assert empty.dynamic_submaps.get(1) is not None
    assert empty.dynamic_submaps.get(1).dynamic_submaps.get(2) is not None
    assert empty.dynamic_submaps.get(1).dynamic_submaps.get(2).dynamic_submaps.get(3) is not None

    map_ = empty.add_dynamic_attribute(2).add_dynamic_attribute(1).add_dynamic_attribute(3)
    assert map_ is not map
    assert map_.get_dynamic_index(2) == 0
    assert map_.get_dynamic_index(1) == 1
    assert map_.get_dynamic_index(3) == 2

def test_caching_map_descriptors():
    import gc
    empty = make_caching_map_type("__getitem__", W_ImpPropertyDescriptor).EMPTY
    a = W_ImpPropertyDescriptor("a")
    b = W_ImpPropertyDescriptor("b")
    c = W_ImpPropertyDescriptor("c")
    d = W_ImpPropertyDescriptor("d")

    map = empty
    for i in [a, b, c]:
        gc.collect()
        map = map.add_dynamic_attribute(i)

    assert map.get_dynamic_index(a) == 0
    assert map.get_dynamic_index(b) == 1
    assert map.get_dynamic_index(c) == 2
    assert map.get_dynamic_index(d) == -1
    assert map.storage_size() == 3

    map2 = empty
    for i in [a, b, c]:
        gc.collect()
        map2 = map2.add_dynamic_attribute(i)
    assert map is map2


    assert empty.dynamic_submaps.get(a) is not None
    assert empty.dynamic_submaps.get(a).dynamic_submaps.get(b) is not None
    assert empty.dynamic_submaps.get(a).dynamic_submaps.get(b).dynamic_submaps.get(c) is not None

    map_ = empty.add_dynamic_attribute(b).add_dynamic_attribute(a).add_dynamic_attribute(c)
    assert map_ is not map
    assert map_.get_dynamic_index(b) == 0
    assert map_.get_dynamic_index(a) == 1
    assert map_.get_dynamic_index(c) == 2
    assert map_.get_dynamic_index(d) == -1
