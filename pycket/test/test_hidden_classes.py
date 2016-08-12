
import pytest

from pycket.hidden_classes                    import make_map_type
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

