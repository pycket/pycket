
import pytest

from pycket.hidden_classes     import make_map_type
from pycket.impersonators.base import W_ImpPropertyDescriptor

def test_map():
    empty = make_map_type("__getitem__").EMPTY
    map = empty.add_attribute(1).add_attribute(2).add_attribute(3)
    assert map.get_index(1) == 0
    assert map.get_index(2) == 1
    assert map.get_index(3) == 2
    assert map.get_index(4) == -1
    assert map.storage_size() == 3

    assert 1 in empty.other_maps
    assert 2 in empty.other_maps[1].other_maps
    assert 3 in empty.other_maps[1].other_maps[2].other_maps

    map_ = empty.add_attribute(2).add_attribute(1).add_attribute(3)
    assert map_ is not map
    assert map_.get_index(2) == 0
    assert map_.get_index(1) == 1
    assert map_.get_index(3) == 2

def test_map_descriptors():
    empty = make_map_type("__getitem__").EMPTY
    a = W_ImpPropertyDescriptor("a")
    b = W_ImpPropertyDescriptor("b")
    c = W_ImpPropertyDescriptor("c")
    d = W_ImpPropertyDescriptor("d")
    map = empty.add_attribute(a).add_attribute(b).add_attribute(c)

    assert map.get_index(a) == 0
    assert map.get_index(b) == 1
    assert map.get_index(c) == 2
    assert map.get_index(d) == -1
    assert map.storage_size() == 3

    assert a in empty.other_maps
    assert b in empty.other_maps[a].other_maps
    assert c in empty.other_maps[a].other_maps[b].other_maps

    map_ = empty.add_attribute(b).add_attribute(a).add_attribute(c)
    assert map_ is not map
    assert map_.get_index(b) == 0
    assert map_.get_index(a) == 1
    assert map_.get_index(c) == 2

