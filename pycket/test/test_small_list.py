
import random
from itertools import chain
from pycket.small_list_alt import small_list, FakeSpace, partition

def random_data(type, n):
    ints = range(10)
    vals = []
    if type == 'p':
        for i in range(n):
            vals.append(object())
    elif type == 'i':
        for i in range(n):
            vals.append(random.choice(ints))
    elif type == 'f':
        for i in range(n):
            vals.append(random.random())
    return vals

def test_small_list():

    @small_list(sizemax=10, immutable=True, space=FakeSpace)
    class X(object):
        def __init__(self, val):
            self.val = val

    root = object()

    for i in range(10):
        for layout in partition(i, ['p', 'i', 'f']):
            data = list(chain(*[random_data(*d) for d in layout]))
            random.shuffle(data)
            obj = X._make(root, data, 42)

            assert obj.val == 42

            spec = obj._map.layout_spec()
            assert zip(['p', 'i', 'f'], spec) == layout

