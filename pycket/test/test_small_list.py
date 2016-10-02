
import random
from itertools import chain
from pycket.small_list_alt import small_list, FakeSpace, partition

def random_data(type, n):
    ints = range(10)
    vals = []
    for i in range(n):
        if type == 'p':
            vals.append(object())
        elif type == 'i':
            vals.append(random.choice(ints))
        else:
            assert type == 'f'
            vals.append(random.random())

    return vals

def test_small_list():

    @small_list(sizemax=10, space=FakeSpace)
    class X(object):
        def __init__(self, val):
            self.val = val

    root = object()

    for N in range(10):
        for layout in partition(N, ['p', 'i', 'f']):
            data = list(chain(*[random_data(*d) for d in layout]))
            random.shuffle(data)
            obj = X._make(root, data, 42)

            assert obj.val == 42

            spec = obj._map.layout_spec()
            assert zip(['p', 'i', 'f'], spec) == layout

            for i in range(N):
                assert data[i] == obj._get_list(i)
