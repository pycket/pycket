
from pycket.hidden_classes import make_typed_map
from pycket.util           import add_copy_method
from rpython.rlib          import jit, debug, objectmodel
from rpython.rlib.unroll   import unrolling_iterable

add_clone_method = add_copy_method("_clone")

def partition(N, partitions):
    "NOT RPYTHON"
    assert partitions
    if len(partitions) == 1:
        curr, = partitions
        yield [(curr, N)]
        return

    curr = partitions[0]
    rest = partitions[1:]
    for i in range(N, -1, -1):
        remainder = N - i
        for j in partition(remainder, rest):
            yield [(curr, i)] + j

xs = list(partition(10, partitions=['p', 'i', 'f']))

for lst in xs:
    assert sum(l[1] for l in lst) == 10

def attr_names(prefix, N, type):
    """NOT RPYTHON"""
    attrs = ["%s_%s_%s" % (prefix, i, type) for i in range(N)]
    unroll = unrolling_iterable(enumerate(attrs))
    return attrs, unroll

def _make_get_list(enum_attrs, suffix, space):
    def _get_list(self, i):
        for j, attr in enum_attrs:
            if j == i:
                val = getattr(self, attr)
                return space.wrap(val)
        raise IndexError
    _get_list.__name__ += suffix
    return _get_list

def _make_set_list(enum_attrs, suffix, space, nonull=False):
    unwrap_attr = "unwrap_%s" % suffix
    def _set_list(self, i, val):
        for j, attr in enum_attrs:
            v = getattr(space, unwrap_attr)(val)
            if j == i:
                if nonull:
                    assert val is not None
                setattr(self, attr, v)
                return
        raise IndexError
    _set_list.__name__ += suffix
    return _set_list

class FakeSpace(object):
    # Fake object space for testing

    root_type = object

    @staticmethod
    def unwrap(x):
        """NOT RPYTHON"""
        return x

    unwrap_p = unwrap
    unwrap_i = unwrap
    unwrap_f = unwrap

    @staticmethod
    def wrap(x):
        """NOT RPYTHON"""
        return x

    # Assigns objects to one of three types 'p', 'i', and 'f'
    # for pointers, integers, and floats.
    @staticmethod
    def typeof(x):
        """NOT RPYTHON"""
        if isinstance(x, int):
            return 'i'
        if isinstance(x, float):
            return 'f'
        return 'p'

def small_list(sizemax=10, nonull=False, attrprefix="list", space=FakeSpace):

    type_prefixes = ['p', 'i', 'f']
    unroll_type_prefixes = unrolling_iterable(type_prefixes)

    def wrapper(cls):

        Map = make_typed_map(space.root_type, type_prefixes)

        def make_class(layout):
            _p, pointers = layout[0]
            _i, integers = layout[1]
            _f, floats   = layout[2]
            assert _p == 'p' and _i == 'i' and _f == 'f'

            SIZE = pointers + integers + floats

            pointer_attrs = ["%s_%s_p" % (attrprefix, i) for i in range(pointers)]
            integer_attrs = ["%s_%s_i" % (attrprefix, i) for i in range(integers)]
            float_attrs   = ["%s_%s_f" % (attrprefix, i) for i in range(floats)]

            unroll_pointer = unrolling_iterable(enumerate(pointer_attrs))
            unroll_integer = unrolling_iterable(enumerate(integer_attrs))
            unroll_float   = unrolling_iterable(enumerate(float_attrs))

            class NewClass(cls):
                _attrs_ = pointer_attrs + integer_attrs + float_attrs
                _attrs_ += ['_map']

                if getattr(cls, '_immutable_', False):
                    _immutable_ = True
                _immutable_fields_ = _attrs_

                @jit.unroll_safe
                def __init__(self, map, elems, *args):
                    # assert len(elems) == SIZE
                    self._map = map
                    if SIZE:
                        for i in range(SIZE):
                            type, index = map.get_index(i)
                            self._set_list_helper(type, index, elems[i])
                    cls.__init__(self, *args)

                def _get_list(self, i):
                    type, index = self._map.get_index(i)
                    return self._get_list_helper(type, index)

                @jit.unroll_safe
                def _get_full_list(self):
                    values = [None] * SIZE
                    for i in range(SIZE):
                        values[i] = self._get_list(i)
                    return values

                def _get_list_helper(self, type, index):
                    if type == 'p':
                        return self._get_list_p(index)
                    if type == 'i':
                        return self._get_list_i(index)
                    if type == 'f':
                        return self._get_list_f(index)
                    assert False, "unknown type"

                def _set_list_helper(self, type, index, val):
                    if nonull:
                        assert val is not None
                    if type == 'p':
                        self._set_list_p(index, val)
                    elif type == 'i':
                        self._set_list_i(index, val)
                    else:
                        assert type == 'f'
                        return self._set_list_f(index, val)

                def _get_size_list(self):
                    return SIZE

                _get_list_p = _make_get_list(unroll_pointer, 'p', space)
                _get_list_i = _make_get_list(unroll_integer, 'i', space)
                _get_list_f = _make_get_list(unroll_float  , 'f', space)

                _set_list_p = _make_set_list(unroll_pointer, 'p', space, nonull=nonull)
                _set_list_i = _make_set_list(unroll_integer, 'i', space, nonull=nonull)
                _set_list_f = _make_set_list(unroll_float  , 'f', space, nonull=nonull)

                def _get_root(self):
                    return self._map.get_root_id()

            add_clone_method(NewClass)
            spec = "Specialized(r=%d,i=%d,f=%d)" % (pointers, integers, floats)
            NewClass.__name__ = cls.__name__ + spec

            @staticmethod
            def make(map, elems, *args):
                return NewClass(map, elems, *args)

            NewClass._new = make
            return NewClass

        class Unspecialized(cls):
            _attrs_ = ['_map', attrprefix]
            _immutable_fields_ = ['_map', attrprefix + '[*]']

            if getattr(cls, '_immutable_', False):
                _immutable_ = True

            def __init__(self, map, elems, *args):
                debug.make_sure_not_resized(elems)
                self._map = map
                setattr(self, attrprefix, elems)
                cls.__init__(self, *args)

            def _get_list(self, i):
                data = getattr(self, attrprefix)
                return data[i]

            def _get_size_list(self):
                data = getattr(self, attrprefix)
                return len(data)

            def _get_full_list(self):
                return getattr(self, attrprefix)

            def _get_root(self):
                return self._map.get_root_id()

        add_clone_method(Unspecialized)
        Unspecialized.__name__ = cls.__name__ + "Unspecialized"

        def make_unspecialized(map, elems, *args):
            return Unspecialized(map, elems, *args)

        def generate_make_function(i, classes):

            @jit.elidable
            def elidable_lookup(map):
                spec = map.layout_spec()
                try:
                    result = classes[spec]
                except KeyError:
                    result = make_unspecialized
                return result

            @jit.unroll_safe
            def make(root, elems, *args):
                # assert len(elems) == i
                map = Map._new(root)
                if elems is not None:
                    # assert len(elems) == i
                    for idx, e in enumerate(elems):
                        type = space.typeof(e)
                        map = map.add_attribute(idx, type)
                cls = elidable_lookup(map)
                return cls(map, elems, *args)
            make.__name__ += "_" + str(i)
            return make

        classes_by_size = []
        make_functions = []
        for i in range(sizemax):
            classes = {}
            for layout in partition(i, type_prefixes):
                newcls = make_class(layout)
                key = tuple(l[1] for l in layout)
                classes[key] = newcls._new
            make_function = generate_make_function(i, classes)
            make_functions.append(make_function)
            classes_by_size.append(classes)
        cls.classes_by_size = classes_by_size

        unroll_size = unrolling_iterable(range(sizemax))

        @staticmethod
        @jit.unroll_safe
        def make(root, elems, *args):
            l = len(elems) if elems is not None else 0
            for i in unroll_size:
                if i == l:
                    make = make_functions[l]
                    return make(root, elems, *args)
            map = Map._new(root)
            return Unspecialized(map, elems, *args)

        cls._make = make
        return cls

    return wrapper

