
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

def attr_names(prefix, N, type):
    """NOT RPYTHON"""
    attrs = ["%s_%s%s" % (prefix, type, i) for i in range(N)]
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
    def typeOf(x):
        """NOT RPYTHON"""
        if isinstance(x, int):
            return 'i'
        if isinstance(x, float):
            return 'f'
        return 'p'

_REQUIRED_ATTRS = [
    "root_type",
    "unwrap_p",
    "unwrap_i",
    "unwrap_f",
    "wrap",
    "typeOf"]

def small_list(sizemax=10, nonull=False, attrname="list", factoryname="_make",
               space=None, cache_constants=True):
    assert space is not None
    for a in _REQUIRED_ATTRS:
        assert hasattr(space, a)

    type_prefixes = ('p', 'i', 'f')
    unroll_type_prefixes = unrolling_iterable(type_prefixes)

    def wrapper(cls):

        Map = make_typed_map(space.root_type, type_prefixes, cache_constants)

        def make_class(layout):
            _p, pointers = layout[0]
            _i, integers = layout[1]
            _f, floats   = layout[2]
            assert _p == 'p' and _i == 'i' and _f == 'f'

            pointer_attrs, unroll_pointer = attr_names(attrname, pointers, 'p')
            integer_attrs, unroll_integer = attr_names(attrname, integers, 'i')
            float_attrs  , unroll_float   = attr_names(attrname, floats  , 'f')

            class NewClass(cls):
                _attrs_ = pointer_attrs + integer_attrs + float_attrs
                _attrs_ += ['_map']

                if getattr(cls, '_immutable_', False):
                    _immutable_ = True
                _immutable_fields_ = _attrs_

                @jit.unroll_safe
                def __init__(self, map, elems, *args):
                    size = map.total_size()
                    self._map = map
                    for i in range(size):
                        type, index = map.get_index(i)
                        if index != -1:
                            self._set_list_helper(type, index, elems[i])
                    cls.__init__(self, *args)

                def _get_list(self, i):
                    if cache_constants:
                        result = self._map.get_static_attribute(i, None)
                        if result is not None:
                            return result
                    type, index = self._map.get_index(i)
                    return self._get_list_helper(type, index)

                @jit.unroll_safe
                def _get_full_list(self):
                    size = self._map.total_size()
                    values = [None] * size
                    for i in range(size):
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
                    return self._map.total_size()

                _get_list_p = _make_get_list(unroll_pointer, 'p', space)
                _get_list_i = _make_get_list(unroll_integer, 'i', space)
                _get_list_f = _make_get_list(unroll_float  , 'f', space)

                _set_list_p = _make_set_list(unroll_pointer, 'p', space, nonull=nonull)
                _set_list_i = _make_set_list(unroll_integer, 'i', space, nonull=nonull)
                _set_list_f = _make_set_list(unroll_float  , 'f', space, nonull=nonull)

                def _get_root(self):
                    return self._map.get_root_id()

            add_clone_method(NewClass)
            spec = "Specialized[r=%d,i=%d,f=%d]" % (pointers, integers, floats)
            NewClass.__name__ = cls.__name__ + spec

            @staticmethod
            def make(map, elems, *args):
                return NewClass(map, elems, *args)

            NewClass._new = make
            return NewClass

        class Unspecialized(cls):
            _attrs_ = ['_map', attrname]
            _immutable_fields_ = ['_map', attrname + '[*]']

            if getattr(cls, '_immutable_', False):
                _immutable_ = True

            def __init__(self, map, elems, *args):
                debug.make_sure_not_resized(elems)
                self._map = map
                setattr(self, attrname, elems)
                cls.__init__(self, *args)

            def _get_list(self, i):
                data = getattr(self, attrname)
                return data[i]

            def _get_size_list(self):
                data = getattr(self, attrname)
                return len(data)

            def _get_full_list(self):
                return getattr(self, attrname)

            def _get_root(self):
                return self._map.get_root_id()

        add_clone_method(Unspecialized)
        Unspecialized.__name__ = cls.__name__ + "Unspecialized"

        def make_unspecialized(map, elems, *args):
            return Unspecialized(map, elems, *args)

        classes = {}
        for i in range(sizemax):
            for layout in partition(i, type_prefixes):
                newcls = make_class(layout)
                key = tuple(l[1] for l in layout)
                classes[key] = newcls._new

        @jit.elidable
        def elidable_lookup(map):
            spec = map.layout_spec()
            return classes.get(spec, make_unspecialized)

        @jit.unroll_safe
        def _make(root, elems, *args):
            map = Map._new(root)
            if elems is not None:
                for idx, e in enumerate(elems):
                    if cache_constants and root.is_constant_field(idx):
                        value = root.get_constant_field(idx)
                        map = map.add_static_attribute(idx, value)
                    else:
                        type = space.typeOf(e)
                        map = map.add_attribute(idx, type)
            cls = elidable_lookup(map)
            return cls(map, elems, *args)
        _make.__name__ = factoryname

        cls.classes = classes
        unroll_size = unrolling_iterable(range(sizemax))

        setattr(cls, factoryname, staticmethod(_make))
        return cls

    return wrapper

