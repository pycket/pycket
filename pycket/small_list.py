import py
from pycket import config

from rpython.rlib.unroll import unrolling_iterable
from rpython.rlib        import jit, debug, objectmodel
from itertools           import product

FIXNUM = 'i'
FLONUM = 'f'
OBJECT = 'r'

def index(spec):
    """NOT RPYTHON"""
    acc = 0
    for i in spec:
        if i == FIXNUM:
            v = 0
        elif i == FLONUM:
            v = 1
        elif i == OBJECT:
            v = 2
        acc = acc * 3 + v
    return acc

ALL_TYPES = (FIXNUM, FLONUM, OBJECT)

def make_specialized_classes(cls, size, attrname="list", types=ALL_TYPES):

    print "Specializing: %s %d" % (cls.__name__, size)

    classes = [None] * (3 ** size)

    for spec in product(types, repeat=size):

        attrs = ["_%s_%s_%s" % (attrname, i, s) for i, s in enumerate(spec)]
        indexes = range(len(attrs))
        unrolling_enumerate_attrs = unrolling_iterable(zip(indexes, attrs, spec))

        class Specialized(cls):
            _immutable_fields_ = attrs

            def _init_list(self, args):
                from pycket.values import W_Fixnum, W_Flonum, W_Object
                assert len(args) == size
                for i, attr, typ in unrolling_enumerate_attrs:
                    val = args[i]
                    if typ == FIXNUM:
                        assert type(val) is W_Fixnum
                        setattr(self, attr, val.value)
                    elif typ == FLONUM:
                        assert type(val) is W_Flonum
                        setattr(self, attr, val.value)
                    else:
                        assert isinstance(val, W_Object)
                        setattr(self, attr, val)

            def _get_list(self, i):
                from pycket.values import wrap
                for j, attr, typ in unrolling_enumerate_attrs:
                    if i == j:
                        val = getattr(self, attr)
                        return wrap(val)
                raise IndexError

            def _set_list(self, i, val):
                from pycket.values import W_Fixnum, W_Flonum, W_Object
                for index, attr, typ in unrolling_enumerate_attrs:
                    if index == i:
                        if typ == FIXNUM:
                            assert type(val) is W_Fixnum
                            setattr(self, attr, val.value)
                        elif typ == FLONUM:
                            assert type(val) is W_Flonum
                            setattr(self, attr, val.value)
                        else:
                            assert isinstance(val, W_Object)
                            setattr(self, attr, val)
                raise IndexError

            def _get_full_list(self):
                from pycket.values import wrap
                results = [None] * size
                for i, attr, typ in unrolling_enumerate_attrs:
                    val = getattr(self, attr)
                    results[i] = wrap(val)
                return results

            def _get_size_list(self):
                return size

            def _clone_small_list(self):
                result = objectmodel.instantiate(Specialized)
                for i, attr, typ in unrolling_enumerate_attrs:
                    val = getattr(self, attr)
                    setattr(result, attr, val)
                return result

        Specialized.__name__ = "Fixed(%s, size=%d, type=%s)" % (cls.__name__, size, "".join(spec))
        classes[index(spec)] = Specialized

    unspecialized = classes[-1]

    @jit.elidable
    def _lookup_class(*signature):
        if len(classes) == 1:
            return unspecialized
        return classes[signature]

    def type_to_index(cls):
        from pycket.values import W_Fixnum, W_Flonum, W_Object
        if cls is W_Fixnum:
            return 0
        if cls is W_Flonum:
            return 1
        assert cls is W_Object
        return 2

    @jit.unroll_safe
    def make(elems, *args):
        from pycket.values import W_Fixnum, W_Flonum, W_Object
        assert len(elems) == size
        spec = 0
        for e in elems:
            t = type(e)
            spec = spec * 3 + type_to_index(t)
        cls = classes[spec]
        result = objectmodel.instantiate(cls)
        cls._init_list(result, elems)
        cls.__init__(result, *args)
        return result

    elems = ["elem%d" % i for i in range(size)]
    args  = ", ".join(elems + ["*args"])
    spec = "0"
    for elem in elems:
        spec = "(%s * 3 + type_to_index(type(%s)))" % (spec, elem)

    sets = []
    for i, elem in enumerate(elems):
        line = "cls._set_list(%d, %s)" % (i, elem)
        sets.append(line)
    sets = "; ".join(sets)

    namespace = locals().copy()
    code = py.code.Source("""
    def make_variadic(%s):
        from pycket.values import W_Fixnum, W_Flonum, W_Object
        assert len(elems) == size
        spec = (%s)
        cls = classes[spec]
        result = objectmodel.instantiate(cls)
        %s
        cls.__init__(result, *args)
        return result
    """ % (args, spec, sets)).compile()
    exec code in namespace

    return make, namespace['make_variadic']

def inline_small_list(sizemax=11, sizemin=0, immutable=False, unbox_num=False, nonull=False,
                      attrname="list", factoryname="make", listgettername="_get_full_list",
                      listsizename="_get_size_list", gettername="_get_list",
                      settername="_set_list"):
    """
    This function is helpful if you have a class with a field storing a
    list and the list is often very small. Calling this function will inline
    the list into instances for the small sizes. This works by adding the
    following methods (names customizable) to the class:

    _get_list(self, i): return ith element of the list

    _set_list(self, i, val): set ith element of the list

    _get_full_list(self): returns a copy of the full list

    _get_size_list(self): returns the length of the list

    @staticmethod
    make(listcontent, *args): makes a new instance with the list's content set to listcontent
    """
    if not config.type_size_specialization:
        sizemin = sizemax = 0
        unbox_num = False

    def wrapper(cls):
        def make_class(size):
            attrs = ["_%s_%s" % (attrname, i) for i in range(size)]
            unrolling_enumerate_attrs = unrolling_iterable(enumerate(attrs))

            def _get_size_list(self):
                return size
            def _get_list(self, i):
                for j, attr in unrolling_enumerate_attrs:
                    if j == i:
                        return getattr(self, attr)
                raise IndexError
            def _get_full_list(self):
                res = [None] * size
                for i, attr in unrolling_enumerate_attrs:
                    res[i] = getattr(self, attr)
                return res
            def _set_list(self, i, val):
                for j, attr in unrolling_enumerate_attrs:
                    if j == i:
                        if nonull:
                            assert val is not None
                        setattr(self, attr, val)
                        return
                raise IndexError
            def _init(self, elems, *args):
                assert len(elems) == size
                for i, attr in unrolling_enumerate_attrs:
                    val = elems[i]
                    if nonull:
                        assert val is not None
                    setattr(self, attr, elems[i])
                cls.__init__(self, *args)
            def _clone(self):
                # Allocate and fill in small list values
                result = objectmodel.instantiate(newcls)
                for _, attr in unrolling_enumerate_attrs:
                    value = getattr(self, attr)
                    setattr(result, attr, value)
                return result

            # Methods for the new class being built
            methods = {
                gettername          : _get_list,
                listsizename        : _get_size_list,
                listgettername      : _get_full_list,
                settername          : _set_list,
                "__init__"          : _init,
                "_clone_small_list" : _clone,
            }

            if immutable:
                methods["_immutable_fields_"] = attrs

            newcls = type(cls)("%sSize%s" % (cls.__name__, size), (cls, ), methods)
            return newcls

        if unbox_num and immutable:
            make_methods = []
            for size in range(sizemin, sizemax):
                make_method, make_variadic = make_specialized_classes(cls, size)
                setattr(cls, "%s%d" % (factoryname, size), staticmethod(make_variadic))
                make_methods.append(make_method)

            specializations = unrolling_iterable(enumerate(range(sizemin, sizemax)))

            def make(elems, *args):
                if elems is None:
                    elems = []
                l = len(elems)
                for i, size in specializations:
                    if size == l:
                        return make_methods[i](elems, *args)
                    result = objectmodel.instantiate(cls_arbitrary)
                    cls.__init__(result, *args)
                    setattr(result, attrname, [None] * l)
            setattr(cls, factoryname, staticmethod(make))
            return cls

        classes = map(make_class, range(sizemin, sizemax))

        # Build the arbitrary sized variant
        def _get_arbitrary(self, i):
            return getattr(self, attrname)[i]
        def _get_size_list_arbitrary(self):
            return len(getattr(self, attrname))
        def _get_list_arbitrary(self):
            return getattr(self, attrname)
        def _set_arbitrary(self, i, val):
            if nonull:
                assert val is not None
            getattr(self, attrname)[i] = val
        def _init(self, elems, *args):
            debug.make_sure_not_resized(elems)
            setattr(self, attrname, elems)
            cls.__init__(self, *args)
        def _clone(self):
            result = objectmodel.instantiate(cls_arbitrary)
            values = getattr(self, attrname)
            if not immutable:
                # Only copy if the storage is mutable
                values = values[:]
            setattr(result, attrname, values)
            return result

        methods = {
            gettername          : _get_arbitrary,
            listsizename        : _get_size_list_arbitrary,
            listgettername      : _get_list_arbitrary,
            settername          : _set_arbitrary,
            "__init__"          : _init,
            "_clone_small_list" : _clone,
        }

        if immutable:
            methods["_immutable_fields_"] = ["%s[*]" % (attrname, )]
        cls_arbitrary = type(cls)("%sArbitrary" % cls.__name__, (cls, ), methods)

        def make(elems, *args):
            if classes:
                if (elems is None or len(elems) == 0):
                    return make0(*args)
            else:
                if elems is None:
                    elems = []
            if sizemin <= len(elems) < sizemax:
                cls = classes[len(elems) - sizemin]
            else:
                cls = cls_arbitrary
            return cls(elems, *args)

        # XXX those could be done more nicely
        def make0(*args):
            if not classes: # no type specialization
                return make([], *args)
            result = objectmodel.instantiate(classes[0])
            cls.__init__(result, *args)
            return result
        def make1(elem, *args):
            if not classes: # no type specialization
                return make([elem], *args)
            result = objectmodel.instantiate(classes[1])
            result._set_list(0, elem)
            cls.__init__(result, *args)
            return result
        def make2(elem1, elem2, *args):
            if not classes: # no type specialization
                return make([elem1, elem2], *args)
            result = objectmodel.instantiate(classes[2])
            result._set_list(0, elem1)
            result._set_list(1, elem2)
            cls.__init__(result, *args)
            return result

        def make_n(size, *args):
            if sizemin <= size < sizemax:
                subcls = classes[size - sizemin]
            else:
                subcls = cls_arbitrary
            result = objectmodel.instantiate(subcls)
            if subcls is cls_arbitrary:
                assert isinstance(result, subcls)
                setattr(result, attrname, [None] * size)
            cls.__init__(result, *args)
            return result

        if unbox_num:
            assert immutable, "unboxing is only supported for immutable objects"
            make, make1, make2 = _add_num_classes(cls, make, make0, make1, make2)
        setattr(cls, factoryname, staticmethod(make))
        setattr(cls, factoryname + "0", staticmethod(make0))
        setattr(cls, factoryname + "1", staticmethod(make1))
        setattr(cls, factoryname + "2", staticmethod(make2))
        setattr(cls, factoryname + "_n", staticmethod(make_n))
        return cls
    return wrapper

def add_clone_method(cls):
    field_names = unrolling_iterable(cls._immutable_fields_)
    def _clone_small_list(self):
        result = objectmodel.instantiate(cls)
        for attr in field_names:
            val = getattr(self, attr)
            setattr(result, attr, val)
        return result
    cls._clone_small_list = _clone_small_list
    return cls

def _add_num_classes(cls, orig_make, orig_make0, orig_make1, orig_make2):
    # XXX quite brute force
    def make(vals, *args):
        from pycket.values import W_Fixnum
        if vals is None or len(vals) == 0:
            return orig_make0(*args)
        if len(vals) == 1:
            return make1(vals[0], *args)
        if len(vals) == 2:
            return make2(vals[0], vals[1], *args)
        return orig_make(vals, *args)
    def make1(w_a, *args):
        from pycket.values import W_Fixnum, W_Flonum
        if isinstance(w_a, W_Fixnum):
            return Size1Fixed(w_a.value, *args)
        if isinstance(w_a, W_Flonum):
            return Size1Flo(w_a.value, *args)
        return orig_make1(w_a, *args)
    def make2(w_a, w_b, *args):
        from pycket.values import W_Fixnum
        if isinstance(w_a, W_Fixnum):
            if isinstance(w_b, W_Fixnum):
                return Size2Fixed11(w_a.value, w_b.value, *args)
            else:
                return Size2Fixed10(w_a.value, w_b, *args)
        elif isinstance(w_b, W_Fixnum):
            return Size2Fixed01(w_a, w_b.value, *args)
        return orig_make2(w_a, w_b, *args)

    class Size1Fixed(cls):
        _immutable_fields_ = ['vals_fixed_0']
        def __init__(self, vals_fixed_0, *args):
            self.vals_fixed_0 = vals_fixed_0
            cls.__init__(self, *args)

        def _get_size_list(self):
            return 1

        def _get_full_list(self):
            return [self._get_list(0)]

        def _get_list(self, i):
            from pycket.values import W_Fixnum
            assert i == 0
            return W_Fixnum(self.vals_fixed_0)

        def _set_list(self, i, val):
            raise NotImplementedError()
    Size1Fixed.__name__ = cls.__name__ + Size1Fixed.__name__
    add_clone_method(Size1Fixed)

    class Size1Flo(cls):
        _immutable_fields_ = ['vals_flo_0']
        def __init__(self, vals_flo_0, *args):
            self.vals_flo_0 = vals_flo_0
            cls.__init__(self, *args)

        def _get_size_list(self):
            return 1

        def _get_full_list(self):
            return [self._get_list(0)]

        def _get_list(self, i):
            from pycket.values import W_Flonum
            assert i == 0
            return W_Flonum(self.vals_flo_0)

        def _set_list(self, i, val):
            raise NotImplementedError()
    Size1Flo.__name__ = cls.__name__ + Size1Flo.__name__
    add_clone_method(Size1Flo)

    class Size2Fixed10(cls):
        _immutable_fields_ = ['vals_fixed_0', 'w_val1']
        def __init__(self, vals_fixed_0, w_val1, *args):
            self.vals_fixed_0 = vals_fixed_0
            self.w_val1 = w_val1
            cls.__init__(self, *args)

        def _get_size_list(self):
            return 2

        def _get_full_list(self):
            return [self._get_list(0), self._get_list(1)]

        def _get_list(self, i):
            from pycket.values import W_Fixnum
            if i == 0:
                return W_Fixnum(self.vals_fixed_0)
            else:
                assert i == 1
                return self.w_val1

        def _set_list(self, i, val):
            raise NotImplementedError()
    Size2Fixed10.__name__ = cls.__name__ + Size2Fixed10.__name__
    add_clone_method(Size2Fixed10)

    class Size2Fixed01(cls):
        _immutable_fields_ = ['w_val0', 'vals_fixed_1']
        def __init__(self, w_val0, vals_fixed_1, *args):
            self.w_val0 = w_val0
            self.vals_fixed_1 = vals_fixed_1
            cls.__init__(self, *args)

        def _get_size_list(self):
            return 2

        def _get_full_list(self):
            return [self._get_list(0), self._get_list(1)]

        def _get_list(self, i):
            from pycket.values import W_Fixnum
            if i == 0:
                return self.w_val0
            else:
                assert i == 1
                return W_Fixnum(self.vals_fixed_1)

        def _set_list(self, i, val):
            raise NotImplementedError()
    Size2Fixed01.__name__ = cls.__name__ + Size2Fixed01.__name__
    add_clone_method(Size2Fixed01)

    class Size2Fixed11(cls):
        _immutable_fields_ = ['vals_fixed_0', 'vals_fixed_1']
        def __init__(self, vals_fixed_0, vals_fixed_1, *args):
            self.vals_fixed_0 = vals_fixed_0
            self.vals_fixed_1 = vals_fixed_1
            cls.__init__(self, *args)

        def _get_size_list(self):
            return 2

        def _get_full_list(self):
            return [self._get_list(0), self._get_list(1)]

        def _get_list(self, i):
            from pycket.values import W_Fixnum
            if i == 0:
                return W_Fixnum(self.vals_fixed_0)
            else:
                assert i == 1
                return W_Fixnum(self.vals_fixed_1)

        def _set_list(self, i, val):
            raise NotImplementedError()
    Size2Fixed11.__name__ = cls.__name__ + Size2Fixed11.__name__
    add_clone_method(Size2Fixed11)

    return make, make1, make2

def example():
    from pycket import values
    classes = make_specialized_classes(object, 5)
    xs = (values.wrap(1), values.wrap(2), values.wrap(1.2), values.w_false, values.w_false)
    print classes(xs)
