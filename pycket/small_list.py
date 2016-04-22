import py
from pycket import config
from pycket.util import add_copy_method

from rpython.rlib.unroll import unrolling_iterable
from rpython.rlib        import jit, debug, objectmodel

add_clone_method = add_copy_method("_clone")

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

            # Methods for the new class being built
            methods = {
                gettername     : _get_list,
                listsizename   : _get_size_list,
                listgettername : _get_full_list,
                settername     : _set_list,
                "__init__"     : _init,
            }

            newcls = type(cls)("%sSize%s" % (cls.__name__, size), (cls, ), methods)

            if immutable:
                setattr(newcls, "_immutable_fields_", attrs)
                newcls = add_clone_method(newcls)

            return newcls

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

        methods = {
            gettername     : _get_arbitrary,
            listsizename   : _get_size_list_arbitrary,
            listgettername : _get_list_arbitrary,
            settername     : _set_arbitrary,
            "__init__"     : _init,
        }

        cls_arbitrary = type(cls)("%sArbitrary" % cls.__name__, (cls, ), methods)

        if immutable:
            setattr(cls_arbitrary, "_immutable_fields_", ["%s[*]" % (attrname,)])
            cls_arbitrary = add_clone_method(cls_arbitrary)

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

