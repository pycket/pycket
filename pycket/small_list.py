from rpython.rlib  import jit, debug

def inline_small_list(sizemax=5, sizemin=0, immutable=False, attrname="list", factoryname="make", unbox_fixnum=False):
    """
    This function is helpful if you have a class with a field storing a
    list and the list is often very small. Calling this function will inline
    the list into instances for the small sizes. This works by adding the
    following methods to the class:

    _get_list(self, i): return ith element of the list

    _set_list(self, i, val): set ith element of the list

    _get_full_list(self): returns a copy of the full list

    @staticmethod
    make(listcontent, *args): makes a new instance with the list's content set to listcontent
    """
    def wrapper(cls):
        from rpython.rlib.unroll import unrolling_iterable
        classes = []
        def make_methods(size):
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
                        return setattr(self, attr, val)
                raise IndexError
            def _init(self, elems, *args):
                assert len(elems) == size
                for i, attr in unrolling_enumerate_attrs:
                    setattr(self, attr, elems[i])
                cls.__init__(self, *args)
            meths = {"_get_list": _get_list, "_get_size_list": _get_size_list, "_get_full_list": _get_full_list, "_set_list": _set_list, "__init__" : _init}
            if immutable:
                meths["_immutable_fields_"] = attrs
            return meths
        classes = [type(cls)("%sSize%s" % (cls.__name__, size), (cls, ), make_methods(size)) for size in range(sizemin, sizemax)]
        def _get_arbitrary(self, i):
            return getattr(self, attrname)[i]
        def _get_size_list_arbitrary(self):
            return len(getattr(self, attrname))
        def _get_list_arbitrary(self):
            return getattr(self, attrname)
        def _set_arbitrary(self, i, val):
            getattr(self, attrname)[i] = val
        def _init(self, elems, *args):
            debug.make_sure_not_resized(elems)
            setattr(self, attrname, elems)
            cls.__init__(self, *args)
        meths = {"_get_list": _get_arbitrary, "_get_size_list": _get_size_list_arbitrary, "_get_full_list": _get_list_arbitrary, "_set_list": _set_arbitrary, "__init__": _init}
        if immutable:
            meths["_immutable_fields_"] = ["%s[*]" % (attrname, )]
        cls_arbitrary = type(cls)("%sArbitrary" % cls.__name__, (cls, ), meths)

        def make(elems, *args):
            if sizemin <= len(elems) < sizemax:
                cls = classes[len(elems) - sizemin]
            else:
                cls = cls_arbitrary
            return cls(elems, *args)
        if unbox_fixnum:
            make = _add_fixnum_classes(cls, make)
        setattr(cls, factoryname, staticmethod(make))
        return cls
    return wrapper

def _add_fixnum_classes(cls, orig_make):
    # XXX quite brute force
    def make(vals, *args):
        from pycket.values import W_Fixnum
        if len(vals) == 1:
            w_a, = vals
            if isinstance(w_a, W_Fixnum):
                return Size1Fixed(w_a.value, *args)
        if len(vals) == 2:
            w_a, w_b = vals
            if isinstance(w_a, W_Fixnum):
                if isinstance(w_b, W_Fixnum):
                    return Size2Fixed11(w_a.value, w_b.value, *args)
                else:
                    return Size2Fixed10(w_a.value, w_b, *args)
            elif isinstance(w_b, W_Fixnum):
                return Size2Fixed01(w_a, w_b.value, *args)
        return orig_make(vals, *args)

    class Size1Fixed(cls):
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


    class Size2Fixed10(cls):
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


    class Size2Fixed01(cls):
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

    class Size2Fixed11(cls):
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

    return make
