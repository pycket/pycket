from rpython.rlib  import jit, debug

def inline_small_list(sizemax=5, sizemin=0, immutable=False, attrname="list", factoryname="make"):
    """ This function is helpful if you have a class with a field storing a
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

        @staticmethod
        def make(elems, *args):
            if sizemin <= len(elems) < sizemax:
                cls = classes[len(elems) - sizemin]
            else:
                cls = cls_arbitrary
            return cls(elems, *args)
        setattr(cls, factoryname, make)
        return cls
    return wrapper
