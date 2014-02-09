
from pycket.values import W_Object, W_Fixnum, W_Flonum
from rpython.rlib import rerased
from rpython.rlib.objectmodel import newlist_hint, import_from_mixin
from rpython.rlib import debug

# Setting this to True will break the tests. Used to compare performance.
_always_use_object_strategy = False

def _find_strategy_class(elements):
    if _always_use_object_strategy or len(elements) == 0:
        # An empty vector stays empty forever. Don't implement special EmptyVectorStrategy.
        return ObjectVectorStrategy.singleton
    single_class = type(elements[0])
    for elem in elements:
        if not isinstance(elem, single_class):
            return ObjectVectorStrategy.singleton
    if single_class is W_Fixnum:
        return FixnumVectorStrategy.singleton
    if single_class is W_Flonum:
        return FlonumVectorStrategy.singleton
    return ObjectVectorStrategy.singleton

class W_Vector(W_Object):
    _immutable_fields_ = ["elems", "len"]
    errorname = "vector"
    def __init__(self, strategy, storage, len):
        self.strategy = strategy
        self.storage = storage
        self.len = len
    @staticmethod
    def fromelements(elems):
        strategy = _find_strategy_class(elems)
        storage = strategy.create_storage_for_elements(elems)
        return W_Vector(strategy, storage, len(elems))
    @staticmethod
    def fromelement(elem, times):
        check_list = [elem]
        if times == 0:
            check_list = []
        strategy = _find_strategy_class(check_list)
        storage = strategy.create_storage_for_element(elem, times)
        return W_Vector(strategy, storage, times)
    def ref(self, i):
        return self.strategy.ref(self, i)
    def set(self, i, v):
        self.strategy.set(self, i, v)
    def length(self):
        return self.len
    def tostring(self):
        l = self.strategy.ref_all(self)
        description = []
        for obj in l:
            description.append(obj.tostring())
        return "#(%s)" % " ".join(description)

    def change_strategy(self, new_strategy):
        old_list = self.strategy.ref_all(self)
        self.strategy = new_strategy
        self.storage = new_strategy.create_storage_for_elements(old_list)

    def equal(self, other):
        # XXX could be optimized using strategies
        if not isinstance(other, W_Vector):
            return False
        if self is other:
            return True
        if self.length() != other.length():
            return False
        for i in range(self.length()):
            if not self.ref(i).equal(other.ref(i)):
                return False
        return True


class SingletonMeta(type):
    def __new__(cls, name, bases, dct):
        result = type.__new__(cls, name, bases, dct)
        result.singleton = result()
        return result

class VectorStrategy(object):
    __metaclass__ = SingletonMeta

    def is_correct_type(self, w_obj):
        raise NotImplementedError("abstract base class")

    def ref(self, w_vector, i):
        self.indexcheck(w_vector, i)
        return self._ref(w_vector, i)
    def set(self, w_vector, i, w_val):
        self.indexcheck(w_vector, i)
        if not (self.is_correct_type(w_val)):
            self.dehomogenize(w_vector)
            w_vector.set(i, w_val) # Now, try again.
        else:
            self._set(w_vector, i, w_val)
    def indexcheck(self, w_vector, i):
        assert 0 <= i < w_vector.len

    def _ref(self, w_vector, i):
        raise NotImplementedError("abstract base class")
    def _set(self, w_vector, i, w_val):
        raise NotImplementedError("abstract base class")
    # def length(self, w_vector):
    #     raise NotImplementedError("abstract base class")
    def ref_all(self, w_vector):
        raise NotImplementedError("abstract base class")

    def create_storage_for_element(self, element, times):
        raise NotImplementedError("abstract base class")
    def create_storage_for_elements(self, elements):
        raise NotImplementedError("abstract base class")


    def dehomogenize(self, w_vector):
        w_vector.change_strategy(ObjectVectorStrategy.singleton)


class UnwrappedVectorStrategyMixin(object):
    # the concrete class needs to implement:
    # erase, unerase, is_correct_type, wrap, unwrap

    def _storage(self, w_vector):
        l = self.unerase(w_vector.storage)
        debug.make_sure_not_resized(l)
        return l

    def _ref(self, w_vector, i):
        return self.wrap(self._storage(w_vector)[i])

    def _set(self, w_vector, i, w_val):
        self._storage(w_vector)[i] = self.unwrap(w_val)

    # def length(self, w_vector):
    #     return len(self._storage(w_vector))

    def ref_all(self, w_vector):
        unwrapped = self._storage(w_vector)
        return [self.wrap(i) for i in unwrapped]

    def create_storage_for_element(self, element, times):
        e = self.unwrap(element)
        return self.erase([e] * times)

    def create_storage_for_elements(self, elements):
        if not elements:
            return self.erase([])
        l = [self.unwrap(elements[0])] * len(elements)
        for i in range(1, len(elements)):
            l[i] = self.unwrap(elements[i])
        return self.erase(l)



class ObjectVectorStrategy(VectorStrategy):
    import_from_mixin(UnwrappedVectorStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("object-vector-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def wrap(self, obj):
        return obj

    def unwrap(self, w_obj):
        return w_obj

    def is_correct_type(self, w_obj):
        return True

    def dehomogenize(self, w_vector):
        assert 0 # should be unreachable because is_correct_type is always True

class FixnumVectorStrategy(VectorStrategy):
    import_from_mixin(UnwrappedVectorStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("fixnum-vector-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def is_correct_type(self, w_obj):
        return isinstance(w_obj, W_Fixnum)

    def wrap(self, val):
        # TODO what primitive datatype is represented by Fixnum?
        assert isinstance(val, int)
        return W_Fixnum(val)

    def unwrap(self, w_val):
        assert isinstance(w_val, W_Fixnum)
        return w_val.value

class FlonumVectorStrategy(VectorStrategy):
    import_from_mixin(UnwrappedVectorStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("flonum-vector-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def is_correct_type(self, w_obj):
        return isinstance(w_obj, W_Flonum)

    def wrap(self, val):
        return W_Flonum(val)

    def unwrap(self, w_val):
        assert isinstance(w_val, W_Flonum)
        return w_val.value
