
from pycket.values import W_Object, W_Fixnum
from rpython.rlib import rerased
from rpython.rlib.objectmodel import newlist_hint

# Setting this to True will break the tests. Used to compare performance.
_always_use_object_strategy = False

def _find_strategy_class(elements):
    if _always_use_object_strategy or len(elements) == 0:
        # An empty vector stays empty forever. Don't implement special EmptyVectorStrategy.
        return ObjectVectorStrategy()
    single_class = type(elements[0])
    for elem in elements:
        if not isinstance(elem, single_class):
            return ObjectVectorStrategy()
    if single_class is W_Fixnum:
        return FixnumVectorStrategy()
    return ObjectVectorStrategy()

class W_Vector(W_Object):
    _immutable_fields_ = ["elems"]
    errorname = "vector"
    def __init__(self, strategy, storage):
        self.strategy = strategy
        self.storage = storage
    @staticmethod
    def fromelements(elems):
        strategy = _find_strategy_class(elems)
        storage = strategy.create_storage_for_elements(elems)
        return W_Vector(strategy, storage)
    @staticmethod
    def fromelement(elem, times):
        check_list = [elem]
        if times == 0:
            check_list = []
        strategy = _find_strategy_class(check_list)
        storage = strategy.create_storage_for_element(elem, times)
        return W_Vector(strategy, storage)
    def ref(self, i):
        return self.strategy.ref(self, i)
    def set(self, i, v): 
        self.strategy.set(self, i, v)
    def length(self):
        return self.strategy.length(self)
    def tostring(self):
        l = self.strategy.ref_all(self)
        description = ""
        for obj in l:
            description += obj.tostring()
        return "#(%s)" % description

    def change_strategy(self, new_strategy):
        old_list = self.strategy.ref_all(self)
        self.strategy = new_strategy
        self.storage = new_strategy.create_storage_for_elements(old_list)

class VectorStrategy(object):
    def isCorrectType(self, w_obj):
        raise NotImplementedError("abstract base class")
    
    def ref(self, w_vector, i):
        self.indexcheck(w_vector, i)
        return self._ref(w_vector, i)
    def set(self, w_vector, i, w_val):
        self.indexcheck(w_vector, i)
        if not (self.isCorrectType(w_val)):
            self.dehomogenize(w_vector)
            w_vector.set(i, w_val) # Now, try again.
        else:
            self._set(w_vector, i, w_val)
    def _ref(self, w_vector, i):
        raise NotImplementedError("abstract base class")
    def _set(self, w_vector, i, w_val):
        raise NotImplementedError("abstract base class")
    def length(self, w_vector):
        raise NotImplementedError("abstract base class")
    def indexcheck(self, w_vector, i):
        raise NotImplementedError("abstract base class")
    def ref_all(self, w_vector):
        raise NotImplementedError("abstract base class")
    
    def create_storage_for_element(self, element, times):
        raise NotImplementedError("abstract base class")
    def create_storage_for_elements(self, elements):
        raise NotImplementedError("abstract base class")
    
    def dehomogenize(self, w_vector):
        w_vector.change_strategy(ObjectVectorStrategy())

class ObjectVectorStrategy(VectorStrategy):
    
    erase, unerase = rerased.new_erasing_pair("object-vector-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    
    def isCorrectType(self, w_obj):
        return True
    
    def _storage(self, w_vector):
        return self.unerase(w_vector.storage)
    def _ref(self, w_vector, i):
        return self._storage(w_vector)[i]
    def _set(self, w_vector, i, w_val):
        self._storage(w_vector)[i] = w_val
    def length(self, w_vector):
        return len(self._storage(w_vector))
    def indexcheck(self, w_vector, i):
        assert 0 <= i < len(self._storage(w_vector))
    def ref_all(self, w_vector):
        return self._storage(w_vector)
    
    def create_storage_for_element(self, element, times):
        return self.erase([element] * times)
    def create_storage_for_elements(self, elements):
        return self.erase(elements)

    def dehomogenize(self, w_vector):
        # Already using object strategy. This will not be executed due to isCorrectType().
        pass

class FixnumVectorStrategy(VectorStrategy):
    
    erase, unerase = rerased.new_erasing_pair("fixnum-vector-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    
    def isCorrectType(self, w_obj):
        return isinstance(w_obj, W_Fixnum)
    
    def wrap(self, val):
        # TODO what primitive datatype is represented by Fixnum?
        assert isinstance(val, int)
        return W_Fixnum(val)
    
    def unwrap(self, w_val):
        assert isinstance(w_val, W_Fixnum)
        return w_val.value
    
    def _storage(self, w_vector):
        return self.unerase(w_vector.storage)
    
    def _ref(self, w_vector, i):
        return self.wrap(self._storage(w_vector)[i])
    
    def _set(self, w_vector, i, w_val):
        self._storage(w_vector)[i] = self.unwrap(w_val)
    
    def length(self, w_vector):
        return len(self._storage(w_vector))
    
    def indexcheck(self, w_vector, i):
        assert 0 <= i < len(self._storage(w_vector))
    
    def ref_all(self, w_vector):
        unwrapped = self._storage(w_vector)
        return [self.wrap(i) for i in unwrapped]

    def create_storage_for_element(self, element, times):
        l = newlist_hint(times)
        e = self.unwrap(element)
        for i in range(times):
            l.append(e)
        return self.erase(l)
    
    def create_storage_for_elements(self, elements):
        l = newlist_hint(len(elements))
        for i in range(len(elements)):
            l.append(self.unwrap(elements[i]))
        return self.erase(l)
