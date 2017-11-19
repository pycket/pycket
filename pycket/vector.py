
from pycket.values import W_MVector, W_VectorSuper, W_Fixnum, W_Flonum, W_Character, UNROLLING_CUTOFF, wrap
from pycket.base import W_Object, SingletonMeta, UnhashableType
from pycket import config

from rpython.rlib import debug, jit, objectmodel, rerased
from rpython.rlib.longlong2float import (
    can_encode_int32,
    decode_int32_from_longlong_nan,
    encode_int32_into_longlong_nan,
    float2longlong,
    is_int32_from_longlong_nan,
    longlong2float
)
from rpython.rlib.objectmodel import import_from_mixin, specialize, we_are_translated
from rpython.rlib.rarithmetic import intmask


@jit.look_inside_iff(lambda elements, immutable:
        jit.loop_unrolling_heuristic(
                elements, len(elements), UNROLLING_CUTOFF))
def _find_strategy_class(elements, immutable):
    if not config.strategies or len(elements) == 0:
        # An empty vector stays empty forever. Don't implement special EmptyVectorStrategy.
        if immutable:
            return ObjectImmutableVectorStrategy.singleton
        return ObjectVectorStrategy.singleton
    single_class = type(elements[0])
    for elem in elements:
        if not isinstance(elem, single_class):
            if immutable:
                return ObjectImmutableVectorStrategy.singleton
            return ObjectVectorStrategy.singleton
    if single_class is W_Fixnum:
        if immutable:
            return FixnumImmutableVectorStrategy.singleton
        return FixnumVectorStrategy.singleton
    if single_class is W_Flonum:
        if immutable:
            return FlonumImmutableVectorStrategy.singleton
        return FlonumVectorStrategy.singleton
    if single_class is W_Character:
        if immutable:
            return CharacterImmutableVectorStrategy.singleton
        return CharacterVectorStrategy.singleton
    if immutable:
        return ObjectImmutableVectorStrategy.singleton
    return ObjectVectorStrategy.singleton

class StrategyVectorMixin(object):
    def get_storage(self):
        return self.storage

    def set_storage(self, storage):
        self.storage = storage

    def ref(self, i):
        return self.get_strategy().ref(self, i)

    def set(self, i, v):
        self.get_strategy().set(self, i, v)

    def immutable(self):
        return self.get_strategy().immutable()

    def vector_set(self, i, new, env, cont, app=None):
        from pycket.interpreter import return_value
        from pycket.values import w_void
        self.set(i, new)
        return return_value(w_void, env, cont)

    def vector_ref(self, i, env, cont, app=None):
        from pycket.interpreter import return_value
        return return_value(self.ref(i), env, cont)

    # unsafe versions
    def unsafe_ref(self, i):
        return self.get_strategy().ref(self, i, check=False)

    def unsafe_set(self, i, v):
        self.get_strategy().set(self, i, v, check=False)

    def change_strategy(self, new_strategy):
        old_list = self.get_strategy().ref_all(self)
        self.set_strategy(new_strategy)
        self.set_storage(new_strategy.create_storage_for_elements(old_list))

    def unrolling_heuristic(self):
        return self.get_strategy().unrolling_heuristic(self)

class W_Vector(W_MVector):
    _immutable_fields_ = ["len"]
    _attrs_ = ["strategy", "storage", "len"]
    errorname = "vector"

    import_from_mixin(StrategyVectorMixin)

    def __init__(self, strategy, storage, len):
        self.strategy = strategy
        self.storage = storage
        self.len = len

    def get_strategy(self):
        return self.strategy

    def set_strategy(self, strategy):
        if not config.strategies:
            assert strategy is ObjectVectorStrategy.singleton
        self.strategy = strategy

    @staticmethod
    def fromelements(elems, immutable=False):
        strategy = _find_strategy_class(elems, immutable)
        storage = strategy.create_storage_for_elements(elems)
        return W_Vector(strategy, storage, len(elems))

    @staticmethod
    def fromelement(elem, times, immutable=False, strategy=None):
        if not config.strategies or times == 0:
            strategy = ObjectVectorStrategy.singleton
        elif strategy is None:
            strategy = ConstantVectorStrategy.singleton
        if immutable:
            strategy = strategy.immutable_variant()
        storage = strategy.create_storage_for_element(elem, times)
        return W_Vector(strategy, storage, times)

    def length(self):
        return self.len

    def tostring(self):
        l = self.strategy.ref_all(self)
        return "#(%s)" % " ".join([obj.tostring() for obj in l])

    def _make_copy(self, immutable=False):
        return self.strategy._copy_storage(self, immutable=immutable)

    def hash_equal(self, info=None):
        raise UnhashableType

    # def hash_equal(self, info=None):
        # x = 0x456789
        # for i in range(self.len):
            # hash = self.ref(i).hash_equal(info=info)
            # x = intmask((1000003 * x) ^ hash)
        # return x

    def equal(self, other):
        # XXX could be optimized using strategies
        if not isinstance(other, W_MVector):
            return False
        if self is other:
            return True
        if self.length() != other.length():
            return False
        for i in range(self.length()):
            if not self.ref(i).equal(other.ref(i)):
                return False
        return True

class W_FlVector(W_VectorSuper):
    _immutable_fields_ = ["len"]
    _attrs_ = ["storage", "len"]
    errorname = "flvector"

    import_from_mixin(StrategyVectorMixin)

    def __init__(self, storage, len):
        self.storage = storage
        self.len = len

    def get_strategy(self):
        return FlonumVectorStrategy.singleton

    def set_strategy(self, strategy):
        assert 0, "unreachable"

    @staticmethod
    def fromelements(elems):
        strategy = FlonumVectorStrategy.singleton
        storage = strategy.create_storage_for_elements(elems)
        return W_FlVector(storage, len(elems))

    @staticmethod
    def fromelement(elem, times):
        check_list = [elem]
        if times == 0:
            check_list = []
        strategy = FlonumVectorStrategy.singleton
        storage = strategy.create_storage_for_element(elem, times)
        return W_FlVector(storage, times)

    def length(self):
        return self.len

    def tostring(self):
        l = self.get_strategy().ref_all(self)
        return "(flvector %s)" % " ".join([obj.tostring() for obj in l])

    def hash_equal(self, info=None):
        x = 0x567890
        for i in range(self.len):
            hash = self.ref(i).hash_equal(info=info)
            x = intmask((1000003 * x) ^ hash)
        return x

    def equal(self, other):
        # XXX could be optimized more
        if not isinstance(other, W_FlVector):
            return False
        if self is other:
            return True
        if self.length() != other.length():
            return False
        for i in range(self.length()):
            if not self.ref(i).equal(other.ref(i)):
                return False
        return True

class VectorStrategy(object):
    """ works for any W_VectorSuper that has
    get/set_strategy, get/set_storage
    """
    __metaclass__ = SingletonMeta

    def is_correct_type(self, w_vector, w_obj):
        raise NotImplementedError("abstract base class")

    def immutable(self):
        return False

    def ref(self, w_vector, i, check=True):
        if check:
            self.indexcheck(w_vector, i)
        return self._ref(w_vector, i)

    def set(self, w_vector, i, w_val, check=True):
        if check:
            self.indexcheck(w_vector, i)
        if not self.is_correct_type(w_vector, w_val):
            self.dehomogenize(w_vector, hint=w_val)
            # Now, try again. no need to use the safe version, we already
            # checked the index
            w_vector.unsafe_set(i, w_val)
        else:
            self._set(w_vector, i, w_val)

    def indexcheck(self, w_vector, i):
        assert 0 <= i < w_vector.length()

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

    def dehomogenize(self, w_vector, hint):
        w_vector.change_strategy(ObjectVectorStrategy.singleton)

class ImmutableVectorStrategyMixin(object):
    def immutable(self):
        return True

    def immutable_variant(self):
        return self

    def _set(self, w_vector, i, w_val):
        assert 0, "unreachable"

    def dehomogenize(self, w_vector, hint):
        assert 0, "unreachable"

class UnwrappedVectorStrategyMixin(object):
    # the concrete class needs to implement:
    # erase, unerase, is_correct_type, wrap, unwrap

    def _copy_storage(self, w_vector, immutable=False):
        strategy = self if not immutable else self.immutable_variant()
        l = self.unerase(w_vector.get_storage())[:]
        return W_Vector(strategy, self.erase(l), w_vector.len)

    def _storage(self, w_vector):
        l = self.unerase(w_vector.get_storage())
        debug.make_sure_not_resized(l)
        return l

    def _ref(self, w_vector, i):
        assert i >= 0
        return self.wrap(self._storage(w_vector)[i])

    def _set(self, w_vector, i, w_val):
        assert i >= 0
        self._storage(w_vector)[i] = self.unwrap(w_val)

    @jit.look_inside_iff(
        lambda strategy, w_vector: jit.isconstant(w_vector.length()) and
               w_vector.length() < UNROLLING_CUTOFF)
    def ref_all(self, w_vector):
        unwrapped = self._storage(w_vector)
        return [self.wrap(i) for i in unwrapped]

    def create_storage_for_element(self, element, times):
        e = self.unwrap(element)
        return self.erase([e] * times)

    @jit.look_inside_iff(
        lambda self, elements_w:
            jit.loop_unrolling_heuristic(
                    elements_w, len(elements_w), UNROLLING_CUTOFF))
    def create_storage_for_elements(self, elements_w):
        if not elements_w:
            return self.erase([])
        l = [self.unwrap(e) for e in elements_w]
        return self.erase(l)

    def wrap(self, obj):
        return obj

    def unwrap(self, w_obj):
        return w_obj

    def unrolling_heuristic(self, w_vector):
        storage = self._storage(w_vector)
        return jit.loop_unrolling_heuristic(storage, w_vector.len, UNROLLING_CUTOFF)

class ConstantVectorStrategy(VectorStrategy):
    # Strategy desribing a vector whose contents are all the same object.
    import_from_mixin(UnwrappedVectorStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("constant-vector-strategy")
    erase   = staticmethod(erase)
    unerase = staticmethod(unerase)

    def is_correct_type(self, w_vector, w_obj):
        from pycket.prims.equal import eqp_logic
        val = self._storage(w_vector)[0]
        return eqp_logic(val, w_obj)

    def create_storage_for_element(self, element, times):
        return self.erase([element])

    def _ref(self, w_vector, i):
        return self._storage(w_vector)[0]

    def _set(self, w_vector, i, w_val):
        if not we_are_translated():
            from pycket.prims.equal import eqp_logic
            self.indexcheck(w_vector, i)
            assert eqp_logic(w_val, self._storage(w_vector)[0])

    def immutable_variant(self):
        return ConstantImmutableVectorStrategy.singleton

    def ref_all(self, w_vector):
        val = self._storage(w_vector)[0]
        return [val] * w_vector.length()

    def dehomogenize(self, w_vector, hint):
        val = self._storage(w_vector)[0]
        len = w_vector.length()
        hinttype = type(hint)
        valtype = type(val)
        if not len:
            newstrategy = ObjectVectorStrategy.singleton
        elif hinttype is valtype:
            if valtype is W_Fixnum:
                newstrategy = FixnumVectorStrategy.singleton
            elif valtype is W_Flonum:
                newstrategy = FlonumVectorStrategy.singleton
            else:
                newstrategy = ObjectVectorStrategy.singleton
        elif hinttype is W_Flonum and valtype is W_Fixnum and can_encode_int32(val.value):
            newstrategy = FlonumTaggedVectorStrategy.singleton
        else:
            newstrategy = ObjectVectorStrategy.singleton
        storage = newstrategy.create_storage_for_element(val, len)
        w_vector.set_strategy(newstrategy)
        w_vector.set_storage(storage)

class ConstantImmutableVectorStrategy(ConstantVectorStrategy):
    import_from_mixin(ImmutableVectorStrategyMixin)

class ObjectVectorStrategy(VectorStrategy):
    import_from_mixin(UnwrappedVectorStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("object-vector-strategy")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def wrap(self, obj):
        return obj

    def unwrap(self, w_obj):
        return w_obj

    def is_correct_type(self, w_vector, w_obj):
        return True

    def create_storage_for_elements(self, elements_w):
        return self.erase(elements_w)

    def dehomogenize(self, w_vector, hint):
        assert 0 # should be unreachable because is_correct_type is always True

    def immutable_variant(self):
        return ObjectImmutableVectorStrategy.singleton

class ObjectImmutableVectorStrategy(ObjectVectorStrategy):
    import_from_mixin(ImmutableVectorStrategyMixin)

class FixnumVectorStrategy(VectorStrategy):
    import_from_mixin(UnwrappedVectorStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("fixnum-vector-strategy")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def is_correct_type(self, w_vector, w_obj):
        return isinstance(w_obj, W_Fixnum)

    def wrap(self, val):
        assert isinstance(val, int)
        return W_Fixnum(val)

    def unwrap(self, w_val):
        assert isinstance(w_val, W_Fixnum)
        return w_val.value

    def immutable_variant(self):
        return FixnumImmutableVectorStrategy.singleton

    def ref_all(self, w_vector):
        unwrapped = self._storage(w_vector)
        return [W_Fixnum.make_or_interned(i) for i in unwrapped]

class FixnumImmutableVectorStrategy(FixnumVectorStrategy):
    import_from_mixin(ImmutableVectorStrategyMixin)

class CharacterVectorStrategy(VectorStrategy):
    import_from_mixin(UnwrappedVectorStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("character-vector-strategy")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def is_correct_type(self, w_vector, w_obj):
        return isinstance(w_obj, W_Character)

    def wrap(self, val):
        return W_Character(val)

    def unwrap(self, w_val):
        assert isinstance(w_val, W_Character)
        return w_val.value

    def immutable_variant(self):
        return CharacterImmutableVectorStrategy.singleton

class CharacterImmutableVectorStrategy(CharacterVectorStrategy):
    import_from_mixin(ImmutableVectorStrategyMixin)

class FlonumVectorStrategy(VectorStrategy):
    import_from_mixin(UnwrappedVectorStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("flonum-vector-strategy")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def is_correct_type(self, w_vector, w_obj):
        return isinstance(w_obj, W_Flonum)

    def wrap(self, val):
        assert isinstance(val, float)
        return W_Flonum(val)

    def unwrap(self, w_val):
        assert isinstance(w_val, W_Flonum)
        return w_val.value

    def immutable_variant(self):
        return FlonumImmutableVectorStrategy.singleton

    def dehomogenize(self, w_vector, hint):
        if type(hint) is W_Fixnum and can_encode_int32(hint.value):
            new_strategy = FlonumTaggedVectorStrategy.singleton
            w_vector.set_strategy(new_strategy)
        else:
            VectorStrategy.dehomogenize(self, w_vector, hint)

class FlonumImmutableVectorStrategy(FlonumVectorStrategy):
    import_from_mixin(ImmutableVectorStrategyMixin)

class FlonumTaggedVectorStrategy(FlonumVectorStrategy):

    def is_correct_type(self, w_vector, w_obj):
        if isinstance(w_obj, W_Fixnum) and can_encode_int32(w_obj.value):
            return True
        return isinstance(w_obj, W_Flonum)

    def wrap(self, val):
        assert isinstance(val, float)
        bits = float2longlong(val)
        if is_int32_from_longlong_nan(bits):
            bits = decode_int32_from_longlong_nan(bits)
            return W_Fixnum(bits)
        return W_Flonum(val)

    def unwrap(self, w_val):
        if isinstance(w_val, W_Fixnum):
            bits = encode_int32_into_longlong_nan(w_val.value)
            return longlong2float(bits)
        assert isinstance(w_val, W_Flonum)
        return w_val.value

    def immutable_variant(self):
        return FlonumTaggedImmutableVectorStrategy.singleton

class FlonumTaggedImmutableVectorStrategy(FlonumTaggedVectorStrategy):
    import_from_mixin(ImmutableVectorStrategyMixin)

@specialize.argtype(0)
def pytype_strategy(lst):
    if not lst:
        strategy = ObjectVectorStrategy.singleton
    elem = lst[0]
    if isinstance(elem, int):
        return FixnumVectorStrategy.singleton
    if isinstance(elem, float):
        return FlonumVectorStrategy.singleton
    if isinstance(elem, W_Object):
        return _find_strategy_class(lst, False)
    assert False, "unsupported type"

@specialize.argtype(0)
def wrap_vector(elems, immutable=False):
    # Allows for direct conversion between RPython lists and vectors with a
    # corresponding strategy simply by copying the underlying list.
    strategy = pytype_strategy(elems)
    if immutable:
        strategy = strategy.immutable_variant()
        storage  = strategy.erase(elems)
    else:
        storage  = strategy.erase(elems[:])
    return W_Vector(strategy, storage, len(elems))

