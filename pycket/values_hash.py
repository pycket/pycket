from pycket.base import W_Object, SingletonMeta
from pycket import values
from pycket.cont import continuation, label

from rpython.rlib.objectmodel import r_dict, compute_hash, import_from_mixin
from rpython.rlib import rerased

class W_HashTable(W_Object):
    errorname = "hash"
    _attrs_ = []
    _settled_ = True

    def hash_items(self):
        raise NotImplementedError("abstract method")

    @label
    def hash_set(self, k, v, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def hash_ref(self, k, env, cont):
        raise NotImplementedError("abstract method")

    def length(self):
        raise NotImplementedError("abstract method")


class W_SimpleHashTable(W_HashTable):
    _attrs_ = ['data']

    @staticmethod
    def hash_value(v):
        raise NotImplementedError("abstract method")

    @staticmethod
    def cmp_value(a, b):
        raise NotImplementedError("abstract method")

    def __init__(self, keys, vals):
        assert len(keys) == len(vals)
        self.data = r_dict(self.cmp_value, self.hash_value, force_non_null=True)
        for i, k in enumerate(keys):
            self.data[k] = vals[i]

    def hash_items(self):
        return self.data.items()

    def tostring(self):
        lst = [values.W_Cons.make(k, v).tostring() for k, v in self.data.iteritems()]
        return "#hash(%s)" % " ".join(lst)

    @label
    def hash_set(self, k, v, env, cont):
        from pycket.interpreter import return_value
        self.data[k] = v
        return return_value(values.w_void, env, cont)

    @label
    def hash_ref(self, k, env, cont):
        from pycket.interpreter import return_value
        return return_value(self.data.get(k, None), env, cont)

    def length(self):
        return len(self.data)

class W_EqvHashTable(W_SimpleHashTable):
    @staticmethod
    def hash_value(k):
        return k.hash_eqv()

    @staticmethod
    def cmp_value(a, b):
        return a.eqv(b)

class W_EqHashTable(W_SimpleHashTable):
    @staticmethod
    def hash_value(k):
        if isinstance(k, values.W_Fixnum):
            return compute_hash(k.value)
        if isinstance(k, values.W_Character):
            return ord(k.value)
        else:
            return compute_hash(k)

    @staticmethod
    def cmp_value(a, b):
        from pycket.prims.equal import eqp_logic
        return eqp_logic(a, b)

def equal_hash_ref_loop(data, idx, key, env, cont):
    from pycket.interpreter import return_value
    from pycket.prims.equal import equal_func, EqualInfo
    if idx >= len(data):
        return return_value(None, env, cont)
    k, v = data[idx]
    info = EqualInfo.BASIC_SINGLETON
    return equal_func(k, key, info, env,
            catch_ref_is_equal_cont(data, idx, key, v, env, cont))

@continuation
def catch_ref_is_equal_cont(data, idx, key, v, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    val = check_one_val(_vals)
    if val is not values.w_false:
        return return_value(v, env, cont)
    return equal_hash_ref_loop(data, idx + 1, key, env, cont)

def equal_hash_set_loop(data, idx, key, val, env, cont):
    from pycket.interpreter import check_one_val, return_value
    from pycket.prims.equal import equal_func, EqualInfo
    if idx >= len(data):
        data.append((key, val))
        return return_value(values.w_void, env, cont)
    k, _ = data[idx]
    info = EqualInfo.BASIC_SINGLETON
    return equal_func(k, key, info, env,
            catch_set_is_equal_cont(data, idx, key, val, env, cont))

@continuation
def catch_set_is_equal_cont(data, idx, key, val, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    cmp = check_one_val(_vals)
    if cmp is not values.w_false:
        data[idx] = (key, val)
        return return_value(values.w_void, env, cont)
    return equal_hash_set_loop(data, idx + 1, key, val, env, cont)


class HashmapStrategy(object):
    __metaclass__ = SingletonMeta

    def get(self, w_dict, w_key, env, cont):
        raise NotImplementedError("abstract base class")

    def set(self, w_dict, w_key, w_val, env, cont):
        raise NotImplementedError("abstract base class")

    def items(self, w_dict):
        raise NotImplementedError("abstract base class")

    def length(self, w_dict):
        raise NotImplementedError("abstract base class")

    def create_storage(self, keys, vals):
        raise NotImplementedError("abstract base class")


def _find_strategy_class(keys):
    if len(keys) == 0:
        return EmptyHashmapStrategy.singleton
        # An empty vector stays empty forever. Don't implement special EmptyVectorStrategy.
        return ObjectVectorStrategy.singleton
    single_class = type(keys[0])
    for elem in keys:
        if not isinstance(elem, single_class):
            return ObjectHashmapStrategy.singleton
    if single_class is values.W_Fixnum:
        return FixnumHashmapStrategy.singleton
    if single_class is values.W_Symbol:
        return SymbolHashmapStrategy.singleton
    if single_class is values.W_String:
        return StringHashmapStrategy.singleton
    return ObjectHashmapStrategy.singleton

class UnwrappedHashmapStrategyMixin(object):
    # the concrete class needs to implement:
    # erase, unerase, is_correct_type, wrap, unwrap
    # create_storage needs to be overwritten if an r_dict is needed

    def get(self, w_dict, w_key, env, cont):
        from pycket.interpreter import return_value
        if self.is_correct_type(w_key):
            w_res = self.unerase(w_dict.hstorage).get(self.unwrap(w_key), None)
            return return_value(w_res, env, cont)
        # XXX should not dehomogenize always
        self.switch_to_object_strategy(w_dict)
        return w_dict.hash_ref(w_key, env, cont)

    def set(self, w_dict, w_key, w_val, env, cont):
        from pycket.interpreter import return_value
        if self.is_correct_type(w_key):
            self.unerase(w_dict.hstorage)[self.unwrap(w_key)] = w_val
            return return_value(values.w_void, env, cont)
        self.switch_to_object_strategy(w_dict)
        return w_dict.hash_set(w_key, w_val, env, cont)

    def items(self, w_dict):
        return [(self.wrap(key), w_val) for key, w_val in self.unerase(w_dict.hstorage).iteritems()]

    def length(self, w_dict):
        return len(self.unerase(w_dict.hstorage))

    def create_storage(self, keys, vals):
        if not keys:
            return self.erase({})
        d = {}
        for i, w_key in enumerate(keys):
            d[self.unwrap(w_key)] = vals[i]
        return self.erase(d)

    def switch_to_object_strategy(self, w_dict):
        d = self.unerase(w_dict.hstorage)
        keys = [self.wrap(key) for key in d.keys()]
        values = d.values()
        strategy = ObjectHashmapStrategy.singleton
        storage = strategy.create_storage(keys, values)
        w_dict.strategy = strategy
        w_dict.hstorage = storage


class EmptyHashmapStrategy(HashmapStrategy):
    erase, unerase = rerased.new_erasing_pair("object-hashmap-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def get(self, w_dict, w_key, env, cont):
        from pycket.interpreter import return_value
        return return_value(None, env, cont) # contains nothing

    def set(self, w_dict, w_key, w_val, env, cont):
        self.switch_to_correct_strategy(w_dict, w_key)
        return w_dict.hash_set(w_key, w_val, env, cont)

    def items(self, w_dict):
        return []

    def length(self, w_dict):
        return 0

    def create_storage(self, keys, vals):
        assert not keys
        assert not vals
        return self.erase(None)

    def switch_to_correct_strategy(self, w_dict, w_key):
        if type(w_key) is values.W_Fixnum:
            strategy = FixnumHashmapStrategy.singleton
        elif type(w_key) is values.W_Symbol:
            strategy = SymbolHashmapStrategy.singleton
        elif type(w_key) is values.W_String:
            strategy = StringHashmapStrategy.singleton
        else:
            strategy = ObjectHashmapStrategy.singleton
        storage = strategy.create_storage([], [])
        w_dict.strategy = strategy
        w_dict.hstorage = storage


class ObjectHashmapStrategy(HashmapStrategy):
    erase, unerase = rerased.new_erasing_pair("object-hashmap-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def get(self, w_dict, w_key, env, cont):
        return equal_hash_ref_loop(self.unerase(w_dict.hstorage), 0, w_key, env, cont)

    def set(self, w_dict, w_key, w_val, env, cont):
        return equal_hash_set_loop(self.unerase(w_dict.hstorage), 0, w_key, w_val, env, cont)

    def items(self, w_dict):
        return self.unerase(w_dict.hstorage)

    def length(self, w_dict):
        return len(self.unerase(w_dict.hstorage))

    def create_storage(self, keys, vals):
        return self.erase([(k, vals[i]) for i, k in enumerate(keys)])


class FixnumHashmapStrategy(HashmapStrategy):
    import_from_mixin(UnwrappedHashmapStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("fixnum-hashmap-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def is_correct_type(self, w_obj):
        return isinstance(w_obj, values.W_Fixnum)

    def wrap(self, val):
        assert isinstance(val, int)
        return values.W_Fixnum(val)

    def unwrap(self, w_val):
        assert isinstance(w_val, values.W_Fixnum)
        return w_val.value


class SymbolHashmapStrategy(HashmapStrategy):
    import_from_mixin(UnwrappedHashmapStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("symbol-hashmap-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def is_correct_type(self, w_obj):
        return isinstance(w_obj, values.W_Symbol)

    def wrap(self, val):
        assert isinstance(val, values.W_Symbol)
        return val

    def unwrap(self, w_val):
        assert isinstance(w_val, values.W_Symbol)
        return w_val


class StringHashmapStrategy(HashmapStrategy):
    import_from_mixin(UnwrappedHashmapStrategyMixin)

    erase, unerase = rerased.new_erasing_pair("string-hashmap-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)

    def is_correct_type(self, w_obj):
        return isinstance(w_obj, values.W_String)

    def wrap(self, val):
        return values.W_String(val)

    def unwrap(self, w_val):
        assert isinstance(w_val, values.W_String)
        # note that even for mutable strings this is safe: racket makes no
        # promises about what happens when you mutate a key of a dict so just
        # using the old value is an ok implementation
        return w_val.value


class W_EqualHashTable(W_HashTable):
    _attrs_ = ['strategy', 'hstorage']
    def __init__(self, keys, vals):
        self.strategy = _find_strategy_class(keys)
        self.hstorage = self.strategy.create_storage(keys, vals)

    def hash_items(self):
        return self.strategy.items(self)

    def hash_set(self, key, val, env, cont):
        return self.strategy.set(self, key, val, env, cont)

    def hash_ref(self, key, env, cont):
        return self.strategy.get(self, key, env, cont)

    def length(self):
        return self.strategy.length(self)

    def tostring(self):
        lst = [values.W_Cons.make(k, v).tostring() for k, v in self.hash_items()]
        return "#hash(%s)" % " ".join(lst)

