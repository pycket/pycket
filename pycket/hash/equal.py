
from pycket                   import config
from pycket                   import values, values_string
from pycket.base              import SingletonMeta, UnhashableType
from pycket.hash.base         import W_HashTable, get_dict_item, w_missing
from pycket.error             import SchemeException
from pycket.cont              import continuation, loop_label
from rpython.rlib             import rerased
from rpython.rlib.objectmodel import compute_hash, import_from_mixin, r_dict, specialize

@loop_label
def equal_hash_ref_loop(data, idx, key, env, cont):
    from pycket.interpreter import return_value
    from pycket.prims.equal import equal_func, EqualInfo
    if idx >= len(data):
        return return_value(w_missing, env, cont)
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

    def get_item(self, w_dict, i):
        raise NotImplementedError("abstract base class")

    def length(self, w_dict):
        raise NotImplementedError("abstract base class")

    def create_storage(self, keys, vals):
        raise NotImplementedError("abstract base class")

def _find_strategy_class(keys):
    if not config.strategies:
        return ObjectHashmapStrategy.singleton
    if len(keys) == 0:
        return EmptyHashmapStrategy.singleton
        # An empty vector stays empty forever. Don't implement special EmptyVectorStrategy.
    single_class = type(keys[0])
    for elem in keys:
        if not isinstance(elem, single_class):
            return ObjectHashmapStrategy.singleton
    if single_class is values.W_Fixnum:
        return FixnumHashmapStrategy.singleton
    if single_class is values.W_Symbol:
        return SymbolHashmapStrategy.singleton
    if single_class is values_string.W_String:
        return StringHashmapStrategy.singleton
    if single_class is values.W_Bytes:
        return ByteHashmapStrategy.singleton
    return ObjectHashmapStrategy.singleton

class UnwrappedHashmapStrategyMixin(object):
    # the concrete class needs to implement:
    # erase, unerase, is_correct_type, wrap, unwrap
    # create_storage needs to be overwritten if an r_dict is needed

    def get(self, w_dict, w_key, env, cont):
        from pycket.interpreter import return_value
        if self.is_correct_type(w_key):
            w_res = self.unerase(w_dict.hstorage).get(self.unwrap(w_key), w_missing)
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

    def get_item(self, w_dict, i):
        key, w_val = get_dict_item(self.unerase(w_dict.hstorage), i)
        return self.wrap(key), w_val

    def length(self, w_dict):
        return len(self.unerase(w_dict.hstorage))

    def create_storage(self, keys, vals):
        d = self._create_empty_dict()
        if not keys:
            return self.erase(d)
        for i, w_key in enumerate(keys):
            d[self.unwrap(w_key)] = vals[i]
        return self.erase(d)

    def _create_empty_dict(self):
        return {}

    def switch_to_object_strategy(self, w_dict):
        d = self.unerase(w_dict.hstorage)
        keys = [self.wrap(key) for key in d.keys()]
        values = d.values()
        strategy = ObjectHashmapStrategy.singleton
        storage = strategy.create_storage(keys, values)
        w_dict.strategy = strategy
        w_dict.hstorage = storage


class EmptyHashmapStrategy(HashmapStrategy):
    erase, unerase = rerased.new_static_erasing_pair("object-hashmap-strategry")

    def get(self, w_dict, w_key, env, cont):
        from pycket.interpreter import return_value
        return return_value(w_missing, env, cont) # contains nothing

    def set(self, w_dict, w_key, w_val, env, cont):
        self.switch_to_correct_strategy(w_dict, w_key)
        return w_dict.hash_set(w_key, w_val, env, cont)

    def items(self, w_dict):
        return []

    def get_item(self, w_dict, i):
        raise IndexError

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
        elif isinstance(w_key, values_string.W_String):
            strategy = StringHashmapStrategy.singleton
        elif isinstance(w_key, values.W_Bytes):
            strategy = ByteHashmapStrategy.singleton
        else:
            strategy = ObjectHashmapStrategy.singleton
        storage = strategy.create_storage([], [])
        w_dict.strategy = strategy
        w_dict.hstorage = storage

class Bucket(object):
    _attrs_ = _immutable_fields_ = ["entries"]

    def __init__(self):
        self.entries = []

    def add_entry(self, key, val):
        self.entries.append((key, val))

    def size(self):
        return len(self.entries)

UNHASHABLE_TAG = 0b0001

def tagged_hash(w_object):
    try:
        return w_object.hash_equal() << 1
    except UnhashableType:
        return UNHASHABLE_TAG

class ObjectHashmapStrategy(HashmapStrategy):
    erase, unerase = rerased.new_static_erasing_pair("object-hashmap-strategry")

    def get_bucket(self, w_dict, w_key, nonull=False):
        hash    = tagged_hash(w_key)
        storage = self.unerase(w_dict.hstorage)
        bucket  = storage.get(hash, None)

        if nonull and bucket is None:
            storage[hash] = bucket = Bucket()

        return bucket

    def get(self, w_dict, w_key, env, cont):
        from pycket.interpreter import return_value
        bucket = self.get_bucket(w_dict, w_key)
        if bucket is None:
            return return_value(w_missing, env, cont)
        entries = bucket.entries
        return equal_hash_ref_loop(entries, 0, w_key, env, cont)

    def set(self, w_dict, w_key, w_val, env, cont):
        bucket = self.get_bucket(w_dict, w_key, nonull=True)
        return equal_hash_set_loop(bucket.entries, 0, w_key, w_val, env, cont)

    def items(self, w_dict):
        items = []
        storage = self.unerase(w_dict.hstorage)
        for bucket in storage.itervalues():
            for item in bucket.entries:
                items.append(item)
        return items

    def get_item(self, w_dict, i):
        storage = self.unerase(w_dict.hstorage)
        for bucket in storage.itervalues():
            size = bucket.size()
            if size > i:
                return bucket.entries[i]
            i -= size
        raise IndexError


    def length(self, w_dict):
        storage = self.unerase(w_dict.hstorage)
        size = 0
        for bucket in storage.itervalues():
            size += bucket.size()
        return size

    def create_storage(self, keys, vals):
        storage = {}
        for i, key in enumerate(keys):
            val  = vals[i]
            hash = tagged_hash(key)
            bucket = storage.get(hash, None)
            if bucket is None:
                storage[hash] = bucket = Bucket()
            bucket.add_entry(key, val)
        return self.erase(storage)


class FixnumHashmapStrategy(HashmapStrategy):
    import_from_mixin(UnwrappedHashmapStrategyMixin)

    erase, unerase = rerased.new_static_erasing_pair("fixnum-hashmap-strategry")

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

    erase, unerase = rerased.new_static_erasing_pair("symbol-hashmap-strategry")

    def is_correct_type(self, w_obj):
        return isinstance(w_obj, values.W_Symbol)

    def wrap(self, val):
        assert isinstance(val, values.W_Symbol)
        return val

    def unwrap(self, w_val):
        assert isinstance(w_val, values.W_Symbol)
        return w_val


def hash_strings(w_b):
    assert isinstance(w_b, values_string.W_String)
    return w_b.hash_equal()

def cmp_strings(w_a, w_b):
    assert isinstance(w_a, values_string.W_String)
    assert isinstance(w_b, values_string.W_String)
    return w_a.equal(w_b)


class StringHashmapStrategy(HashmapStrategy):
    import_from_mixin(UnwrappedHashmapStrategyMixin)

    erase, unerase = rerased.new_static_erasing_pair("string-hashmap-strategry")

    def is_correct_type(self, w_obj):
        return isinstance(w_obj, values_string.W_String)

    def wrap(self, w_val):
        return w_val

    def unwrap(self, w_val):
        return w_val

    def _create_empty_dict(self):
        return r_dict(cmp_strings, hash_strings)


def hash_bytes(w_b):
    assert isinstance(w_b, values.W_Bytes)
    return w_b.hash_equal()

def cmp_bytes(w_a, w_b):
    assert isinstance(w_a, values.W_Bytes)
    assert isinstance(w_b, values.W_Bytes)
    return w_a.value == w_b.value

class ByteHashmapStrategy(HashmapStrategy):
    import_from_mixin(UnwrappedHashmapStrategyMixin)

    erase, unerase = rerased.new_static_erasing_pair("byte-hashmap-strategry")

    def is_correct_type(self, w_obj):
        return isinstance(w_obj, values.W_Bytes)

    def wrap(self, val):
        return val

    def unwrap(self, w_val):
        assert isinstance(w_val, values.W_Bytes)
        return w_val

    def _create_empty_dict(self):
        return r_dict(cmp_bytes, hash_bytes)

class W_EqualHashTable(W_HashTable):
    _attrs_ = ['strategy', 'hstorage', 'is_immutable']
    _immutable_fields_ = ['is_immutable']
    def __init__(self, keys, vals, immutable=False):
        self.is_immutable = immutable
        self.strategy = _find_strategy_class(keys)
        self.hstorage = self.strategy.create_storage(keys, vals)

    def immutable(self):
        return self.is_immutable

    def hash_items(self):
        return self.strategy.items(self)

    def hash_set(self, key, val, env, cont):
        return self.strategy.set(self, key, val, env, cont)

    def hash_ref(self, key, env, cont):
        return self.strategy.get(self, key, env, cont)

    def get_item(self, i):
        return self.strategy.get_item(self, i)

    def length(self):
        return self.strategy.length(self)

    def make_empty(self):
        return W_EqualHashTable([], [], immutable=self.is_immutable)

    def tostring(self):
        lst = [values.W_Cons.make(k, v).tostring() for k, v in self.hash_items()]
        return "#hash(%s)" % " ".join(lst)

