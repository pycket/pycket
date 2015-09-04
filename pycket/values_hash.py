from pycket       import config
from pycket       import values, values_string
from pycket.base  import W_Object, SingletonMeta
from pycket.cont  import continuation, label, loop_label

from rpython.rlib             import rerased
from rpython.rlib.objectmodel import compute_hash, import_from_mixin, r_dict, specialize


class W_Missing(W_Object):
    def __init__(self):
        pass

w_missing = W_Missing() # sentinel for missing values

class W_HashTable(W_Object):
    errorname = "hash"
    _attrs_ = ["is_immutable"]
    _immutable_fields_ = ["is_immutable"]
    _settled_ = True

    def hash_items(self):
        raise NotImplementedError("abstract method")

    @label
    def hash_set(self, k, v, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def hash_ref(self, k, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def hash_remove_inplace(self, k, env, cont):
        raise NotImplementedError("abstract method")

    def length(self):
        raise NotImplementedError("abstract method")

    def make_empty(self):
        raise NotImplementedError("abstract method")

    def immutable(self):
        return self.is_immutable

    def get_item(self, i):
        # see get_dict_item at the bottom of the file for the interface
        raise NotImplementedError("abstract method")

@specialize.arg(0)
def make_simple_table(cls, keys=None, vals=None, immutable=False):
    data = r_dict(cls.cmp_value, cls.hash_value, force_non_null=True)
    if keys is not None and vals is not None:
        assert len(keys) == len(vals)
        for i, k in enumerate(keys):
            data[k] = vals[i]
    return cls(data, immutable)

class W_SimpleHashTable(W_HashTable):
    _attrs_ = ['data']
    _immutable_fields_ = ["data"]

    @staticmethod
    def hash_value(v):
        raise NotImplementedError("abstract method")

    @staticmethod
    def cmp_value(a, b):
        raise NotImplementedError("abstract method")

    def __init__(self, data, immutable):
        self.is_immutable = immutable
        self.data         = data

    def make_copy(self):
        raise NotImplementedError("abstract method")

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
    def hash_remove_inplace(self, k, env, cont):
        from pycket.interpreter import return_value
        del self.data[k]
        return return_value(values.w_void, env, cont)

    @label
    def hash_ref(self, k, env, cont):
        from pycket.interpreter import return_value
        return return_value(self.data.get(k, w_missing), env, cont)

    def length(self):
        return len(self.data)

class W_EqvHashTable(W_SimpleHashTable):

    def make_empty(self):
        return make_simple_table(W_EqvHashTable, immutable=self.is_immutable)

    def make_copy(self):
        return W_EqvHashTable(self.data.copy(), immutable=self.is_immutable)

    @staticmethod
    def hash_value(k):
        return k.hash_eqv()

    @staticmethod
    def cmp_value(a, b):
        return a.eqv(b)

    def get_item(self, i):
        return get_dict_item(self.data, i)

class W_EqHashTable(W_SimpleHashTable):

    def make_copy(self):
        return W_EqHashTable(self.data.copy(), immutable=self.is_immutable)

    def make_empty(self):
        return make_simple_table(W_EqHashTable, immutable=self.is_immutable)

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

    def get_item(self, i):
        return get_dict_item(self.data, i)

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


class ObjectHashmapStrategy(HashmapStrategy):
    erase, unerase = rerased.new_static_erasing_pair("object-hashmap-strategry")

    def get(self, w_dict, w_key, env, cont):
        return equal_hash_ref_loop(self.unerase(w_dict.hstorage), 0, w_key, env, cont)

    def set(self, w_dict, w_key, w_val, env, cont):
        return equal_hash_set_loop(self.unerase(w_dict.hstorage), 0, w_key, w_val, env, cont)

    def items(self, w_dict):
        return self.unerase(w_dict.hstorage)

    def get_item(self, w_dict, i):
        try:
            return self.unerase(w_dict.hstorage)[i]
        except IndexError:
            raise

    def length(self, w_dict):
        return len(self.unerase(w_dict.hstorage))

    def create_storage(self, keys, vals):
        return self.erase([(k, vals[i]) for i, k in enumerate(keys)])


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
    _attrs_ = ['strategy', 'hstorage']
    def __init__(self, keys, vals, immutable=False):
        self.is_immutable = immutable
        self.strategy = _find_strategy_class(keys)
        self.hstorage = self.strategy.create_storage(keys, vals)

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

def get_dict_item(d, i):
    """ return item of dict d at position i. Raises a KeyError if the index
    carries no valid entry. Raises IndexError if the index is beyond the end of
    the dict. """
    return d.items()[i]

def ll_get_dict_item(RES, dict, i):
    from rpython.rtyper.lltypesystem import lltype
    from rpython.rtyper.lltypesystem.rordereddict import recast
    entries = dict.entries
    assert i >= 0
    if i >= dict.num_ever_used_items:
        raise IndexError
    if entries.valid(i):
        entry = entries[i]
        r = lltype.malloc(RES.TO)
        r.item0 = recast(RES.TO.item0, entry.key)
        r.item1 = recast(RES.TO.item1, entry.value)
        return r
    else:
        raise KeyError

from rpython.rtyper.extregistry import ExtRegistryEntry


class Entry(ExtRegistryEntry):
    _about_ = get_dict_item

    def compute_result_annotation(self, s_d, s_i):
        from rpython.annotator.model import SomeTuple, SomeInteger
        s_key = s_d.dictdef.read_key()
        s_value = s_d.dictdef.read_value()
        return SomeTuple([s_key, s_value])

    def specialize_call(self, hop):
        from rpython.rtyper.lltypesystem import lltype
        # somewhat evil hackery
        dictrepr = hop.rtyper.getrepr(hop.args_s[0])
        v_dict, v_index = hop.inputargs(dictrepr, lltype.Signed)
        r_tuple = hop.rtyper.getrepr(hop.s_result)
        cTUPLE = hop.inputconst(lltype.Void, r_tuple.lowleveltype)
        hop.exception_is_here()
        v_res = hop.gendirectcall(ll_get_dict_item, cTUPLE, v_dict, v_index)
        return v_res


