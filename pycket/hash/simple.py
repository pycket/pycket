
from pycket                          import values
from pycket.error                    import SchemeException
from pycket.hash.base                import (
    W_MutableHashTable,
    W_ImmutableHashTable,
    w_missing,
    get_dict_item)
from pycket.hash.persistent_hash_map import make_persistent_hash_type
from rpython.rlib.objectmodel        import compute_hash, r_dict, specialize
from rpython.rlib.rarithmetic        import r_uint

@specialize.arg(0)
def make_simple_mutable_table(cls, keys=None, vals=None):
    data = r_dict(cls.cmp_value, cls.hash_value, force_non_null=True)
    if keys is not None and vals is not None:
        assert len(keys) == len(vals)
        for i, k in enumerate(keys):
            data[k] = vals[i]
    return cls(data)

@specialize.arg(0)
def make_simple_mutable_table_assocs(cls, assocs, who):
    if not assocs.is_proper_list():
        raise SchemeException("%s: not given proper list" % who)
    data = r_dict(cls.cmp_value, cls.hash_value, force_non_null=True)
    while isinstance(assocs, values.W_Cons):
        entry, assocs = assocs.car(), assocs.cdr()
        if not isinstance(entry, values.W_Cons):
            raise SchemeException("%s: expected list of pairs" % who)
        key, val = entry.car(), entry.cdr()
        data[key] = val
    return cls(data)

@specialize.arg(0)
def make_simple_immutable_table(cls, keys=None, vals=None):
    table = cls.EMPTY
    if keys is not None and vals is not None:
        assert len(keys) == len(vals)
        for i, k in enumerate(keys):
            table = table.assoc(k, vals[i])
    return table

@specialize.arg(0)
def make_simple_immutable_table_assocs(cls, assocs, who):
    if not assocs.is_proper_list():
        raise SchemeException("%s: not given proper list" % who)
    table = cls.EMPTY
    while isinstance(assocs, values.W_Cons):
        entry, assocs = assocs.car(), assocs.cdr()
        if not isinstance(entry, values.W_Cons):
            raise SchemeException("%s: expected list of pairs" % who)
        key, val = entry.car(), entry.cdr()
        table = table.assoc(key, val)
    return table

class W_SimpleMutableHashTable(W_MutableHashTable):
    _attrs_ = ['data']
    _immutable_fields_ = ["data"]

    @staticmethod
    def hash_value(v):
        raise NotImplementedError("abstract method")

    @staticmethod
    def cmp_value(a, b):
        raise NotImplementedError("abstract method")

    def __init__(self, data):
        self.data = data

    def make_copy(self):
        raise NotImplementedError("abstract method")

    def hash_items(self):
        return self.data.items()

    def hash_empty(self):
        self.data = r_dict(self.cmp_value, self.hash_value, force_non_null=True)

    def tostring(self):
        lst = [values.W_Cons.make(k, v).tostring() for k, v in self.data.iteritems()]
        return "#hash(%s)" % " ".join(lst)

    def hash_set(self, k, v, env, cont):
        from pycket.interpreter import return_value
        self.data[k] = v
        return return_value(values.w_void, env, cont)

    def hash_remove_inplace(self, k, env, cont):
        from pycket.interpreter import return_value
        del self.data[k]
        return return_value(values.w_void, env, cont)

    def hash_ref(self, k, env, cont):
        from pycket.interpreter import return_value
        return return_value(self.data.get(k, w_missing), env, cont)

    def length(self):
        return len(self.data)

class W_EqvMutableHashTable(W_SimpleMutableHashTable):

    def make_empty(self):
        return make_simple_mutable_table(W_EqvMutableHashTable)

    def make_copy(self):
        return W_EqvMutableHashTable(self.data.copy(), immutable=False)

    @staticmethod
    def hash_value(k):
        return k.hash_eqv()

    @staticmethod
    def cmp_value(a, b):
        return a.eqv(b)

    def get_item(self, i):
        return get_dict_item(self.data, i)

class W_EqMutableHashTable(W_SimpleMutableHashTable):

    def make_copy(self):
        return W_EqMutableHashTable(self.data.copy())

    def make_empty(self):
        return make_simple_mutable_table(W_EqMutableHashTable)

    @staticmethod
    def hash_value(k):
        if isinstance(k, values.W_Fixnum):
            return compute_hash(k.value)
        if isinstance(k, values.W_Character):
            return ord(k.value)
        return compute_hash(k)

    @staticmethod
    def cmp_value(a, b):
        from pycket.prims.equal import eqp_logic
        return eqp_logic(a, b)

    def get_item(self, i):
        return get_dict_item(self.data, i)

W_EqvImmutableHashTable = make_persistent_hash_type(
        super=W_ImmutableHashTable,
        keytype=values.W_Object,
        valtype=values.W_Object,
        name="W_EqvImmutableHashTable",
        hashfun=lambda x: r_uint(W_EqvMutableHashTable.hash_value(x)),
        equal=W_EqvMutableHashTable.cmp_value)

W_EqImmutableHashTable = make_persistent_hash_type(
        super=W_ImmutableHashTable,
        keytype=values.W_Object,
        valtype=values.W_Object,
        name="W_EqImmutableHashTable",
        hashfun=lambda x: r_uint(W_EqMutableHashTable.hash_value(x)),
        equal=W_EqMutableHashTable.cmp_value)

class __extend__(W_EqvImmutableHashTable):

    def length(self):
        return len(self)

    def make_copy(self):
        return self

    def make_empty(self):
        return W_EqvImmutableHashTable.EMPTY

    def hash_ref(self, k, env, cont):
        from pycket.interpreter import return_value
        result = self.val_at(k, w_missing)
        return return_value(result, env, cont)

    def hash_remove(self, key, env, cont):
        from pycket.interpreter import return_value
        removed = self.without(key)
        return return_value(removed, env, cont)

    def tostring(self):
        assert type(self) is W_EqvImmutableHashTable
        entries = [None] * len(self)
        i = 0
        for k, v in self.iteritems():
            entries[i] = "(%s . %s)" % (k.tostring(), v.tostring())
            i += 1
        return "#hasheqv(%s)" % " ".join(entries)

class __extend__(W_EqImmutableHashTable):

    def length(self):
        return len(self)

    def make_copy(self):
        return self

    def make_empty(self):
        return W_EqImmutableHashTable.EMPTY

    def hash_ref(self, key, env, cont):
        from pycket.interpreter import return_value
        result = self.val_at(key, w_missing)
        return return_value(result, env, cont)

    def hash_remove(self, key, env, cont):
        from pycket.interpreter import return_value
        removed = self.without(key)
        return return_value(removed, env, cont)

    def tostring(self):
        assert type(self) is W_EqImmutableHashTable
        entries = [None] * len(self)
        i = 0
        for k, v in self.iteritems():
            entries[i] = "(%s . %s)" % (k.tostring(), v.tostring())
            i += 1
        return "#hasheq(%s)" % " ".join(entries)

