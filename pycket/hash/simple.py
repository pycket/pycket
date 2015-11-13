
from pycket                   import values
from pycket.cont              import label
from pycket.error             import SchemeException
from pycket.hash.base         import W_HashTable, w_missing, get_dict_item
from rpython.rlib.objectmodel import compute_hash, r_dict, specialize

@specialize.arg(0)
def make_simple_table(cls, keys=None, vals=None, immutable=False):
    data = r_dict(cls.cmp_value, cls.hash_value, force_non_null=True)
    if keys is not None and vals is not None:
        assert len(keys) == len(vals)
        for i, k in enumerate(keys):
            data[k] = vals[i]
    return cls(data, immutable)

@specialize.arg(0)
def make_simple_table_assocs(cls, assocs, who, immutable=False):
    data = r_dict(cls.cmp_value, cls.hash_value, force_non_null=True)
    if not assocs.is_proper_list():
        raise SchemeException("%s: not given proper list" % who)
    while isinstance(data, values.W_Cons):
        entry, assocs = assocs.car(), assocs.cdr()
        if not isinstance(entry, values.W_Cons):
            raise SchemeException("%s: expected list of pairs" % who)
        key, val = entry.car(), entry.cdr()
        data[key] = val
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


