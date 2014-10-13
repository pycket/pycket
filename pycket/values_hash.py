from pycket.base import W_Object
from pycket import values
from pycket.cont import continuation, label
from rpython.rlib.objectmodel import r_dict, compute_hash

def eq_hash(k):
    if isinstance(k, values.W_Fixnum):
        return compute_hash(k.value)
    else:
        return compute_hash(k)

class W_HashTable(W_Object):
    errorname = "hash"

    def hash_keys(self):
        raise NotImplementedError("abstract method")

    @label
    def hash_set(self, k, v, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def hash_ref(self, k, env, cont):
        raise NotImplementedError("abstract method")

class W_SimpleHashTable(W_HashTable):

    @staticmethod
    def hash_value(v):
        raise NotImplementedError("abstract method")

    @staticmethod
    def cmp_value(a, b):
        raise NotImplementedError("abstract method")

    def __init__(self, keys, vals):
        from pycket.prims.equal import eqp_logic
        assert len(keys) == len(vals)
        self.data = r_dict(self.cmp_value, self.hash_value, force_non_null=True)
        for i, k in enumerate(keys):
            self.data[k] = vals[i]

    def hash_keys(self):
        return self.data.keys()

    def tostring(self):
        lst = [W_Cons.make(k, v).tostring() for k, v in self.data.iteritems()]
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

class W_EqvHashTable(W_SimpleHashTable):
    @staticmethod
    def hash_value(k):
        return eq_hash(k)

    @staticmethod
    def cmp_value(a, b):
        return a.eqv(b)

class W_EqHashTable(W_SimpleHashTable):
    @staticmethod
    def hash_value(k):
        return eq_hash(k)

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

class W_EqualHashTable(W_HashTable):
    def __init__(self, keys, vals):
        self.mapping = [(k, vals[i]) for i, k in enumerate(keys)]

    def hash_keys(self):
        return [k for k, _ in self.mapping]

    def tostring(self):
        lst = [W_Cons.make(k, v).tostring() for k, v in self.mapping]
        return "#hash(%s)" % " ".join(lst)

    @label
    def hash_set(self, key, val, env, cont):
        return equal_hash_set_loop(self.mapping, 0, key, val, env, cont)

    @label
    def hash_ref(self, key, env, cont):
        return equal_hash_ref_loop(self.mapping, 0, key, env, cont)
