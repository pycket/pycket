from pycket                   import config
from pycket                   import values, values_string
from pycket.base              import W_Object, SingletonMeta
from pycket.error             import SchemeException
from pycket.cont              import continuation, label, loop_label
from rpython.rlib             import rerased
from rpython.rlib.objectmodel import compute_hash, import_from_mixin, r_dict, specialize

class W_Missing(W_Object):
    _attrs_ = []
    def __init__(self):
        pass

w_missing = W_Missing() # sentinel for missing values

class W_HashTable(W_Object):
    errorname = "hash"
    _attrs_ = []
    _immutable_fields_ = []

    def hash_items(self):
        raise NotImplementedError("abstract method")

    def hash_set(self, k, v, env, cont):
        raise NotImplementedError("abstract method")

    def hash_ref(self, k, env, cont):
        raise NotImplementedError("abstract method")

    def _hash_ref(self, k):
        raise NotImplementedError("abstract method")

    def _hash_set(self, k, v):
        raise NotImplementedError("abstract method")

    def hash_remove(self, k, env, cont):
        raise NotImplementedError("abstract method")

    def hash_remove_inplace(self, k, env, cont):
        raise NotImplementedError("abstract method")

    def length(self):
        raise NotImplementedError("abstract method")

    def make_empty(self):
        raise NotImplementedError("abstract method")

    def get_item(self, i):
        # see get_dict_item at the bottom of the file for the interface
        raise NotImplementedError("abstract method")

    def hash_iterate_next(self, pos):
        i = pos.value
        if i >= self.length() - 1:
            return values.w_false
        return values.wrap(i + 1)

    def hash_iterate_first(self):
        if self.length() == 0:
            raise IndexError
        return 0

class W_MutableHashTable(W_HashTable):
    _attrs_ = []
    _immutable_fields_ = []
    def immutable(self):
        return False

class W_ImmutableHashTable(W_HashTable):
    _attrs_ = []
    _immutable_fields_ = []
    def immutable(self):
        return True

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

@specialize.arg(2)
def next_valid_index(d, i, valid=bool):
    """
    Probes the hash table for the next valid index into the table. Raises
    IndexError when the end of the table is reached
    """
    while True:
        i += 1
        try:
            val = get_dict_item(d, i)
            if not valid(val):
                continue
        except KeyError:
            continue
        else:
            return i

from rpython.rtyper.extregistry import ExtRegistryEntry

class Entry(ExtRegistryEntry):
    _about_ = get_dict_item

    def compute_result_annotation(self, s_d, s_i):
        from rpython.annotator.model import SomeTuple, SomeInteger
        from rpython.annotator.bookkeeper import getbookkeeper
        position = getbookkeeper().position_key
        s_key = s_d.dictdef.read_key(position)
        s_value = s_d.dictdef.read_value(position)
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


