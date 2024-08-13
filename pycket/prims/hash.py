#! /usr/bin/env python
# -*- coding: utf-8 -*-
import sys
from pycket              import impersonators as imp
from pycket              import values, values_string
from pycket.hash.base    import W_HashTable, W_ImmutableHashTable, w_missing
from pycket.hash.simple  import (
    W_EqvMutableHashTable, W_EqMutableHashTable,
    W_EqvImmutableHashTable, W_EqImmutableHashTable,
    make_simple_mutable_table, make_simple_mutable_table_assocs,
    make_simple_immutable_table, make_simple_immutable_table_assocs)
from pycket.hash.equal   import W_EqualHashTable
from pycket.impersonators.baseline import W_ImpHashTable, W_ChpHashTable
from pycket.cont         import continuation, loop_label
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, procedure, define_nyi
from rpython.rlib        import jit, objectmodel

_KEY = 0
_VALUE = 1
_KEY_AND_VALUE = 2
_PAIR = 3

PREFIXES = ["unsafe-mutable", "unsafe-immutable"]

def prefix_hash_names(base):
    result = [base]
    for pre in PREFIXES:
        result.append("%s-%s" % (pre, base))
    return result

@expose(prefix_hash_names("hash-iterate-first"), [W_HashTable])
def hash_iterate_first(ht):
    if ht.length() == 0:
        return values.w_false
    return values.W_Fixnum.ZERO

@expose(prefix_hash_names("hash-iterate-next"), [W_HashTable, values.W_Fixnum])
def hash_iterate_next(ht, pos):
    return ht.hash_iterate_next(pos)

@objectmodel.specialize.arg(5)
def hash_iter_ref(ht, n, bad_index_v, env, cont, returns):
    from pycket.interpreter import return_value, return_multi_vals
    try:
        w_key, w_val = ht.get_item(n)
        if returns == _KEY:
            return return_value(w_key, env, cont)
        if returns == _VALUE:
            return return_value(w_val, env, cont)
        if returns == _KEY_AND_VALUE:
            vals = values.Values._make2(w_key, w_val)
            return return_multi_vals(vals, env, cont)
        if returns == _PAIR:
            vals = values.W_Cons.make(w_key, w_val)
            return return_value(vals, env, cont)
        assert False, "unknown return code"
    except (KeyError, IndexError):
        if bad_index_v is not None:
            return return_value(bad_index_v, env, cont)
        raise SchemeException("hash-iterate-key: invalid position")

@expose(prefix_hash_names("hash-iterate-key"),
        [W_HashTable, values.W_Fixnum, default(values.W_Object, None)], simple=False)
def hash_iterate_key(ht, pos, bad_index_v, env, cont):
    return hash_iter_ref(ht, pos.value, bad_index_v, env, cont, returns=_KEY)

@expose(prefix_hash_names("hash-iterate-value"),
        [W_HashTable, values.W_Fixnum, default(values.W_Object, None)], simple=False)
def hash_iterate_value(ht, pos, bad_index_v, env, cont):
    return hash_iter_ref(ht, pos.value, bad_index_v, env, cont, returns=_VALUE)

@expose(prefix_hash_names("hash-iterate-key+value"),
        [W_HashTable, values.W_Fixnum, default(values.W_Object, None)], simple=False)
def hash_iterate_key_value(ht, pos, bad_index_v, env, cont):
    return hash_iter_ref(ht, pos.value, bad_index_v, env, cont, returns=_KEY_AND_VALUE)

@expose(prefix_hash_names("hash-iterate-pair"),
        [W_HashTable, values.W_Fixnum, default(values.W_Object, None)], simple=False)
def hash_iterate_pair(ht, pos, bad_index_v, env, cont):
    return hash_iter_ref(ht, pos.value, bad_index_v, env, cont, returns=_PAIR)

@expose("hash-for-each", [W_HashTable, procedure, default(values.W_Object, values.w_false)], simple=False)
def hash_for_each(ht, f, try_order, env, cont):
    # FIXME: implmeent try-order? -- see hash-map
    return hash_for_each_loop(ht, f, 0, env, cont)

@loop_label
def hash_for_each_loop(ht, f, index, env, cont):
    from pycket.interpreter import return_value
    try:
        w_key, w_value = ht.get_item(index)
    except KeyError:
        return hash_for_each_loop(ht, f, index + 1, env, cont)
    except IndexError:
        return return_value(values.w_void, env, cont)
    return f.call([w_key, w_value], env,
            hash_for_each_cont(ht, f, index, env, cont))

@continuation
def hash_for_each_cont(ht, f, index, env, cont, _vals):
    return hash_for_each_loop(ht, f, index + 1, env, cont)

@expose("hash-map", [W_HashTable, procedure, default(values.W_Object, values.w_false)], simple=False)
def hash_map(h, f, try_order, env, cont):
    # FIXME : If try-order? is true, then the order of keys and values
    # passed to proc is normalized under certain circumstances, such
    # as when the keys are all symbols and hash is not an
    # impersonator.
    from pycket.interpreter import return_value
    acc = values.w_null
    return hash_map_loop(f, h, 0, acc, env, cont)
    # f.enable_jitting()
    # return return_value(w_missing, env,
            # hash_map_cont(f, h, 0, acc, env, cont))

@loop_label
def hash_map_loop(f, ht, index, w_acc, env, cont):
    from pycket.interpreter import return_value
    try:
        w_key, w_value = ht.get_item(index)
    except KeyError:
        return hash_map_loop(f, ht, index + 1, w_acc, env, cont)
    except IndexError:
        return return_value(w_acc, env, cont)
    after = hash_map_cont(f, ht, index, w_acc, env, cont)
    return f.call([w_key, w_value], env, after)

@continuation
def hash_map_cont(f, ht, index, w_acc, env, cont, _vals):
    from pycket.interpreter import check_one_val
    w_val = check_one_val(_vals)
    w_acc = values.W_Cons.make(w_val, w_acc)
    return hash_map_loop(f, ht, index + 1, w_acc, env, cont)

@jit.elidable
def from_assocs(assocs, fname):
    if not assocs.is_proper_list():
        raise SchemeException("%s: expected proper list" % fname)
    keys = []
    vals = []
    while isinstance(assocs, values.W_Cons):
        val, assocs = assocs.car(), assocs.cdr()
        if not isinstance(val, values.W_Cons):
            raise SchemeException("%s: expected list of pairs" % fname)
        keys.append(val.car())
        vals.append(val.cdr())
    return keys[:], vals[:]

@expose(["make-weak-hasheq", "make-ephemeron-hasheq"], [default(values.W_List, values.w_null)])
def make_weak_hasheq(assocs):
    # FIXME: not actually weak
    return make_simple_mutable_table_assocs(W_EqMutableHashTable, assocs, "make-weak-hasheq")

@expose(["make-weak-hasheqv", "make-ephemeron-hasheqv"], [default(values.W_List, values.w_null)])
def make_weak_hasheqv(assocs):
    # FIXME: not actually weak
    return make_simple_mutable_table_assocs(W_EqvMutableHashTable, assocs, "make-weak-hasheqv")

@expose(["make-weak-hash", "make-late-weak-hasheq", "make-ephemeron-hash"], [default(values.W_List, None)])
def make_weak_hash(assocs):
    if assocs is None:
        return W_EqualHashTable([], [], immutable=False)
    return W_EqualHashTable(*from_assocs(assocs, "make-weak-hash"), immutable=False)

@expose("make-immutable-hash", [default(values.W_List, values.w_null)])
def make_immutable_hash(assocs):
    keys, vals = from_assocs(assocs, "make-immutable-hash")
    return W_EqualHashTable(keys, vals, immutable=True)

@expose("make-immutable-hasheq", [default(values.W_List, values.w_null)])
def make_immutable_hasheq(assocs):
    return make_simple_immutable_table_assocs(W_EqImmutableHashTable, assocs, "make-immutable-hasheq")

@expose("make-immutable-hasheqv", [default(values.W_List, values.w_null)])
def make_immutable_hasheqv(assocs):
    return make_simple_immutable_table_assocs(W_EqvImmutableHashTable, assocs, "make-immutable-hasheq")

@expose("hash")
def hash(args):
    if len(args) % 2 != 0:
        raise SchemeException("hash: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return W_EqualHashTable(keys, vals, immutable=True)

@expose("hasheq")
def hasheq(args):
    if len(args) % 2 != 0:
        raise SchemeException("hasheq: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return make_simple_immutable_table(W_EqImmutableHashTable, keys, vals)

@expose("hasheqv")
def hasheqv(args):
    if len(args) % 2 != 0:
        raise SchemeException("hasheqv: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return make_simple_immutable_table(W_EqvImmutableHashTable, keys, vals)

@expose("make-hash", [default(values.W_List, values.w_null)])
def make_hash(pairs):
    return W_EqualHashTable(*from_assocs(pairs, "make-hash"))

@expose("make-hasheq", [default(values.W_List, values.w_null)])
def make_hasheq(pairs):
    return make_simple_mutable_table_assocs(W_EqMutableHashTable, pairs, "make-hasheq")

@expose("make-hasheqv", [default(values.W_List, values.w_null)])
def make_hasheqv(pairs):
    return make_simple_mutable_table_assocs(W_EqvMutableHashTable, pairs, "make-hasheqv")

@expose("hash-set!", [W_HashTable, values.W_Object, values.W_Object], simple=False)
def hash_set_bang(ht, k, v, env, cont):
    if ht.immutable():
        raise SchemeException("hash-set!: given immutable table")
    return ht.hash_set(k, v, env, cont)

@continuation
def hash_set_cont(key, val, env, cont, _vals):
    from pycket.interpreter import check_one_val
    table = check_one_val(_vals)
    return table.hash_set(key, val, env, return_table_cont(table, env, cont))

@continuation
def return_table_cont(table, env, cont, _vals):
    from pycket.interpreter import return_value
    return return_value(table, env, cont)

@expose("hash-set", [W_HashTable, values.W_Object, values.W_Object], simple=False)
def hash_set(table, key, val, env, cont):
    from pycket.interpreter import return_value
    if not table.immutable():
        raise SchemeException("hash-set: not given an immutable table")
    # Fast path
    if isinstance(table, W_ImmutableHashTable):
        new_table = table.assoc(key, val)
        return return_value(new_table, env, cont)

    return hash_copy(table, env,
            hash_set_cont(key, val, env, cont))

@continuation
def hash_ref_cont(default, k, env, cont, _vals):
    from pycket.interpreter import return_value, check_one_val
    val = check_one_val(_vals)
    if val is not w_missing:
        return return_value(val, env, cont)
    if default is None:
        raise SchemeException("key %s not found"%k.tostring())
    if default.iscallable():
        return default.call([], env, cont)
    return return_value(default, env, cont)

@expose("hash-ref", [W_HashTable, values.W_Object, default(values.W_Object, None)], simple=False)
def hash_ref(ht, k, default, env, cont):
    return ht.hash_ref(k, env, hash_ref_cont(default, k, env, cont))

@expose("hash-remove!", [W_HashTable, values.W_Object], simple=False)
def hash_remove_bang(ht, k, env, cont):
    if ht.immutable():
        raise SchemeException("hash-remove!: expected mutable hash table")
    return ht.hash_remove_inplace(k, env, cont)

@expose("hash-remove", [W_HashTable, values.W_Object], simple=False)
def hash_remove(ht, k, env, cont):
    if not ht.immutable():
        raise SchemeException("hash-remove: expected immutable hash table")
    return ht.hash_remove(k, env, cont)

@continuation
def hash_clear_cont(ht, env, cont, _vals):
    return hash_clear_loop(ht, env, cont)

def hash_clear_loop(ht, env, cont):
    from pycket.interpreter import return_value
    if ht.length() == 0:
        return return_value(values.w_void, env, cont)

    w_k, w_v = ht.get_item(0)
    return ht.hash_remove_inplace(w_k, env, hash_clear_cont(ht, env, cont))

@expose("hash-clear!", [W_HashTable], simple=False)
def hash_clear_bang(ht, env, cont):
    from pycket.interpreter import return_value

    if ht.is_impersonator():
        ht.hash_clear_proc(env, cont)

        return hash_clear_loop(ht, env, cont)
    else:
        ht.hash_empty()
        return return_value(values.w_void, env, cont)

define_nyi("hash-clear", [W_HashTable])

@expose("hash-count", [W_HashTable])
def hash_count(hash):
    return values.W_Fixnum(hash.length())

@continuation
def hash_keys_subset_huh_cont(keys_vals, hash_2, idx, env, cont, _vals):
    from pycket.interpreter import return_value, check_one_val
    val = check_one_val(_vals)
    if val is values.w_false:
        return return_value(values.w_false, env, cont)
    else:
        return hash_keys_subset_huh_loop(keys_vals, hash_2, idx + 1, env, cont)

@loop_label
def hash_keys_subset_huh_loop(keys_vals, hash_2, idx, env, cont):
    from pycket.interpreter import return_value
    if idx >= len(keys_vals):
        return return_value(values.w_true, env, cont)
    else:
        return hash_ref([hash_2, keys_vals[idx][0], values.w_false], env,
                        hash_keys_subset_huh_cont(keys_vals, hash_2, idx, env, cont))

@jit.elidable
def uses_same_eq_comparison(hash_1, hash_2):
    h_1 = hash_1
    h_2 = hash_2

    if hash_1.is_impersonator() or hash_1.is_chaperone():
        h_1 = hash_1.get_proxied()
    if hash_2.is_impersonator() or hash_2.is_chaperone():
        h_2 = hash_2.get_proxied()

    # equal?
    if isinstance(h_1, W_EqualHashTable):
        return isinstance(h_2, W_EqualHashTable)
    # eq?
    elif isinstance(h_1, W_EqMutableHashTable) or isinstance(h_1, W_EqImmutableHashTable):
        return isinstance(h_2, W_EqMutableHashTable) or isinstance(h_2, W_EqImmutableHashTable)
    # eqv?
    elif isinstance(h_1, W_EqvMutableHashTable) or isinstance(h_1, W_EqvImmutableHashTable):
        return isinstance(h_2, W_EqvMutableHashTable) or isinstance(h_2, W_EqvImmutableHashTable)
    else:
        return False

@expose("hash-keys-subset?", [W_HashTable, W_HashTable], simple=False)
def hash_keys_subset_huh(hash_1, hash_2, env, cont):
    if not uses_same_eq_comparison(hash_1, hash_2):
        raise SchemeException("hash-keys-subset?: given hash tables do not use the same key comparison -- first table : %s - second table: %s" % (hash_1.tostring(), hash_2.tostring()))
    return hash_keys_subset_huh_loop(hash_1.hash_items(), hash_2, 0, env, cont)

@continuation
def hash_ref_key_cont(default, key, env, cont, _vals):
    """Continuation used in hash-ref-key
    Returns the key (as opposed to hash-ref returning the value).
    """
    from pycket.interpreter import check_one_val, return_value
    val = check_one_val(_vals)
    if val is not w_missing:
        return return_value(key, env, cont)
    if default is None:
        raise SchemeException("key %s not found"%key.tostring())
    if default.iscallable():
        return default.call([], env, cont)
    return return_value(default, env, cont)

@expose("hash-ref-key", [W_HashTable, values.W_Object, default(values.W_Object, None)], simple=False)
def hash_ref_key(ht, key, default, env, cont):
    """Returns the key held by ht that is equivalent to key according to ht's key-comparison function.
    """
    return ht.hash_ref(key, env, hash_ref_key_cont(default, key, env, cont))

@continuation
def hash_copy_ref_cont(keys, idx, src, new, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    return new.hash_set(keys[idx][0], val, env,
            hash_copy_set_cont(keys, idx, src, new, env, cont))

@continuation
def hash_copy_set_cont(keys, idx, src, new, env, cont, _vals):
    return hash_copy_loop(keys, idx + 1, src, new, env, cont)

@loop_label
def hash_copy_loop(keys, idx, src, new, env, cont):
    from pycket.interpreter import return_value
    if idx >= len(keys):
        return return_value(new, env, cont)
    return src.hash_ref(keys[idx][0], env,
            hash_copy_ref_cont(keys, idx, src, new, env, cont))

def hash_copy(src, env, cont):
    from pycket.interpreter import return_value
    if isinstance(src, W_ImmutableHashTable):
        new = src.make_copy()
        return return_value(new, env, cont)
    new = src.make_empty()
    if src.length() == 0:
        return return_value(new, env, cont)
    return hash_copy_loop(src.hash_items(), 0, src, new, env, cont)

expose("hash-copy", [W_HashTable], simple=False)(hash_copy)

# FIXME: not implemented
@expose("equal-hash-code", [values.W_Object])
def equal_hash_code(v):

    # only for improper path cache entries
    if isinstance(v, values.W_Cons):
        if v.is_proper_list():
            return values.W_Fixnum.ZERO

        nm = v.car()
        p = v.cdr()
        if isinstance(nm, values_string.W_String) and \
           isinstance(p, values.W_Path) and \
           isinstance(p.path, str):
            return values.W_Fixnum(objectmodel.compute_hash((nm.tostring(), p.path)))

    return values.W_Fixnum.ZERO

@expose("equal-secondary-hash-code", [values.W_Object])
def equal_secondary_hash_code(v):
    return values.W_Fixnum.ZERO

@expose("eq-hash-code", [values.W_Object])
def eq_hash_code(v):
    t = type(v)
    if t is values.W_Fixnum:
        return v

    if t is values.W_Flonum:
        hash = objectmodel.compute_hash(v.value)
    elif t is values.W_Character:
        hash = objectmodel.compute_hash(v.value)
    else:
        hash = objectmodel.compute_hash(v)
    return values.W_Fixnum(hash)

@expose("eqv-hash-code", [values.W_Object])
def eqv_hash_code(v):
    hash = v.hash_eqv()
    return values.W_Fixnum(hash)
