#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket              import impersonators as imp
from pycket              import values
from pycket.hash.base    import W_HashTable, W_ImmutableHashTable, w_missing
from pycket.hash.simple  import (
    W_EqvMutableHashTable, W_EqMutableHashTable,
    W_EqvImmutableHashTable, W_EqImmutableHashTable,
    make_simple_mutable_table, make_simple_mutable_table_assocs,
    make_simple_immutable_table, make_simple_immutable_table_assocs)
from pycket.hash.equal   import W_EqualHashTable
from pycket.cont         import continuation, loop_label
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, procedure, define_nyi
from rpython.rlib        import jit

@expose("hash-iterate-first", [W_HashTable])
def hash_iterate_first(ht):
    if ht.length() == 0:
        return values.w_false
    return values.W_Fixnum.make(0)

@expose("hash-iterate-next", [W_HashTable, values.W_Fixnum])
def hash_iterate_next(ht, pos):
    if ht.length()-1 == pos.value:
        return values.w_false
    return values.W_Fixnum.make(pos.value + 1)

def hash_iter_ref(ht, pos, key=False):
    n = pos.value
    try:
        w_key, w_val = ht.get_item(n)
        if key:
            return w_key
        else:
            return w_val
    except KeyError:
        raise SchemeException("hash-iterate-key: invalid position")
    except IndexError:
        raise SchemeException("hash-iterate-key: invalid position")


@expose("hash-iterate-key",  [W_HashTable, values.W_Fixnum])
def hash_iterate_key(ht, pos):
    return hash_iter_ref(ht, pos, key=True)

@expose("hash-iterate-value",  [W_HashTable, values.W_Fixnum])
def hash_iterate_value(ht, pos):
    return hash_iter_ref(ht, pos, key=False)

@expose("hash-for-each", [W_HashTable, procedure], simple=False)
def hash_for_each(h, f, env, cont):
    from pycket.interpreter import return_value
    f.enable_jitting()
    return return_value(values.w_void, env,
            hash_for_each_cont(f, h, 0, env, cont))

@continuation
def hash_for_each_cont(f, ht, index, env, cont, _vals):
    from pycket.interpreter import return_value
    nextindex = index + 1
    try:
        w_key, w_value = ht.get_item(index)
    except KeyError:
        return return_value(values.w_void, env,
                hash_for_each_cont(f, ht, nextindex, env, cont))
    except IndexError:
        return return_value(values.w_void, env, cont)
    after = hash_for_each_cont(f, ht, nextindex, env, cont)
    return f.call([w_key, w_value], env, after)


@expose("hash-map", [W_HashTable, procedure], simple=False)
def hash_map(h, f, env, cont):
    from pycket.interpreter import return_value
    acc = values.w_null
    f.enable_jitting()
    return return_value(w_missing, env,
            hash_map_cont(f, h, 0, acc, env, cont))

@continuation
def hash_map_cont(f, ht, index, w_acc, env, cont, vals):
    from pycket.interpreter import return_value, check_one_val
    w_val = check_one_val(vals)
    if w_val is not w_missing:
        w_acc = values.W_Cons.make(w_val, w_acc)
    nextindex = index + 1
    try:
        w_key, w_value = ht.get_item(index)
    except KeyError:
        return return_value(w_missing, env,
                hash_map_cont(f, ht, nextindex, w_acc, env, cont))
    except IndexError:
        return return_value(w_acc, env, cont)
    after = hash_map_cont(f, ht, nextindex, w_acc, env, cont)
    return f.call([w_key, w_value], env, after)

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

@expose("make-weak-hasheq", [])
def make_weak_hasheq():
    # FIXME: not actually weak
    return make_simple_mutable_table(W_EqvMutableHashTable, None, None)

@expose("make-weak-hash", [default(values.W_List, None)])
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
def hash_ref_cont(default, env, cont, _vals):
    from pycket.interpreter import return_value, check_one_val
    val = check_one_val(_vals)
    if val is not w_missing:
        return return_value(val, env, cont)
    if default is None:
        raise SchemeException("key not found")
    if default.iscallable():
        return default.call([], env, cont)
    return return_value(default, env, cont)

@expose("hash-ref", [W_HashTable, values.W_Object, default(values.W_Object, None)], simple=False)
def hash_ref(ht, k, default, env, cont):
    return ht.hash_ref(k, env, hash_ref_cont(default, env, cont))

@expose("hash-remove!", [W_HashTable, values.W_Object], simple=False)
def hash_remove_bang(ht, k, env, cont):
    return ht.hash_remove_inplace(k, env, cont)

define_nyi("hash-remove", [W_HashTable, values.W_Object])

define_nyi("hash-clear!", [W_HashTable])

define_nyi("hash-clear", [W_HashTable])

@expose("hash-count", [W_HashTable])
def hash_count(hash):
    return values.W_Fixnum(hash.length())

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
    new = src.make_empty()
    return hash_copy_loop(src.hash_items(), 0, src, new, env, cont)

expose("hash-copy", [W_HashTable], simple=False)(hash_copy)

# FIXME: not implemented
@expose("equal-hash-code", [values.W_Object])
def equal_hash_code(v):
    return values.W_Fixnum(0)

@expose("equal-secondary-hash-code", [values.W_Object])
def equal_secondary_hash_code(v):
    return values.W_Fixnum(0)
