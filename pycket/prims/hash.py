#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket              import impersonators as imp
from pycket              import values
from pycket              import values_hash
from pycket.values_hash  import (
    W_HashTable, W_SimpleHashTable, W_EqvHashTable, W_EqualHashTable, W_EqHashTable,
    make_simple_table, w_missing)
from pycket.cont         import continuation, loop_label
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, procedure, define_nyi

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


@expose("make-weak-hasheq", [])
def make_weak_hasheq():
    # FIXME: not actually weak
    return make_simple_table(W_EqvHashTable, [], [])

@expose("make-immutable-hash", [default(values.W_List, values.w_null)])
def make_immutable_hash(assocs):
    pairs = values.from_list(assocs)
    keys  = [None] * len(pairs)
    vals  = [None] * len(pairs)
    for i, pair in enumerate(pairs):
        if not isinstance(pair, values.W_Cons):
            raise SchemeException("make-immutable-hash: expected list of pairs")
        keys[i] = pair.car()
        vals[i] = pair.cdr()
    return W_EqualHashTable(keys, vals, immutable=True)

@expose("make-immutable-hasheq", [default(values.W_List, values.w_null)])
def make_immutable_hasheq(assocs):
    pairs = values.from_list(assocs)
    keys  = [None] * len(pairs)
    vals  = [None] * len(pairs)
    for i, pair in enumerate(pairs):
        if not isinstance(pair, values.W_Cons):
            raise SchemeException("make-immutable-hasheq: expected list of pairs")
        keys[i] = pair.car()
        vals[i] = pair.cdr()
    return make_simple_table(W_EqHashTable, keys, vals, immutable=True)

@expose("hash")
def hash(args):
    if len(args) % 2 != 0:
        raise SchemeException("hash: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return W_EqualHashTable(keys, vals)

@expose("hasheq")
def hasheq(args):
    if len(args) % 2 != 0:
        raise SchemeException("hasheq: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return make_simple_table(W_EqHashTable, keys, vals)

@expose("hasheqv")
def hasheqv(args):
    if len(args) % 2 != 0:
        raise SchemeException("hasheqv: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return make_simple_table(W_EqvHashTable, keys, vals)

@expose("make-hash", [default(values.W_List, values.w_null)])
def make_hash(pairs):
    lsts = values.from_list(pairs)
    keys = []
    vals = []
    for lst in lsts:
        if not isinstance(lst, values.W_Cons):
            raise SchemeException("make-hash: expected list of pairs")
        keys.append(lst.car())
        vals.append(lst.cdr())
    return W_EqualHashTable(keys, vals)

@expose("make-hasheq", [default(values.W_List, values.w_null)])
def make_hasheq(pairs):
    lsts = values.from_list(pairs)
    keys = []
    vals = []
    for lst in lsts:
        if not isinstance(lst, values.W_Cons):
            raise SchemeException("make-hash: expected list of pairs")
        keys.append(lst.car())
        vals.append(lst.cdr())
    return make_simple_table(W_EqHashTable, keys, vals)

@expose("make-hasheqv", [default(values.W_List, values.w_null)])
def make_hasheqv(pairs):
    lsts = values.from_list(pairs)
    keys = []
    vals = []
    for lst in lsts:
        if not isinstance(lst, values.W_Cons):
            raise SchemeException("make-hash: expected list of pairs")
        keys.append(lst.car())
        vals.append(lst.cdr())
    return make_simple_table(W_EqvHashTable, keys, vals)

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
    if isinstance(table, W_SimpleHashTable):
        copy = table.make_copy()
        copy.data[key] = val
        return return_value(copy, env, cont)

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
    return ht.hash_remove(k, env, cont)

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
