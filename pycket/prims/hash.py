#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket              import impersonators as imp
from pycket              import values
from pycket.values_hash  import (
    W_HashTable, W_EqvHashTable, W_EqualHashTable, W_EqHashTable)
from pycket.cont         import continuation
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, procedure, define_nyi

@expose("hash-for-each", [W_HashTable, procedure], simple=False)
def hash_for_each(h, f, env, cont):
    from pycket.interpreter import return_value
    items = h.hash_items()
    return return_value(values.w_void, env,
            hash_for_each_cont(f, items, h, 0, env, cont))

@continuation
def hash_for_each_cont(f, items, ht, n, env, cont, _vals):
    from pycket.interpreter import return_value
    if n == len(items):
        return return_value(values.w_void, env, cont)
    after = hash_for_each_cont(f, items, ht, n + 1, env, cont)
    k, v = items[n]
    return f.call([k, v], env, after)

@expose("make-weak-hasheq", [])
def make_weak_hasheq():
    # FIXME: not actually weak
    return W_EqvHashTable([], [])

@expose("make-immutable-hash", [default(values.W_Object, values.w_null)])
def make_immutable_hash(assocs):
    # FIXME: Not annotated as immutable
    lsts = values.from_list(assocs)
    keys = []
    vals = []
    for lst in lsts:
        if not isinstance(lst, values.W_Cons):
            raise SchemeException("make-hash: expected list of pairs")
        keys.append(lst.car())
        vals.append(lst.cdr())
    return W_EqualHashTable(keys, vals)

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
    return W_EqHashTable(keys, vals)

@expose("hasheqv")
def hasheqv(args):
    if len(args) % 2 != 0:
        raise SchemeException("hasheqv: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return W_EqvHashTable(keys, vals)

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
    return W_EqHashTable(keys, vals)

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
    return W_EqvHashTable(keys, vals)

@expose("hash-set!", [W_HashTable, values.W_Object, values.W_Object], simple=False)
def hash_set_bang(ht, k, v, env, cont):
    return ht.hash_set(k, v, env, cont)

define_nyi("hash-set", [W_HashTable, values.W_Object, values.W_Object], simple=False)
# def hash_set(ht, k, v, env, cont):
#     raise NotImplementedError()
#     return ht.hash_set(k, v, env, cont)
#     #return ht

@continuation
def hash_ref_cont(default, env, cont, _vals):
    from pycket.interpreter import return_value, check_one_val
    val = check_one_val(_vals)
    if val is not None:
        return return_value(val, env, cont)
    if default is None:
        raise SchemeException("key not found")
    if default.iscallable():
        return default.call([], env, cont)
    return return_value(default, env, cont)

@expose("hash-ref", [W_HashTable, values.W_Object, default(values.W_Object, None)], simple=False)
def hash_ref(ht, k, default, env, cont):
    return ht.hash_ref(k, env, hash_ref_cont(default, env, cont))

define_nyi("hash-remove!", [W_HashTable, values.W_Object])
# def hash_remove_bang(hash, key):
#     raise NotImplementedError()
#     return hash

define_nyi("hash-remove", [W_HashTable, values.W_Object])
# def hash_remove(hash, key):
#     raise NotImplementedError()
#     return hash

define_nyi("hash-clear!", [W_HashTable])
# def hash_clear_bang(hash):
#     raise NotImplementedError()
#     return W_EqvHashTable([], [])

define_nyi("hash-clear", [W_HashTable])
# def hash_clear(hash):
#     raise NotImplementedError()
#     return W_EqvHashTable([], [])

define_nyi("hash-map", [W_HashTable, values.W_Object])
# def hash_map(hash, proc):
#     raise NotImplementedError()
#     return hash

@expose("hash-count", [W_HashTable])
def hash_count(hash):
    return values.W_Fixnum(hash.length())

define_nyi("hash-iterate-first", [W_HashTable])
# def hash_iterate_first(hash):
#     raise NotImplementedError()
#     # if not hash.data:
#     #     return values.w_false
#     # else:
#     #     return hash.ref(0)
#     return values.w_false

define_nyi("hash-iterate-next", [W_HashTable, values.W_Fixnum])
# def hash_iterate_next(hash, pos):
#     raise NotImplementedError()
#     # next_pos = pos.value + 1
#     # return hash.ref(next_pos) if hash.ref(next_pos) is not None else values.w_false
#     return values.w_false

define_nyi("hash-iterate-key", [W_HashTable, values.W_Fixnum])
# def hash_iterate_key(hash, pos):
#     raise NotImplementedError()
#     # return hash.ref(pos.value)
#     return values.w_false

define_nyi("hash-iterate-value", [W_HashTable, values.W_Fixnum])
# def hash_iterate_value(hash, pos):
#     raise NotImplementedError()
#     # return hash.ref(pos.value)
#     return values.w_false

define_nyi("hash-copy", [W_HashTable])
# def hash_iterate_value(hash):
#     raise NotImplementedError()
#     # FIXME: implementation
#     return hash

define_nyi("equal-hash-code", [values.W_Object])
# def equal_hash_code(v):
#     raise NotImplementedError()
#     return values.W_Fixnum(0)

define_nyi("equal-secondary-hash-code", [values.W_Object])
# def equal_hash_code(v):
#     raise NotImplementedError()
#     return values.W_Fixnum(0)
