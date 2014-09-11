#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket              import impersonators as imp
from pycket              import values
from pycket.cont         import continuation
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, procedure

@expose("hash-for-each", [values.W_HashTable, procedure], simple=False)
def hash_for_each(h, f, env, cont):
    from pycket.interpreter import return_value
    keys = h.hash_keys()
    return return_value(values.w_void, env,
            hash_for_each_cont(f, keys, h, 0, env, cont))

@continuation
def get_result_cont(f, keys, ht, n, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    val = check_one_val(_vals)
    after = hash_for_each_cont(f, keys, ht, n + 1, env, cont)
    if val is None:
        return return_value(values.w_void, env, after)
    return f.call([keys[n], val], env, after)

@continuation
def hash_for_each_cont(f, keys, ht, n, env, cont, _vals):
    from pycket.interpreter import return_value
    if n == len(keys):
        return return_value(values.w_void, env, cont)
    return ht.hash_ref(keys[n], env, get_result_cont(f, keys, ht, n, env, cont))

@expose("make-weak-hasheq", [])
def make_weak_hasheq():
    # FIXME: not actually weak
    return values.W_EqvHashTable([], [])

@expose("make-immutable-hash", [default(values.W_Object, None)])
def make_immutable_hash(assocs):
    # FIXME: not impelemented
    return values.W_EqvHashTable([], [])

@expose("hash")
def hash(args):
    if len(args) % 2 != 0:
        raise SchemeException("hash: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return values.W_EqvHashTable(keys, vals)

@expose("hasheq")
def hasheq(args):
    if len(args) % 2 != 0:
        raise SchemeException("hasheq: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return values.W_EqHashTable(keys, vals)

@expose("hasheqv")
def hasheqv(args):
    if len(args) % 2 != 0:
        raise SchemeException("hasheqv: key does not have a corresponding value")
    keys = [args[i] for i in range(0, len(args), 2)]
    vals = [args[i] for i in range(1, len(args), 2)]
    return values.W_EqvHashTable(keys, vals)

@expose("make-hash")
def make_hash(args):
    return values.W_EqvHashTable([], [])

@expose("make-hasheq")
def make_hasheq(args):
    return values.W_EqHashTable([], [])

@expose("make-hasheqv")
def make_hasheqv(args):
    return values.W_EqvHashTable([], [])

@expose("hash-set!", [values.W_HashTable, values.W_Object, values.W_Object], simple=False)
def hash_set_bang(ht, k, v, env, cont):
    return ht.hash_set(k, v, env, cont)

@expose("hash-set", [values.W_HashTable, values.W_Object, values.W_Object], simple=False)
def hash_set(ht, k, v, env, cont):
    # FIXME: implementation
    return ht.hash_set(k, v, env, cont)
    #return ht

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

@expose("hash-ref", [values.W_HashTable, values.W_Object, default(values.W_Object, None)], simple=False)
def hash_ref(ht, k, default, env, cont):
    return ht.hash_ref(k, env, hash_ref_cont(default, env, cont))

@expose("hash-remove!", [values.W_HashTable, values.W_Object])
def hash_remove_bang(hash, key):
    # FIXME: not implemented
    return hash

@expose("hash-remove", [values.W_HashTable, values.W_Object])
def hash_remove(hash, key):
    # FIXME: not implemented
    return hash

@expose("hash-clear!", [values.W_HashTable])
def hash_clear_bang(hash):
    # FIXME: not implemented
    return values.W_EqvHashTable([], [])

@expose("hash-clear", [values.W_HashTable])
def hash_clear(hash):
    # FIXME: not implemented
    return values.W_EqvHashTable([], [])

@expose("hash-map", [values.W_HashTable, values.W_Object])
def hash_map(hash, proc):
    # FIXME: not implemented
    return hash

@expose("hash-count", [values.W_HashTable])
def hash_count(hash):
    # FIXME: implementation
    return values.W_Fixnum(len(hash.data))

@expose("hash-iterate-first", [values.W_HashTable])
def hash_iterate_first(hash):
    # FIXME: implementation
    # if not hash.data:
    #     return values.w_false
    # else:
    #     return hash.ref(0)
    return values.w_false

@expose("hash-iterate-next", [values.W_HashTable, values.W_Fixnum])
def hash_iterate_next(hash, pos):
    # FIXME: implementation
    # next_pos = pos.value + 1
    # return hash.ref(next_pos) if hash.ref(next_pos) is not None else values.w_false
    return values.w_false

@expose("hash-iterate-key", [values.W_HashTable, values.W_Fixnum])
def hash_iterate_key(hash, pos):
    # FIXME: implementation
    # return hash.ref(pos.value)
    return values.w_false

@expose("hash-iterate-value", [values.W_HashTable, values.W_Fixnum])
def hash_iterate_value(hash, pos):
    # FIXME: implementation
    # return hash.ref(pos.value)
    return values.w_false

@expose("hash-copy", [values.W_HashTable])
def hash_iterate_value(hash):
    # FIXME: implementation
    return hash

@expose("equal-hash-code", [values.W_Object])
def equal_hash_code(v):
    # FIXME: not implemented
    return values.W_Fixnum(0)

