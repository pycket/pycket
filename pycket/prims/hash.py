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
    return return_value(None, env,
            hash_map_cont(f, h, 0, acc, env, cont))

@continuation
def hash_map_cont(f, ht, index, w_acc, env, cont, vals):
    from pycket.interpreter import return_value
    vals = vals._get_full_list()
    if len(vals) != 1:
        raise SchemeException("hash-map: wrong number of results")
    w_val, = vals
    if w_val is not None:
        w_acc = values.W_Cons.make(w_val, w_acc)
    nextindex = index + 1
    try:
        w_key, w_value = ht.get_item(index)
    except KeyError:
        return return_value(None, env,
                hash_map_cont(f, ht, nextindex, w_acc, env, cont))
    except IndexError:
        return return_value(w_acc, env, cont)
    after = hash_map_cont(f, ht, nextindex, w_acc, env, cont)
    return f.call([w_key, w_value], env, after)


@expose("make-weak-hasheq", [])
def make_weak_hasheq():
    # FIXME: not actually weak
    return W_EqvHashTable([], [])

@expose("make-immutable-hash", [default(values.W_List, values.w_null)])
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
    from pycket import values
    return ht.hash_set(k, v, env, cont)

define_nyi("hash-set", [W_HashTable, values.W_Object, values.W_Object], simple=False)

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

define_nyi("hash-remove", [W_HashTable, values.W_Object])

define_nyi("hash-clear!", [W_HashTable])

define_nyi("hash-clear", [W_HashTable])

@expose("hash-count", [W_HashTable])
def hash_count(hash):
    return values.W_Fixnum(hash.length())

define_nyi("hash-iterate-first", False, [W_HashTable])

define_nyi("hash-iterate-next", [W_HashTable, values.W_Fixnum])

define_nyi("hash-iterate-key", [W_HashTable, values.W_Fixnum])

define_nyi("hash-iterate-value", [W_HashTable, values.W_Fixnum])

define_nyi("hash-copy", [W_HashTable])

# FIXME: not implemented
@expose("equal-hash-code", [values.W_Object])
def equal_hash_code(v):
    print "NOT YET IMPLEMENTED: equal-hash-code"
    return values.W_Fixnum(0)

define_nyi("equal-secondary-hash-code", [values.W_Object])
