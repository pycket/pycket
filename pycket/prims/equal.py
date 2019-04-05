
import pycket.impersonators as imp
import pycket.vector        as values_vector
from pycket              import values
from pycket              import values_struct
from pycket              import values_string
from pycket.cont         import continuation, label, loop_label
from pycket.error        import SchemeException
from pycket.prims.expose import expose, procedure
from rpython.rlib        import jit, objectmodel

from pycket.hash.base   import W_HashTable

# All of my hate...
# Configuration table for information about how to perform equality checks.
# Based on the Racket internal implementation.
# https://github.com/plt/racket/blob/106cd16d359c7cb594f4def8f427c55992d41a6d/racket/src/racket/src/bool.c
class EqualInfo(object):
    _immutable_fields_ = ["for_chaperone"]

    BASIC        = 0
    CHAPERONE    = 1
    IMPERSONATOR = 2

    def __init__(self, for_chaperone=BASIC):
        """ NOT_RPYTHON """
        self.for_chaperone = for_chaperone

EqualInfo.BASIC_SINGLETON = EqualInfo(EqualInfo.BASIC)
EqualInfo.CHAPERONE_SINGLETON = EqualInfo(EqualInfo.CHAPERONE)
EqualInfo.IMPERSONATOR_SINGLETON = EqualInfo(EqualInfo.IMPERSONATOR)

@expose("equal?", [values.W_Object] * 2, simple=False)
def equalp(a, b, env, cont):
    # FIXME: broken for cycles, etc
    info = EqualInfo.BASIC_SINGLETON
    return equal_func_unroll_n(a, b, info, env, cont, n=5)

@expose("equal?/recur", [values.W_Object, values.W_Object, procedure])
def eqp_recur(v1, v2, recur_proc):
    # TODO:
    return values.w_void

@continuation
def equal_car_cont(a, b, info, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    eq = check_one_val(_vals)
    if eq is values.w_false:
        return return_value(values.w_false, env, cont)
    return equal_func(a, b, info, env, cont)

@continuation
def equal_unbox_right_cont(r, info, env, cont, _vals):
    from pycket.interpreter import check_one_val
    l = check_one_val(_vals)
    return r.unbox(env, equal_unbox_done_cont(l, info, env, cont))

@continuation
def equal_unbox_done_cont(l, info, env, cont, _vals):
    from pycket.interpreter import check_one_val
    r = check_one_val(_vals)
    return equal_func(l, r, info, env, cont)

# This function assumes that a and b have the same length
@loop_label
def equal_vec_func(a, b, idx, info, env, cont):
    from pycket.interpreter import return_value
    if idx >= a.length():
        return return_value(values.w_true, env, cont)
    return a.vector_ref(idx, env, equal_vec_left_cont(a, b, idx, info, env, cont))

# Receive the first value for a given index
@continuation
def equal_vec_left_cont(a, b, idx, info, env, cont, _vals):
    from pycket.interpreter import check_one_val
    l = check_one_val(_vals)
    return b.vector_ref(idx, env,
                equal_vec_right_cont(a, b, idx, l, info, env, cont))

# Receive the second value for a given index
@continuation
def equal_vec_right_cont(a, b, idx, l, info, env, cont, _vals):
    from pycket.interpreter import check_one_val
    r = check_one_val(_vals)
    return equal_func(l, r, info, env, equal_vec_done_cont(a, b, idx, info, env, cont))

# Receive the comparison of the two elements and decide what to do
@continuation
def equal_vec_done_cont(a, b, idx, info, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    eq = check_one_val(_vals)
    if eq is values.w_false:
        return return_value(values.w_false, env, cont)
    inc = idx + 1
    return equal_vec_func(a, b, inc, info, env, cont)

@continuation
def equal_ht_done_cont(hash_1_items, hash_2, idx, info, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    eq = check_one_val(_vals)
    if eq is values.w_false:
        return return_value(values.w_false, env, cont)
    inc = idx + 1
    return equal_ht_func(hash_1_items, hash_2, inc, info, env, cont)

@continuation
def equal_ht_cont(hash_1_items, hash_2, idx, info, env, cont, _vals):
    from pycket.interpreter import return_value, check_one_val
    hash_2_val = check_one_val(_vals)
    if hash_2_val is values.w_false:
        return return_value(values.w_false, env, cont)
    else:
        return equal_func(hash_1_items[idx][1], hash_2_val, info, env,
                          equal_ht_done_cont(hash_1_items, hash_2, idx, info, env, cont))

@loop_label
def equal_ht_func(hash_1_items, hash_2, idx, info, env, cont):
    from pycket.interpreter import return_value
    from pycket.prims.hash import hash_ref

    if idx >= len(hash_1_items):
        return return_value(values.w_true, env, cont)
    else:
        return hash_ref([hash_2, hash_1_items[idx][0], values.w_false],
                        env,
                        equal_ht_cont(hash_1_items, hash_2, idx, info, env, cont))

def equal_func(a, b, info, env, cont):
    return equal_func_loop(a, b, info, env, cont)

def equal_func_unroll_n(a, b, info, env, cont, n):
    # n says how many times to call equal_func before going through loop label
    if n > 0:
        jit.promote(n)
        return equal_func_impl(a, b, info, env, cont, n - 1)
    return equal_func_loop(a, b, info, env, cont)


@loop_label
def equal_func_loop(a, b, info, env, cont):
    return equal_func_impl(a, b, info, env, cont, 0)

def equal_func_impl(a, b, info, env, cont, n):
    from pycket.interpreter import return_value

    if a.eqv(b):
        return return_value(values.w_true, env, cont)

    for_chaperone = jit.promote(info).for_chaperone
    if (for_chaperone >= EqualInfo.CHAPERONE and b.is_non_interposing_chaperone()):
        return equal_func_unroll_n(a, b.get_proxied(), info, env, cont, n)

    # Enter into chaperones/impersonators if we have permission to do so
    if ((for_chaperone == EqualInfo.CHAPERONE and a.is_chaperone()) or
        (for_chaperone == EqualInfo.IMPERSONATOR and a.is_impersonator())):
        return equal_func_unroll_n(a.get_proxied(), b, info, env, cont, n)

    # If we are doing a chaperone/impersonator comparison, then we do not have
    # a chaperone-of/impersonator-of relation if `a` is not a proxy and
    # `b` is a proxy.
    if for_chaperone != EqualInfo.BASIC and not a.is_proxy() and b.is_proxy():
        return return_value(values.w_false, env, cont)

    if isinstance(a, values_string.W_String) and isinstance(b, values_string.W_String):
        is_chaperone = for_chaperone == EqualInfo.CHAPERONE
        if is_chaperone and (not a.immutable() or not b.immutable()):
            return return_value(values.w_false, env, cont)
        return return_value(values.W_Bool.make(a.equal(b)), env, cont)

    if isinstance(a, values.W_Bytes) and isinstance(b, values.W_Bytes):
        is_chaperone = info.for_chaperone == EqualInfo.CHAPERONE
        if is_chaperone and (not a.immutable() or not b.immutable()):
            return return_value(values.w_false, env, cont)
        return return_value(values.W_Bool.make(a.equal(b)), env, cont)

    if isinstance(a, values.W_Cons) and isinstance(b, values.W_Cons):
        cont = equal_car_cont(a.cdr(), b.cdr(), info, env, cont)
        return equal_func_unroll_n(a.car(), b.car(), info, env, cont, n)

    if isinstance(a, values.W_MCons) and isinstance(b, values.W_MCons):
        cont = equal_car_cont(a.cdr(), b.cdr(), info, env, cont)
        return equal_func_unroll_n(a.car(), b.car(), info, env, cont, n)

    if isinstance(a, values.W_Box) and isinstance(b, values.W_Box):
        is_chaperone = for_chaperone == EqualInfo.CHAPERONE
        if is_chaperone and (not a.immutable() or not b.immutable()):
            return return_value(values.w_false, env, cont)
        return a.unbox(env, equal_unbox_right_cont(b, info, env, cont))

    if isinstance(a, values.W_MVector) and isinstance(b, values.W_MVector):
        is_chaperone = for_chaperone == EqualInfo.CHAPERONE
        if is_chaperone and (not a.immutable() or not b.immutable()):
            return return_value(values.w_false, env, cont)
        if a.length() != b.length():
            return return_value(values.w_false, env, cont)
        return equal_vec_func(a, b, 0, info, env, cont)

    if isinstance(a, W_HashTable) and isinstance(b, W_HashTable):
        if len(a.hash_items()) != len(b.hash_items()):
            return return_value(values.w_false, env, cont)
        return equal_ht_func(a.hash_items(), b, 0, info, env, cont)

    if isinstance(a, values_struct.W_RootStruct) and isinstance(b, values_struct.W_RootStruct):
        a_type = a.struct_type()
        b_type = b.struct_type()
        w_prop = a_type.read_prop(values_struct.w_prop_equal_hash)
        if w_prop:
            w_prop = b_type.read_prop(values_struct.w_prop_equal_hash)
            if w_prop:
                w_equal_proc, w_hash_proc, w_hash2_proc = equal_hash_args(w_prop)
                # FIXME: it should work with cycles properly and be an equal?-recur
                w_equal_recur = equalp.w_prim
                return w_equal_proc.call([a, b, w_equal_recur], env, cont)
        if not a_type.isopaque and not b_type.isopaque:
            # This is probably not correct even if struct2vector were done
            # correct, due to side effects, but it is close enough for now.
            # Though the racket documentation says that `equal?` can elide
            # impersonator/chaperone handlers.
            a_imm = a_type.all_fields_immutable()
            b_imm = b_type.all_fields_immutable()
            a = values_struct.struct2vector(a, immutable=a_imm)
            b = values_struct.struct2vector(b, immutable=b_imm)
            return equal_func_unroll_n(a, b, info, env, cont, n)

    if for_chaperone == EqualInfo.BASIC and a.is_proxy() and b.is_proxy():
        return equal_func_unroll_n(a.get_proxied(), b.get_proxied(), info, env, cont, n)

    if a.equal(b):
        return return_value(values.w_true, env, cont)

    return return_value(values.w_false, env, cont)

# TODO: Should probably store these values in a uniform manner in the
# struct property rather than parsing them every use.
def equal_hash_args(w_prop):
    if isinstance(w_prop, values_vector.W_Vector):
        return w_prop.ref(0), w_prop.ref(1), w_prop.ref(2)
    if isinstance(w_prop, values.W_List):
        lst = values.from_list(w_prop)
        assert len(lst) >= 3
        return lst[0], lst[1], lst[2]
    raise SchemeException("invalid prop:equal+hash arg " + w_prop.tostring())

def eqp_logic(a, b):
    if a is b:
        return True
    elif isinstance(a, values.W_Fixnum) and isinstance(b, values.W_Fixnum):
        return a.value == b.value
    elif isinstance(a, values.W_Flonum) and isinstance(b, values.W_Flonum):
        return a.value == b.value
    elif isinstance(a, values.W_Character) and isinstance(b, values.W_Character):
        return a.value == b.value
    return False

@expose("eq?", [values.W_Object] * 2)
def eqp(a, b):
    return values.W_Bool.make(eqp_logic(a, b))

@jit.unroll_safe
def procedure_closure_contents_eq_n(a, b, n):
    if a is b:
        return values.w_true
    if n == 0:
        return values.w_false
    if (isinstance(a, values.W_Closure1AsEnv) and
        isinstance(b, values.W_Closure1AsEnv)):
        if a.caselam is not b.caselam:
            return values.w_false
        size = a._get_size_list()
        if size != b._get_size_list():
            return values.w_false
        for i in range(size):
            a_i = a._get_list(i)
            b_i = b._get_list(i)
            if a_i is b_i:
                continue
            if (isinstance(a_i, values.W_Closure1AsEnv) and
                isinstance(b_i, values.W_Closure1AsEnv)):
                if values.w_false is procedure_closure_contents_eq_n(a_i, b_i, n-1):
                    return values.w_false
            elif not eqp_logic(a_i, b_i):
                return values.w_false
        return values.w_true
    return values.w_false

@expose("procedure-closure-contents-eq?", [procedure] * 2)
def procedure_closure_contents_eq(a, b):
    return procedure_closure_contents_eq_n(a, b, 1)

@expose("eqv?", [values.W_Object] * 2)
def eqvp(a, b):
    res = a.eqv(b)
    if not objectmodel.we_are_translated() and res:
        # this is a useful sanity check during testing:
        # check that the following invarinat is true:
        # a.eqv(b) => a.hash_eqv() == b.hash_eqv()
        assert a.hash_eqv() == b.hash_eqv()
    return values.W_Bool.make(res)

