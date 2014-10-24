
import pycket.impersonators as imp
import pycket.vector        as values_vector
from pycket              import values
from pycket              import values_struct
from pycket              import values_string
from pycket.cont         import continuation, label, loop_label
from pycket.prims.expose import expose, procedure
from rpython.rlib import jit, objectmodel

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
    return equal_func(a, b, info, env, cont)

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
    if idx.value >= a.length():
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
    inc = values.W_Fixnum(idx.value + 1)
    return equal_vec_func(a, b, inc, info, env, cont)

@label
def equal_func(a, b, info, env, cont):
    from pycket.interpreter import return_value

    for_chaperone = jit.promote(info.for_chaperone)
    while True:
        if a.eqv(b):
            return return_value(values.w_true, env, cont)

        # Enter into chaperones/impersonators if we have permission to do so
        if ((for_chaperone == EqualInfo.CHAPERONE and a.is_chaperone()) or
            (for_chaperone == EqualInfo.IMPERSONATOR and a.is_impersonator())):
            a = a.get_proxied()
            continue
            #return equal_func(a.get_proxied(), b, info, env, cont)

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
            a, b = a.car(), b.car()
            continue

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
            return equal_vec_func(a, b, values.W_Fixnum(0), info, env, cont)

        if isinstance(a, values_struct.W_RootStruct) and isinstance(b, values_struct.W_RootStruct):
            a_type = a.struct_type()
            b_type = b.struct_type()
            for w_car, w_prop in a_type.props:
                if w_car.isinstance(values_struct.w_prop_equal_hash):
                    for w_car, w_prop in b_type.props:
                        if w_car.isinstance(values_struct.w_prop_equal_hash):
                            assert isinstance(w_prop, values_vector.W_Vector)
                            w_equal_proc, w_hash_proc, w_hash2_proc = \
                                w_prop.ref(0), w_prop.ref(1), w_prop.ref(2)
                            # FIXME: it should work with cycles properly and be an equal?-recur
                            w_equal_recur = values.W_Prim("equal?-recur", equalp)
                            return w_equal_proc.call([a, b, w_equal_recur], env, cont)
            if not a.struct_type().isopaque and not b.struct_type().isopaque:
                # This is probably not correct even if struct2vector were done
                # correct, due to side effects, but it is close enough for now.
                # Though the racket documentation says that `equal?` can elide
                # impersonator/chaperone handlers.
                a_imm = len(a_type.immutables) == a_type.total_field_cnt
                b_imm = len(b_type.immutables) == b_type.total_field_cnt
                a = values_struct.struct2vector(a, immutable=a_imm)
                b = values_struct.struct2vector(b, immutable=b_imm)
                continue

        return return_value(values.w_false, env, cont)

def eqp_logic(a, b):
    if a is b:
        return True
    elif isinstance(a, values.W_Fixnum) and isinstance(b, values.W_Fixnum):
        return a.value == b.value
    elif isinstance(a, values.W_Character) and isinstance(b, values.W_Character):
        return a.value == b.value
    return False

@expose("eq?", [values.W_Object] * 2)
def eqp(a, b):
    return values.W_Bool.make(eqp_logic(a, b))

@expose("procedure-closure-contents-eq?", [procedure] * 2)
def procedure_closure_contents_eq(a, b):
    # FIXME: provide actual information
    return values.W_Bool.make((a is b))

@expose("eqv?", [values.W_Object] * 2)
def eqvp(a, b):
    res = a.eqv(b)
    if not objectmodel.we_are_translated() and res:
        # this is a useful sanity check during testing:
        # check that the following invarinat is true:
        # a.eqv(b) => a.hash_eqv() == b.hash_eqv()
        assert a.hash_eqv() == b.hash_eqv()
    return values.W_Bool.make(res)

