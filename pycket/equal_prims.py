
import pycket.impersonators as imp
import pycket.vector        as values_vector
from pycket            import values
from pycket            import values_struct
from pycket.cont       import continuation, label
from pycket.exposeprim import expose, procedure

@expose("equal?", [values.W_Object] * 2, simple=False)
def equalp(a, b, env, cont):
    # FIXME: broken for cycles, etc
    return equal_func(a, b, env, cont)

@expose("equal?/recur", [values.W_Object, values.W_Object, procedure])
def eqp_recur(v1, v2, recur_proc):
    # TODO:
    return values.w_void

@continuation
def equal_car_cont(a, b, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    eq = check_one_val(_vals)
    if eq is values.w_false:
        return return_value(values.w_false, env, cont)
    return equal_func(a, b, env, cont)

@continuation
def equal_unbox_right_cont(r, env, cont, _vals):
    from pycket.interpreter import check_one_val
    l = check_one_val(_vals)
    return r.unbox(env, equal_unbox_done_cont(l, env, cont))

@continuation
def equal_unbox_done_cont(l, env, cont, _vals):
    from pycket.interpreter import check_one_val
    r = check_one_val(_vals)
    return equal_func(l, r, env, cont)

# This function assumes that a and b have the same length
@label
def equal_vec_func(a, b, idx, env, cont):
    from pycket.interpreter import return_value
    if idx.value >= a.length():
        return return_value(values.w_true, env, cont)
    return a.vector_ref(idx, env, equal_vec_left_cont(a, b, idx, env, cont))

# Receive the first value for a given index
@continuation
def equal_vec_left_cont(a, b, idx, env, cont, _vals):
    from pycket.interpreter import check_one_val
    l = check_one_val(_vals)
    return b.vector_ref(idx, env,
                equal_vec_right_cont(a, b, idx, l, env, cont))

# Receive the second value for a given index
@continuation
def equal_vec_right_cont(a, b, idx, l, env, cont, _vals):
    from pycket.interpreter import check_one_val
    r = check_one_val(_vals)
    return equal_func(l, r, env, equal_vec_done_cont(a, b, idx, env, cont))

# Receive the comparison of the two elements and decide what to do
@continuation
def equal_vec_done_cont(a, b, idx, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    eq = check_one_val(_vals)
    if eq is values.w_false:
        return return_value(values.w_false, env, cont)
    inc = values.W_Fixnum(idx.value + 1)
    return equal_vec_func(a, b, inc, env, cont)

@label
def equal_func(a, b, env, cont):
    from pycket.interpreter import return_value
    if imp.is_impersonator_of(a, b) or imp.is_impersonator_of(b, a):
        return return_value(values.w_true, env, cont)
    if isinstance(a, values.W_String) and isinstance(b, values.W_String):
        return return_value(values.W_Bool.make(a.value == b.value), env, cont)
    if isinstance(a, values.W_Cons) and isinstance(b, values.W_Cons):
        return equal_func(a.car(), b.car(), env,
                    equal_car_cont(a.cdr(), b.cdr(), env, cont))
    if isinstance(a, values.W_Box) and isinstance(b, values.W_Box):
        return a.unbox(env, equal_unbox_right_cont(b, env, cont))
    if isinstance(a, values.W_MVector) and isinstance(b, values.W_MVector):
        if a.length() != b.length():
            return return_value(values.w_false, env, cont)
        return equal_vec_func(a, b, values.W_Fixnum(0), env, cont)
    if isinstance(a, values_struct.W_RootStruct) and isinstance(b, values_struct.W_RootStruct):
        if not a.eqv(b):
            for w_car, w_prop in a.struct_type().props:
                if w_car.isinstance(values_struct.w_prop_equal_hash):
                    for w_car, w_prop in b.struct_type().props:
                        if w_car.isinstance(values_struct.w_prop_equal_hash):
                            assert isinstance(w_prop, values_vector.W_Vector)
                            w_equal_proc, w_hash_proc, w_hash2_proc = \
                                w_prop.ref(0), w_prop.ref(1), w_prop.ref(2)
                            # FIXME: it should work with cycles properly and be an equal?-recur
                            w_equal_recur = values.W_Prim("equal?-recur", equalp)
                            return w_equal_proc.call([a, b, w_equal_recur], env, cont)
            if not a.struct_type().isopaque and not b.struct_type().isopaque:
                l = values_struct.struct2vector(a)
                r = values_struct.struct2vector(b)
                return equal_func(l, r, env, cont)
        else:
            return return_value(values.w_true, env, cont)

    return return_value(values.W_Bool.make(a.eqv(b)), env, cont)

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

