#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket import impersonators as imp
from pycket import values
from pycket import vector as values_vector
from pycket.cont import continuation, label, loop_label
from pycket.error import SchemeException
from pycket.prims.expose import unsafe, default, expose, subclass_unsafe
from pycket.prims import equal
from rpython.rlib import jit
from pycket.impersonators import W_ImpVector

@expose("vector")
def vector(args):
    return values_vector.W_Vector.fromelements(args)

@expose("flvector")
def flvector(args):
    return values_vector.W_FlVector.fromelements(args)

@expose("extflvector?", [values.W_Object])
def extflvector(obj):
    return values.w_false

# FIXME: immutable
@expose("vector-immutable")
def vector_immutable(args):
    return values_vector.W_Vector.fromelements(args, immutable=True)

@expose(["make-vector", "make-fxvector"],
        [values.W_Fixnum, default(values.W_Object, values.W_Fixnum.ZERO)],
        partial_type='w_vector')
def make_vector(w_size, w_val):
    size = w_size.value
    if size < 0:
        raise SchemeException("make-vector: expected a positive fixnum")
    return values_vector.W_Vector.fromelement(w_val, size)

@expose("make-flvector", [values.W_Fixnum, default(values.W_Flonum, values.W_Flonum.ZERO)])
def make_flvector(w_size, w_val):
    size = w_size.value
    if size < 0:
        raise SchemeException("make-flvector: expected a positive fixnum")
    return values_vector.W_FlVector.fromelement(w_val, size)

@expose("vector-length", [values_vector.W_MVector])
def vector_length(v):
    return values.W_Fixnum(v.length())

@expose("vector*-length", [values_vector.W_MVector])
def vector_length(v):
    if isinstance(v, W_ImpVector):
        raise SchemeException("vector*-length is constrained to work on vectors that are not impersonators.")
    return values.W_Fixnum(v.length())

@expose("flvector-length", [values_vector.W_FlVector])
def flvector_length(v):
    return values.W_Fixnum(v.length())

def vector_ref_impl(v, i, env, cont, calling_app):
    idx = i.value
    if not (0 <= idx < v.length()):
        raise SchemeException("vector-ref: index out of bounds")
    return v.vector_ref(idx, env, cont, app=calling_app)

@expose("vector*-ref", [values.W_MVector, values.W_Fixnum], simple=False, extra_info=True)
def vector_star_ref(v, i, env, cont, calling_app):
    if isinstance(v, W_ImpVector):
        raise SchemeException("vector*-ref is constrained to work on vectors that are not impersonators.")
    return vector_ref_impl(v, i, env, cont, calling_app)

@expose("vector-ref", [values.W_MVector, values.W_Fixnum], simple=False, extra_info=True)
def vector_ref(v, i, env, cont, calling_app):
    return vector_ref_impl(v, i, env, cont, calling_app)

@expose("flvector-ref", [values_vector.W_FlVector, values.W_Fixnum], simple=False)
def flvector_ref(v, i, env, cont):
    idx = i.value
    if not (0 <= idx < v.length()):
        raise SchemeException("vector-ref: index out of bounds")
    return v.vector_ref(idx, env, cont)

def vector_set_impl(v, i, new, env, cont, calling_app):
    if v.immutable():
        raise SchemeException("vector-set!: given immutable vector")
    idx = i.value
    if not (0 <= idx < v.length()):
        raise SchemeException("vector-set!: index out of bounds")
    return v.vector_set(idx, new, env, cont, app=calling_app)

@expose("vector-set!", [values.W_MVector, values.W_Fixnum, values.W_Object],
        simple=False, extra_info=True)
def vector_set(v, i, new, env, cont, calling_app):
    return vector_set_impl(v, i, new, env, cont, calling_app)

@expose("vector*-set!", [values.W_MVector, values.W_Fixnum, values.W_Object],
        simple=False, extra_info=True)
def vector_set(v, i, new, env, cont, calling_app):
    if isinstance(v, W_ImpVector):
        raise SchemeException("vector*-set! is constrained to work on vectors that are not impersonators.")
    return vector_set_impl(v, i, new, env, cont, calling_app)

@expose("flvector-set!", [values_vector.W_FlVector, values.W_Fixnum, values.W_Flonum],
        simple=False, extra_info=True)
def flvector_set(v, i, new, env, cont, calling_app):
    idx = i.value
    if not (0 <= idx < v.length()):
        raise SchemeException("flvector-set!: index out of bounds")
    return v.vector_set(idx, new, env, cont, app=calling_app)

def copy_vector(v, env, cont):
    from pycket.interpreter import return_value
    if isinstance(v, values_vector.W_Vector):
        return return_value(v._make_copy(immutable=True), env, cont)

    len = v.length()
    if not len:
        vector = values_vector.W_Vector.fromelements([])
        return return_value(vector, env, cont)

    # Do a little peeking to provide a hint to the strategy
    base = imp.get_base_object(v)
    assert isinstance(base, values_vector.W_Vector)
    data = values_vector.W_Vector.fromelement(
            base.ref(0), len, strategy=base.get_strategy())

    return copy_vector_loop(v, data, len, 0, env, cont)

@loop_label
def copy_vector_loop(v, data, len, idx, env, cont):
    from pycket.interpreter import return_value
    if idx >= len:
        # Freeze vector and return
        strategy = data.strategy
        data.strategy = strategy.immutable_variant()
        return return_value(data, env, cont)
    return v.vector_ref(idx, env,
            copy_vector_ref_cont(v, data, len, idx, env, cont))

@continuation
def copy_vector_ref_cont(v, data, len, idx, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    data.set(idx, val)
    return copy_vector_loop(v, data, len, idx + 1, env, cont)

@expose("vector->immutable-vector", [values_vector.W_MVector], simple=False)
def vector2immutablevector(v, env, cont):
    from pycket.interpreter import return_value
    if v.immutable():
        return return_value(v, env, cont)
    return copy_vector(v, env, cont)


@continuation
def vector_cas_success(env, cont, _vals):
    from pycket.interpreter import return_value
    return return_value(values.w_true, env, cont)

@continuation
def vector_cas_bang_cont(vec, pos_idx, old_val, new_val, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    current_vec_val = check_one_val(_vals)
    if equal.eqp_logic(current_vec_val,old_val): #eq?
        return vec.vector_set(pos_idx, new_val, env, vector_cas_success(env, cont))
    return return_value(values.w_false, env, cont)

# FIXME: Chaperones
@expose("vector-cas!", [values.W_MVector, values.W_Fixnum, values.W_Object, values.W_Object], simple=False)
def vector_cas_bang(vec, pos, old_val, new_val, env, cont):
    if isinstance(vec, imp.W_ImpVector):
        raise SchemeException("vector-cas!: exptects a non impersonator vector")

    return vec.vector_ref(pos.value, env,
                          vector_cas_bang_cont(vec, pos.value, old_val, new_val, env, cont))

@expose("vector-copy!",
        [values.W_MVector, values.W_Fixnum, values.W_MVector,
         default(values.W_Fixnum, None), default(values.W_Fixnum, None)],
        simple=False)
def vector_copy(dest, _dest_start, src, _src_start, _src_end, env, cont):
    from pycket.interpreter import return_value

    if dest.immutable():
        raise SchemeException("vector-copy!: given an immutable destination")
    src_start  = _src_start.value if _src_start is not None else 0
    src_end    = _src_end.value if _src_end is not None else src.length()
    dest_start = _dest_start.value

    src_range  = src_end - src_start
    dest_range = dest.length() - dest_start

    if src_range == 0:
        return return_value(values.w_void, env, cont)

    if not (0 <= dest_start < dest.length()):
        raise SchemeException("vector-copy!: destination start out of bounds")
    if not (0 <= src_start <= src.length()) or not (0 <= src_start <= src.length()):
        raise SchemeException("vector-copy!: source start/end out of bounds")
    if dest_range < src_range:
        raise SchemeException("vector-copy!: not enough room in target vector")

    return vector_copy_loop(src, src_start, src_end, dest, dest_start, 0, env, cont)

@loop_label
def vector_copy_loop(src, src_start, src_end, dest, dest_start, i, env, cont):
    from pycket.interpreter import return_value
    src_idx = i + src_start
    if src_idx >= src_end:
        return return_value(values.w_void, env, cont)
    return src.vector_ref(src_idx, env,
                vector_copy_cont_get(src, src_start, src_end, dest,
                    dest_start, i, env, cont))

@continuation
def goto_vector_copy_loop(src, src_start, src_end, dest, dest_start, next, env, cont, _vals):
    return vector_copy_loop(
            src, src_start, src_end, dest, dest_start, next, env, cont)

@continuation
def vector_copy_cont_get(src, src_start, src_end, dest, dest_start, i, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val  = check_one_val(_vals)
    idx  = i + dest_start
    next = i + 1
    return dest.vector_set(idx, val, env,
                goto_vector_copy_loop(src, src_start, src_end,
                    dest, dest_start, next, env, cont))

# FIXME: Chaperones
@expose("unsafe-vector-ref", [subclass_unsafe(values.W_MVector), unsafe(values.W_Fixnum)], simple=False)
def unsafe_vector_ref(v, i, env, cont):
    from pycket.interpreter import return_value
    if isinstance(v, imp.W_ImpVector) or isinstance(v, imp.W_ChpVector):
        return v.vector_ref(i.value, env, cont)
    else:
        assert type(v) is values_vector.W_Vector
        val = i.value
        assert val >= 0
        return return_value(v.unsafe_ref(val), env, cont)

@expose("unsafe-flvector-ref", [unsafe(values_vector.W_FlVector), unsafe(values.W_Fixnum)])
def unsafe_flvector_ref(v, i):
    return v.unsafe_ref(i.value)

@expose("unsafe-vector*-ref", [unsafe(values_vector.W_Vector), unsafe(values.W_Fixnum)])
def unsafe_vector_star_ref(v, i):
    return v.unsafe_ref(i.value)

# FIXME: Chaperones
@expose("unsafe-vector-set!",
        [subclass_unsafe(values.W_MVector), unsafe(values.W_Fixnum), values.W_Object],
        simple=False)
def unsafe_vector_set(v, i, new, env, cont):
    from pycket.interpreter import return_value
    if isinstance(v, imp.W_ImpVector) or isinstance(v, imp.W_ChpVector):
        return v.vector_set(i.value, new, env, cont)
    else:
        assert type(v) is values_vector.W_Vector
        v.unsafe_set(i.value, new)
        return return_value(values.w_void, env, cont)

@expose("unsafe-vector*-set!",
        [unsafe(values_vector.W_Vector), unsafe(values.W_Fixnum), values.W_Object])
def unsafe_vector_star_set(v, i, new):
    return v.unsafe_set(i.value, new)

@expose("unsafe-flvector-set!",
        [unsafe(values_vector.W_FlVector), unsafe(values.W_Fixnum), unsafe(values.W_Flonum)])
def unsafe_flvector_set(v, i, new):
    return v.unsafe_set(i.value, new)

@expose("unsafe-vector-length", [subclass_unsafe(values.W_MVector)])
def unsafe_vector_length(v):
    return values.W_Fixnum(v.length())

@expose("unsafe-vector*-length", [unsafe(values_vector.W_Vector)])
def unsafe_vector_star_length(v):
    return values.W_Fixnum(v.length())

@expose("unsafe-flvector-length", [unsafe(values_vector.W_FlVector)])
def unsafe_flvector_length(v):
    return values.W_Fixnum(v.length())

