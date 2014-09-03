#! /usr/bin/env python
# -*- coding: utf-8 -*-

from ..      import impersonators as imp
from ..      import values
from ..      import vector as values_vector
from ..cont  import continuation, label
from ..error import SchemeException
from .expose import unsafe, default, expose

@expose("vector")
def vector(args):
    return values_vector.W_Vector.fromelements(args)

# FIXME: immutable
@expose("vector-immutable")
def vector_immutable(args):
    return values_vector.W_Vector.fromelements(args, immutable=True)

@expose("make-vector", [values.W_Fixnum, default(values.W_Object, values.W_Fixnum(0))])
def make_vector(w_size, w_val):
    size = w_size.value
    if size < 0:
        raise SchemeException("make-vector: expected a positive fixnum")
    return values_vector.W_Vector.fromelement(w_val, size)

@expose("vector-length", [values_vector.W_MVector])
def vector_length(v):
    return values.W_Fixnum(v.length())

@expose("vector-ref", [values.W_MVector, values.W_Fixnum], simple=False)
def vector_ref(v, i, env, cont):
    idx = i.value
    if not (0 <= idx < v.length()):
        raise SchemeException("vector-ref: index out of bounds")
    return v.vector_ref(i, env, cont)

@expose("vector-set!", [values.W_MVector, values.W_Fixnum, values.W_Object], simple=False)
def vector_set(v, i, new, env, cont):
    if v.immutable():
        raise SchemeException("vector-set!: given immutable vector")
    idx = i.value
    if not (0 <= idx < v.length()):
        raise SchemeException("vector-set!: index out of bounds")
    return v.vector_set(i, new, env, cont)

@expose("vector-copy!",
        [values.W_MVector, values.W_Fixnum, values.W_MVector,
         default(values.W_Fixnum, None), default(values.W_Fixnum, None)],
        simple=False)
def vector_copy(dest, _dest_start, src, _src_start, _src_end, env, cont):
    if dest.immutable():
        raise SchemeException("vector-copy!: given an immutable destination")
    src_start  = _src_start.value if _src_start is not None else 0
    src_end    = _src_end.value if _src_end is not None else src.length()
    dest_start = _dest_start.value

    src_range  = src_end - src_start
    dest_range = dest.length() - dest_start

    if not (0 <= dest_start < dest.length()):
        raise SchemeException("vector-copy!: destination start out of bounds")
    if not (0 <= src_start < src.length()) or not (0 <= src_start < src.length()):
        raise SchemeException("vector-copy!: source start/end out of bounds")
    if dest_range < src_range:
        raise SchemeException("vector-copy!: not enough room in target vector")

    return vector_copy_loop(src, src_start, src_end,
                dest, dest_start, values.W_Fixnum(0), env, cont)

@label
def vector_copy_loop(src, src_start, src_end, dest, dest_start, i, env, cont):
    from ..interpreter import return_value
    src_idx = i.value + src_start
    if src_idx >= src_end:
        return return_value(values.w_void, env, cont)
    idx = values.W_Fixnum(src_idx)
    return src.vector_ref(idx, env,
                vector_copy_cont_get(src, src_start, src_end, dest,
                    dest_start, i, env, cont))

@continuation
def goto_vector_copy_loop(src, src_start, src_end, dest, dest_start, next, env, cont, _vals):
    return vector_copy_loop(
            src, src_start, src_end, dest, dest_start, next, env, cont)

@continuation
def vector_copy_cont_get(src, src_start, src_end, dest, dest_start, i, env, cont, _vals):
    from ..interpreter import check_one_val
    val  = check_one_val(_vals)
    idx  = values.W_Fixnum(i.value + dest_start)
    next = values.W_Fixnum(i.value + 1)
    return dest.vector_set(idx, val, env,
                goto_vector_copy_loop(src, src_start, src_end,
                    dest, dest_start, next, env, cont))

# FIXME: Chaperones
@expose("unsafe-vector-ref", [values.W_Object, unsafe(values.W_Fixnum)], simple=False)
def unsafe_vector_ref(v, i, env, cont):
    from ..interpreter import return_value
    if isinstance(v, imp.W_ImpVector) or isinstance(v, imp.W_ChpVector):
        return v.vector_ref(i, env, cont)
    else:
        assert type(v) is values_vector.W_Vector
        val = i.value
        assert val >= 0
        return return_value(v._ref(val), env, cont)

@expose("unsafe-vector*-ref", [unsafe(values_vector.W_Vector), unsafe(values.W_Fixnum)])
def unsafe_vector_star_ref(v, i):
    return v._ref(i.value)

# FIXME: Chaperones
@expose("unsafe-vector-set!", [values.W_Object, unsafe(values.W_Fixnum), values.W_Object], simple=False)
def unsafe_vector_set(v, i, new, env, cont):
    from ..interpreter import return_value
    if isinstance(v, imp.W_ImpVector) or isinstance(v, imp.W_ChpVector):
        return v.vector_set(i, new, env, cont)
    else:
        assert type(v) is values_vector.W_Vector
        return return_value(v._set(i.value, new), env, cont)

@expose("unsafe-vector*-set!",
        [unsafe(values_vector.W_Vector), unsafe(values.W_Fixnum), values.W_Object])
def unsafe_vector_star_set(v, i, new):
    return v._set(i.value, new)

@expose("unsafe-vector-length", [values.W_MVector])
def unsafe_vector_length(v):
    return values.W_Fixnum(v.length())

@expose("unsafe-vector*-length", [unsafe(values_vector.W_Vector)])
def unsafe_vector_star_length(v):
    return values.W_Fixnum(v.length())
