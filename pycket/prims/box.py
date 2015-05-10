
from pycket              import values
from pycket.prims        import equal as eq_prims
from pycket.prims.expose import default, expose, subclass_unsafe

@expose("box", [values.W_Object])
def box(v):
    return values.W_MBox(v)

@expose("box-immutable", [values.W_Object])
def box_immutable(v):
    return values.W_IBox(v)

@expose("unbox", [values.W_Box], simple=False)
def unbox(b, env, cont):
    return b.unbox(env, cont)

@expose("unsafe-unbox", [subclass_unsafe(values.W_Box)], simple=False)
def unsafe_unbox(b, env, cont):
    return b.unbox(env, cont)

@expose("set-box!", [values.W_Box, values.W_Object], simple=False)
def set_box(box, v, env, cont):
    return box.set_box(v, env, cont)

@expose("unsafe-set-box!", [subclass_unsafe(values.W_Box), values.W_Object], simple=False)
def unsafe_set_box(box, v, env, cont):
    return box.set_box(v, env, cont)

# This implementation makes no guarantees about atomicity
@expose("box-cas!", [values.W_MBox, values.W_Object, values.W_Object])
def box_cas(box, old, new):
    if eq_prims.eqp_logic(box.value, old):
        box.value = new
        return values.w_true
    return values.w_false

@expose("make-weak-box", [values.W_Object])
def make_weak_box(val):
    return values.W_WeakBox(val)

@expose("weak-box-value",
        [values.W_WeakBox, default(values.W_Object, values.w_false)])
def weak_box_value(wb, default):
    v = wb.get()
    return v if v is not None else default

