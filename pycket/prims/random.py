from rpython.rlib import rrandom, rarithmetic

from pycket.prims.expose import default, expose, expose_val
from pycket              import values, values_parameter
from pycket              import vector as values_vector
from pycket.error        import SchemeException

# XXX for now just always use a global rng

rng = rrandom.Random()

@expose("random")
def random(args):
    if not args:
        # random flonum
        return values.W_Flonum(rng.random())
    a1 = args[0]
    if isinstance(a1, values.W_Fixnum):
        upper = a1.value
        return values.W_Fixnum(int(rng.random() * upper))
    if isinstance(a1, values.W_PseudoRandomGenerator):
        return values.W_Flonum(rng.random())
    raise SchemeException("random: invalid arguments")

@expose("random-seed", [values.W_Fixnum])
def random_seed(seed):
    key = [rarithmetic.r_uint(seed.value)]
    rng.init_by_array(key)
    return values.w_void

@expose("make-pseudo-random-generator", [])
def make_pseudo_random_generator():
    return values.W_PseudoRandomGenerator()

current_pseudo_random_generator = values_parameter.W_Parameter(values.W_PseudoRandomGenerator())
expose_val("current-pseudo-random-generator", current_pseudo_random_generator)

@expose("pseudo-random-generator->vector", [values.W_PseudoRandomGenerator])
def pseudo_random_generator_to_vector(gen):
    return values_vector.W_Vector.fromelements([])

@expose("vector->pseudo-random-generator", [values.W_PseudoRandomGenerator, default(values.W_MVector, None)])
def vector_to_pseudo_random_generator(gen, vec):
    return values.W_PseudoRandomGenerator()

@expose("pseudo-random-generator-vector?", [values.W_Object])
def pseudo_random_generator_vector_huh(vec):
    return values.W_Bool.make(isinstance(vec, values.W_MVector) and vec.length() == 0)

