
from pycket.exposeprim import default, expose
from pycket            import values
from pycket            import vector as values_vector

# FIXME : Make the random functions actually do what they are supposed to do
# random things
@expose("random")
def random(args):
    return values.W_Fixnum(1)

@expose("random-seed", [values.W_Fixnum])
def random_seed(seed):
    return values.w_void

@expose("make-pseudo-random-generator", [])
def make_pseudo_random_generator():
    return values.W_PseudoRandomGenerator()

@expose("current-pseudo-random-generator")
def current_pseudo_random_generator(args):
    if not args:
        return values.W_PseudoRandomGenerator()
    return values.w_void

@expose("pseudo-random-generator->vector", [values.W_PseudoRandomGenerator])
def pseudo_random_generator_to_vector(gen):
    return values_vector.W_Vector.fromelements([])

@expose("vector->pseudo-random-generator", [values.W_PseudoRandomGenerator, default(values.W_MVector, None)])
def vector_to_pseudo_random_generator(gen, vec):
    return values.W_PseudoRandomGenerator()

@expose("pseudo-random-generator-vector?", [values.W_Object])
def pseudo_random_generator_vector_huh(vec):
    return values.W_Bool.make(isinstance(vec, values.W_MVector) and vec.length() == 0)

