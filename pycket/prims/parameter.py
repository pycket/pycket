
from pycket              import values
from pycket              import values_parameter
from pycket.base         import W_Object
from pycket.error        import SchemeException
from pycket.prims.expose import expose, expose_val, default, procedure

@expose("make-parameter",
        [values.W_Object, default(values.W_Object, values.w_false)])
def make_parameter(init, guard):
    return values_parameter.W_Parameter(init, guard)

@expose("make-derived-parameter",
        [values_parameter.W_BaseParameter, procedure, procedure])
def make_derived_parameter(param, guard, wrap):
    return values_parameter.W_DerivedParameter(param, guard, wrap)

@expose("extend-parameterization",
        [values.W_Object, values.W_Object, values.W_Object])
def extend_paramz(paramz, key, val):
    if not isinstance(key, values_parameter.W_BaseParameter):
        raise SchemeException("Not a parameter: " + key.tostring())
    if isinstance(paramz, values_parameter.W_Parameterization):
        return paramz.extend([key], [val])
    else:
        return paramz # This really is the Racket behavior

def call_with_parameterization(f, args, paramz, env, cont):
    cont.update_cm(values.parameterization_key, paramz)
    return f.call(args, env, cont)

@expose("call-with-parameterization",
        [values.W_Object, values_parameter.W_Parameterization], simple=False)
def call_w_paramz(f, paramz, env, cont):
    return call_with_parameterization(f, [], paramz, env, cont)

def call_with_extended_paramz(f, args, keys, vals, env, cont):
    from pycket.values import parameterization_key
    # XXX seems untested?
    paramz = cont.get_mark_first(parameterization_key)
    assert isinstance(paramz, values_parameter.W_Parameterization) # XXX is this always right?
    paramz_new = paramz.extend(keys, vals)
    return call_with_parameterization(f, args, paramz_new, env, cont)

expose_val("parameterization-key", values.parameterization_key)
expose_val("print-mpair-curly-braces", values_parameter.W_Parameter(values.w_false))
expose_val("print-pair-curly-braces", values_parameter.W_Parameter(values.w_false))

