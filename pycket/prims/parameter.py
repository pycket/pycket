
from pycket                 import values
from pycket.cont            import call_cont, continuation, BaseCont
from pycket                 import values_parameter
from pycket                 import vector
from pycket.argument_parser import ArgParser
from pycket.arity           import Arity
from pycket.base            import W_Object
from pycket.error           import SchemeException
from pycket.prims.expose    import expose, expose_val, default, procedure, make_procedure
from rpython.rlib           import jit

@expose("reparameterize", [values_parameter.W_Parameterization])
def reparameterize(p):
    return p

OBJ_SYM = values.W_Symbol.make(values_parameter.OBJ_NAME)

@expose("make-parameter",
        [values.W_Object,
         default(values.W_Object, values.w_false),
         default(values.W_Symbol, OBJ_SYM)])
def make_parameter(init, guard, name):
    return values_parameter.W_Parameter(init, guard, name.tostring())

@expose("make-derived-parameter",
        [values_parameter.W_BaseParameter, procedure, procedure])
def make_derived_parameter(param, guard, wrap):
    return values_parameter.W_DerivedParameter(param, guard, wrap)

@expose("extend-parameterization", arity=Arity.geq(1), simple=False)
@jit.unroll_safe
def scheme_extend_parameterization(args, env, cont):
    from pycket.interpreter import return_value
    if len(args) == 0:
        raise SchemeException("extend-parameterization: expected 1 or more arguments")

    config = args[0]
    argc = len(args)

    if argc < 2 or argc % 2 != 1: # or not isinstance(config, values_parameter.W_Parameterization):
        return return_value(config, env, cont)

    # strip out the proxies from impersonated/chaperoned parameters
    # as parameter-procedures can be proxied
    for i in range(1, len(args)):
        if args[i].is_impersonator() or args[i].is_chaperone():
            args[i] = args[i].get_proxied()

    parser = ArgParser("extend-parameterization", args, start_at=1)
    params = [None]*((argc-1)/2)
    keys = [None]*((argc-1)/2)
    i = 0
    while parser.has_more():
        params[i]  = parser.expect(values_parameter.W_BaseParameter)
        keys[i]    = parser.expect(values.W_Object)
        i += 1

    assert isinstance(config, values_parameter.W_Parameterization)
    return config.extend(params, keys, env, cont)

def call_with_parameterization(f, args, paramz, env, cont):
    cont.update_cm(values.parameterization_key, paramz)
    return f.call(args, env, cont)

@expose("call-with-parameterization",
        [values_parameter.W_Parameterization, values.W_Object], simple=False)
def call_w_paramz(paramz, f, env, cont):
    return call_with_parameterization(f, [], paramz, env, cont)

@continuation
def call_with_paramz_cont(f, args, env, cont, _vals):
    from pycket.interpreter import check_one_val
    paramz  = check_one_val(_vals)
    return call_with_parameterization(f, args, paramz, env, cont)

# only used in input_output.py
def call_with_extended_paramz(f, args, keys, vals, env, cont):
    from pycket.values import parameterization_key
    paramz = cont.get_mark_first(parameterization_key)
    assert isinstance(paramz, values_parameter.W_Parameterization)
    return paramz.extend(keys, vals, env, call_with_paramz_cont(f, args, env, cont))

@make_procedure("eval-jit-enabled-guard", [values.W_Object], simple=False)
def eval_jit_enabled_guard(arg, env, cont):
    from pycket.interpreter import return_value
    # never disable the jit
    return return_value(values.w_void, env, cont)

expose_val("compile-enforce-module-constants", values_parameter.W_Parameter(values.w_true))

# compilation should avoid function-call inlining and other optimizations that may cause information to be lost from stack traces
expose_val("compile-context-preservation-enabled", values_parameter.W_Parameter(values.w_true))

expose_val("compile-allow-set!-undefined", values_parameter.W_Parameter(values.w_false))

current_cmd_args_param = values_parameter.W_Parameter(vector.W_Vector.fromelements([]))
expose_val("current-command-line-arguments", current_cmd_args_param)
expose_val("eval-jit-enabled", values_parameter.W_Parameter(values.w_true, eval_jit_enabled_guard))
expose_val("exnh", values_parameter.W_Parameter(values.w_false))
expose_val("load-on-demand-enabled", values_parameter.W_Parameter(values.w_true))
expose_val("read-on-demand-source", values_parameter.W_Parameter(values.w_true))
expose_val("parameterization-key", values.parameterization_key)
expose_val("print-mpair-curly-braces", values_parameter.W_Parameter(values.w_false))
expose_val("print-pair-curly-braces", values_parameter.W_Parameter(values.w_false))
expose_val("error-print-source-location", values_parameter.W_Parameter(values.w_true))

expose_val("port-count-lines-enabled", values_parameter.W_Parameter(values.w_false))

REALM = values.W_Symbol.make("pycket")

expose_val("current-compile-realm", values_parameter.W_Parameter(REALM))

# error-syntax->string-handler determines the error syntax conversion handler,
# which is used to print a syntax form that is embedded in an error message
expose_val("error-syntax->string-handler", values_parameter.W_Parameter(values.w_false))

READ_TRUE_PARAMS = """
read-accept-reader
read-accept-lang
read-accept-compiled
read-accept-bar-quote
read-square-bracket-as-paren
read-curly-brace-as-paren
read-accept-box
read-accept-graph
read-decimal-as-inexact
read-accept-dot
read-accept-infix-dot
read-accept-quasiquote
"""

READ_FALSE_PARAMS = """
read-square-bracket-with-tag
read-curly-brace-with-tag
read-cdot
read-single-flonum
read-syntax-accept-graph
"""

for name in READ_TRUE_PARAMS.split():
    expose_val(name, values_parameter.W_Parameter(values.w_true))

for name in READ_FALSE_PARAMS.split():
    expose_val(name, values_parameter.W_Parameter(values.w_false))
