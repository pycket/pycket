
from pycket                 import values, values_parameter
from pycket.arity           import Arity
from pycket.argument_parser import ArgParser, EndOfInput
from pycket.prims.expose    import default, expose, expose_val
from rpython.rlib           import jit

DEBUG = values.W_Symbol.make("debug")

w_default_logger = values.W_Logger(values.w_false, values.w_false, values.w_false, [])

LOG_LEVEL = ['none', 'fatal', 'error', 'warning', 'info', 'debug']
LOG_LEVEL = tuple(map(values.W_Symbol.make, LOG_LEVEL) + [values.w_false])

@expose("make-logger", arity=Arity.geq(0))
@jit.unroll_safe
def make_logger(args):
    parser = ArgParser("make-logger", args)

    topic           = values.w_false
    parent          = values.w_false
    propagate_level = DEBUG

    try:
        topic = parser.expect(values.W_Symbol, values.w_false)
        parent = parser.expect(values.W_Logger, values.w_false)
        propagate_level = parser.expect(*LOG_LEVEL)
    except EndOfInput:
        pass

    # Any remaining arguments are propagate topics
    propagate_topic = parser.expect_many(values.W_Symbol, values.w_false)

    return values.W_Logger(topic, parent, propagate_level, propagate_topic)

@expose("log-level?", [values.W_Object, values.W_Object, default(values.W_Object, values.w_false)])
def log_level(logger, level, topic):
    # TODO: Actual implementation
    return values.w_false

@expose("log-message", arity=Arity.oneof(4, 5, 6))
def log_message(args):
    # TODO: Actual implementation
    return

@expose("logger-name", [values.W_Logger])
def logger_name(logger):
    return logger.topic

w_current_logger = values_parameter.W_Parameter(w_default_logger)
expose_val("current-logger", w_current_logger)

