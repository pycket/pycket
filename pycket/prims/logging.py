
from pycket                 import values, values_parameter, values_string
from pycket.arity           import Arity
from pycket.argument_parser import ArgParser, EndOfInput
from pycket.prims.expose    import default, expose, expose_val
from rpython.rlib           import jit

DEBUG = values.W_Symbol.make("debug")

LOG_LEVEL_STR = ['none', 'fatal', 'error', 'warning', 'info', 'debug']
LOG_LEVEL = tuple(map(values.W_Symbol.make, LOG_LEVEL_STR) + [values.w_false])

logger_max_wanted_level = values.W_Symbol.make("debug")

def get_level_from_index(lvl_index):
    if not (lvl_index >= 0 and lvl_index <= 5):
        raise Exception("invalid level index : %s" % lvl_index)
    return LOG_LEVEL[lvl_index] # W_Symbol

def level_value(lvl_sym):
    lvl_str = lvl_sym.tostring()
    try:
        return LOG_LEVEL_STR.index(lvl_str)
    except ValueError:
        raise Exception("invalid level type : %s" % lvl_str)

def level_geq(lvl_sym_1, lvl_sym_2):
    return level_value(lvl_sym_1) >= level_value(lvl_sym_2)

def check_level(lvl_sym):
    from pycket.error import SchemeException
    if lvl_sym not in LOG_LEVEL:
        raise SchemeException("Invalid log level : %s" % lvl_sym.tostring())

init_syslog_level = values.W_Symbol.make("none") # INIT_SYSLOG_LEVEL
init_stderr_level = values.W_Symbol.make("error") # SCHEME_LOG_ERROR
init_stdout_level = values.W_Symbol.make("none")

w_main_logger = values.W_Logger(values.w_false,
                                values.w_false,
                                values.w_false,
                                [],
                                init_syslog_level,
                                init_stderr_level,
                                init_stdout_level)

@expose("make-logger", arity=Arity.geq(0))
@jit.unroll_safe
def make_logger(args):
    parser = ArgParser("make-logger", args)

    topic           = values.w_false
    parent          = values.w_false
    propagate_level = DEBUG # propagate everything

    try:
        topic = parser.expect(values.W_Symbol, values.w_false)
        parent = parser.expect(values.W_Logger, values.w_false)
        propagate_level = parser.expect(*LOG_LEVEL)
    except EndOfInput:
        pass

    # Any remaining arguments are propagate topics
    propagate_topic = parser.expect_many(values.W_Symbol, values.w_false)

    init_syslog_level = w_main_logger.get_syslog_level()
    init_stderr_level = w_main_logger.get_stderr_level()
    init_stdout_level = w_main_logger.get_stdout_level()

    return values.W_Logger(topic, parent, propagate_level, propagate_topic, init_syslog_level, init_stderr_level, init_stdout_level)

@expose("log-level?", [values.W_Object, values.W_Object, default(values.W_Object, values.w_false)])
def log_level(logger, level, topic):
    # performance-logger, info, performance
    if logger.is_anyone_interested(level, topic):
        return values.w_true
    return values.w_false

@expose("log-message", arity=Arity.oneof(4, 5, 6))
def log_message(args):
    # currently relying on this information about log-level? :
    # Use this function to avoid work generating an event for
    # log-message if no receiver is interested in the information;
    # this shortcut is built into log-fatal, log-error, log-warning,
    # log-info, log-debug, and forms bound by define-logger

    # The complete solution is to :
    # FIXME : implement log_receivers, create an event and distribute it

    parser = ArgParser("log-message", args)


    # logger : logger?
    logger = parser.expect(values.W_Logger)

    # level : log-level/c
    level = parser.expect(*LOG_LEVEL)

    # topic : (or/c symbol? #f) = (logger-name logger)
    try:
        topic = parser.expect(values.W_Symbol, values.w_false)
    except Exception, e:
        topic = logger.get_name()

    # message : string?
    message = parser.expect(values_string.W_String)

    # data : any/c
    data = parser.expect(values.W_Object)

    try:
        # prefix-message? : any/c = #t
        prefix_message_huh = parser.expect(values.W_Object, values.w_true)
    except EndOfInput:
        prefix_message_huh = values.w_true

    print_str = message.tostring()

    if (prefix_message_huh is not values.w_false) and (topic is not values.w_false):
        print_str = "%s: %s" % (topic.tostring(), print_str)

    print(print_str)

    return values.w_void

@expose("logger-name", [values.W_Logger])
def logger_name(logger):
    return logger.topic

w_current_logger = values_parameter.W_Parameter(w_main_logger)
expose_val("current-logger", w_current_logger)
