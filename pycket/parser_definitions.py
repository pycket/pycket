
from pycket                 import values
from pycket.argument_parser import ArgParser, EndOfInput

# Common definitions for various argument validation functions

class __extend_parser__(ArgParser):
    object          = (values.W_Object, values.w_false)
    prompt_tag      = (values.W_ContinuationPromptTag,)
    symbol_or_false = (values.W_Symbol, values.w_false)
    logger_or_false = (values.W_Logger, values.w_false)


