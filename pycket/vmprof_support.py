
from pycket.AST   import AST
from pycket.base  import W_Object
from pycket.error import SchemeException
from rpython.rlib import rvmprof

def _get_code(ast, env):
    return ast

vmprof_profile = rvmprof.vmprof_execute_code("pycket", _get_code, result_class=W_Object)

def _get_full_name(ast):
    return "rkt:func_name:func_line:filename"

rvmprof.register_code_object_class(AST, _get_full_name)

def enable(fileno, period):
    """Enable vmprof.  Writes go to the given 'fileno', a file descriptor
    opened for writing.  *The file descriptor must remain open at least
    until disable() is called.*

    'interval' is a float representing the sampling interval, in seconds.
    Must be smaller than 1.0
    """
    try:
        rvmprof.enable(fileno, period)
    except rvmprof.VMProfError:
        print "Error enabling vmprof"
        raise

def disable():
    """Disable vmprof.  Remember to close the file descriptor afterwards
    if necessary.
    """
    try:
        rvmprof.disable()
    except rvmprof.VMProfError:
        print "Error disabling vmprof"
        raise

