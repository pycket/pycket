
from pycket.AST   import AST
from pycket.base  import W_Object
from rpython.rlib import rvmprof

def _get_code(ast, env):
    return ast

vmprof_profile = rvmprof.vmprof_execute_code("pycket", _get_code, result_class=W_Object)

def _get_full_name(ast):
    return "rkt:func_name:func_line:filename"

rvmprof.register_code_object_class(AST, _get_full_name)

