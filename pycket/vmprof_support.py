
from pycket.AST   import AST
from pycket.base  import W_Object
from pycket.error import SchemeException
from rpython.rlib import objectmodel, rvmprof

def _get_code(ast, *args):
    return ast

vmprof_profile = rvmprof.vmprof_execute_code("pycket", _get_code, result_class=W_Object)

def _get_full_name(ast):
    # TODO: Provide mor source information for code lacking a surrounding lambda
    lam = ast.surrounding_lambda
    if lam is None:
        mod = ast.surrounding_module
        if mod is None:
            filename = "<unknown>"
        else:
            filename = mod.full_module_path()
        return "rkt:<module>:%s:?" % filename
    filename = lam.srcfile
    srcline  = lam.srcpos
    return "rkt:<lambda>:%s:%s" % (filename, srcline)

rvmprof.register_code_object_class(AST, _get_full_name)

def _init_ready(ast):
    rvmprof.register_code(ast, _get_full_name)

def make_ready(self):
    todo = [self]
    while todo:
        node = todo.pop()
        rvmprof.register_code(node, _get_full_name)
        todo.extend(node.direct_children())
    return self

AST.make_ready = make_ready
AST._attrs_ += ['_vmprof_unique_id']

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

