
from rpython.rlib.objectmodel import we_are_translated

class SchemeException(Exception):
    def __init__(self, msg):
        if not we_are_translated():
            Exception.__init__(self, msg)
        self.msg = msg
        self.context_ast = None
        self.context_module = None

    def format_error(self): # pragma: no cover
        # only error printing
        result = self.msg
        if self.context_ast:
            result += "\n  while executing: %s" % (
                self.context_ast.tostring(), )
            if self.context_ast.surrounding_lambda:
                result += "\n  in function: %s" % (
                self.context_ast.surrounding_lambda.tostring(),)
        context = self.context_module
        if context is not None:
            result += "\n  in module: %s" % context.full_module_path()
        return result

