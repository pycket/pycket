from rpython.rlib.objectmodel import we_are_translated

class SchemeException(Exception):
    def __init__(self, msg):
        if not we_are_translated():
            Exception.__init__(self, msg)
        self.msg = msg
        self.context_ast = None
        self.context_module = None
        self.cont = None

    def format_error(self): # pragma: no cover
        # only error printing
        result = self.msg
        if self.context_ast:
            result += "\n  while executing: %s" % (
                self.context_ast.tostring(), )
            if self.context_ast.surrounding_lambda:
                result += "\n  in function: %s" % (
                self.context_ast.surrounding_lambda.tostring(),)
        if self.context_module:
            result += "\n  in module: %s" % (self.context_module.name, )
        if self.cont:
            result += "\n" + self.cont.stacktrace()
        return result


