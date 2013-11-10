from rpython.rlib.objectmodel import we_are_translated

class SchemeException(Exception):
    def __init__(self, msg):
        if not we_are_translated():
            Exception.__init__(self, msg)
        self.msg = msg


