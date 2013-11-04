class W_Object:
    def call(self, args):
        raise Exception ("not callable")


class W_Cons(W_Object):
    def __init__(self, a, d):
        self.car = a
        self.cdr = d

class W_Fixnum(W_Object):
    def __init__(self, val):
        self.value = val

class W_String(W_Object):
    def __init__(self, val):
        self.value = val

class W_Symbol(W_Object):
    def __init__(self, val):
        self.value = val

class W_Prim (W_Object):
    def __init__ (self, name, code):
        self.name = name
        self.code = code
    
    def call(self, args, frame):
        from pycket.interpreter import Value
        return Value(self.code(args)), frame
