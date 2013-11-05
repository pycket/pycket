class W_Object:
    def call(self, args, env, frame):
        raise Exception ("not callable")

class W_Cons(W_Object):
    def __init__(self, a, d):
        self.car = a
        self.cdr = d

class W_Fixnum(W_Object):
    def __init__(self, val):
        self.value = val

class W_Void (W_Object):
    def __init__(self): pass

w_void = W_Void()

class W_Bool(W_Object):
    @staticmethod
    def make(b):
        if b: return w_true
        else: return w_false
    def __init__(self, val):
        self.value = val

w_false = W_Bool(False)
w_true = W_Bool(True)

class W_String(W_Object):
    def __init__(self, val):
        self.value = val

class W_Symbol(W_Object):
    all_symbols = {}
    @staticmethod
    def make(string):
        if string in W_Symbol.all_symbols:
            return W_Symbol.all_symbols[string]
        else:
            W_Symbol.all_symbols[string] = w_result = W_Symbol(string)
            return w_result
    def __repr__(self):
        return self.value
    def __init__(self, val):
        self.value = val

class W_Prim (W_Object):
    def __init__ (self, name, code):
        self.name = name
        self.code = code
    
    def call(self, args, env, frame):
        from pycket.interpreter import Value
        return Value(self.code(args)), env, frame

class W_Closure (W_Object):
    def __init__ (self, lam, env):
        self.lam = lam
        self.env = env
    def call(self, args, env, frame):
        from pycket.interpreter import make_begin, ConsEnv        
        if len (self.lam.formals) != len(args):
            raise Exception("wrong number of arguments, expected %s but got %s"%(len (self.lam.formals), len(args)))
        return make_begin(self.lam.body, ConsEnv(self.lam.formals, args, self.env), frame)

        
