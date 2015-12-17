
from pycket import values


class W_CType(values.W_Object):

    errorname = "ctype"
    _attrs_ = []

    def __init__(self):
        raise NotImplementedError("abstract base class")

    def sizeof(self):
        raise NotImplementedError("abstract base class")


class W_PrimitiveCType(W_CType):

    _immutable_fields_ = ["name", "size"]

    def __init__(self, name, size):
        self.name = name
        self.size = size

    def sizeof(self):
        return self.size

    def tostring(self):
        return self.name

class W_DerivedCType(W_CType):

    _immutable_fields_ = ["ctype", "racket_to_c", "c_to_racket"]

    def __init__(self, ctype, racket_to_c, c_to_racket):
        assert isinstance(ctype, W_CType)
        self.ctype       = ctype
        self.racket_to_c = racket_to_c
        self.c_to_racket = c_to_racket

    def sizeof(self):
        return self.ctype.sizeof()

    def tostring(self):
        if self.racket_to_c is values.w_false and self.c_to_racket is values.w_false:
            return self.ctype.tostring()
        return "#<ctype>"


