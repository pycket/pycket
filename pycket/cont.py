from pycket.values import Object

class BasicCont(Object):
    def cont_name(self):
        return "Cont"
    def tostring(self):
        return "%s()"%(self.cont_name())

class Cont(BasicCont):
    _immutable_fields_ = ["prev", "env"]
    def tostring(self):
        if self.prev:
            return "%s(%s)"%(self.cont_name(),self.prev.tostring())
        else:
            return "%s()"%(self.cont_name())
