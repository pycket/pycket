class Cont(object):
    def tostring(self):
        "NOT_RPYTHON"
        if self.prev:
            return "%s(%s)"%(self.__class__.__name__,self.prev.tostring())
        else:
            return "%s()"%(self.__class__.__name__)


