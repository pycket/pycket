from pycket.error          import SchemeException
from rpython.tool.pairtype import extendabletype
from rpython.rlib          import jit, objectmodel

class UnhashableType(Exception):
    pass

class W_ProtoObject(object):
    """ abstract base class of both actual values (W_Objects) and multiple
    return values (Values)"""
    _attrs_ = []
    _settled_ = True

    def as_real_value(self):
        raise NotImplementedError("not a real value!")

    def num_values(val):
        raise NotImplementedError("not a real value!")

    def get_value(val, index):
        raise NotImplementedError("not a real value!")

    def get_all_values(self):
        raise NotImplementedError("not a real value!")

    def get_type(self):
        raise NotImplementedError("not a real value!")

class W_Object(W_ProtoObject):
    __metaclass__ = extendabletype
    _attrs_ = []
    errorname = "%%%%unreachable%%%%"

    def __init__(self):
        raise NotImplementedError("abstract base class")

    def num_values(self):
        return 1

    def get_value(self, index):
        assert index == 0
        return self

    def get_type(self):
        return 'w_object'

    def get_all_values(self):
        return [self]

    def iscallable(self):
        return False

    def call(self, args, env, cont):
        raise SchemeException("%s is not callable\n  args were: %s" % (self.tostring(),
                                                                       [s.tostring() for s in args]))

    def call_with_extra_info(self, args, env, cont, calling_app):
        return self.call(args, env, cont)

    def call_interpret(self, racket_vals):
        from pycket.interpreter import Done, interpret_one
        from pycket.env import ToplevelEnv, w_global_config
        from pycket.cont import NilCont, Prompt
        from pycket import values, values_parameter
        from pycket.prims.control import default_uncaught_exception_handler

        __pycketconfig = w_global_config.get_pycketconfig()

        t_env = ToplevelEnv(__pycketconfig)

        cont = NilCont()
        cont = Prompt(values.w_default_continuation_prompt_tag, None, t_env, cont)
        cont.update_cm(values.parameterization_key, values_parameter.top_level_config)
        cont.update_cm(values.exn_handler_key, default_uncaught_exception_handler)

        try:
            ast, env, cont = self.call_with_extra_info(racket_vals, t_env, cont, None)
            return interpret_one(ast, env, cont)
        except Done, e:
            return e.values
        except SchemeException, e:
            raise e


    def enable_jitting(self):
        pass # need to override in callables that are based on an AST

    # an arity is a pair of a list of numbers and either -1 or a non-negative integer
    def get_arity(self, promote=False):
        if self.iscallable():
            from pycket.arity import Arity
            return Arity.unknown
        else:
            raise SchemeException("%s does not have arity" % self.tostring())

    def get_result_arity(self):
        if self.iscallable():
            return None
        else:
            raise SchemeException("%s does not have result arity" % self.tostring())

    # Interface for structs

    def ref_with_extra_info(self, field, app, env, cont):
        raise SchemeException("%s is not a struct" % self.tostring())

    def set_with_extra_info(self, field, val, app, env, cont):
        raise SchemeException("%s is not a struct" % self.tostring())

    def struct_type(self):
        return None

    def get_prop(self, property, env, cont):
        raise SchemeException("%s is not a struct" % self.tostring())

    # Interface for proxies

    def is_impersonator(self):
        return self.is_chaperone()
    def is_chaperone(self):
        return False
    def is_proxy(self):
        return self.is_chaperone() or self.is_impersonator()
    def get_proxied(self):
        return self
    def get_base(self):
        return self
    def get_properties(self):
        return {}
    def is_non_interposing_chaperone(self):
        return False
    def replace_proxied(self, other):
        raise ValueError("Not a proxy")

    def is_proper_list(self, seen=[]):
        return False

    def immutable(self):
        return False

    def equal(self, other):
        return self is other # default implementation

    def eqv(self, other):
        return self is other # default implementation

    def hash_equal(self, info=None):
        return 10

    def hash_eqv(self):
        # default to hash_eq
        return objectmodel.compute_hash(self)

    def tostring(self):
        return str(self)

    def to_sexp(self, partial_env={}, env=None):
        return self

    # for expose
    @classmethod
    def make_unwrapper(cls, unbox=False):
        if cls is W_Object:
            return lambda x: x, ''
        def unwrap(w_object):
            if isinstance(w_object, cls):
                return w_object
            return None
        return unwrap, cls.errorname

class SingletonMeta(type):
    def __new__(cls, name, bases, dct):
        result = type.__new__(cls, name, bases, dct)
        result.singleton = result()
        return result

class SingleResultMixin(object):
    def get_result_arity(self):
        from pycket.arity import Arity
        return Arity.ONE
