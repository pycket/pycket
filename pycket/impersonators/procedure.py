
from pycket                    import values
from pycket.base               import SingletonMeta
from pycket.cont               import call_extra_cont, continuation, label
from pycket.hidden_classes     import make_map_type
from pycket.impersonators      import (
    ChaperoneMixin,
    ImpersonatorMixin,
    ProxyMixin,
    W_ImpPropertyDescriptor,
    check_chaperone_results_loop,
    get_base_object,
    make_property_map
)
from pycket.small_list         import inline_small_list
from rpython.rlib.objectmodel  import import_from_mixin, specialize

EMPTY_PROPERTY_MAP = make_map_type("get_storage_index").EMPTY

@specialize.arg(0)
def make_interpose_procedure(cls, code, check, prop_keys, prop_vals):
    empty = EMPTY_PROPERTY_MAP
    map = make_property_map(prop_keys, EMPTY_PROPERTY_MAP)
    fixed = prop_vals[:] if prop_vals is not None else None
    return cls.make(fixed, code, check, map)

class W_InterposeProcedure(values.W_Procedure):
    errorname = "interpose-procedure"
    _immutable_fields_ = ["inner", "base", "check", "property_map"]

    def __init__(self, code, check, map):
        assert code.iscallable()
        assert check is values.w_false or check.iscallable()
        self.inner = code
        self.check = check
        self.property_map = map
        if isinstance(code, W_InterposeProcedure):
            self.base = code.base
        else:
            self.base = code

    def get_storage_index(self, idx):
        return self._get_list(idx)

    def get_proxied(self):
        return self.inner

    def get_base(self):
        return self.base

    def is_proxied(self):
        return True

    def get_property(self, prop, default=None):
        return self.property_map.lookup(prop, self, default=None)

    def get_arity(self):
        return self.get_base().get_arity()

    def post_call_cont(self, args, prop, env, cont, calling_app):
        raise NotImplementedError("abstract method")

    def immutable(self):
        return True

    def tostring(self):
        return get_base_object(self.base).tostring()

    @staticmethod
    def has_self_arg():
        return False

    @label
    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

    def is_non_interposing_chaperone(self):
        return self.check is values.w_false

    def call_with_extra_info(self, args, env, cont, calling_app):
        from pycket.values import W_ThunkProcCMK
        from pycket.impersonators.base import w_impersonator_prop_application_mark
        if self.check is values.w_false:
            return self.inner.call_with_extra_info(args, env, cont, calling_app)
        prop = self.get_property(w_impersonator_prop_application_mark)
        after = self.post_call_cont(args, prop, env, cont, calling_app)
        # if isinstance(prop, values.W_Cons):
            # key, val = prop.car(), prop.cdr()
            # if isinstance(key, values.W_ContinuationMarkKey):
                # body = W_ThunkProcCMK(self.check, args)
                # return key.set_cmk(body, val, cont, env, after)
            # cont.update_cm(key, val)
        if self.has_self_arg():
            args = [self] + args
        return self.check.call_with_extra_info(args, env, after, calling_app)

@inline_small_list(immutable=True, unbox_num=True)
class W_ImpProcedure(W_InterposeProcedure):
    import_from_mixin(ImpersonatorMixin)

    errorname = "imp-procedure"

    def post_call_cont(self, args, prop, env, cont, calling_app):
        return imp_proc_cont(len(args), self.inner, prop, calling_app, env, cont)

@inline_small_list(immutable=True, unbox_num=True)
class W_ImpProcedureStar(W_InterposeProcedure):
    import_from_mixin(ImpersonatorMixin)

    errorname = "imp-procedure"

    def post_call_cont(self, args, prop, env, cont, calling_app):
        return imp_proc_cont(len(args), self.inner, prop, calling_app, env, cont)

    @staticmethod
    def has_self_arg():
        return True

@inline_small_list(immutable=True, unbox_num=True)
class W_ChpProcedure(W_InterposeProcedure):
    import_from_mixin(ChaperoneMixin)

    errorname = "chp-procedure"

    def post_call_cont(self, args, prop, env, cont, calling_app):
        return chp_proc_cont(args, self.inner, prop, calling_app, env, cont)

@inline_small_list(immutable=True, unbox_num=True)
class W_ChpProcedureStar(W_InterposeProcedure):
    import_from_mixin(ChaperoneMixin)

    errorname = "chp-procedure*"

    def post_call_cont(self, args, prop, env, cont, calling_app):
        return chp_proc_cont(args, self.inner, prop, calling_app, env, cont)

    @staticmethod
    def has_self_arg():
        return True

# Continuation used when calling an impersonator of a procedure.
@continuation
def imp_proc_cont(arg_count, proc, prop, calling_app, env, cont, _vals):
    vals = _vals.get_all_values()
    if len(vals) == arg_count + 1:
        vals, check = vals[1:], vals[0]
        cont = call_extra_cont(check, calling_app, env, cont)
    else:
        assert len(vals) == arg_count
    if isinstance(prop, values.W_Cons):
        # XXX Handle the case where |key| is a proxied continuation mark key
        key, val = prop.car(), prop.cdr()
        cont.update_cm(key, val)
    return proc.call_with_extra_info(vals, env, cont, calling_app)

# Continuation used when calling an impersonator of a procedure.
# Have to examine the results before checking
@continuation
def chp_proc_cont(orig, proc, prop, calling_app, env, cont, _vals):
    vals = _vals.get_all_values()
    arg_count = len(orig)
    check_result = len(vals) == arg_count + 1
    if check_result:
        check = vals[0]
        caller = call_extra_cont(check, calling_app, env, cont)
        cont = call_extra_cont(proc, calling_app, env, caller)
    else:
        assert len(vals) == arg_count
        caller = cont

    if isinstance(prop, values.W_Cons):
        # XXX Handle the case where |key| is a proxied continuation mark key
        key, val = prop.car(), prop.cdr()
        caller.update_cm(key, val)

    if check_result:
        args = values.Values.make(vals[1:])
        original = values.Values.make(orig)
        return check_chaperone_results_loop(args, original, 0, env, cont)
    return proc.call_with_extra_info(vals, env, cont, calling_app)

