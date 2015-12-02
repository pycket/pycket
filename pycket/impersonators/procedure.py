
from pycket                    import values
from pycket.base               import SingletonMeta
from pycket.cont               import call_extra_cont, continuation, label
from pycket.hidden_classes     import make_caching_map_type
from pycket.impersonators      import (
    ChaperoneMixin,
    EMPTY_PROPERTY_MAP,
    ImpersonatorMixin,
    InlineProxyMixin,
    W_ImpPropertyDescriptor,
    check_chaperone_results,
    check_chaperone_results_loop,
    get_base_object,
    make_specialized_property_map
)
from pycket.small_list         import inline_small_list
from rpython.rlib              import jit
from rpython.rlib.objectmodel  import import_from_mixin, specialize


@specialize.arg(0)
def make_interpose_procedure(cls, code, check, prop_keys, prop_vals):
    empty = EMPTY_PROPERTY_MAP
    map = make_specialized_property_map(prop_keys, EMPTY_PROPERTY_MAP)
    fixed = prop_vals[:] if prop_vals is not None else None
    return cls.make(fixed, code, check, map)

class W_InterposeProcedure(values.W_Procedure):
    errorname = "interpose-procedure"
    _immutable_fields_ = ["check"]

    import_from_mixin(InlineProxyMixin)

    def __init__(self, code, check, map):
        assert code.iscallable()
        assert check is values.w_false or check.iscallable()
        self.check = check
        self.init_proxy(code, map)

    def get_arity(self):
        return self.get_base().get_arity()

    def post_call_cont(self, args, prop, env, cont, calling_app):
        raise NotImplementedError("abstract method")

    def immutable(self):
        return True

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
        orig = values.Values.make(args)
        return chp_proc_cont(orig, self.inner, prop, calling_app, env, cont)

@inline_small_list(immutable=True, unbox_num=True)
class W_ChpProcedureStar(W_InterposeProcedure):
    import_from_mixin(ChaperoneMixin)

    errorname = "chp-procedure*"

    def post_call_cont(self, args, prop, env, cont, calling_app):
        orig = values.Values.make(args)
        return chp_proc_cont(orig, self.inner, prop, calling_app, env, cont)

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

@continuation
def chp_proc_post_proc_cont(check, calling_app, env, cont, _vals):
    vals = _vals.get_all_values()
    cont = check_chaperone_results(_vals, env, cont)
    return check.call_with_extra_info(vals, env, cont, calling_app)

# Continuation used when calling an impersonator of a procedure.
# Have to examine the results before checking
@continuation
def chp_proc_cont(orig, proc, prop, calling_app, env, cont, _vals):
    vals = _vals.get_all_values()
    arg_count = orig.num_values()
    check_result = len(vals) == arg_count + 1
    if check_result:
        check  = vals[0]
        calling_frame = chp_proc_post_proc_cont(check, calling_app, env, cont)
        cont          = call_extra_cont(proc, calling_app, env, calling_frame)
    else:
        assert len(vals) == arg_count
        calling_frame = cont
        cont          = call_extra_cont(proc, calling_app, env, cont)

    if isinstance(prop, values.W_Cons):
        # XXX Handle the case where |key| is a proxied continuation mark key
        key, val = prop.car(), prop.cdr()
        # if isinstance(key, values.W_ContinuationMarkKey):
            # body = W_ThunkProcCMK(proc, vals)
            # return key.set_cmk(body, val, calling_frame, env, cont)
        calling_frame.update_cm(key, val)
        cont.marks = calling_frame.marks

    if check_result:
        args = values.Values.make(vals[1:])
        return check_chaperone_results_loop(args, orig, 0, env, cont)
    return check_chaperone_results_loop(_vals, orig, 0, env, cont)

