
from pycket                    import values
from pycket.base               import SingletonMeta
from pycket.cont               import call_extra_cont, continuation, label
from pycket.impersonators      import (
    ChaperoneMixin,
    ImpersonatorMixin,
    ProxyMixin,
    W_ImpPropertyDescriptor,
    check_chaperone_results_loop
)
from rpython.rlib.objectmodel  import import_from_mixin

class W_InterposeProcedure(values.W_Procedure):
    import_from_mixin(ProxyMixin)

    errorname = "interpose-procedure"
    _immutable_fields_ = ["inner", "check", "properties", "self_arg"]
    def __init__(self, code, check, prop_keys, prop_vals, self_arg=False):
        assert code.iscallable()
        assert check is values.w_false or check.iscallable()
        assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)

        self.inner = code
        self.check = check
        self.self_arg = self_arg
        self.properties = {}
        if prop_keys is not None:
            for i, k in enumerate(prop_keys):
                assert isinstance(k, W_ImpPropertyDescriptor)
                self.properties[k] = prop_vals[i]

    def get_arity(self):
        return self.inner.get_arity()

    def post_call_cont(self, args, env, cont, calling_app):
        raise NotImplementedError("abstract method")

    @label
    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

    def is_non_interposing_chaperone(self):
        return self.check is values.w_false

    def call_with_extra_info(self, args, env, cont, calling_app):
        from pycket.values import W_ThunkProcCMK
        from pycket.impersonators.impersonators import w_impersonator_prop_application_mark
        if self.check is values.w_false:
            return self.inner.call_with_extra_info(args, env, cont, calling_app)
        after = self.post_call_cont(args, env, cont, calling_app)
        prop = self.properties.get(w_impersonator_prop_application_mark, None)
        if isinstance(prop, values.W_Cons):
            key, val = prop.car(), prop.cdr()
            if isinstance(key, values.W_ContinuationMarkKey):
                body = W_ThunkProcCMK(self.check, args)
                return key.set_cmk(body, val, cont, env, after)
            cont.update_cm(key, val)
        if self.self_arg:
            args = [self] + args
        return self.check.call_with_extra_info(args, env, after, calling_app)

class W_ImpProcedure(W_InterposeProcedure):
    import_from_mixin(ImpersonatorMixin)

    errorname = "imp-procedure"

    def post_call_cont(self, args, env, cont, calling_app):
        return imp_proc_cont(len(args), self.inner, calling_app, env, cont)

class W_ChpProcedure(W_InterposeProcedure):
    import_from_mixin(ChaperoneMixin)

    errorname = "chp-procedure"

    def post_call_cont(self, args, env, cont, calling_app):
        return chp_proc_cont(args, self.inner, calling_app, env, cont)


# Continuation used when calling an impersonator of a procedure.
@continuation
def imp_proc_cont(arg_count, proc, calling_app, env, cont, _vals):
    vals = _vals.get_all_values()
    if len(vals) == arg_count:
        return proc.call_with_extra_info(vals, env, cont, calling_app)
    if len(vals) == arg_count + 1:
        args, check = vals[1:], vals[0]
        return proc.call_with_extra_info(args, env,
                call_extra_cont(check, calling_app, env, cont), calling_app)
    assert False

# Continuation used when calling an impersonator of a procedure.
# Have to examine the results before checking
@continuation
def chp_proc_cont(orig, proc, calling_app, env, cont, _vals):
    vals = _vals.get_all_values()
    arg_count = len(orig)
    if len(vals) == arg_count:
        return proc.call_with_extra_info(vals, env, cont, calling_app)
    if len(vals) == arg_count + 1:
        args, check = values.Values.make(vals[1:]), vals[0]
        return check_chaperone_results_loop(args, orig, 0, env,
                call_extra_cont(proc, calling_app, env,
                    call_extra_cont(check, calling_app, env, cont)))
    assert False

