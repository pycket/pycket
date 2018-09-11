#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont              import continuation, label, guarded_loop, call_cont, call_extra_cont
from pycket.prims.expose      import make_call_method
from pycket.error             import SchemeException
from pycket.values            import UNROLLING_CUTOFF
from pycket                   import values
from pycket                   import values_struct
from pycket.hash.base         import W_HashTable
from rpython.rlib             import jit
from rpython.rlib.objectmodel import import_from_mixin, specialize

@jit.unroll_safe
def get_base_object(x):
    if isinstance(x, W_InterposeStructBase):
        return x.base
    while x.is_proxy():
        x = x.get_proxied()
    return x

class ProxyMixin(object):
    def get_proxied(self):
        return self.inner

    def is_proxy(self):
        return True

    def get_properties(self):
        return self.properties

    def get_property(self, prop, default=None):
        if self.properties is None:
            return default
        return self.properties.get(prop, default)

    def immutable(self):
        return get_base_object(self.inner).immutable()

    def tostring(self):
        return get_base_object(self.inner).tostring()

class ChaperoneMixin(object):
    def is_chaperone(self):
        return True

class ImpersonatorMixin(object):
    def is_impersonator(self):
        return True

def traceable_proxy(self, field, *args):
    if jit.we_are_jitted():
        return True
    goto = self.mask[field]
    return goto is not self.base and self.inner is not self.base

# Check if a proxied struct has more than n levels to descend through
def enter_above_depth(n):
    @jit.unroll_safe
    def above_threshold(self, field, *args):
        if jit.we_are_jitted():
            return True
        for _ in range(n):
            if not isinstance(self, W_InterposeStructBase):
                return False
            goto = self.mask[field]
            if goto is self:
                self = self.inner
        return True
    return above_threshold

@jit.unroll_safe
def lookup_property(obj, prop):
    while obj.is_proxy():
        val = obj.get_property(prop, None)
        if val is not None:
            return val
        obj = obj.get_proxied()
    return None

@continuation
def check_chaperone_results(args, env, cont, vals):
    # We are allowed to receive more values than arguments to compare them to.
    # Additional ones are ignored for this checking routine.
    assert vals.num_values() >= args.num_values()
    return check_chaperone_results_loop(vals, args, 0, env, cont)

@jit.unroll_safe
def check_chaperone_results_loop(vals, args, idx, env, cont):
    from pycket.interpreter import return_multi_vals
    from pycket.prims.equal import equal_func_unroll_n, EqualInfo
    num_vals = args.num_values()
    while idx < num_vals and vals.get_value(idx) is None and args.get_value(idx) is None:
        idx += 1
    if idx >= num_vals:
        return return_multi_vals(vals, env, cont)
    info = EqualInfo.CHAPERONE_SINGLETON
    # XXX it would be best to store the parameter on the toplevel env and make
    # it changeable via a cmdline parameter to pycket-c
    unroll_n_times = 2 # XXX needs tuning
    return equal_func_unroll_n(vals.get_value(idx), args.get_value(idx), info, env,
            catch_equal_cont(vals, args, idx, env, cont), unroll_n_times)

@continuation
def catch_equal_cont(vals, args, idx, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    if val is values.w_false:
        raise SchemeException("Expecting original value or chaperone")
    return check_chaperone_results_loop(vals, args, idx + 1, env, cont)

@continuation
@jit.unroll_safe
def chaperone_reference_cont(f, args, app, env, cont, _vals):
    args_size = args.num_values()
    vals_size = _vals.num_values()
    all_args = [None] * (args_size + vals_size)
    idx = 0
    for i in range(args_size):
        all_args[idx] = args.get_value(i)
        idx += 1
    for i in range(vals_size):
        all_args[idx] = _vals.get_value(i)
        idx += 1
    return f.call_with_extra_info(all_args, env, check_chaperone_results(_vals, env, cont), app)

@continuation
def impersonate_reference_cont(f, args, app, env, cont, _vals):
    old = _vals.get_all_values()
    return f.call_with_extra_info(args + old, env, cont, app)

# Procedures

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
        if isinstance(key, values.W_ContinuationMarkKey):
            body = values.W_ThunkProcCMK(proc, vals)
            return key.set_cmk(body, val, cont, env, cont)
        cont.update_cm(key, val)
    return proc.call_with_extra_info(vals, env, cont, calling_app)

# Continuation used when calling an impersonator of a procedure.
# Have to examine the results before checking
@continuation
def chp_proc_cont(orig, proc, prop, calling_app, env, cont, _vals):
    vals = _vals.get_all_values()
    arg_count = orig.num_values()
    check_result = len(vals) == arg_count + 1

    # Push the appropriate continuation frames for performing result checking.
    # We need to keep track of the frame in which the wrapped procedure executes
    # in to install the appropriate continuation marks if
    # impersonator-prop:application-mark was attached to the chaperone.
    if check_result:
        check, vals   = vals[0], vals[1:]
        calling_frame = chp_proc_post_proc_cont(check, calling_app, env, cont)
        cont          = call_extra_cont(proc, calling_app, env, calling_frame)
    else:
        assert len(vals) == arg_count
        calling_frame = cont
        cont          = call_extra_cont(proc, calling_app, env, cont)

    if isinstance(prop, values.W_Cons):
        # XXX Handle the case where |key| is a proxied continuation mark key
        key, val = prop.car(), prop.cdr()
        if isinstance(key, values.W_ContinuationMarkKey):
            cont = chp_proc_do_set_cmk_cont(proc, key, val, calling_frame, env, cont)
        else:
            calling_frame.update_cm(key, val)
            cont.marks = calling_frame.marks

    if check_result:
        args = values.Values.make(vals)
    else:
        args = _vals

    return check_chaperone_results_loop(args, orig, 0, env, cont)

@continuation
def chp_proc_post_proc_cont(check, calling_app, env, cont, _vals):
    vals = _vals.get_all_values()
    cont = check_chaperone_results(_vals, env, cont)
    return check.call_with_extra_info(vals, env, cont, calling_app)

@continuation
def chp_proc_do_set_cmk_cont(proc, key, val, calling_frame, env, cont, _vals):
    vals = _vals.get_all_values()
    body = values.W_ThunkProcCMK(proc, vals)
    return key.set_cmk(body, val, calling_frame, env, cont)


@specialize.arg(0)
def make_interpose_procedure(cls, proc, check, keys, vals):
    assert not keys and not vals or len(keys) == len(vals)
    properties = {}
    if keys is not None:
        for i, k in enumerate(keys):
            assert isinstance(k, W_ImpPropertyDescriptor)
            properties[k] = vals[i]
    return cls(proc, check, properties)

class W_InterposeProcedure(values.W_Procedure):
    import_from_mixin(ProxyMixin)

    errorname = "interpose-procedure"
    _immutable_fields_ = ["inner", "check", "properties"]

    @jit.unroll_safe
    def __init__(self, code, check, props):
        assert code.iscallable()
        assert check is values.w_false or check.iscallable()
        self.inner = code
        self.check = check
        self.properties = props

    @staticmethod
    def has_self_arg():
        return False

    def get_arity(self, promote=False):
        return self.inner.get_arity(promote)

    def post_call_cont(self, args, prop, env, cont, calling_app):
        raise NotImplementedError("abstract method")

    def safe_proxy(self):
        return True

    @label
    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

    def is_non_interposing_chaperone(self):
        return self.check is values.w_false

    def call_with_extra_info(self, args, env, cont, calling_app):
        if self.check is values.w_false:
            return self.inner.call_with_extra_info(args, env, cont, calling_app)
        if not self.safe_proxy():
            return self.check.call_with_extra_info(args, env, cont, calling_app)
        prop = self.get_property(w_impersonator_prop_application_mark)
        after = self.post_call_cont(args, prop, env, cont, calling_app)
        if self.has_self_arg():
            args = [self] + args
        return self.check.call_with_extra_info(args, env, after, calling_app)

    # XXX Tricksy bits ahead. Since structs can act like procedures, a struct
    # may be proxied by a procedure proxy, thus it supports struct type,
    # ref, set, and struct property access.
    def ref_with_extra_info(self, field, app, env, cont):
        return self.inner.ref_with_extra_info(field, app, env, cont)

    def set_with_extra_info(self, field, val, app, env, cont):
        return self.inner.set_with_extra_info(field, val, app, env, cont)

    def struct_type(self):
        return self.inner.struct_type()

    def get_prop(self, property, env, cont):
        return self.inner.get_prop(property, env, cont)

class W_ImpProcedure(W_InterposeProcedure):
    import_from_mixin(ImpersonatorMixin)

    errorname = "imp-procedure"

    def post_call_cont(self, args, prop, env, cont, calling_app):
        return imp_proc_cont(len(args), self.inner, prop, calling_app, env, cont)

class W_ChpProcedure(W_InterposeProcedure):
    import_from_mixin(ChaperoneMixin)

    errorname = "chp-procedure"

    def post_call_cont(self, args, prop, env, cont, calling_app):
        orig = values.Values.make(args)
        return chp_proc_cont(orig, self.inner, prop, calling_app, env, cont)

class W_ImpProcedureStar(W_ImpProcedure):
    @staticmethod
    def has_self_arg():
        return True

class W_ChpProcedureStar(W_ChpProcedure):
    @staticmethod
    def has_self_arg():
        return True

class W_UnsafeImpProcedure(W_ImpProcedure):
    def safe_proxy(self):
        return False

class W_UnsafeChpProcedure(W_ChpProcedure):
    def safe_proxy(self):
        return False

class W_InterposeBox(values.W_Box):
    import_from_mixin(ProxyMixin)

    errorname = "interpose-box"
    _immutable_fields_ = ["inner", "unboxh", "seth", "properties"]

    @jit.unroll_safe
    def __init__(self, box, unboxh, seth, prop_keys, prop_vals):
        assert isinstance(box, values.W_Box)
        assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)
        self.inner = box
        self.unboxh = unboxh
        self.seth = seth
        self.properties = {}
        if prop_keys is not None:
            for i, k in enumerate(prop_keys):
                assert isinstance(k, W_ImpPropertyDescriptor)
                self.properties[k] = prop_vals[i]

    def immutable(self):
        return get_base_object(self.inner).immutable()

    def post_unbox_cont(self, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_box_cont(self, val, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def unbox(self, env, cont):
        after = self.post_unbox_cont(env, cont)
        return self.inner.unbox(env, after)

    @label
    def set_box(self, val, env, cont):
        after = self.post_set_box_cont(val, env, cont)
        return self.seth.call([self.inner, val], env, after)

class W_ChpBox(W_InterposeBox):
    import_from_mixin(ChaperoneMixin)

    def post_unbox_cont(self, env, cont):
        arg = values.Values.make1(self.inner)
        return chaperone_reference_cont(self.unboxh, arg, None, env, cont)

    def post_set_box_cont(self, val, env, cont):
        val = values.Values.make1(val)
        return check_chaperone_results(val, env,
                imp_box_set_cont(self.inner, env, cont))

    def immutable(self):
        return self.inner.immutable()

@continuation
def imp_box_set_cont(b, env, cont, vals):
    from pycket.interpreter import check_one_val
    return b.set_box(check_one_val(vals), env, cont)

class W_ImpBox(W_InterposeBox):
    import_from_mixin(ImpersonatorMixin)

    errorname = "imp-box"

    def post_unbox_cont(self, env, cont):
        return impersonate_reference_cont(self.unboxh, [self.inner], None, env, cont)

    def post_set_box_cont(self, val, env, cont):
        return imp_box_set_cont(self.inner, env, cont)

@continuation
def imp_vec_set_cont(v, i, app, env, cont, vals):
    from pycket.interpreter import check_one_val
    return v.vector_set(i.value, check_one_val(vals), env, cont, app=app)

class W_InterposeVector(values.W_MVector):
    import_from_mixin(ProxyMixin)

    errorname = "interpose-vector"
    _immutable_fields_ = ["inner", "refh", "seth", "properties"]

    @jit.unroll_safe
    def __init__(self, v, r, s, prop_keys, prop_vals):
        assert isinstance(v, values.W_MVector)
        assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)
        self.inner = v
        self.refh = r
        self.seth = s
        self.properties = {}
        if prop_keys is not None:
            for i, k in enumerate(prop_keys):
                assert isinstance(k, W_ImpPropertyDescriptor)
                self.properties[k] = prop_vals[i]

    def length(self):
        return get_base_object(self.inner).length()

    def post_set_cont(self, new, i, env, cont, app=None):
        raise NotImplementedError("abstract method")

    def post_ref_cont(self, i, env, cont, app=None):
        raise NotImplementedError("abstract method")

    def vector_set(self, i, new, env, cont, app=None):
        idx = values.W_Fixnum(i)
        after = self.post_set_cont(new, idx, env, cont, app=app)
        return self.seth.call([self.inner, idx, new], env, after)

    @jit.unroll_safe
    def vector_ref(self, i, env, cont, app=None):
        idx = values.W_Fixnum(i)
        while isinstance(self, W_InterposeVector):
            cont = self.post_ref_cont(idx, env, cont, app=app)
            self = self.inner
        return self.vector_ref(i, env, cont, app=app)

@specialize.arg(0)
def make_interpose_vector(cls, vector, refh, seth, prop_keys, prop_vals):
    return cls(vector, refh, seth, prop_keys, prop_vals)

# Vectors
class W_ImpVector(W_InterposeVector):
    import_from_mixin(ImpersonatorMixin)

    errorname = "impersonate-vector"

    def post_set_cont(self, new, i, env, cont, app=None):
        return imp_vec_set_cont(self.inner, i, app, env, cont)

    def post_ref_cont(self, i, env, cont, app=None):
        return impersonate_reference_cont(self.refh, [self.inner, i], app, env, cont)

class W_ChpVector(W_InterposeVector):
    import_from_mixin(ChaperoneMixin)

    errorname = "chaperone-vector"

    def post_set_cont(self, new, i, env, cont, app=None):
        arg = values.Values.make1(new)
        return check_chaperone_results(arg, env,
                imp_vec_set_cont(self.inner, i, app, env, cont))

    def post_ref_cont(self, i, env, cont, app=None):
        args = values.Values.make2(self.inner, i)
        return chaperone_reference_cont(self.refh, args, app, env, cont)

# Are we dealing with a struct accessor/mutator/propert accessor or a
# chaperone/impersonator thereof.
def valid_struct_proc(x):
    v = get_base_object(x)
    return (isinstance(v, values_struct.W_StructFieldAccessor) or
            isinstance(v, values_struct.W_StructFieldMutator) or
            isinstance(v, values_struct.W_StructPropertyAccessor))

@continuation
def imp_struct_set_cont(orig_struct, setter, field, app, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    if setter is values.w_false:
        return orig_struct.set_with_extra_info(field, val, app, env, cont)
    return setter.call_with_extra_info([orig_struct, val], env, cont, app)

@specialize.arg(0)
def make_struct_proxy(cls, inner, overrides, handlers, keys, vals):
    return cls(inner, overrides, handlers, keys, vals)

# Representation of a struct that allows interposition of operations
# onto accessors/mutators
class W_InterposeStructBase(values_struct.W_RootStruct):
    import_from_mixin(ProxyMixin)

    _immutable_fields_ = ["inner", "base", "mask[*]", "accessors[*]", "mutators[*]", "struct_info_handler", "struct_props", "properties"]

    @jit.unroll_safe
    def __init__(self, inner, overrides, handlers, prop_keys, prop_vals):
        from pycket.prims.struct_structinfo import struct_info
        assert isinstance(inner, values_struct.W_RootStruct)
        assert len(overrides) == len(handlers)
        assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)

        self.inner = inner

        field_cnt = inner.struct_type().total_field_cnt
        accessors = [values.w_false] * (field_cnt * 2)
        mutators  = [values.w_false] * (field_cnt * 2)

        # The mask field contains an array of pointers to the next object
        # in the proxy stack that overrides a given field operation.
        # In some cases, this allows us to jump straight to the base struct,
        # but in general may allow us to skip many levels of ref operations at
        # once.
        if isinstance(inner, W_InterposeStructBase):
            self.base = inner.base
            mask      = inner.mask[:]
        else:
            self.base = inner
            mask      = [inner] * field_cnt

        assert isinstance(self.base, values_struct.W_Struct)

        struct_props = None
        properties   = None

        # Does not deal with properties as of yet
        # TODO: Avoid allocating the mutators and accessors when they are empty
        st = inner.struct_type()
        for i, op in enumerate(overrides):
            base = get_base_object(op)
            if isinstance(base, values_struct.W_StructFieldAccessor):
                op = values.w_false if type(op) is values_struct.W_StructFieldAccessor else op
                offset = st.get_offset(base.accessor.type)
                index = jit.promote(base.field + offset)
                mask[index] = self
                accessors[2 * index] = op
                accessors[2 * index + 1] = handlers[i]
            elif isinstance(base, values_struct.W_StructFieldMutator):
                jit.promote(base.field)
                offset = st.get_offset(base.mutator.type)
                index = jit.promote(base.field + offset)
                op = values.w_false if type(op) is values_struct.W_StructFieldMutator else op
                mutators[2 * index] = op
                mutators[2 * index + 1] = handlers[i]
            elif isinstance(base, values_struct.W_StructPropertyAccessor):
                if struct_props is None:
                    struct_props = {}
                struct_props[base] = (op, handlers[i])
            elif base is struct_info:
                self.struct_info_handler = handlers[i]
            else:
                assert False
        if prop_keys is not None:
            for i, k in enumerate(prop_keys):
                assert isinstance(k, W_ImpPropertyDescriptor)
                if properties is None:
                    properties = {}
                properties[k] = prop_vals[i]

        self.accessors    = accessors
        self.mutators     = mutators
        self.properties   = properties
        self.struct_props = struct_props
        self.mask         = mask

    def post_ref_cont(self, interp, app, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, field, val, app, env, cont):
        raise NotImplementedError("abstract method")

    @jit.unroll_safe
    def is_non_interposing_chaperone(self):
        acc = self.accessors
        for i in range(1, len(self.accessors), 2):
            if acc[i] is not values.w_false:
                return False
        return bool(self.properties)

    def struct_type(self):
        return get_base_object(self.base).struct_type()

    @guarded_loop(enter_above_depth(5))
    def ref_with_extra_info(self, field, app, env, cont):
        goto = self.mask[field]
        if goto is not self:
            return goto.ref_with_extra_info(field, app, env, cont)
        op = self.accessors[2 * field]
        interp = self.accessors[2 * field + 1]
        if interp is not values.w_false:
            cont = self.post_ref_cont(interp, app, env, cont)
        if op is values.w_false:
            return self.inner.ref_with_extra_info(field, app, env, cont)
        return op.call_with_extra_info([self.inner], env, cont, app)

    @guarded_loop(enter_above_depth(5))
    def set_with_extra_info(self, field, val, app, env, cont):
        op = self.mutators[2 * field]
        interp = self.mutators[2 * field + 1]
        if interp is values.w_false:
            return self.inner.set_with_extra_info(field, val, app, env, cont)
        after = self.post_set_cont(op, field, val, app, env, cont)
        return interp.call_with_extra_info([self, val], env, after, app)

    @label
    def get_prop(self, property, env, cont):
        if self.struct_props is None:
            return self.inner.get_prop(property, env, cont)
        op, interp = self.struct_props.get(property, (None, None))
        if op is None or interp is None:
            return self.inner.get_prop(property, env, cont)
        after = self.post_ref_cont(interp, None, env, cont)
        return op.call([self.inner], env, after)

    def get_struct_info(self, env, cont):
        from pycket.interpreter import return_multi_vals
        if self.struct_info_handler is not None:
            cont = call_cont(self.struct_info_handler, env, cont)
        return self.inner.get_struct_info(env, cont)

    def get_arity(self, promote=False):
        return get_base_object(self.inner).get_arity(promote)

    # FIXME: This is incorrect
    def vals(self):
        return self.inner.vals()

# Need to add checks that we are only impersonating mutable fields
class W_ImpStruct(W_InterposeStructBase):
    import_from_mixin(ImpersonatorMixin)

    def post_ref_cont(self, interp, app, env, cont):
        return impersonate_reference_cont(interp, [self], app, env, cont)

    def post_set_cont(self, op, field, val, app, env, cont):
        return imp_struct_set_cont(self.inner, op, field, app, env, cont)

class W_ChpStruct(W_InterposeStructBase):
    import_from_mixin(ChaperoneMixin)

    def post_ref_cont(self, interp, app, env, cont):
        arg = values.Values.make1(self)
        return chaperone_reference_cont(interp, arg, app, env, cont)

    def post_set_cont(self, op, field, val, app, env, cont):
        arg = values.Values.make1(val)
        return check_chaperone_results(arg, env,
                imp_struct_set_cont(self.inner, op, field, app, env, cont))

class W_InterposeContinuationMarkKey(values.W_ContinuationMarkKey):
    import_from_mixin(ProxyMixin)

    errorname = "interpose-continuation-mark-key"
    _immutable_fields_ = ["inner", "get_proc", "set_proc", "properties"]
    def __init__(self, mark, get_proc, set_proc, prop_keys, prop_vals):
        assert get_proc.iscallable()
        assert set_proc.iscallable()
        assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)
        self.inner    = mark
        self.get_proc = get_proc
        self.set_proc = set_proc
        self.properties = {}
        if prop_vals is not None:
            for i, k in enumerate(prop_keys):
                assert isinstance(k, W_ImpPropertyDescriptor)
                self.properties[k] = prop_vals[i]

    def post_set_cont(self, body, value, env, cont):
        raise NotImplementedError("abstract method")

    def post_get_cont(self, value, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def get_cmk(self, value, env, cont):
        return self.get_proc.call([value], env,
                self.post_get_cont(value, env, cont))

    @label
    def set_cmk(self, body, value, update, env, cont):
        return self.set_proc.call([value], env,
                self.post_set_cont(body, value, env, cont))

@continuation
def imp_cmk_post_set_cont(body, inner, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    return inner.set_cmk(body, val, cont, env, cont)

@continuation
def imp_cmk_post_get_cont(key, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    return key.get_cmk(val, env, cont)

class W_ChpContinuationMarkKey(W_InterposeContinuationMarkKey):
    import_from_mixin(ChaperoneMixin)

    def post_get_cont(self, value, env, cont):
        arg = values.Values.make1(value)
        return check_chaperone_results(arg, env,
                imp_cmk_post_get_cont(self.inner, env, cont))

    def post_set_cont(self, body, value, env, cont):
        arg = values.Values.make1(value)
        return check_chaperone_results(arg, env,
                imp_cmk_post_set_cont(body, self.inner, env, cont))

class W_ImpContinuationMarkKey(W_InterposeContinuationMarkKey):
    import_from_mixin(ImpersonatorMixin)

    def post_get_cont(self, value, env, cont):
        return imp_cmk_post_get_cont(self.inner, env, cont)

    def post_set_cont(self, body, value, env, cont):
        return imp_cmk_post_set_cont(body, self.inner, env, cont)

class W_InterposeHashTable(W_HashTable):
    import_from_mixin(ProxyMixin)

    errorname = "interpose-hash-table"
    _immutable_fields_ = ["inner", "set_proc", "ref_proc", "remove_proc",
                          "key_proc", "clear_proc", "properties"]
    def __init__(self, inner, ref_proc, set_proc, remove_proc, key_proc,
                 clear_proc, prop_keys, prop_vals):
        assert isinstance(inner, W_HashTable)
        assert set_proc.iscallable()
        assert ref_proc.iscallable()
        assert remove_proc.iscallable()
        assert key_proc.iscallable()
        assert clear_proc is values.w_false or clear_proc.iscallable()
        self.inner       = inner
        self.set_proc    = set_proc
        self.ref_proc    = ref_proc
        self.remove_proc = remove_proc
        self.key_proc    = key_proc
        self.clear_proc  = clear_proc
        self.properties  = {}
        if prop_keys is not None:
            for i, k in enumerate(prop_keys):
                assert isinstance(k, W_ImpPropertyDescriptor)
                self.properties[k] = prop_vals[i]

    def post_ref_cont(self, key, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, key, val, env, cont):
        raise NotImplementedError("abstract method")

    def hash_keys(self):
        return get_base_object(self.inner).hash_keys()

    def hash_clear_proc(self, env, cont):
        from pycket.interpreter import return_value
        if self.clear_proc is values.w_false or not self.clear_proc.iscallable():
            return return_value(values.w_void, env, cont)
        else:
            return self.clear_proc.call([self.inner], env, cont)

    @label
    def hash_set(self, key, val, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def hash_ref(self, key, env, cont):
        after = self.post_ref_cont(key, env, cont)
        return self.ref_proc.call([self.inner, key], env, after)

@continuation
def imp_hash_table_ref_cont(ht, old, env, cont, _vals):
    from pycket.interpreter import return_value
    if _vals.num_values() != 2:
        return return_value(None, env, cont)
        raise SchemeException("hash-ref handler produced the wrong number of results")
    key, post = _vals.get_all_values()
    after = imp_hash_table_post_ref_cont(post, ht, old, env, cont)
    return ht.hash_ref(key, env, after)

@continuation
def imp_hash_table_post_ref_cont(post, ht, old, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_multi_vals
    val = check_one_val(_vals)
    if val is None:
        # XXX this looks wrong, check_one_val raises if there are multiple
        # values
        return return_multi_vals(_vals, env, cont)
    return post.call([ht, old, val], env, cont)

@continuation
def chp_hash_table_ref_cont(ht, old, env, cont, _vals):
    if _vals.num_values() != 2:
        raise SchemeException("hash-ref handler produced the wrong number of results")
    key, post = _vals.get_all_values()
    val = values.Values.make1(key)
    after = check_chaperone_results(val, env,
                imp_hash_table_post_ref_cont(post, ht, old, env, cont))
    return ht.hash_ref(key, env, after)

class W_ImpHashTable(W_InterposeHashTable):
    import_from_mixin(ImpersonatorMixin)

    def post_set_cont(self, key, val, env, cont):
        pass

    def post_ref_cont(self, key, env, cont):
        return imp_hash_table_ref_cont(self.inner, key, env, cont)

class W_ChpHashTable(W_InterposeHashTable):
    import_from_mixin(ChaperoneMixin)

    def post_set_cont(self, key, val, env, cont):
        pass

    def post_ref_cont(self, key, env, cont):
        return chp_hash_table_ref_cont(self.inner, key, env, cont)

class W_ImpPropertyDescriptor(values.W_Object):
    errorname = "chaperone-property"
    _immutable_fields_ = ["name"]
    def __init__(self, name):
        self.name = name
    def tostring(self):
        return "#<chaperone-property>"

class W_ImpPropertyFunction(values.W_Procedure):
    _immutable_fields_ = ["descriptor"]
    def __init__(self, descriptor):
        self.descriptor = descriptor

class W_ImpPropertyPredicate(W_ImpPropertyFunction):
    errorname = "impersonator-property-predicate"

    @make_call_method([values.W_Object])
    def call(self, obj):
        return values.W_Bool.make(lookup_property(obj, self.descriptor) is not None)

class W_ImpPropertyAccessor(W_ImpPropertyFunction):
    errorname = "impersonator-property-accessor"

    @make_call_method([values.W_Object])
    def call(self, obj):
        return lookup_property(obj, self.descriptor)

w_impersonator_prop_application_mark = W_ImpPropertyDescriptor("impersonator-prop:application-mark")

