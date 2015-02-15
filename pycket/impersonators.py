#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont import continuation, label, call_cont, call_extra_cont
from pycket.prims.expose import make_call_method
from pycket.error import SchemeException
from pycket.values import UNROLLING_CUTOFF
from pycket import values
from pycket import values_struct
from pycket import values_hash
from rpython.rlib import jit

@jit.unroll_safe
def get_base_object(x):
    if isinstance(x, W_InterposeStructBase):
        return x.base
    while x.is_proxy():
        x = x.get_proxied()
    return x

def make_proxy(proxied="inner", properties="properties"):
    def wrapper(cls):
        def get_proxied(self):
            return getattr(self, proxied)
        def is_proxy(self):
            return True
        def get_properties(self):
            return getattr(self, properties)
        def immutable(self):
            return get_base_object(self).immutable()
        def tostring(self):
            return get_base_object(getattr(self, proxied)).tostring()
        setattr(cls, "get_proxied", get_proxied)
        setattr(cls, "is_proxy", is_proxy)
        setattr(cls, "get_properties", get_properties)
        setattr(cls, "immutable", immutable)
        setattr(cls, "tostring", tostring)
        return cls
    return wrapper

def make_chaperone(cls):
    def is_chaperone(self):
        return True
    setattr(cls, "is_chaperone", is_chaperone)
    return cls

def make_impersonator(cls):
    def is_impersonator(self):
        return True
    setattr(cls, "is_impersonator", is_impersonator)
    return cls

@jit.unroll_safe
def lookup_property(obj, prop):
    while obj.is_proxy():
        props = obj.get_properties()
        val   = props.get(prop, None) if props is not None else None
        if val is not None:
            return val
        obj = obj.get_proxied()
    return None

@continuation
def check_chaperone_results(args, env, cont, vals):
    # We are allowed to receive more values than arguments to compare them to.
    # Additional ones are ignored for this checking routine.
    assert vals.num_values() >= len(args)
    return check_chaperone_results_loop(vals, args, 0, env, cont)

@jit.unroll_safe
def check_chaperone_results_loop(vals, args, idx, env, cont):
    from pycket.interpreter import return_multi_vals
    from pycket.prims.equal import equal_func_unroll_n, EqualInfo
    while idx < len(args) and vals.get_value(idx) is None and args[idx] is None:
        idx += 1
    if idx >= len(args):
        return return_multi_vals(vals, env, cont)
    info = EqualInfo.CHAPERONE_SINGLETON
    # XXX it would be best to store the parameter on the toplevel env and make
    # it changeable via a cmdline parameter to pycket-c
    unroll_n_times = 2 # XXX needs tuning
    return equal_func_unroll_n(vals.get_value(idx), args[idx], info, env,
            catch_equal_cont(vals, args, idx, env, cont), unroll_n_times)

@continuation
def catch_equal_cont(vals, args, idx, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    if val is values.w_false:
        raise SchemeException("Expecting original value or chaperone")
    return check_chaperone_results_loop(vals, args, idx + 1, env, cont)

@continuation
def chaperone_reference_cont(f, args, env, cont, _vals):
    old = _vals.get_all_values()
    return f.call(args + old, env, check_chaperone_results(old, env, cont))

@continuation
def impersonate_reference_cont(f, args, env, cont, _vals):
    old = _vals.get_all_values()
    return f.call(args + old, env, cont)

# Procedures

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

@make_proxy(proxied="inner", properties="properties")
class W_InterposeProcedure(values.W_Procedure):
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

    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

    def is_non_interposing_chaperone(self):
        return self.check is values.w_false

    def call_with_extra_info(self, args, env, cont, calling_app):
        from pycket.values import W_ThunkProcCMK
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

@make_impersonator
class W_ImpProcedure(W_InterposeProcedure):
    errorname = "imp-procedure"

    def post_call_cont(self, args, env, cont, calling_app):
        return imp_proc_cont(len(args), self.inner, calling_app, env, cont)

@make_chaperone
class W_ChpProcedure(W_InterposeProcedure):
    errorname = "chp-procedure"

    def post_call_cont(self, args, env, cont, calling_app):
        return chp_proc_cont(args, self.inner, calling_app, env, cont)

@make_proxy(proxied="inner", properties="properties")
class W_InterposeBox(values.W_Box):
    errorname = "interpose-box"
    _immutable_fields_ = ["inner", "unbox", "set", "properties"]

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
        return self.inner.immutable()

    def post_unbox_cont(self, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_box_cont(self, val, env, cont):
        raise NotImplementedError("abstract method")

    def unbox(self, env, cont):
        after = self.post_unbox_cont(env, cont)
        return self.inner.unbox(env, after)

    def set_box(self, val, env, cont):
        after = self.post_set_box_cont(val, env, cont)
        return self.seth.call([self.inner, val], env, after)

@make_chaperone
class W_ChpBox(W_InterposeBox):
    errorname = "chp-box"
    _immutable_fields_ = ["inner", "unbox", "set"]

    def post_unbox_cont(self, env, cont):
        return chaperone_reference_cont(self.unboxh, [self.inner], env, cont)

    def post_set_box_cont(self, val, env, cont):
        return check_chaperone_results([val], env,
                imp_box_set_cont(self.inner, env, cont))

    def immutable(self):
        return self.inner.immutable()

@continuation
def imp_box_set_cont(b, env, cont, vals):
    from pycket.interpreter import check_one_val
    return b.set_box(check_one_val(vals), env, cont)

@make_impersonator
class W_ImpBox(W_InterposeBox):
    errorname = "imp-box"
    _immutable_fields_ = ["inner", "unbox", "set"]

    def post_unbox_cont(self, env, cont):
        return impersonate_reference_cont(self.unboxh, [self.inner], env, cont)

    def post_set_box_cont(self, val, env, cont):
        return imp_box_set_cont(self.inner, env, cont)

@continuation
def imp_vec_set_cont(v, i, env, cont, vals):
    from pycket.interpreter import check_one_val
    return v.vector_set(i, check_one_val(vals), env, cont)

@make_proxy(proxied="inner", properties="properties")
class W_InterposeVector(values.W_MVector):
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
        return self.inner.length()

    def post_set_cont(self, new, i, env, cont):
        raise NotImplementedError("abstract method")

    def post_ref_cont(self, i, env, cont):
        raise NotImplementedError("abstract method")

    def vector_set(self, i, new, env, cont):
        after = self.post_set_cont(new, i, env, cont)
        return self.seth.call([self.inner, i, new], env, after)

    def vector_ref(self, i, env, cont):
        after = self.post_ref_cont(i, env, cont)
        return self.inner.vector_ref(i, env, after)

# Vectors
@make_impersonator
class W_ImpVector(W_InterposeVector):
    errorname = "impersonate-vector"

    def post_set_cont(self, new, i, env, cont):
        return imp_vec_set_cont(self.inner, i, env, cont)

    def post_ref_cont(self, i, env, cont):
        return impersonate_reference_cont(self.refh, [self.inner, i], env, cont)

@make_chaperone
class W_ChpVector(W_InterposeVector):
    errorname = "chaperone-vector"

    def post_set_cont(self, new, i, env, cont):
        return check_chaperone_results([new], env,
                imp_vec_set_cont(self.inner, i, env, cont))

    def post_ref_cont(self, i, env, cont):
        return chaperone_reference_cont(self.refh, [self.inner, i], env, cont)

# Are we dealing with a struct accessor/mutator/propert accessor or a
# chaperone/impersonator thereof.
def valid_struct_proc(x):
    v = get_base_object(x)
    return (isinstance(v, values_struct.W_StructFieldAccessor) or
            isinstance(v, values_struct.W_StructFieldMutator) or
            isinstance(v, values_struct.W_StructPropertyAccessor))

@continuation
def imp_struct_set_cont(orig_struct, setter, struct_id, field, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    if setter is values.w_false:
        return orig_struct.set(struct_id, field, val, env, cont)
    return setter.call([orig_struct, val], env, cont)

# Representation of a struct that allows interposition of operations
# onto accessors/mutators
@make_proxy(proxied="inner", properties="properties")
class W_InterposeStructBase(values_struct.W_RootStruct):
    _immutable_fields = ["inner", "base", "mask[*]", "accessors[*]", "mutators[*]", "struct_props", "properties"]

    @jit.unroll_safe
    def __init__(self, inner, overrides, handlers, prop_keys, prop_vals):
        assert isinstance(inner, values_struct.W_RootStruct)
        assert len(overrides) == len(handlers)
        assert not prop_keys and not prop_vals or len(prop_keys) == len(prop_vals)

        self.inner = inner

        field_cnt = inner.struct_type().total_field_cnt
        accessors = [values.w_false] * field_cnt * 2
        mutators  = [values.w_false] * field_cnt * 2

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
        for i, op in enumerate(overrides):
            base = get_base_object(op)
            if isinstance(base, values_struct.W_StructFieldAccessor):
                op = values.w_false if type(op) is values_struct.W_StructFieldAccessor else op
                jit.promote(base.field)
                mask[base.field] = self
                accessors[2 * base.field] = op
                accessors[2 * base.field + 1] = handlers[i]
            elif isinstance(base, values_struct.W_StructFieldMutator):
                jit.promote(base.field)
                op = values.w_false if type(op) is values_struct.W_StructFieldMutator else op
                mutators[2 * base.field] = op
                mutators[2 * base.field + 1] = handlers[i]
            elif isinstance(base, values_struct.W_StructPropertyAccessor):
                if struct_props is None:
                    struct_props = {}
                struct_props[base] = (op, handlers[i])
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

    def post_ref_cont(self, interp, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, struct_id, field, val, env, cont):
        raise NotImplementedError("abstract method")

    @jit.unroll_safe
    def is_non_interposing_chaperone(self):
        acc = self.accessors
        for i in range(1, len(self.accessors), 2):
            if acc[i] is not values.w_false:
                return False
        return bool(self.properties)

    def struct_type(self):
        return self.base.struct_type()

    def ref(self, struct_id, field, env, cont):
        goto = self.mask[field]
        if goto is not self:
            return goto.ref(struct_id, field, env, cont)
        op = self.accessors[2 * field]
        interp = self.accessors[2 * field + 1]
        after = self.post_ref_cont(interp, env, cont)
        if op is values.w_false:
            return self.inner.ref(struct_id, field, env, after)
        return op.call([self.inner], env, after)

    def set(self, struct_id, field, val, env, cont):
        op = self.mutators[2 * field]
        interp = self.mutators[2 * field + 1]
        if interp is values.w_false:
            return self.inner.set(struct_id, field, val, env, cont)
        after = self.post_set_cont(op, struct_id, field, val, env, cont)
        return interp.call([self, val], env, after)

    def get_prop(self, property, env, cont):
        if self.struct_props is None:
            return self.inner.get_prop(property, env, cont)
        op, interp = self.struct_props.get(property, (None, None))
        if op is None or interp is None:
            return self.inner.get_prop(property, env, cont)
        after = self.post_ref_cont(interp, env, cont)
        return op.call([self.inner], env, after)

    def get_arity(self):
        return self.inner.get_arity()

    # FIXME: This is incorrect
    def vals(self):
        return self.inner.vals()

# Need to add checks that we are only impersonating mutable fields
@make_impersonator
class W_ImpStruct(W_InterposeStructBase):

    def post_ref_cont(self, interp, env, cont):
        return impersonate_reference_cont(interp, [self], env, cont)

    def post_set_cont(self, op, struct_id, field, val, env, cont):
        return imp_struct_set_cont(self.inner, op, struct_id, field, env, cont)

@make_chaperone
class W_ChpStruct(W_InterposeStructBase):

    def post_ref_cont(self, interp, env, cont):
        return chaperone_reference_cont(interp, [self], env, cont)

    def post_set_cont(self, op, struct_id, field, val, env, cont):
        return check_chaperone_results([val], env,
                imp_struct_set_cont(self.inner, op, struct_id, field, env, cont))

@make_proxy(proxied="inner", properties="properties")
class W_InterposeContinuationMarkKey(values.W_ContinuationMarkKey):
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

    def get_cmk(self, value, env, cont):
        return self.get_proc.call([value], env,
                self.post_get_cont(value, env, cont))

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

@make_chaperone
class W_ChpContinuationMarkKey(W_InterposeContinuationMarkKey):
    def post_get_cont(self, value, env, cont):
        return check_chaperone_results([value], env,
                imp_cmk_post_get_cont(self.inner, env, cont))

    def post_set_cont(self, body, value, env, cont):
        return check_chaperone_results([value], env,
                imp_cmk_post_set_cont(body, self.inner, env, cont))

@make_impersonator
class W_ImpContinuationMarkKey(W_InterposeContinuationMarkKey):

    def post_get_cont(self, value, env, cont):
        return imp_cmk_post_get_cont(self.inner, env, cont)

    def post_set_cont(self, body, value, env, cont):
        return imp_cmk_post_set_cont(body, self.inner, env, cont)

@make_proxy(proxied="inner", properties="properties")
class W_InterposeHashTable(values_hash.W_HashTable):
    errorname = "interpose-hash-table"
    _immutable_fields_ = ["inner", "set_proc", "ref_proc", "remove_proc",
                          "key_proc", "clear_proc", "properties"]
    def __init__(self, inner, ref_proc, set_proc, remove_proc, key_proc,
                 clear_proc, prop_keys, prop_vals):
        assert isinstance(inner, values_hash.W_HashTable)
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

    def hash_set(self, key, val, env, cont):
        raise NotImplementedError("abstract method")

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
    after = check_chaperone_results([key], env,
                imp_hash_table_post_ref_cont(post, ht, old, env, cont))
    return ht.hash_ref(key, env, after)

@make_impersonator
class W_ImpHashTable(W_InterposeHashTable):

    def post_set_cont(self, key, val, env, cont):
        pass

    def post_ref_cont(self, key, env, cont):
        return imp_hash_table_ref_cont(self.inner, key, env, cont)

@make_chaperone
class W_ChpHashTable(W_InterposeHashTable):

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

