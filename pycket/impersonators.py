#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont import continuation, label, call_cont, call_extra_cont
from pycket.prims.expose import make_call_method
from pycket.error import SchemeException
from pycket import values
from pycket import values_struct
from rpython.rlib import jit

@jit.unroll_safe
def get_base_object(x):
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
        val = obj.get_properties().get(prop, None)
        if val is not None:
            return val
        obj = obj.get_proxied()
    return None

@continuation
def check_chaperone_results(args, env, cont, vals):
    # We are allowed to receive more values than arguments to compare them to.
    # Additional ones are ignored for this checking routine.
    assert vals._get_size_list() >= len(args)
    return check_chaperone_results_loop(vals, args, 0, env, cont)

def check_chaperone_results_loop(vals, args, idx, env, cont):
    from pycket.interpreter import return_multi_vals
    from pycket.prims.equal import equal_func, EqualInfo
    while idx < len(args) and vals._get_list(idx) is None and args[idx] is None:
        idx += 1
    if idx >= len(args):
        return return_multi_vals(vals, env, cont)
    info = EqualInfo.CHAPERONE_SINGLETON
    return equal_func(vals._get_list(idx), args[idx], info, env,
            catch_equal_cont(vals, args, idx, env, cont))

@continuation
def catch_equal_cont(vals, args, idx, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    if val is values.w_false:
        raise SchemeException("Expecting original value or chaperone")
    return check_chaperone_results_loop(vals, args, idx + 1, env, cont)

@continuation
def chaperone_reference_cont(f, args, env, cont, _vals):
    old = _vals._get_full_list()
    return f.call(args + old, env, check_chaperone_results(old, env, cont))

@continuation
def impersonate_reference_cont(f, args, env, cont, _vals):
    old = _vals._get_full_list()
    return f.call(args + old, env, cont)

# Procedures

# Continuation used when calling an impersonator of a procedure.
@continuation
def imp_proc_cont(arg_count, proc, calling_app, env, cont, _vals):
    vals = _vals._get_full_list()
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
    vals = _vals._get_full_list()
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
    _immutable_fields_ = ["inner", "check", "properties"]
    def __init__(self, code, check, prop_keys, prop_vals):
        assert code.iscallable()
        assert check.iscallable()
        assert len(prop_keys) == len(prop_vals)
        self.inner = code
        self.check = check
        self.properties = {}
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

    def call_with_extra_info(self, args, env, cont, calling_app):
        from pycket.values import W_ThunkProcCMK
        after = self.post_call_cont(args, env, cont, calling_app)
        prop = self.properties.get(w_impersonator_prop_application_mark, None)
        if isinstance(prop, values.W_Cons):
            key, val = prop.car(), prop.cdr()
            if isinstance(key, values.W_ContinuationMarkKey):
                body = W_ThunkProcCMK(self.check, args)
                return key.set_cmk(body, val, cont, env, after)
            cont.update_cm(key, val)
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
        assert len(prop_keys) == len(prop_vals)
        self.inner = box
        self.unboxh = unboxh
        self.seth = seth
        self.properties = {}
        for i, k in enumerate(prop_keys):
            assert isinstance(k, W_ImpPropertyDescriptor)
            self.properties[k] = prop_vals[i]

    def immutable(self):
        return self.inner.immutable()

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
    def __init__(self, v, r, s, prop_keys, prop_vals):
        assert isinstance(v, values.W_MVector)
        assert len(prop_keys) == len(prop_vals)
        self.inner = v
        self.refh = r
        self.seth = s
        self.properties = {}
        for i, k in enumerate(prop_keys):
            assert isinstance(k, W_ImpPropertyDescriptor)
            self.properties[k] = prop_vals[i]

    def length(self):
        return self.inner.length()

    def post_set_cont(self, new, i, env, cont):
        raise NotImplementedError("abstract method")

    def post_ref_cont(self, i, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def vector_set(self, i, new, env, cont):
        after = self.post_set_cont(new, i, env, cont)
        return self.seth.call([self.inner, i, new], env, after)

    @label
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
def imp_struct_set_cont(orig_struct, setter, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    return setter.call([orig_struct, val], env, cont)

# Representation of a struct that allows interposition of operations
# onto accessors/mutators
@make_proxy(proxied="inner", properties="properties")
class W_InterposeStructBase(values_struct.W_RootStruct):
    _immutable_fields = ["inner", "accessors", "mutators", "struct_props", "handlers", "properties"]

    def __init__(self, inner, overrides, handlers, prop_keys, prop_vals):
        assert isinstance(inner, values_struct.W_RootStruct)
        assert len(overrides) == len(handlers)
        assert len(prop_keys) == len(prop_vals)
        self.inner        = inner
        self.accessors    = {}
        self.mutators     = {}
        self.struct_props = {}
        self.properties   = {}
        # Does not deal with properties as of yet
        for i, op in enumerate(overrides):
            base = get_base_object(op)
            if isinstance(base, values_struct.W_StructFieldAccessor):
                self.accessors[base.field.value] = (op, handlers[i])
            elif isinstance(base, values_struct.W_StructFieldMutator):
                self.mutators[base.field.value] = (op, handlers[i])
            elif isinstance(base, values_struct.W_StructPropertyAccessor):
                self.struct_props[base] = (op, handlers[i])
            else:
                assert False
        for i, k in enumerate(prop_keys):
            assert isinstance(k, W_ImpPropertyDescriptor)
            self.properties[k] = prop_vals[i]

    def post_ref_cont(self, interp, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, val, env, cont):
        raise NotImplementedError("abstract method")

    def struct_type(self):
        return self._struct_type()

    @jit.elidable
    def _struct_type(self):
        struct = get_base_object(self.inner)
        assert isinstance(struct, values_struct.W_Struct)
        return struct.struct_type()

    @label
    def ref(self, struct_id, field, env, cont):
        (op, interp) = self.accessors.get(field, (None, None))
        if interp is None:
            return self.inner.ref(struct_id, field, env, cont)
        after = self.post_ref_cont(interp, env, cont)
        return op.call([self.inner], env, after)

    @label
    def set(self, struct_id, field, val, env, cont):
        (op, interp) = self.mutators.get(field, (None, None))
        if interp is None or op is None:
            return self.inner.set(struct_id, field, val, env, cont)
        after = self.post_set_cont(op, val, env, cont)
        return interp.call([self.inner, val], env, after)

    @label
    def get_prop(self, property, env, cont):
        (op, interp) = self.struct_props.get(property, (None, None))
        if interp is None or op is None:
            return self.inner.get_prop(property, env, cont)
        after = self.post_ref_cont(interp, env, cont)
        return op.call([self.inner], env, after)

    # FIXME: This is incorrect
    def vals(self):
        return self.inner.vals()

# Need to add checks that we are only impersonating mutable fields
@make_impersonator
class W_ImpStruct(W_InterposeStructBase):

    def post_ref_cont(self, interp, env, cont):
        return impersonate_reference_cont(interp, [self.inner], env, cont)

    def post_set_cont(self, op, val, env, cont):
        return imp_struct_set_cont(self.inner, op, env, cont)

@make_chaperone
class W_ChpStruct(W_InterposeStructBase):

    def post_ref_cont(self, interp, env, cont):
        return chaperone_reference_cont(interp, [self.inner], env, cont)

    def post_set_cont(self, op, val, env, cont):
        return check_chaperone_results([val], env,
                imp_struct_set_cont(self.inner, op, env, cont))

@make_proxy(proxied="inner", properties="properties")
class W_InterposeContinuationMarkKey(values.W_ContinuationMarkKey):
    errorname = "interpose-continuation-mark-key"
    _immutable_fields_ = ["inner", "get_proc", "set_proc", "properties"]
    def __init__(self, mark, get_proc, set_proc, prop_keys, prop_vals):
        assert get_proc.iscallable()
        assert set_proc.iscallable()
        assert len(prop_keys) == len(prop_vals)
        self.inner    = mark
        self.get_proc = get_proc
        self.set_proc = set_proc
        self.properties = {}
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
class W_InterposeHashTable(values.W_HashTable):
    errorname = "interpose-hash-table"
    _immutable_fields_ = ["inner", "set_proc", "ref_proc", "remove_proc",
                          "key_proc", "clear_proc", "properties"]
    def __init__(self, inner, ref_proc, set_proc, remove_proc, key_proc,
                 clear_proc, prop_keys, prop_vals):
        assert isinstance(inner, values.W_HashTable)
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
        for i, k in enumerate(prop_keys):
            assert isinstance(k, W_ImpPropertyDescriptor)
            self.properties[k] = prop_vals[i]

    def post_ref_cont(self, key, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, key, val, env, cont):
        raise NotImplementedError("abstract method")

    def hash_keys(self):
        return get_base_object(self.inner).hash_keys()

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
    if _vals._get_size_list() != 2:
        return return_value(None, env, cont)
        raise SchemeException("hash-ref handler produced the wrong number of results")
    key, post = _vals._get_full_list()
    after = imp_hash_table_post_ref_cont(post, ht, old, env, cont)
    return ht.hash_ref(key, env, after)

@continuation
def imp_hash_table_post_ref_cont(post, ht, old, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_multi_vals
    val = check_one_val(_vals)
    if val is None:
        return return_multi_vals(_vals, env, cont)
    return post.call([ht, old, val], env, cont)

@continuation
def chp_hash_table_ref_cont(ht, old, env, cont, _vals):
    if _vals._get_size_list() != 2:
        raise SchemeException("hash-ref handler produced the wrong number of results")
    key, post = _vals._get_full_list()
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

