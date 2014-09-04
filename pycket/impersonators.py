#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont import continuation, label, call_cont
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
        setattr(cls, "get_proxied", get_proxied)
        setattr(cls, "is_proxy", is_proxy)
        setattr(cls, "get_properties", get_properties)
        setattr(cls, "immutable", immutable)
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

@jit.unroll_safe
def is_impersonator_of(a, b):
    while True:
        if a is b:
            return True
        elif a.is_impersonator():
            a = a.get_proxied()
        else:
            return is_chaperone_of(a, b)

# Check that one value is a chaperone of the other
@jit.unroll_safe
def is_chaperone_of(a, b):
    while True:
        if a is b:
            return True
        elif a.is_chaperone():
            a = a.get_proxied()
        else:
            return False

# Procedures

# Continuation used when calling an impersonator of a procedure.
@continuation
def imp_proc_cont(args, proc, env, cont, _vals):
    vals = _vals._get_full_list()
    arg_count = len(args)
    if len(vals) == arg_count:
        return proc.call(vals, env, cont)
    elif len(vals) == arg_count + 1:
        args, check = vals[1:], vals[0]
        return proc.call(args, env, call_cont(check, env, cont))
    else:
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
            self.properties[k.name] = prop_vals[i]

    def get_arity(self):
        return self.inner.get_arity()

    def post_call_cont(self, args, env, cont):
        raise NotImplementedError("abstract method")

    def _call(self, args, env, cont):
        jit.promote(self)
        after = self.post_call_cont(args, env, cont)
        return self.check.call(args, env, after)

    def tostring(self):
        return self.inner.tostring()

@make_impersonator
class W_ImpProcedure(W_InterposeProcedure):
    errorname = "imp-procedure"

    @jit.elidable
    def post_call_cont(self, args, env, cont):
        return imp_proc_cont(args, self.inner, env, cont)

# Check that the results of che call to check are all chaperones of
# the original function outputs.
@continuation
def chp_proc_ret_cont(orig, env, cont, _vals):
    from pycket.interpreter import return_multi_vals
    vals = _vals._get_full_list()
    assert len(vals) == len(orig)
    for i in range(len(vals)):
        if not is_chaperone_of(vals[i], orig[i]):
            raise SchemeException("Expecting original value or chaperone")
    return return_multi_vals(_vals, env, cont)

# Capture the original output of the function to compare agains the result of
# the check operation
@continuation
def chp_proc_call_check_cont(check, env, cont, _vals):
    vals = _vals._get_full_list()
    return check.call(vals, env, chp_proc_ret_cont(vals, env, cont))

# Continuation used when calling a chaperone of a procedure.
@continuation
def chp_proc_cont(args, proc, env, cont, _vals):
    vals = _vals._get_full_list()
    assert len(vals) >= len(args)
    if len(vals) == len(args):
        for i in range(len(args)):
            if not is_chaperone_of(vals[i], args[i]):
                raise SchemeException("Expecting original value or chaperone")
        return proc.call(vals, env, cont)
    elif len(vals) == len(args) + 1:
        vals, check = vals[1:], vals[0]
        for i in range(len(args)):
            if not is_chaperone_of(vals[i], args[i]):
                raise SchemeException("Expecting original value or chaperone")
        return proc.call(vals, env,
                chp_proc_call_check_cont(check, env, cont))
    else:
        assert False

@make_chaperone
class W_ChpProcedure(W_InterposeProcedure):
    errorname = "chp-procedure"
    _immutable_fields_ = ["inner", "check"]

    @jit.elidable
    def post_call_cont(self, args, env, cont):
        return chp_proc_cont(args, self.inner, env, cont)

# Boxes
@continuation
def chp_unbox_cont(f, box, env, cont, vals):
    from pycket.interpreter import check_one_val
    old = check_one_val(vals)
    return f.call([box, old], env, chp_unbox_cont_ret(old, env, cont))

@continuation
def chp_unbox_cont_ret(old, env, cont, vals):
    from pycket.interpreter import check_one_val, return_multi_vals
    new = check_one_val(vals)
    if is_chaperone_of(new, old):
        return return_multi_vals(vals, env, cont)
    else:
        raise SchemeException("Expecting original value or chaperone of thereof")

@continuation
def chp_box_set_cont(b, orig, env, cont, vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(vals)
    if not is_chaperone_of(val, orig):
        raise SchemeException("Expecting original value or chaperone")
    return b.set_box(val, env, cont)

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
            self.properties[k.name] = prop_vals[i]

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

    def tostring(self):
        return self.inner.tostring()

@make_chaperone
class W_ChpBox(W_InterposeBox):
    errorname = "chp-box"
    _immutable_fields_ = ["inner", "unbox", "set"]

    @jit.elidable
    def post_unbox_cont(self, env, cont):
        return chp_unbox_cont(self.unboxh, self.inner, env, cont)

    @jit.elidable
    def post_set_box_cont(self, val, env, cont):
        return chp_box_set_cont(self.inner, val, env, cont)

    def immutable(self):
        return self.inner.immutable()

@continuation
def imp_unbox_cont(f, box, env, cont, vals):
    from pycket.interpreter import check_one_val
    return f.call([box, check_one_val(vals)], env, cont)

@continuation
def imp_box_set_cont(b, val, env, cont, vals):
    from pycket.interpreter import check_one_val
    return b.set_box(check_one_val(vals), env, cont)

@make_impersonator
class W_ImpBox(W_InterposeBox):
    errorname = "imp-box"
    _immutable_fields_ = ["inner", "unbox", "set"]

    @jit.elidable
    def post_unbox_cont(self, env, cont):
        return imp_unbox_cont(self.unboxh, self.inner, env, cont)

    @jit.elidable
    def post_set_box_cont(self, val, env, cont):
        return imp_box_set_cont(self.inner, val, env, cont)

@continuation
def imp_vec_set_cont(orig, v, i, env, cont, vals):
    from pycket.interpreter import check_one_val
    return v.vector_set(i, check_one_val(vals), env, cont)

@continuation
def chp_vec_set_cont(orig, v, i, env, cont, vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(vals)
    if not is_chaperone_of(val, orig):
        raise SchemeException("Expecting original value or chaperone")
    return v.vector_set(i, val, env, cont)

@continuation
def imp_vec_ref_cont(f, i, v, env, cont, vals):
    from pycket.interpreter import check_one_val
    return f.call([v, i, check_one_val(vals)], env, cont)

@continuation
def chp_vec_ref_cont(f, i, v, env, cont, vals):
    from pycket.interpreter import check_one_val
    old = check_one_val(vals)
    return f.call([v, i, old], env, chp_vec_ref_cont_ret(old, env, cont))

@continuation
def chp_vec_ref_cont_ret(old, env, cont, vals):
    from pycket.interpreter import check_one_val, return_multi_vals
    new = check_one_val(vals)
    if is_chaperone_of(new, old):
        return return_multi_vals(vals, env, cont)
    else:
        raise SchemeException("Expecting original value or chaperone of thereof")

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
            self.properties[k.name] = prop_vals[i]

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

    @jit.elidable
    def post_set_cont(self, new, i, env, cont):
        return imp_vec_set_cont(new, self.inner, i, env, cont)

    @jit.elidable
    def post_ref_cont(self, i, env, cont):
        return imp_vec_ref_cont(self.refh, i, self.inner, env, cont)

@make_chaperone
class W_ChpVector(W_InterposeVector):
    errorname = "chaperone-vector"

    @jit.elidable
    def post_set_cont(self, new, i, env, cont):
        return chp_vec_set_cont(new, self.inner, i, env, cont)

    @jit.elidable
    def post_ref_cont(self, i, env, cont):
        return chp_vec_ref_cont(self.refh, i, self.inner, env, cont)

# Are we dealing with a struct accessor/mutator/propert accessor or a
# chaperone/impersonator thereof.
def valid_struct_proc(x):
    v = get_base_object(x)
    return (isinstance(v, values_struct.W_StructFieldAccessor) or
            isinstance(v, values_struct.W_StructFieldMutator))

@continuation
def imp_struct_ref_cont(interp, orig_struct, env, cont, _vals):
    from pycket.interpreter import check_one_val
    field_v = check_one_val(_vals)
    return interp.call([orig_struct, field_v], env, cont)

@continuation
def imp_struct_set_cont(orig_struct, setter, orig_val, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    return setter.call([orig_struct, val], env, cont)

# Representation of a struct that allows interposition of operations
# onto accessors/mutators
@make_proxy(proxied="inner", properties="properties")
class W_InterposeStructBase(values_struct.W_RootStruct):
    _immutable_fields = ["inner", "accessors", "mutators", "handlers", "properties"]

    def __init__(self, inner, overrides, handlers, prop_keys, prop_vals):
        assert isinstance(inner, values_struct.W_RootStruct)
        assert len(overrides) == len(handlers)
        assert len(prop_keys) == len(prop_vals)
        self.inner = inner
        self.accessors = {}
        self.mutators = {}
        self.properties = {}
        # Does not deal with properties as of yet
        for i, op in enumerate(overrides):
            base = get_base_object(op)
            if isinstance(base, values_struct.W_StructFieldAccessor):
                self.accessors[base.field.value] = (op, handlers[i])
            elif isinstance(base, values_struct.W_StructFieldMutator):
                self.mutators[base.field.value] = (op, handlers[i])
            else:
                assert False
        for i, k in enumerate(prop_keys):
            assert isinstance(k, W_ImpPropertyDescriptor)
            self.properties[k.name] = prop_vals[i]

    def post_ref_cont(self, interp, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, val, env, cont):
        raise NotImplementedError("abstract method")

    @jit.elidable
    def struct_type(self):
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
        return interp.call([self.inner, val], env,
                self.post_set_cont(op, val, env, cont))

    # FIXME: This is incorrect
    def vals(self):
        return self.inner.vals()

# Need to add checks that we are only impersonating mutable fields
@make_impersonator
class W_ImpStruct(W_InterposeStructBase):

    @jit.elidable
    def post_ref_cont(self, interp, env, cont):
        return imp_struct_ref_cont(interp, self.inner, env, cont)

    @jit.elidable
    def post_set_cont(self, op, val, env, cont):
        return imp_struct_set_cont(self.inner, op, val, env, cont)

@continuation
def chp_struct_ref_cont(interp, orig_struct, env, cont, _vals):
    from pycket.interpreter import check_one_val
    field_v = check_one_val(_vals)
    return interp.call([orig_struct, field_v], env,
            chp_struct_ref_post_cont(field_v, env, cont))

@continuation
def chp_struct_ref_post_cont(field_v, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_multi_vals
    val = check_one_val(_vals)
    if not is_chaperone_of(val, field_v):
        raise SchemeException("chaperone handlers must produce chaperone of original value")
    return return_multi_vals(_vals, env, cont)

# called after interp function
@continuation
def chp_struct_set_cont(orig_struct, setter, orig_val, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    if not is_chaperone_of(val, orig_val):
        raise SchemeException("chaperone handlers must produce chaperone of original value")
    return setter.call([orig_struct, val], env, cont)

@make_chaperone
class W_ChpStruct(W_InterposeStructBase):

    @jit.elidable
    def post_ref_cont(self, interp, env, cont):
        return chp_struct_ref_cont(interp, self.inner, env, cont)

    @jit.elidable
    def post_set_cont(self, op, val, env, cont):
        return chp_struct_set_cont(self.inner, op, val, env, cont)

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
            self.properties[k.name] = prop_vals[i]

    def post_set_cont(self, body, key, value, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def get_cmk(self, value, env, cont):
        raise NotImplementedError("abstract method")

    @label
    def set_cmk(self, body, value, env, cont):
        return self.set_proc.call(value, env,
                self.post_set_cont(body, value, env, cont))

@continuation
def imp_cmk_post_set_cont(body, inner, value, env, cont, _val):
    from pycket.interpreter import check_one_val
    val = check_one_val(_val)
    return inner.set_cmk(val, body, env, cont)

@make_chaperone
class W_ChaperoneContinuationMarkKey(W_InterposeContinuationMarkKey):
    def post_set_cont(self, body, key, value, env, cont):
        raise NotImplementedError("abstract method")

@make_impersonator
class W_ImpersonateContinuationMarkKey(W_InterposeContinuationMarkKey):
    def post_set_cont(self, body, key, value, env, cont):
        return imp_cmk_post_set_cont(body, self.inner, key, value, env, cont)

class W_ImpPropertyDescriptor(values.W_Object):
    errorname = "chaperone-property"
    _immutable_fields_ = ["name"]
    def __init__(self, name):
        self.name = name
    def tostring(self):
        return "#<chaperone-property>"

class W_ImpPropertyFunction(values.W_Procedure):
    _immutable_fields_ = ["name"]
    def __init__(self, name):
        self.name = name

class W_ImpPropertyPredicate(W_ImpPropertyFunction):
    errorname = "impersonator-property-predicate"

    @make_call_method([values.W_Object])
    def call(self, obj):
        return values.W_Bool.make(lookup_property(obj, self.name) is not None)

class W_ImpPropertyAccessor(W_ImpPropertyFunction):
    errorname = "impersonator-property-accessor"

    @make_call_method([values.W_Object])
    def call(self, obj):
        return lookup_property(obj, self.name)

w_impersonator_prop_application_mark = W_ImpPropertyDescriptor("impersonator-prop:application-mark")

