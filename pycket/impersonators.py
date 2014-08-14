#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont import continuation, label, call_cont
from pycket.error import SchemeException
from pycket import values
from pycket import values_struct
from rpython.rlib import jit

# A mixin for describing the general behaviour for a proxied object.
# This is mostly useful for preventing an annoying amount of duplication
# when implementing the various impersonator/chaperone operations.
# NOTE: These must be the first argument in the list of super classes for
# a class declaration.
# This assumes that all proxied have a field called `inner` for the proxied
# object.
class Proxy(object):
    _mixin_ = True
    def get_proxied(self):
        return self.inner
    def is_proxy(self):
        return True

# Mixin for describing the behaviour of chaperones
class Chaperone(Proxy):
    _mixin_ = True
    def is_chaperone(self):
        return True

# Mixin for describing the behaviour of chaperones
class Impersonator(Proxy):
    _mixin_ = True
    def is_impersonator(self):
        return True

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

class W_InterposeProcedure(values.W_Procedure):
    errorname = "interpose-prcedure"
    _immutable_fields_ = ["inner", "check"]
    def __init__(self, code, check):
        assert code.iscallable()
        assert check.iscallable()
        self.inner = code
        self.check = check

    def get_arity(self):
        return self.inner.get_arity()

    def post_call_cont(self, args, env, cont):
        raise NotImplementedError("abstract method")

    def _call(self, args, env, cont):
        jit.promote(self)
        after = self.post_call_cont(args, env, cont)
        return self.check.call(args, env, after)

    def tostring(self):
        return "ImpProcedure<%s>" % self.inner.tostring()

class W_ImpProcedure(Impersonator, W_InterposeProcedure):
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

class W_ChpProcedure(Chaperone, W_InterposeProcedure):
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

class W_InterposeBox(values.W_Box):
    errorname = "interpose-box"
    _immutable_fields_ = ["inner", "unbox", "set"]

    def __init__(self, box, unboxh, seth):
        assert isinstance(box, values.W_MBox)
        self.inner = box
        self.unboxh = unboxh
        self.seth = seth

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

class W_ChpBox(Chaperone, W_InterposeBox):
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

class W_ImpBox(Impersonator, W_InterposeBox):
    errorname = "imp-box"
    _immutable_fields_ = ["inner", "unbox", "set"]

    @jit.elidable
    def post_unbox_cont(self, env, cont):
        return imp_unbox_cont(self.unboxh, self.inner, env, cont)

    @jit.elidable
    def post_set_box_cont(self, val, env, cont):
        return imp_box_set_cont(self.inner, val, env, cont)

# Vectors
class W_ImpVector(Impersonator, values.W_MVector):
    errorname = "imp-vector"
    _immutable_fields_ = ["inner", "refh", "seth"]
    def __init__(self, v, r, s):
        assert isinstance(v, values.W_MVector)
        self.inner = v
        self.refh = r
        self.seth = s

    def length(self):
        return self.inner.length()

class W_ChpVector(Chaperone, values.W_MVector):
    errorname = "chp-procedure"
    _immutable_fields_ = ["inner", "refh", "seth"]
    def __init__(self, v, r, s):
        assert isinstance(v, values.W_MVector)
        self.inner  = v
        self.refh = r
        self.seth = s

    def length(self):
        return self.inner.length()

    def immutable(self):
        return self.inner.immutable()

# Are we dealing with a struct accessor/mutator/propert accessor or a
# chaperone/impersonator thereof.
def valid_struct_proc(x):
    v = get_base_procedure(x)
    return (isinstance(v, values_struct.W_StructFieldAccessor) or
            isinstance(v, values_struct.W_StructFieldMutator) or
            isinstance(v, values_struct.W_StructPropertyAccessor))

@jit.unroll_safe
def get_base_procedure(x):
    while x.is_proxy():
        x = x.get_proxied()
    return x

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
class W_InterposeStructBase(values_struct.W_RootStruct):
    _immutable_fields = ["inner", "accessors", "mutators", "handlers"]

    def __init__(self, inner, overrides, handlers):
        assert isinstance(inner, values_struct.W_RootStruct)
        assert len(overrides) == len(handlers)
        self.inner = inner
        self.accessors = {}
        self.mutators = {}
        self.properties = {}
        # Does not deal with properties as of yet
        for i, op in enumerate(overrides):
            base = get_base_procedure(op)
            if isinstance(base, values_struct.W_StructFieldAccessor):
                self.accessors[base.field.value] = (op, handlers[i])
            elif isinstance(base, values_struct.W_StructFieldMutator):
                self.mutators[base.field.value] = (op, handlers[i])
            else:
                assert False

    def post_ref_cont(self, interp, env, cont):
        raise NotImplementedError("abstract method")

    def post_set_cont(self, op, val, env, cont):
        raise NotImplementedError("abstract method")

    def _ref(self, value):
        return self.inner._ref(value)

    def _set(self, key, val):
        return self.inner._set(key, val)

    @jit.elidable
    def struct_type(self):
        struct = self.inner
        while isinstance(struct, W_InterposeStructBase):
            struct = struct.inner
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
class W_ImpStruct(Impersonator, W_InterposeStructBase):

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

class W_ChpStruct(Chaperone, W_InterposeStructBase):

    @jit.elidable
    def post_ref_cont(self, interp, env, cont):
        return chp_struct_ref_cont(interp, self.inner, env, cont)

    @jit.elidable
    def post_set_cont(self, op, val, env, cont):
        return chp_struct_set_cont(self.inner, op, val, env, cont)

class W_ImpProperty(values.W_Object):
    errorname = "impersonator-property"
    _immutable_fields_ = ["name"]

class W_ImpPropertyDescriptor(values.W_Procedure):
    errorname = "impersonator-property-descriptor"
    _immutable_fields_ = ["name"]

class W_ImpPropertyAccessor(values.W_Procedure):
    errorname = "impersonator-property-accessor"
    _immutable_fields_ = ["name"]

