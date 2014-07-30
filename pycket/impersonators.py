#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont import continuation, label, call_cont
from pycket.error import SchemeException
from pycket import values
from pycket import struct
from rpython.rlib import jit

def is_impersonator_of(a, b):
    if a is b:
        return True
    if isinstance(a, W_ImpVector):
        return is_impersonator_of(a.vec, b)
    if isinstance(a, W_ImpProcedure):
        return is_impersonator_of(a.code, b)
    if isinstance(a, W_ImpBox):
        return is_impersonator_of(a.box, b)
    if isinstance(a, W_ImpStruct):
        return is_impersonator_of(a.struct, b)
    return is_chaperone_of(a, b)

# Check that one value is a chaperone of the other
def is_chaperone_of(a, b):
    if a is b:
        return True
    if isinstance(a, W_ChpVector):
        return is_chaperone_of(a.vec, b)
    if isinstance(a, W_ChpProcedure):
        return is_chaperone_of(a.code, b)
    if isinstance(a, W_ChpBox):
        return is_chaperone_of(a.box, b)
    if isinstance(a, W_ChpStruct):
        return is_chaperone_of(a.struct, b)
    return False

def is_chaperone(x):
    return (isinstance(x, W_ChpVector) or
            isinstance(x, W_ChpBox) or
            isinstance(x, W_ChpProcedure) or
            isinstance(x, W_ChpStruct))

def is_impersonator(x):
    return (isinstance(x, W_ImpVector) or
            isinstance(x, W_ImpBox) or
            isinstance(x, W_ImpProcedure) or
            isinstance(x, W_ImpStruct) or
            is_chaperone(x))

# Procedures

# Continuation used when calling an impersonator of a procedure.
@continuation
def imp_proc_cont(arg_count, proc, env, cont, _vals):
    from pycket.interpreter import tailcall
    vals = _vals._get_full_list()
    if len(vals) == arg_count:
        return tailcall(proc, vals, env, cont)
    elif len(vals) == arg_count + 1:
        args, check = vals[1:], vals[0]
        return tailcall(proc, args, env, call_cont(check, env, cont))
    else:
        assert False

class W_ImpProcedure(values.W_Procedure):
    errorname = "imp-procedure"
    _immutable_fields_ = ["code", "check"]
    def __init__(self, code, check):
        assert isinstance(code, values.W_Procedure)
        assert isinstance(check, values.W_Procedure)
        self.code  = code
        self.check = check

    def get_arity(self):
        return self.code.get_arity()

    def call(self, args, env, cont):
        from pycket.interpreter import tailcall
        jit.promote(self)
        return tailcall(self.check, args, env,
                imp_proc_cont(len(args), self.code, env, cont))

    def tostring(self):
        return "ImpProcedure<%s>" % self.code.tostring()

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
    from pycket.interpreter import tailcall
    vals = _vals._get_full_list()
    return tailcall(check, vals, env, chp_proc_ret_cont(vals, env, cont))

# Continuation used when calling a chaperone of a procedure.
@continuation
def chp_proc_cont(args, proc, env, cont, _vals):
    from pycket.interpreter import tailcall
    vals = _vals._get_full_list()
    assert len(vals) >= len(args)
    if len(vals) == len(args):
        for i in range(len(args)):
            if not is_chaperone_of(vals[i], args[i]):
                raise SchemeException("Expecting original value or chaperone")
        return tailcall(proc, vals, env, cont)
    elif len(vals) == len(args) + 1:
        vals, check = vals[1:], vals[0]
        for i in range(len(args)):
            if not is_chaperone_of(vals[i], args[i]):
                raise SchemeException("Expecting original value or chaperone")
        return tailcall(proc, vals, env,
                chp_proc_call_check_cont(check, env, cont))
    else:
        assert False

class W_ChpProcedure(values.W_Procedure):
    errorname = "chp-procedure"
    _immutable_fields_ = ["code", "check"]
    def __init__(self, code, check):
        assert isinstance(code, values.W_Procedure)
        assert isinstance(check, values.W_Procedure)
        self.code  = code
        self.check = check

    def get_arity(self):
        return self.code.get_arity()

    def call(self, args, env, cont):
        from pycket.interpreter import tailcall
        jit.promote(self)
        return tailcall(self.check, args, env,
                chp_proc_cont(args, self.code, env, cont))

    def tostring(self):
        return "ChpProcedure<%s>" % self.code.tostring()

# Boxes

@continuation
def chp_unbox_cont(f, box, env, cont, vals):
    from pycket.interpreter import check_one_val, tailcall
    old = check_one_val(vals)
    return tailcall(f, [box, old], env, chp_unbox_cont_ret(old, env, cont))

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
    from pycket.interpreter import check_one_val, jump
    val = check_one_val(vals)
    if not is_chaperone_of(val, orig):
        raise SchemeException("Expecting original value or chaperone")
    return jump(env, b.set_box(val, env, cont))

class W_ChpBox(values.W_Box):
    errorname = "chp-box"
    _immutable_fields_ = ["box", "unbox", "set"]

    def __init__(self, box, unboxh, seth):
        assert isinstance(box, values.W_MBox)
        self.box = box
        self.unboxh = unboxh
        self.seth = seth

    def immutable(self):
        return self.box.immutable()

    @label
    def unbox(self, env, cont):
        from pycket.interpreter import jump
        return jump(env,
                self.box.unbox(env,
                    chp_unbox_cont(self.unboxh, self.box, env, cont)))

    @label
    def set_box(self, val, env, cont):
        from pycket.interpreter import tailcall
        return tailcall(self.seth, [self.box, val], env,
                chp_box_set_cont(self.box, val, env, cont))

    def tostring(self):
        return self.box.tostring()

@continuation
def imp_unbox_cont(f, box, env, cont, vals):
    from pycket.interpreter import check_one_val, tailcall
    return tailcall(f, [box, check_one_val(vals)], env, cont)

@continuation
def imp_box_set_cont(b, env, cont, vals):
    from pycket.interpreter import check_one_val, jump
    return jump(env, b.set_box(check_one_val(vals), env, cont))

class W_ImpBox(values.W_Box):
    errorname = "imp-box"
    _immutable_fields_ = ["box", "unbox", "set"]

    def __init__(self, box, unboxh, seth):
        assert isinstance(box, values.W_Box)
        self.box = box
        self.unboxh = unboxh
        self.seth = seth

    @label
    def unbox(self, env, cont):
        from pycket.interpreter import jump
        return jump(env,
                self.box.unbox(env,
                    imp_unbox_cont(self.unboxh, self.box, env, cont)))

    @label
    def set_box(self, val, env, cont):
        from pycket.interpreter import tailcall
        return tailcall(self.seth, [self.box, val], env,
                imp_box_set_cont(self.box, env, cont))

    def tostring(self):
        return self.box.tostring()

# Vectors
class W_ImpVector(values.W_MVector):
    errorname = "imp-vector"
    _immutable_fields_ = ["vec", "refh", "seth"]
    def __init__(self, v, r, s):
        assert isinstance(v, values.W_MVector)
        self.vec = v
        self.refh = r
        self.seth = s

    def length(self):
        return self.vec.length()

class W_ChpVector(values.W_MVector):
    errorname = "chp-procedure"
    _immutable_fields_ = ["vec", "refh", "seth"]
    def __init__(self, v, r, s):
        assert isinstance(v, values.W_MVector)
        self.vec  = v
        self.refh = r
        self.seth = s

    def length(self):
        return self.vec.length()

    def immutable(self):
        return self.vec.immutable()

# Are we dealing with a struct accessor/mutator/propert accessor or a
# chaperone/impersonator thereof.
def valid_struct_proc(x):
    if isinstance(x, W_ChpProcedure):
        return valid_struct_proc(x.code)
    if isinstance(x, W_ImpProcedure):
        return valid_struct_proc(x.code)
    return (isinstance(x, struct.W_StructFieldAccessor) or
            isinstance(x, struct.W_StructFieldMutator) or
            isinstance(x, struct.W_StructPropertyAccessor))

def get_base_field_accessor(x):
    if (isinstance(x, struct.W_StructFieldAccessor) or
        isinstance(x, struct.W_StructFieldMutator) or
        isinstance(x, struct.W_StructPropertyAccessor)):
        return x
    if isinstance(x, W_ChpProcedure):
        return get_base_field_accessor(x.code)
    if isinstance(x, W_ImpProcedure):
        return get_base_field_accessor(x.code)
    assert False

@continuation
def imp_struct_ref_cont(interp, orig_struct, env, cont, _vals):
    from pycket.interpreter import check_one_val, tailcall
    field_v = check_one_val(_vals)
    return tailcall(interp, [orig_struct, field_v], env, cont)

@continuation
def imp_struct_set_cont(orig_struct, setter, env, cont, _vals):
    from pycket.interpreter import check_one_val, tailcall
    val = check_one_val(_vals)
    return tailcall(setter, [orig_struct, val], env, cont)

# Representation of a struct that allows interposition of operations
# onto accessors/mutators
class W_InterposeStructBase(struct.W_RootStruct):
    _immutable_fields = ["struct", "accessors", "mutators", "handlers"]

    def __init__(self, inner, overrides, handlers):
        assert isinstance(inner, struct.W_RootStruct)
        assert len(overrides) == len(handlers)
        struct.W_RootStruct.__init__(self, inner.type, inner.isopaque)
        self.struct = inner
        self.accessors = {}
        self.mutators = {}
        self.properties = {}
        # Does not deal with properties as of yet
        for i, op in enumerate(overrides):
            base = get_base_field_accessor(op)
            if isinstance(base, struct.W_StructFieldAccessor):
                self.accessors[base.field.value] = (op, handlers[i])
            elif isinstance(base, struct.W_StructFieldMutator):
                self.mutators[base.field.value] = (op, handlers[i])
            else:
                assert False

    @label
    def ref(self, struct_id, field, env, cont):
        raise NotImplementedError("abstract base class")

    @label
    def set(self, struct_id, field, val, env, cont):
        raise NotImplementedError("abstract base class")

    def vals(self):
        return self.struct.vals()

# Need to add checks that we are only impersonating mutable fields
class W_ImpStruct(W_InterposeStructBase):

    @label
    def ref(self, struct_id, field, env, cont):
        from pycket.interpreter import jump, tailcall
        (op, interp) = self.accessors.get(field, (None, None))
        if interp is None:
            return jump(env, self.struct.ref(struct_id, field, env, cont))
        after = imp_struct_ref_cont(interp, self.struct, env, cont)
        return tailcall(op, [self.struct], env, after)

    @label
    def set(self, struct_id, field, val, env, cont):
        from pycket.interpreter import jump, tailcall
        (op, interp) = self.mutators.get(field, (None, None))
        if interp is None or op is None:
            return jump(env, self.struct.set(struct_id, field, val, env, cont))
        return tailcall(interp, [self.struct, val], env,
                imp_struct_set_cont(self.struct, op, env, cont))

@continuation
def chp_struct_ref_cont(interp, orig_struct, env, cont, _vals):
    from pycket.interpreter import check_one_val, tailcall
    field_v = check_one_val(_vals)
    return tailcall(interp, [orig_struct, field_v], env,
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
    from pycket.interpreter import check_one_val, tailcall
    val = check_one_val(_vals)
    if not is_chaperone_of(val, orig_val):
        raise SchemeException("chaperone handlers must produce chaperone of original value")
    return tailcall(setter, [orig_struct, val], env, cont)

class W_ChpStruct(W_InterposeStructBase):

    @label
    def ref(self, struct_id, field, env, cont):
        from pycket.interpreter import jump, tailcall
        (op, interp) = self.accessors.get(field, (None, None))
        if interp is None:
            return jump(env, self.struct.ref(struct_id, field, env, cont))
        after = chp_struct_ref_cont(interp, self.struct, env, cont)
        return tailcall(op, [self.struct], env, after)

    @label
    def set(self, struct_id, field, val, env, cont):
        from pycket.interpreter import jump, tailcall
        (op, interp) = self.mutators.get(field, (None, None))
        if interp is None or op is None:
            return jump(env, self.struct.set(struct_id, field, val, env, cont))
        return tailcall(interp, [self.struct, val], env,
                chp_struct_set_cont(self.struct, op, val, env, cont))

