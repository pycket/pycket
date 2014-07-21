#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont import continuation, call_cont, jump_call
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
    return False

def is_chaperone(x):
    return (isinstance(x, W_ChpVector) or
            isinstance(x, W_ChpBox) or
            isinstance(x, W_ChpProcedure))

def is_impersonator(x):
    return (isinstance(x, W_ImpVector) or
            isinstance(x, W_ImpBox) or
            isinstance(x, W_ImpProcedure) or
            is_chaperone(x))

# Procedures

# Continuation used when calling an impersonator of a procedure.
@continuation
def imp_proc_cont(arg_count, proc, env, cont, _vals):
    from pycket.interpreter import jump
    vals = _vals._get_full_list()
    if len(vals) == arg_count:
        return jump(env, jump_call(proc, vals, env, cont))
    elif len(vals) == arg_count + 1:
        args, check = vals[:-1], vals[-1]
        return jump(env,
                jump_call(proc, vals, env,
                    call_cont(check, env, cont)))
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
        from pycket.interpreter import jump
        jit.promote(self)
        return jump(env,
                jump_call(self.check, args, env,
                    imp_proc_cont(len(args), self.code, env, cont)))

    def equal(self, other):
        if not isinstance(other, values.W_Procedure):
            return False
        # We are the same procedure if we have the same identity or
        # our underlying procedure is equal to our partner.
        return self is other or other.equal(self.code)

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
    from pycket.interpreter import jump
    vals = _vals._get_full_list()
    return jump(env, jump_call(check, vals, env, chp_proc_ret_cont(vals, env, cont)))

# Continuation used when calling a chaperone of a procedure.
@continuation
def chp_proc_cont(args, proc, env, cont, _vals):
    from pycket.interpreter import jump
    vals = _vals._get_full_list()
    assert len(vals) >= len(args)
    for i in range(len(args)):
        if not is_chaperone_of(vals[i], args[i]):
            raise SchemeException("Expecting original value or chaperone")
    if len(vals) == len(args):
        return proc.call(vals, env, cont)
    elif len(vals) == len(args) + 1:
        args, check = vals[:-1], vals[-1]
        return jump(env,
                jump_call(proc, args, env,
                    chp_proc_call_check_cont(check, env, cont)))
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
        from pycket.interpreter import jump
        jit.promote(self)
        return jump(env,
                jump_call(self.check, args, env,
                    chp_proc_cont(args, self.code, env, cont)))

    def equal(self, other):
        if not isinstance(other, values.W_Procedure):
            return False

        return self is other or other.equal(self.code)

    def tostring(self):
        return "ChpProcedure<%s>" % self.code.tostring()

# Boxes

class W_ChpBox(values.W_Box):
    errorname = "chp-box"
    _immutable_fields_ = ["box", "unbox", "set"]

    def __init__(self, box, unbox, set):
        assert isinstance(box, values.W_MBox)
        self.box = box
        self.unbox = unbox
        self.set = set

    def immutable(self):
        return self.box.immutable()

class W_ImpBox(values.W_Box):
    errorname = "imp-box"
    _immutable_fields_ = ["box", "unbox", "set"]

    def __init__(self, box, unbox, set):
        assert isinstance(box, values.W_Box)
        self.box = box
        self.unbox = unbox
        self.set = set

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

    def equal(self, other):
        if not isinstance(other, values.W_MVector):
            return False
        if self is other:
            return True
        if self.vec is other:
            return True
        if self.length() != other.length():
            return False
        for i in range(self.length()):
            # FIXME: we need to call user code here
            if not self.vec.ref(i).equal(other.ref(i)):
                return False
        return True

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

    def equal(self, other):
        if not isinstance(other, values.W_MVector):
            return False
        if self is other:
            return True
        if self.length() != other.length():
            return False
        for i in range(self.length()):
            if not self.vec.ref(i).equal(other.ref(i)):
                return False
        return True

def valid_struct_proc(x):
    return type(x) in [struct.W_StructFieldAccessor,
                      struct.W_StructFieldMutator,
                      struct.W_StructPropertyAccessor]
    #return (isinstance(x, struct.W_StructFieldAccessor) or
            #isinstance(x, struct.W_StructFieldMutator) or
            #isinstance(x, struct.W_StructPropertyAccessor))

@continuation
def imp_struct_ref_cont(interp, orig_struct, env, cont, _vals):
    from pycket.interpreter import check_one_val, jump
    field_v = check_one_val(_vals)
    return jump(env, jump_call(interp, [orig_struct, field_v], env, cont))

@continuation
def imp_struct_set_cont(orig_struct, struct_id, field, env, cont, _vals):
    from pycket.interpreter import check_one_val, jump
    return jump(env,
            orig_struct.set(struct_id, field, check_one_val(_vals), env, cont))

class W_ImpStruct(struct.W_Struct):
    _immutable_fields = ["struct", "accessors", "mutators", "handlers"]

    def __init__(self, struct, overrides, handlers):
        assert isinstance(struct, struct.W_Struct)
        assert all([valid_struct_proc(i) for i in overrides])
        assert all([isinstance(i, values.W_Procedure) for i in handlers])
        assert len(overrides) == len(handlers)
        self.struct = struct
        self.accessors = {}
        self.mutators = {}
        for i, op in enumerate(overrides):
            if isinstance(op, struct.W_StructFieldAccessor):
                self.accessors[op._field] = handlers[i]
            elif isinstance(op, struct.W_StructFieldMutator):
                self.mutators[op._field] = handlers[i]
            else:
                assert False

    @continuation
    def ref(self, struct_id, field, env, cont, _vals):
        from pycket.interpreter import jump
        interp = self.accessors.get(field)
        after = cont if inter is None else imp_struct_ref_cont(interp, self.struct, env, cont)
        return jump(env, self.struct.ref(struct_id, field, env, after))

    @continuation
    def set(self, struct_id, field, val, env, cont, _vals):
        from pycket.interpreter import jump
        interp = self.mutators.get(field)
        if interp is not None:
            return jump(env,
                    jump_call(interp, [self.struct, val], env,
                        imp_struct_set_cont(self.struct, struct_id, field, env, cont)))
        return jump(env, self.struct.set(struct_id, field, val, env, cont))


