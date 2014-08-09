#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket.cont import continuation, label, call_cont
from pycket.error import SchemeException
from pycket import values
from pycket import values_struct
from rpython.rlib import jit

# Check that one value is an impersonator of the other
@jit.unroll_safe
def is_impersonator_of(a, b):
    while True:
        if a is b:
            return True
        elif isinstance(a, W_ImpVector):
            a = a.vec
        elif isinstance(a, W_ImpProcedure):
            a = a.code
        elif isinstance(a, W_ImpBox):
            a = a.box
        elif isinstance(a, W_ImpStruct):
            a = a.struct
        else:
            return is_chaperone_of(a, b)

# Check that one value is a chaperone of the other
@jit.unroll_safe
def is_chaperone_of(a, b):
    while True:
        if a is b:
            return True
        elif isinstance(a, W_ChpVector):
            a = a.vec
        elif isinstance(a, W_ChpProcedure):
            a = a.code
        elif isinstance(a, W_ChpBox):
            a = a.box
        elif isinstance(a, W_ChpStruct):
            a = a.struct
        else:
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
    vals = _vals._get_full_list()
    if len(vals) == arg_count:
        return proc.call(vals, env, cont)
    elif len(vals) == arg_count + 1:
        args, check = vals[1:], vals[0]
        return proc.call(args, env, call_cont(check, env, cont))
    else:
        assert False

class W_ImpProcedure(values.W_Object):
    errorname = "imp-procedure"
    _immutable_fields_ = ["code", "check"]
    def __init__(self, code, check):
        assert code.iscallable()
        assert check.iscallable()
        self.code  = code
        self.check = check

    def iscallable(self):
        return True

    def get_arity(self):
        return self.code.get_arity()

    def call(self, args, env, cont):
        jit.promote(self)
        return self.check.call(args, env,
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

class W_ChpProcedure(values.W_Object):
    errorname = "chp-procedure"
    _immutable_fields_ = ["code", "check"]
    def __init__(self, code, check):
        assert code.iscallable()
        assert check.iscallable()
        self.code  = code
        self.check = check

    def iscallable(self):
        return True

    def get_arity(self):
        return self.code.get_arity()

    def call(self, args, env, cont):
        jit.promote(self)
        return self.check.call(args, env,
                chp_proc_cont(args, self.code, env, cont))

    def tostring(self):
        return "ChpProcedure<%s>" % self.code.tostring()

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
        return self.seth.call([self.box, val], env,
                chp_box_set_cont(self.box, val, env, cont))

    def tostring(self):
        return self.box.tostring()

@continuation
def imp_unbox_cont(f, box, env, cont, vals):
    from pycket.interpreter import check_one_val
    return f.call([box, check_one_val(vals)], env, cont)

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
        return self.seth.call([self.box, val], env,
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
    v = get_base_procedure(x)
    return (isinstance(v, values_struct.W_StructFieldAccessor) or
            isinstance(v, values_struct.W_StructFieldMutator) or
            isinstance(v, values_struct.W_StructPropertyAccessor))

@jit.unroll_safe
def get_base_procedure(x):
    while True:
        if isinstance(x, W_ChpProcedure):
            x = x.code
        elif isinstance(x, W_ImpProcedure):
            x = x.code
        else:
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
    _immutable_fields = ["struct", "accessors", "mutators", "handlers"]

    def __init__(self, inner, overrides, handlers):
        assert isinstance(inner, values_struct.W_RootStruct)
        assert len(overrides) == len(handlers)
        self.struct = inner
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

    @staticmethod
    def post_ref_cont():
        raise NotImplementedError("abstract method")

    @staticmethod
    def post_set_cont():
        raise NotImplementedError("abstract method")

    def _ref(self, value):
        return self.struct._ref(value)

    def _set(self, key, val):
        return self.struct._set(key, val)

    @jit.elidable
    def struct_type(self):
        struct = self.struct
        while isinstance(struct, W_InterposeStructBase):
            struct = struct.struct
        return struct.struct_type()

    @label
    def ref(self, struct_id, field, env, cont):
        from pycket.interpreter import jump
        (op, interp) = self.accessors.get(field, (None, None))
        if interp is None:
            return jump(env, self.struct.ref(struct_id, field, env, cont))
        after = self.post_ref_cont()(interp, self.struct, env, cont)
        return op.call([self.struct], env, after)

    @label
    def set(self, struct_id, field, val, env, cont):
        from pycket.interpreter import jump
        (op, interp) = self.mutators.get(field, (None, None))
        if interp is None or op is None:
            return jump(env, self.struct.set(struct_id, field, val, env, cont))
        return interp.call([self.struct, val], env,
                self.post_set_cont()(self.struct, op, val, env, cont))

    def vals(self):
        return self.struct.vals()

# Need to add checks that we are only impersonating mutable fields
class W_ImpStruct(W_InterposeStructBase):

    @staticmethod
    @jit.elidable
    def post_ref_cont():
        return imp_struct_ref_cont

    @staticmethod
    @jit.elidable
    def post_set_cont():
        return imp_struct_set_cont


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

class W_ChpStruct(W_InterposeStructBase):

    @staticmethod
    @jit.elidable
    def post_ref_cont():
        return chp_struct_ref_cont

    @staticmethod
    @jit.elidable
    def post_set_cont():
        return chp_struct_set_cont

