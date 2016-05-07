
from pycket              import impersonators as imp
from pycket              import values
from pycket              import values_struct
from pycket.arity        import Arity
from pycket.hash.base    import W_HashTable
from pycket.error        import SchemeException
from pycket.prims.expose import expose, expose_val
from pycket.prims.equal  import equal_func, EqualInfo
from rpython.rlib        import jit

expose_val("impersonator-prop:application-mark", imp.w_impersonator_prop_application_mark)

# Used to find the first impersonator-property
@jit.unroll_safe
def find_prop_start_index(args):
    for i, v in enumerate(args):
        if isinstance(v, imp.W_ImpPropertyDescriptor):
            return i
    return len(args)

@jit.unroll_safe
def unpack_properties(args, name):
    idx = find_prop_start_index(args)

    if idx == len(args):
        props    = None
        prop_len = 0
    else:
        args, props = args[:idx], args[idx:]
        prop_len = len(props)

    if prop_len % 2 != 0:
        raise SchemeException(name + ": not all properties have corresponding values")

    count = prop_len / 2

    # Avoid allocation in the event that we don't need to do anything
    if prop_len == 0:
        count = 0
        prop_keys = None
        prop_vals = None
    else:
        count = prop_len / 2
        prop_keys = [None] * count
        prop_vals = [None] * count

    for i in range(count):
        key = props[i*2]
        prop_keys[i] = key
        prop_vals[i] = props[i*2+1]

        if not isinstance(key, imp.W_ImpPropertyDescriptor):
            desc = "%s: %s is not a property descriptor" % (name, key.tostring())
            raise SchemeException(desc)

    return args, prop_keys, prop_vals

def unpack_vector_args(args, name):
    args, prop_keys, prop_vals = unpack_properties(args, name)
    if len(args) != 3:
        raise SchemeException(name + ": not given 3 required arguments")
    v, refh, seth = args
    if not isinstance(v, values.W_MVector):
        raise SchemeException(name + ": first arg not a vector")
    if not refh.iscallable() or not seth.iscallable():
        raise SchemeException(name + ": provided handler is not callable")

    return v, refh, seth, prop_keys, prop_vals

def unpack_procedure_args(args, name):
    args, prop_keys, prop_vals = unpack_properties(args, name)
    if len(args) != 2:
        raise SchemeException(name + ": not given 2 required arguments")
    proc, check = args
    if not proc.iscallable():
        raise SchemeException(name + ": first argument is not a procedure")
    if check is not values.w_false and not check.iscallable():
        raise SchemeException(name + ": handler is not a procedure or #f")
    return proc, check, prop_keys, prop_vals

def unpack_box_args(args, name):
    args, prop_keys, prop_vals = unpack_properties(args, name)
    if len(args) != 3:
        raise SchemeException(name + ": not given three required arguments")
    box, unboxh, seth = args
    if not unboxh.iscallable():
        raise SchemeException(name + ": supplied unbox handler is not callable")
    if not seth.iscallable():
        raise SchemeException(name + ": supplied set-box! handler is not callable")
    return box, unboxh, seth, prop_keys, prop_vals

def unpack_cmk_args(args, name):
    args, prop_keys, prop_vals = unpack_properties(args, name)
    if len(args) != 3:
        raise SchemeException(name + ": not give three required arguments")
    key, get, set = args
    if not isinstance(key, values.W_ContinuationMarkKey):
        raise SchemeException(name + ": supplied key is not a continuation-mark-key")
    if not get.iscallable():
        raise SchemeException(name + ": supplied get-proc is not callable")
    if not set.iscallable():
        raise SchemeException(name + ": supplied set-proc is not callable")
    return key, get, set, prop_keys, prop_vals

def unpack_hash_args(args, name):
    args, prop_keys, prop_vals = unpack_properties(args, name)
    clear_proc = values.w_false
    if len(args) == 5:
        hash, ref_proc, set_proc, remove_proc, key_proc = args
    elif len(args) == 6:
        hash, ref_proc, set_proc, remove_proc, key_proc, clear_proc = args
    else:
        raise SchemeException(name + ": wrong number of arguments")
    if not isinstance(hash, W_HashTable):
        raise SchemeException(name + ": first argument is not a hash")
    if not ref_proc.iscallable():
        raise SchemeException(name + ": ref-proc is not callable")
    if not set_proc.iscallable():
        raise SchemeException(name + ": set-proc is not callable")
    if not remove_proc.iscallable():
        raise SchemeException(name + ": remove-proc is not callable")
    if not key_proc.iscallable():
        raise SchemeException(name + ": key-proc is not callable")
    if clear_proc is not values.w_false and not clear_proc.iscallable():
        raise SchemeException(name + ": clear-proc is not callable")
    return hash, ref_proc, set_proc, remove_proc, key_proc, clear_proc, prop_keys, prop_vals

@expose("impersonate-hash", arity=Arity.geq(5))
def impersonate_hash(args):
    unpacked = unpack_hash_args(args, "impersonate-hash")
    if unpacked[0].immutable():
        raise SchemeException("impersonate-hash: cannot impersonate immutable hash")
    return imp.W_ImpHashTable(*unpacked)

@expose("chaperone-hash", arity=Arity.geq(5))
def chaperone_hash(args):
    unpacked = unpack_hash_args(args, "chaperone-hash")
    return imp.W_ImpHashTable(*unpacked)

@expose("impersonate-procedure", arity=Arity.geq(2))
def impersonate_procedure(args):
    proc, check, keys, vals = unpack_procedure_args(args, "impersonate-procedure")
    if check is values.w_false and not keys:
        return proc
    return imp.make_interpose_procedure(imp.W_ImpProcedure, proc, check, keys, vals)

@expose("unsafe-impersonate-procedure", arity=Arity.geq(2))
def impersonate_procedure(args):
    proc, check, keys, vals = unpack_procedure_args(args, "unsafe-impersonate-procedure")
    if check is values.w_false and not keys:
        return proc
    return imp.make_interpose_procedure(imp.W_UnsafeImpProcedure, proc, check, keys, vals)

@expose("impersonate-procedure*", arity=Arity.geq(2))
def impersonate_procedure_star(args):
    proc, check, keys, vals = unpack_procedure_args(args, "impersonate-procedure*")
    if check is values.w_false and not keys:
        return proc
    return imp.make_interpose_procedure(imp.W_ImpProcedureStar, proc, check, keys, vals)

@expose("chaperone-procedure", arity=Arity.geq(2))
def chaperone_procedure(args):
    proc, check, keys, vals = unpack_procedure_args(args, "chaperone-procedure")
    if check is values.w_false and not keys:
        return proc
    return imp.make_interpose_procedure(imp.W_ChpProcedure, proc, check, keys, vals)

@expose("unsafe-chaperone-procedure", arity=Arity.geq(2))
def chaperone_procedure(args):
    proc, check, keys, vals = unpack_procedure_args(args, "unsafe-chaperone-procedure")
    if check is values.w_false and not keys:
        return proc
    return imp.make_interpose_procedure(imp.W_UnsafeChpProcedure, proc, check, keys, vals)

@expose("chaperone-procedure*", arity=Arity.geq(2))
def chaperone_procedure_star(args):
    proc, check, keys, vals = unpack_procedure_args(args, "chaperone-procedure*")
    if check is values.w_false and not keys:
        return proc
    return imp.make_interpose_procedure(imp.W_ChpProcedureStar, proc, check, keys, vals)

@expose("impersonate-vector", arity=Arity.geq(3))
def impersonate_vector(args):
    unpacked = unpack_vector_args(args, "impersonate-vector")
    if unpacked[0].immutable():
        raise SchemeException("impersonate-vector: cannot impersonate immutable vector")
    return imp.make_interpose_vector(imp.W_ImpVector, *unpacked)

@expose("chaperone-vector", arity=Arity.geq(3))
def chaperone_vector(args):
    unpacked = unpack_vector_args(args, "chaperone-vector")
    return imp.make_interpose_vector(imp.W_ChpVector, *unpacked)

# Need to check that fields are mutable
@expose("impersonate-struct", arity=Arity.geq(1))
@jit.unroll_safe
def impersonate_struct(args):
    if len(args) == 1 and isinstance(args[0], values_struct.W_RootStruct):
        return args[0]

    args, prop_keys, prop_vals = unpack_properties(args, "impersonate-struct")

    if len(args) < 1:
        raise SchemeException("impersonate-struct: arity mismatch")

    struct, args = args[0], args[1:]

    if args and isinstance(args[0], values_struct.W_StructType):
        args = args[1:]

    if len(args) % 2 != 0:
        raise SchemeException("impersonate-struct: arity mismatch")

    if not isinstance(struct, values_struct.W_RootStruct):
        raise SchemeException("impersonate-struct: not given struct")

    struct_type = struct.struct_type()
    assert isinstance(struct_type, values_struct.W_StructType)

    # Consider storing immutables in an easier form in the structs implementation
    immutables = struct_type.immutables

    all_false = True
    count     = len(args) / 2
    overrides = [None] * count
    handlers  = [None] * count
    for i in range(count):
        ovr = args[i * 2]
        hnd = args[i * 2 + 1]

        overrides[i] = ovr
        handlers[i]  = hnd

        if not imp.valid_struct_proc(ovr):
            raise SchemeException("impersonate-struct: not given valid field accessor")
        elif (isinstance(ovr, values_struct.W_StructFieldMutator) and
                ovr.field in immutables):
            raise SchemeException("impersonate-struct: cannot impersonate immutable field")
        elif (isinstance(ovr, values_struct.W_StructFieldAccessor) and
                ovr.field in immutables):
            raise SchemeException("impersonate-struct: cannot impersonate immutable field")

        if hnd is not values.w_false:
            all_false = False
        if not hnd.iscallable() and hnd is not values.w_false:
            raise SchemeException("impersonate-struct: supplied hander is not a procedure")

    if all_false and not prop_keys:
        return struct

    return imp.make_struct_proxy(imp.W_ImpStruct,
            struct, overrides, handlers, prop_keys, prop_vals)

@expose("chaperone-struct", arity=Arity.geq(1))
@jit.unroll_safe
def chaperone_struct(args):
    from pycket.prims.struct_structinfo import struct_info
    if len(args) == 1 and isinstance(args[0], values_struct.W_RootStruct):
        return args[0]

    args, prop_keys, prop_vals = unpack_properties(args, "chaperone-struct")
    if len(args) < 1:
        raise SchemeException("chaperone-struct: arity mismatch")

    struct, args = args[0], args[1:]

    if args and isinstance(args[0], values_struct.W_StructType):
        args = args[1:]

    if len(args) % 2 != 0:
        raise SchemeException("chaperone-struct: arity mismatch")

    if not isinstance(struct, values_struct.W_RootStruct):
        raise SchemeException("chaperone-struct: not given struct")

    all_false = True
    count     = len(args) / 2
    overrides = [None] * count
    handlers  = [None] * count
    for i in range(count):
        ovr = args[i * 2]
        hnd = args[i * 2 + 1]

        overrides[i] = ovr
        handlers[i]  = hnd
        if not imp.valid_struct_proc(ovr) and ovr is not struct_info:
            raise SchemeException("chaperone-struct: not given valid field accessor")
        if hnd is not values.w_false:
            all_false = False
        if not hnd.iscallable() and hnd is not values.w_false:
            raise SchemeException("chaperone-struct: supplied hander is not a procedure")

    if all_false and not prop_keys:
        return struct

    return imp.make_struct_proxy(imp.W_ChpStruct,
            struct, overrides, handlers, prop_keys, prop_vals)

@expose("chaperone-box", arity=Arity.geq(3))
def chaperone_box(args):
    unpacked = unpack_box_args(args, "chaperone-box")
    return imp.W_ChpBox(*unpacked)

@expose("impersonate-box", arity=Arity.geq(3))
def impersonate_box(args):
    unpacked = unpack_box_args(args, "impersonate-box")
    if unpacked[0].immutable():
        raise SchemeException("Cannot impersonate immutable box")
    return imp.W_ImpBox(*unpacked)

@expose("chaperone-continuation-mark-key")
def ccmk(args):
    unpacked = unpack_cmk_args(args, "chaperone-continuation-mark-key")
    return imp.W_ChpContinuationMarkKey(*unpacked)

@expose("impersonate-continuation-mark-key")
def icmk(args):
    unpacked = unpack_cmk_args(args, "impersonate-continuation-mark-key")
    return imp.W_ImpContinuationMarkKey(*unpacked)

@expose(["chaperone-struct-type", "impersonate-struct-type"])
def cst(args):
    # XXX Not correct
    return args[0]

@expose("chaperone-of?", [values.W_Object, values.W_Object], simple=False)
def chaperone_of(a, b, env, cont):
    info = EqualInfo.CHAPERONE_SINGLETON
    return equal_func(a, b, info, env, cont)

@expose("impersonator-of?", [values.W_Object, values.W_Object], simple=False)
def impersonator_of(a, b, env, cont):
    info = EqualInfo.IMPERSONATOR_SINGLETON
    return equal_func(a, b, info, env, cont)

@expose("impersonator?", [values.W_Object])
def impersonator(x):
    return values.W_Bool.make(x.is_impersonator())

@expose("chaperone?", [values.W_Object])
def chaperone(x):
    return values.W_Bool.make(x.is_chaperone())

@expose("make-impersonator-property", [values.W_Symbol], simple=False)
def make_imp_prop(sym, env, cont):
    from pycket.interpreter import return_multi_vals
    name = sym.utf8value
    prop = imp.W_ImpPropertyDescriptor(name)
    pred = imp.W_ImpPropertyPredicate(prop)
    accs = imp.W_ImpPropertyAccessor(prop)
    return return_multi_vals(values.Values.make([prop, pred, accs]), env, cont)

