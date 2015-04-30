#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os
import time
from pycket import impersonators as imp
from pycket import values, values_string
from pycket.cont import continuation, loop_label, call_cont
from pycket import cont
from pycket import values_parameter
from pycket import values_struct
from pycket import values_hash
from pycket import values_regex
from pycket import vector as values_vector
from pycket.error import SchemeException
from pycket.prims.expose import (unsafe, default, expose, expose_val,
                                 procedure, make_call_method, define_nyi,
                                 subclass_unsafe)
from rpython.rlib import jit
from rpython.rlib.rbigint import rbigint
from rpython.rlib.rsre import rsre_re as re

# import for side effects
from pycket.prims import continuation_marks
from pycket.prims import equal as eq_prims
from pycket.prims import foreign
from pycket.prims import hash
from pycket.prims import impersonator
from pycket.prims import input_output
from pycket.prims import numeric
from pycket.prims import parameter
from pycket.prims import random
from pycket.prims import regexp
from pycket.prims import string
from pycket.prims import struct_structinfo
from pycket.prims import undefined
from pycket.prims import vector

from rpython.rlib import jit

def make_pred(name, cls):
    @expose(name, [values.W_Object], simple=True)
    def predicate_(a):
        return values.W_Bool.make(isinstance(a, cls))
    predicate_.__name__ +=  cls.__name__

def make_pred_eq(name, val):
    typ = type(val)
    @expose(name, [values.W_Object], simple=True)
    def pred_eq(a):
        return values.W_Bool.make(isinstance(a, typ) and a is val)

for args in [
        ("output-port?", values.W_OutputPort),
        ("input-port?", values.W_InputPort),
        ("pair?", values.W_Cons),
        ("mpair?", values.W_MCons),
        ("number?", values.W_Number),
        ("complex?", values.W_Number),
        ("fixnum?", values.W_Fixnum),
        ("flonum?", values.W_Flonum),
        ("vector?", values.W_MVector),
        ("string?", values_string.W_String),
        ("symbol?", values.W_Symbol),
        ("boolean?", values.W_Bool),
        ("inspector?", values_struct.W_StructInspector),
        ("struct-type?", values_struct.W_StructType),
        ("struct-constructor-procedure?", values_struct.W_StructConstructor),
        ("struct-predicate-procedure?", values_struct.W_StructPredicate),
        ("struct-type-property?", values_struct.W_StructProperty),
        ("struct-type-property-accessor-procedure?",
         values_struct.W_StructPropertyAccessor),
        ("box?", values.W_Box),
        ("regexp?", values_regex.W_Regexp),
        ("pregexp?", values_regex.W_PRegexp),
        ("byte-regexp?", values_regex.W_ByteRegexp),
        ("byte-pregexp?", values_regex.W_BytePRegexp),
        ("variable-reference?", values.W_VariableReference),
        ("syntax?", values.W_Syntax),
        ("thread-cell?", values.W_ThreadCell),
        ("thread-cell-values?", values.W_ThreadCellValues),
        ("semaphore?", values.W_Semaphore),
        ("semaphore-peek-evt?", values.W_SemaphorePeekEvt),
        ("path?", values.W_Path),
        ("bytes?", values.W_Bytes),
        ("pseudo-random-generator?", values.W_PseudoRandomGenerator),
        ("char?", values.W_Character),
        ("continuation?", values.W_Continuation),
        ("continuation-mark-set?", values.W_ContinuationMarkSet),
        ("continuation-mark-key?", values.W_ContinuationMarkKey),
        ("primitive?", values.W_Prim),
        ("keyword?", values.W_Keyword),
        ("weak-box?", values.W_WeakBox),
        ("ephemeron?", values.W_Ephemeron),
        ("placeholder?", values.W_Placeholder),
        ("hash-placeholder?", values.W_HashTablePlaceholder),
        ("module-path-index?", values.W_ModulePathIndex),
        ("resolved-module-path?", values.W_ResolvedModulePath),
        ("impersonator-property-accessor-procedure?",
         imp.W_ImpPropertyAccessor),
        ("impersonator-property?", imp.W_ImpPropertyDescriptor),
        ("parameter?", values_parameter.W_BaseParameter),
        ("parameterization?", values_parameter.W_Parameterization),
        # FIXME: Assumes we only have eq-hashes
        # XXX tests tests tests tests!
        ("hash?", values_hash.W_HashTable),
        ("hash-eq?", values_hash.W_HashTable),
        ("hash-eqv?", values_hash.W_HashTable),
        ("hash-equal?", values_hash.W_HashTable),
        ("hash-weak?", values_hash.W_HashTable)
        ]:
    make_pred(*args)

for args in [
        ("void?", values.w_void),
        ("false?", values.w_false),
        ("null?", values.w_null),
        ]:
    make_pred_eq(*args)

@expose("byte?", [values.W_Object])
def byte_huh(val):
    if isinstance(val, values.W_Fixnum):
        return values.W_Bool.make(0 <= val.value <= 255)
    return values.w_false

@expose("procedure?", [values.W_Object])
def procedurep(n):
    return values.W_Bool.make(n.iscallable())

@expose("syntax-original?", [values.W_Syntax])
def syntax_original(v):
    return values.w_false

@expose("syntax-tainted?", [values.W_Syntax])
def syntax_tainted(v):
    return values.w_false

@expose("syntax->datum", [values.W_Syntax])
def syntax_to_datum(stx):
    return stx.val

# FIXME: not implemented
@expose("datum->syntax", [values.W_Object, values.W_Object,
  default(values.W_Object, None), default(values.W_Object, None),
  default(values.W_Object, None)])
def datum_to_syntax(ctxt, v, srcloc, prop, ignored):
    print "NOT YET IMPLEMENTED: datum->syntax"
    assert isinstance(ctxt, values.W_Syntax) or ctxt is values.w_false
    return values.W_Syntax(v)

@expose("syntax-source", [values.W_Syntax])
def syntax_source(stx):
    # XXX Obviously not correct
    return values.w_false

@expose("syntax-source-module", [values.W_Syntax, default(values.W_Object, values.w_false)])
def syntax_source_module(stx, src):
    # XXX Obviously not correct
    return values.w_false

@expose(["syntax-line", "syntax-column", "syntax-position", "syntax-span"], [values.W_Syntax])
def syntax_numbers(stx):
    # XXX Obviously not correct
    return values.w_false

@expose("compiled-module-expression?", [values.W_Object])
def compiled_module_expression(v):
    return values.w_false

expose_val("null", values.w_null)
expose_val("true", values.w_true)
expose_val("false", values.w_false)
expose_val("break-enabled-key", values.break_enabled_key)
expose_val("exception-handler-key", values.exn_handler_key)

# FIXME: need stronger guards for all of these
for name in ["prop:evt",
             "prop:output-port",
             "prop:impersonator-of",
             "prop:method-arity-error",
             "prop:exn:srclocs",
             "prop:custom-print-quotable"]:
    expose_val(name, values_struct.W_StructProperty(
        values.W_Symbol.make(name), values.w_false))

expose_val("prop:procedure", values_struct.w_prop_procedure)
expose_val("prop:checked-procedure", values_struct.w_prop_checked_procedure)
expose_val("prop:arity-string", values_struct.w_prop_arity_string)
expose_val("prop:incomplete-arity", values_struct.w_prop_incomplete_arity)
expose_val("prop:custom-write", values_struct.w_prop_custom_write)
expose_val("prop:equal+hash", values_struct.w_prop_equal_hash)
expose_val("prop:chaperone-unsafe-undefined",
           values_struct.w_prop_chaperone_unsafe_undefined)
expose_val("prop:set!-transformer", values_struct.w_prop_set_bang_transformer)
expose_val("prop:rename-transformer", values_struct.w_prop_rename_transformer)

@expose("raise-type-error", [values.W_Symbol, values_string.W_String, values.W_Object])
def raise_type_error(name, expected, v):
    raise SchemeException("%s: expected %s in %s" % (name.tostring(), expected.tostring(), v.tostring()))

@continuation
def check_cont(proc, v, v1, v2, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    val = check_one_val(_vals)
    if val is not values.w_false:
        return return_value(v._ref(1), env, cont)
    return proc.call([v, v1, v2], env, cont)

@continuation
def receive_first_field(proc, v, v1, v2, env, cont, _vals):
    from pycket.interpreter import check_one_val
    first_field = check_one_val(_vals)
    return first_field.call([v1, v2], env,
                            check_cont(proc, v, v1, v2, env, cont))

@expose("checked-procedure-check-and-extract",
        [values_struct.W_StructType, values.W_Object, procedure,
         values.W_Object, values.W_Object], simple=False)
def do_checked_procedure_check_and_extract(type, v, proc, v1, v2, env, cont):
    from pycket.interpreter import check_one_val, return_value
    if isinstance(v, values_struct.W_RootStruct):
        struct_type = v.struct_type()
        while isinstance(struct_type, values_struct.W_StructType):
            if struct_type is type:
                return return_value(v._ref(0), env,
                    receive_first_field(proc, v, v1, v2, env, cont))
            struct_type = struct_type.super
    return proc.call([v, v1, v2], env, cont)

################################################################
# printing

@expose("current-logger", [])
def current_logger():
    return values.current_logger

@expose("make-logger", [values.W_Symbol, values.W_Logger])
def make_logger(name, parent):
    return values.W_Logger()

@expose("system-library-subpath", [default(values.W_Object, values.w_false)])
def sys_lib_subpath(mode):
    return values.W_Path("x86_64-linux") # FIXME

@expose("primitive-closure?", [values.W_Object])
def prim_clos(v):
    return values.w_false

################################################################
# built-in struct types

def define_struct(name, super=values.w_null, fields=[]):
    immutables = []
    for i in range(len(fields)):
        immutables.append(values.W_Fixnum(i))
    struct_type, struct_constr, struct_pred, struct_acc, struct_mut = \
        values_struct.W_StructType.make_simple(values.W_Symbol.make(name),
            super, values.W_Fixnum(len(fields)), values.W_Fixnum(0),
            values.w_false, values.w_null, values.w_false, values.w_false,
            values.to_list(immutables)).make_struct_tuple()
    expose_val("struct:" + name, struct_type)
    expose_val(name, struct_constr)
    # this is almost always also provided
    expose_val("make-" + name, struct_constr)
    expose_val(name + "?", struct_pred)
    for field, field_name in enumerate(fields):
        w_num = values.W_Fixnum(field)
        w_name =  values.W_Symbol.make(field_name)
        acc = values_struct.W_StructFieldAccessor(struct_acc, w_num, w_name)
        expose_val(name + "-" + field_name, acc)
    return struct_type


exn = \
    define_struct("exn", values.w_null, ["message", "continuation-marks"])
exn_fail = \
    define_struct("exn:fail", exn)
exn_fail_contract = \
    define_struct("exn:fail:contract", exn_fail)
exn_fail_contract_arity = \
    define_struct("exn:fail:contract:arity", exn_fail)
exn_fail_contract_divide_by_zero = \
    define_struct("exn:fail:contract:divide-by-zero", exn_fail)
exn_fail_contract_non_fixnum_result = \
    define_struct("exn:fail:contract:non-fixnum-result", exn_fail)
exn_fail_contract_continuation = \
    define_struct("exn:fail:contract:continuation", exn_fail)
exn_fail_contract_variable = \
    define_struct("exn:fail:contract:variable", exn_fail, ["id"])
exn_fail_syntax = \
    define_struct("exn:fail:syntax", exn_fail, ["exprs"])
exn_fail_syntax_unbound = \
    define_struct("exn:fail:syntax:unbound", exn_fail_syntax)
exn_fail_syntax_missing_module = \
    define_struct("exn:fail:syntax:missing-module", exn_fail_syntax, ["path"])
exn_fail_read = \
    define_struct("exn:fail:read", exn_fail, ["srclocs"])
exn_fail_read_eof = \
    define_struct("exn:fail:read:eof", exn_fail_read)
exn_fail_read_non_char = \
    define_struct("exn:fail:read:non-char", exn_fail_read)
exn_fail_fs = \
    define_struct("exn:fail:filesystem", exn_fail)
exn_fail_fs_exists = \
    define_struct("exn:fail:filesystem:exists", exn_fail_fs)
exn_fail_fs_version = \
    define_struct("exn:fail:filesystem:version", exn_fail_fs)
exn_fail_fs_errno = \
    define_struct("exn:fail:filesystem:errno", exn_fail_fs, ["errno"])
exn_fail_fs_missing_module = \
    define_struct("exn:fail:filesystem:missing-module", exn_fail_fs, ["path"])
exn_fail_network = \
    define_struct("exn:fail:network", exn_fail)
exn_fail_network_errno = \
    define_struct("exn:fail:network:errno", exn_fail_network, ["errno"])
exn_fail_out_of_memory = \
    define_struct("exn:fail:out-of-memory", exn_fail)
exn_fail_unsupported = \
    define_struct("exn:fail:unsupported", exn_fail)
exn_fail_user = \
    define_struct("exn:fail:user", exn_fail)
exn_break = \
    define_struct("exn:break", exn)
exn_break_hang_up = \
    define_struct("exn:break:hang-up", exn_break)
exn_break_terminate = \
    define_struct("exn:break:terminate", exn_break)

srcloc = define_struct("srcloc",
                       fields=["source", "line", "column", "position", "span"])
date_struct = define_struct("date", fields=["second",
                                            "minute",
                                            "hour",
                                            "day",
                                            "month",
                                            "year",
                                            "week-day",
                                            "year-day",
                                            "dst?"
                                            "time-zone-offset"])
date_star_struct = define_struct("date*", date_struct,
                                 fields=["nanosecond", "time-zone-name"])

arity_at_least = define_struct("arity-at-least", values.w_null, ["value"])


for args in [ ("subprocess?",),
              ("file-stream-port?",),
              ("terminal-port?",),
              ("byte-ready?",),
              ("char-ready?",),
              ("bytes-converter?",),
              ("char-symbolic?",),
              ("char-graphic?",),
              ("char-blank?",),
              ("char-iso-control?",),
              ("char-punctuation?",),
              ("char-upper-case?",),
              ("char-title-case?",),
              ("char-lower-case?",),
              ("compiled-expression?",),
              ("custom-print-quotable?",),
              ("liberal-define-context?",),
              ("handle-evt?",),
              ("special-comment?",),
              ("exn:srclocs?",),
              ("logger?",),
              ("log-receiver?",),
              # FIXME: these need to be defined with structs
              ("date-dst?",),
              ("thread?",),
              ("thread-running?",),
              ("thread-dead?",),
              ("custodian?",),
              ("custodian-box?",),
              ("namespace?",),
              ("security-guard?",),
              ("thread-group?",),
              ("will-executor?",),
              ("evt?",),
              ("semaphore-try-wait?",),
              ("channel?",),
              ("readtable?",),
              ("path-for-some-system?",),
              ("file-exists?",),
              ("directory-exists?",),
              ("link-exists?",),
              ("relative-path?",),
              ("absolute-path?",),
              ("internal-definition-context?",),
              ("rename-transformer?",),
              ("identifier?",),
              ("port?",),
              ("sequence?",),
              ("namespace-anchor?",),
              ("chaperone-channel",),
              ("impersonate-channel",),
              ]:
    define_nyi(*args)


@expose("set!-transformer?", [values.W_Object])
def set_bang_transformer(v):
    if isinstance(v, values.W_AssignmentTransformer):
        return values.w_true
    elif isinstance(v, values_struct.W_RootStruct):
        w_prop = v.struct_type().read_prop(values_struct.w_prop_set_bang_transformer)
        return values.W_Bool.make(w_prop is not None)
    else:
        return values.w_false


@expose("object-name", [values.W_Object])
def object_name(v):
    if isinstance(v, values.W_Prim):
        return values.W_Symbol.make(v.name)
    return values_string.W_String.fromstr_utf8(v.tostring()) # XXX really?

@expose("namespace-variable-value", [values.W_Symbol,
    default(values.W_Object, values.w_true),
    default(values.W_Object, values.w_true),
    default(values.W_Object, None)])
def namespace_variable_value(sym, use_mapping, failure_thunk, namespace):
    return values.w_void

@expose("find-main-config", [])
def find_main_config():
    return values.w_false

@expose("version", [], simple=False)
def version(env, cont):
    from pycket.interpreter import return_value
    toplevel = env.toplevel_env()
    version = toplevel.globalconfig.lookup("version")
    result = values_string.W_String.fromascii("unknown version" if version is None else version)
    return return_value(result, env, cont)

@continuation
def sem_post_cont(sem, env, cont, vals):
    sem.post()
    from pycket.interpreter import return_multi_vals
    return return_multi_vals(vals, env, cont)

@expose("call-with-semaphore", simple=False)
def call_with_sem(args, env, cont):
    if len(args) < 2:
        raise SchemeException("error call-with-semaphore")
    sem = args[0]
    f = args[1]
    if len(args) == 2:
        new_args = []
        fail = None
    else:
        new_args = args[3:]
        if args[2] is values.w_false:
            fail = None
        else:
            fail = args[2]
    assert isinstance(sem, values.W_Semaphore)
    assert f.iscallable()
    sem.wait()
    return f.call(new_args, env, sem_post_cont(sem, env, cont))

@expose("current-thread", [])
def current_thread():
    return values.W_Thread()

@expose("semaphore-post", [values.W_Semaphore])
def sem_post(s):
    s.post()

@expose("semaphore-wait", [values.W_Semaphore])
def sem_wait(s):
    s.wait()

@expose("procedure-rename", [procedure, values.W_Object])
def procedure_rename(p, n):
    return p

@continuation
def proc_arity_cont(result, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    result.append(check_one_val(_vals))
    if len(result) == 1:
        return return_value(result[0], env, cont)
    return return_value(values.to_list(result[:]), env, cont)

@expose("procedure-arity", [procedure], simple=False)
def do_procedure_arity(proc, env, cont):
    from pycket.interpreter import return_value
    result = []
    arity = proc.get_arity()
    for item in arity.arity_list:
        result.append(values.W_Fixnum(item))
    if arity.at_least != -1:
        val = [values.W_Fixnum(arity.at_least)]
        return arity_at_least.constr.call(val, env, proc_arity_cont(result, env, cont))
    if len(result) == 1:
        return return_value(result[0], env, cont)
    return return_value(values.to_list(result[:]), env, cont)

@expose("procedure-arity?", [values.W_Object])
def do_is_procedure_arity(n):
    if isinstance(n, values.W_Fixnum):
        if n.value >= 0:
            return values.w_true
    elif isinstance(n, values_struct.W_RootStruct) and\
        n.struct_type().name == "arity-at-least":
        return values.w_true
    elif isinstance(n, values.W_List):
        for item in values.from_list(n):
            if not (isinstance(item, values.W_Fixnum) or\
                (isinstance(item, values_struct.W_RootStruct) and\
                item.struct_type().name == "arity-at-least")):
                return values.w_false
        return values.w_true
    return values.w_false

@expose("procedure-arity-includes?",
        [procedure, values.W_Integer, default(values.W_Object, values.w_false)])
def procedure_arity_includes(proc, k, kw_ok):
    if kw_ok is values.w_false:
        if isinstance(proc, values_struct.W_RootStruct):
            w_prop_val = proc.struct_type().read_prop(values_struct.w_prop_incomplete_arity)
            if w_prop_val is not None:
                return values.w_false
    arity = proc.get_arity()
    if isinstance(k, values.W_Fixnum):
        k_val = k.value
        if arity.list_includes(k_val):
            return values.w_true
        if arity.at_least != -1 and k_val >= arity.at_least:
            return values.w_true
    elif isinstance(k, values.W_Bignum):
        k_val = k.value
        if arity.at_least != -1 and k_val.ge(rbigint.fromint(arity.at_least)):
            return values.w_true
    return values.w_false

@expose("procedure-struct-type?", [values_struct.W_StructType])
def do_is_procedure_struct_type(struct_type):
    return values.W_Bool.make(struct_type.prop_procedure is not None)

@expose("procedure-extract-target", [procedure], simple=False)
def do_procedure_extract_target(proc, env, cont):
    from pycket.interpreter import return_value
    if isinstance(proc, values_struct.W_RootStruct):
        prop_procedure = proc.struct_type().prop_procedure
        procedure_source = proc.struct_type().procedure_source
        if isinstance(prop_procedure, values.W_Fixnum):
            return proc.struct_type().accessor.access(
                procedure_source, prop_procedure.value, env, cont, None)
    return return_value(values.w_false, env, cont)

@expose("variable-reference-constant?",
        [values.W_VariableReference], simple=False)
def varref_const(varref, env, cont):
    from pycket.interpreter import return_value
    return return_value(values.W_Bool.make(not(varref.varref.is_mutable(env))),
                        env, cont)

@expose("variable-reference->resolved-module-path",
        [values.W_VariableReference])
def varref_rmp(varref):
    return values.W_ResolvedModulePath(values.W_Path(varref.varref.path))

@expose("variable-reference->module-source",  [values.W_VariableReference])
def varref_ms(varref):
    # FIXME: not implemented
    return values.W_Symbol.make("dummy_module")

@expose("variable-reference->module-path-index", [values.W_VariableReference])
def varref_to_mpi(ref):
    from pycket.interpreter import ModuleVar
    if not isinstance(ref, ModuleVar):
        return values.w_false
    return values.W_ModulePathIndex()

@expose("variable-reference->module-base-phase", [values.W_VariableReference])
def varref_to_mbp(ref):
    # XXX Obviously not correct
    return values.W_Fixnum(0)

@expose("resolved-module-path-name", [values.W_ResolvedModulePath])
def rmp_name(rmp):
    return rmp.name

def is_module_path(v):
    if isinstance(v, values.W_Symbol):
        # FIXME: not always right
        return True
    if isinstance(v, values.W_Path):
        return True
    if isinstance(v, values_string.W_String):
        return True
    if isinstance(v, values.W_List):
        vs = values.from_list(v)
        for p in vs:
            if not is_module_path(p):
                return False
        return True
    # FIXME
    return False

@expose("module-path?", [values.W_Object])
def module_pathp(v):
    return values.W_Bool.make(is_module_path(v))

@expose("values")
def do_values(args_w):
    return values.Values.make(args_w)

@expose("call-with-values", [procedure] * 2, simple=False, extra_info=True)
def call_with_values (producer, consumer, env, cont, extra_call_info):
    # FIXME: check arity
    return producer.call_with_extra_info([], env, call_cont(consumer, env, cont), extra_call_info)

@continuation
def time_apply_cont(initial, env, cont, vals):
    from pycket.interpreter import return_multi_vals
    final = time.clock()
    ms = values.W_Fixnum(int((final - initial) * 1000))
    vals_w = vals.get_all_values()
    results = values.Values.make([values.to_list(vals_w),
                                  ms, ms, values.W_Fixnum(0)])
    return return_multi_vals(results, env, cont)

@expose("continuation-prompt-available?")
def cont_prompt_avail(args):
    return values.w_false

# FIXME: this is a data type
@expose("continuation-prompt-tag?")
def cont_prompt_tag(args):
    return values.w_false

@continuation
def dynamic_wind_pre_cont(value, post, env, cont, _vals):
    return value.call([], env, dynamic_wind_value_cont(post, env, cont))

@continuation
def dynamic_wind_value_cont(post, env, cont, _vals):
    return post.call([], env, dynamic_wind_post_cont(_vals, env, cont))

@continuation
def dynamic_wind_post_cont(val, env, cont, _vals):
    from pycket.interpreter import return_multi_vals
    return return_multi_vals(val, env, cont)

@expose("dynamic-wind", [procedure, procedure, procedure], simple=False)
def dynamic_wind(pre, value, post, env, cont):
    return pre.call([], env, dynamic_wind_pre_cont(value, post, env, cont))


@expose(["call/cc", "call-with-current-continuation",
         "call/ec", "call-with-escape-continuation"],
        [procedure], simple=False, extra_info=True)
def callcc(a, env, cont, extra_call_info):
    return a.call_with_extra_info([values.W_Continuation(cont)], env, cont, extra_call_info)

@expose("time-apply", [procedure, values.W_List], simple=False, extra_info=True)
def time_apply(a, args, env, cont, extra_call_info):
    initial = time.clock()
    return  a.call_with_extra_info(values.from_list(args),
                                   env, time_apply_cont(initial, env, cont), 
                                   extra_call_info)

@expose("apply", simple=False, extra_info=True)
def apply(args, env, cont, extra_call_info):
    if not args:
        raise SchemeException("apply expected at least one argument, got 0")
    fn = args[0]
    if not fn.iscallable():
        raise SchemeException("apply expected a procedure, got something else")
    lst = args[-1]
    try:
        rest = values.from_list(lst)
    except SchemeException:
        raise SchemeException(
            "apply expected a list as the last argument, got something else")
    args_len = len(args)-1
    assert args_len >= 0
    others = args[1:args_len]
    new_args = others + rest
    return fn.call_with_extra_info(new_args, env, cont, extra_call_info)

@expose("make-semaphore", [default(values.W_Fixnum, values.W_Fixnum(0))])
def make_semaphore(n):
    return values.W_Semaphore(n.value)

@expose("semaphore-peek-evt", [values.W_Semaphore])
def sem_peek_evt(s):
    return values.W_SemaphorePeekEvt(s)


@expose("not", [values.W_Object])
def notp(a):
    return values.W_Bool.make(a is values.w_false)

@expose("length", [values.W_List])
def length(a):
    n = 0
    while True:
        if a is values.w_null:
            return values.W_Fixnum(n)
        if isinstance(a, values.W_Cons):
            a = a.cdr()
            n = n+1
        else:
            raise SchemeException("length: not a list")

@expose("list")
def do_list(args):
    return values.to_list(args)

@expose("list*")
def do_liststar(args):
    if not args:
        raise SchemeException("list* expects at least one argument")
    return values.to_improper(args[:-1], args[-1])

@expose("assq", [values.W_Object, values.W_List])
def assq(a, b):
    while isinstance(b, values.W_Cons):
        head, b = b.car(), b.cdr()
        if not isinstance(head, values.W_Cons):
            raise SchemeException("assq: found a non-pair element")
        if eq_prims.eqp_logic(a, head.car()):
            return head
    if b is not values.w_null:
        raise SchemeException("assq: reached a non-pair")
    return values.w_false

@expose("cons", [values.W_Object, values.W_Object])
def do_cons(a, b):
    return values.W_Cons.make(a,b)

@expose("car", [values.W_Cons])
def do_car(a):
    return a.car()

@expose("cdr", [values.W_Cons])
def do_cdr(a):
    return a.cdr()

@expose("cadr")
def do_cadr(args):
    return do_car([do_cdr(args)])

@expose("caar")
def do_caar(args):
    return do_car([do_car(args)])

@expose("cdar")
def do_cdar(args):
    return do_cdr([do_car(args)])

@expose("cddr")
def do_cddr(args):
    return do_cdr([do_cdr(args)])

@expose("caaar")
def do_caaar(args):
    return do_car([do_car([do_car(args)])])

@expose("caadr")
def do_caadr(args):
    return do_car([do_car([do_cdr(args)])])

@expose("caddr")
def do_caddr(args):
    return do_car([do_cdr([do_cdr(args)])])

@expose("cadar")
def do_cadar(args):
    return do_car([do_cdr([do_car(args)])])

@expose("cdaar")
def do_cdaar(args):
    return do_cdr([do_car([do_car(args)])])

@expose("cdadr")
def do_cdadr(args):
    return do_cdr([do_car([do_cdr(args)])])

@expose("cddar")
def do_cddar(args):
    return do_cdr([do_cdr([do_car(args)])])

@expose("cdddr")
def do_caddr(args):
    return do_cdr([do_cdr([do_cdr(args)])])

@expose("caaaar")
def do_caaaar(args):
    return do_car([do_car([do_car([do_car(args)])])])

@expose("caaadr")
def do_caaadr(args):
    return do_car([do_car([do_car([do_cdr(args)])])])

@expose("caadar")
def do_caadar(args):
    return do_car([do_car([do_cdr([do_car(args)])])])

@expose("caaddr")
def do_caaddr(args):
    return do_car([do_car([do_cdr([do_cdr(args)])])])

@expose("cadaar")
def do_cadaar(args):
    return do_car([do_cdr([do_car([do_car(args)])])])

@expose("cadadr")
def do_cadadr(args):
    return do_car([do_cdr([do_car([do_cdr(args)])])])

@expose("caddar")
def do_caddar(args):
    return do_car([do_cdr([do_cdr([do_car(args)])])])

@expose("cadddr")
def do_cadddr(args):
    return do_car([do_cdr([do_cdr([do_cdr(args)])])])

@expose("cdaaar")
def do_cdaaar(args):
    return do_cdr([do_car([do_car([do_car(args)])])])

@expose("cdaadr")
def do_cdaadr(args):
    return do_cdr([do_car([do_car([do_cdr(args)])])])

@expose("cdadar")
def do_cdadar(args):
    return do_cdr([do_car([do_cdr([do_car(args)])])])

@expose("cdaddr")
def do_cdaddr(args):
    return do_cdr([do_car([do_cdr([do_cdr(args)])])])

@expose("cddaar")
def do_cddaar(args):
    return do_cdr([do_cdr([do_car([do_car(args)])])])

@expose("cddadr")
def do_cddadr(args):
    return do_cdr([do_cdr([do_car([do_cdr(args)])])])

@expose("cdddar")
def do_cdddar(args):
    return do_cdr([do_cdr([do_cdr([do_car(args)])])])

@expose("cddddr")
def do_cddddr(args):
    return do_cdr([do_cdr([do_cdr([do_cdr(args)])])])


@expose("mlist")
def do_mlist(args):
    return values.to_mlist(args)

@expose("mcons", [values.W_Object, values.W_Object])
def do_mcons(a, b):
    return values.W_MCons(a,b)

@expose("mcar", [values.W_MCons])
def do_mcar(a):
    return a.car()

@expose("mcdr", [values.W_MCons])
def do_mcdr(a):
    return a.cdr()

@expose("set-mcar!", [values.W_MCons, values.W_Object])
def do_set_mcar(a, b):
    a.set_car(b)

@expose("set-mcdr!", [values.W_MCons, values.W_Object])
def do_set_mcdr(a, b):
    a.set_cdr(b)

@expose("map", simple=False)
def do_map(args, env, cont):
    # XXX this is currently not properly jitted
    from pycket.interpreter import jump
    if not args:
        raise SchemeException("map expected at least two argument, got 0")
    fn, lists = args[0], args[1:]
    if not fn.iscallable():
        raise SchemeException("map expected a procedure, got something else")

    # FIXME: more errorchecking
    assert len(args) >= 0
    return map_loop(fn, lists, env, cont)

@loop_label
def map_loop(f, lists, env, cont):
    from pycket.interpreter import return_value
    lists_new = []
    args = []
    for l in lists:
        if not isinstance(l, values.W_Cons):
            if l is not values.w_null:
                raise SchemeException("map: not given a proper list")
            return return_value(values.w_null, env, cont)
        args.append(l.car())
        lists_new.append(l.cdr())
    return f.call(args, env, map_first_cont(f, lists_new, env, cont))

@continuation
def map_first_cont(f, lists, env, cont, _vals):
    from pycket.interpreter import check_one_val
    val = check_one_val(_vals)
    return map_loop(f, lists, env, map_cons_cont(f, lists, val, env, cont))

@continuation
def map_cons_cont(f, lists, val, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    rest = check_one_val(_vals)
    return return_value(values.W_Cons.make(val, rest), env, cont)

@expose("for-each", simple=False)
def for_each(args, env, cont):
    from pycket.interpreter import return_value
    if len(args) < 2:
        raise SchemeException("for-each: expected at least a procedure and a list")
    f = args[0]
    if not f.iscallable():
        raise SchemeException("for-each: expected a procedure, but got %s"%f)
    ls = args[1:]
    for l in ls:
        if not isinstance(l, values.W_List):
            raise SchemeException("for-each: expected a list, but got %s"%l)
    return return_value(values.w_void, env, for_each_cont(f, ls, env, cont))

@continuation
def for_each_cont(f, ls, env, cont, vals):
    # XXX this is currently not properly jitted
    from pycket.interpreter import return_value
    l = ls[0]
    if l is values.w_null:
        for l in ls:
            assert l is values.w_null
        return return_value(values.w_void, env, cont)
    cars = [l.car() for l in ls]
    cdrs = [l.cdr() for l in ls]
    return f.call(cars, env, for_each_cont(f, cdrs, env, cont))


@expose("append")
def append(lists):
    if not lists:
        return values.w_null
    lists, acc = lists[:-1], lists[-1]
    while lists:
        vals = values.from_list(lists.pop())
        acc = values.to_improper(vals, acc)
    return acc

@expose("reverse", [values.W_List])
def reverse(w_l):
    acc = values.w_null
    while isinstance(w_l, values.W_Cons):
        val, w_l = w_l.car(), w_l.cdr()
        acc = values.W_Cons.make(val, acc)

    if w_l is not values.w_null:
        raise SchemeException("reverse: not given proper list")

    return acc

@expose("void")
def do_void(args): return values.w_void


### Boxes

@expose("box", [values.W_Object])
def box(v):
    return values.W_MBox(v)

@expose("box-immutable", [values.W_Object])
def box_immutable(v):
    return values.W_IBox(v)

@expose("unbox", [values.W_Box], simple=False)
def unbox(b, env, cont):
    return b.unbox(env, cont)

@expose("set-box!", [values.W_Box, values.W_Object], simple=False)
def set_box(box, v, env, cont):
    return box.set_box(v, env, cont)

# This implementation makes no guarantees about atomicity
@expose("box-cas!", [values.W_MBox, values.W_Object, values.W_Object])
def box_cas(box, old, new):
    if eq_prims.eqp_logic(box.value, old):
        box.value = new
        return values.w_true
    return values.w_false

@expose("make-weak-box", [values.W_Object])
def make_weak_box(val):
    return values.W_WeakBox(val)

@expose("weak-box-value",
        [values.W_WeakBox, default(values.W_Object, values.w_false)])
def weak_box_value(wb, default):
    v = wb.get()
    return v if v is not None else default

@expose("make-ephemeron", [values.W_Object] * 2)
def make_ephemeron(key, val):
    return values.W_Ephemeron(key, val)

@expose("ephemeron-value",
        [values.W_Ephemeron, default(values.W_Object, values.w_false)])
def ephemeron_value(ephemeron, default):
    v = ephemeron.get()
    return v if v is not None else default

# FIXME: implementation
define_nyi("make-reader-graph", [values.W_Object])
# def make_reader_graph(val):
#     raise NotImplementedError()
#     return val

@expose("make-placeholder", [values.W_Object])
def make_placeholder(val):
    return values.W_Placeholder(val)

@expose("placeholder-set!", [values.W_Placeholder, values.W_Object])
def placeholder_set(ph, datum):
    ph.value = datum
    return values.w_void

@expose("placeholder-get", [values.W_Placeholder])
def placeholder_get(ph):
    return ph.value

@expose("make-hash-placeholder", [values.W_List])
def make_hash_placeholder(vals):
    return values.W_HashTablePlaceholder([], [])

@expose("make-hasheq-placeholder", [values.W_List])
def make_hasheq_placeholder(vals):
    return values.W_HashTablePlaceholder([], [])

@expose("make-hasheqv-placeholder", [values.W_List])
def make_hasheqv_placeholder(vals):
    return values.W_HashTablePlaceholder([], [])

@expose("list?", [values.W_Object])
def listp(v):
    return values.W_Bool.make(v.is_proper_list())

@expose("list-ref", [values.W_Cons, values.W_Fixnum])
def list_ref(lst, pos):
    # XXX inefficient
    return values.from_list(lst)[pos.value]

@expose("list-tail", [values.W_Object, values.W_Fixnum])
def list_tail(lst, pos):
    start_pos = pos.value
    if start_pos == 0:
        return lst
    else:
        if isinstance(lst, values.W_Cons):
            assert start_pos > 0
            # XXX inefficient
            return values.to_list(values.from_list(lst)[start_pos:])
        else:
            return values.w_null

@expose("current-inexact-milliseconds", [])
def curr_millis():
    return values.W_Flonum(time.clock()*1000)

@expose("error")
def error(args):
    if len(args) == 1:
        sym = args
        assert isinstance(sym, values.W_Symbol)
        raise SchemeException("error: %s" % sym.tostring())
    else:
        first_arg = args[0]
        if isinstance(first_arg, values_string.W_String):
            from rpython.rlib.rstring import StringBuilder
            msg = StringBuilder()
            msg.append(first_arg.tostring())
            v = args[1:]
            for item in v:
                msg.append(" %s" % item.tostring())
            raise SchemeException(msg.build())
        else:
            src = first_arg
            form = args[1]
            v = args[2:]
            assert isinstance(src, values.W_Symbol)
            assert isinstance(form, values_string.W_String)
            raise SchemeException("%s: %s" % (
                src.tostring(), input_output.format(form, v, "error")))

@expose("list->vector", [values.W_List])
def list2vector(l):
    return values_vector.W_Vector.fromelements(values.from_list(l))

# FIXME: make this work with chaperones/impersonators
@expose("vector->list", [values_vector.W_Vector])
def vector2list(v):
    es = []
    for i in range(v.len):
        es.append(v.ref(i))
    return values.to_list(es)

# FIXME: make that a parameter
@expose("current-command-line-arguments", [], simple=False)
def current_command_line_arguments(env, cont):
    from pycket.interpreter import return_value
    w_v = values_vector.W_Vector.fromelements(
            env.toplevel_env().commandline_arguments)
    return return_value(w_v, env, cont)

# Unsafe pair ops
@expose("unsafe-car", [subclass_unsafe(values.W_Cons)])
def unsafe_car(p):
    return p.car()

@expose("unsafe-cdr", [subclass_unsafe(values.W_Cons)])
def unsafe_cdr(p):
    return p.cdr()

@expose("path-string?", [values.W_Object])
def path_stringp(v):
    # FIXME: handle zeros in string
    return values.W_Bool.make(
        isinstance(v, values_string.W_String) or isinstance(v, values.W_Path))

@expose("complete-path?", [values.W_Object])
def complete_path(v):
    # FIXME: stub
    return values.w_false

@expose("path->string", [values.W_Path])
def path2string(p):
    return values_string.W_String.fromstr_utf8(p.path)

@expose("path->bytes", [values.W_Path])
def path2bytes(p):
    return values.W_Bytes.from_string(p.path)

@expose("cleanse-path", [values.W_Object])
def cleanse_path(p):
    if isinstance(p, values_string.W_String):
        return values.W_Path(p.as_str_ascii())
    if isinstance(p, values.W_Path):
        return p
    raise SchemeException("cleanse-path expects string or path")

@expose("port-next-location", [values.W_Object], simple=False)
def port_next_loc(p, env, cont):
    from pycket.interpreter import return_multi_vals
    return return_multi_vals(values.Values.make([values.w_false] * 3),
                             env, cont)

@expose("port-writes-special?", [values.W_Object])
def port_writes_special(v):
    return values.w_false

@expose("port-writes-atomic?", [values.W_Object])
def port_writes_atomic(v):
    return values.w_false

@expose("port-provides-progress-evts?", [values.W_Object])
def port_ppe(v):
    return values.w_false

@expose("file-position*", [values.W_Object])
def file_pos_star(v):
    return values.w_false



@expose("symbol-unreadable?", [values.W_Symbol])
def sym_unreadable(v):
    if v.unreadable:
        return values.w_true
    return values.w_false

@expose("symbol-interned?", [values.W_Symbol])
def string_to_symbol(v):
    return values.W_Bool.make(v.is_interned())


@expose("immutable?", [values.W_Object])
def immutable(v):
    return values.W_Bool.make(v.immutable())

@expose("eval-jit-enabled", [])
def jit_enabled():
    return values.w_true

@expose("make-thread-cell",
        [values.W_Object, default(values.W_Bool, values.w_false)])
def make_thread_cell(v, pres):
    return values.W_ThreadCell(v, False if pres is values.w_false else True)

@expose("thread-cell-ref", [values.W_ThreadCell])
def thread_cell_ref(cell):
    return cell.value

@expose("thread-cell-set!", [values.W_ThreadCell, values.W_Object])
def thread_cell_set(cell, v):
    cell.value = v
    return values.w_void

@expose("current-preserved-thread-cell-values",
        [default(values.W_ThreadCellValues, None)])
def current_preserved_thread_cell_values(v):
    # Generate a new thread-cell-values object
    if v is None:
        return values.W_ThreadCellValues()

    # Otherwise, we restore the values
    for cell, val in v.assoc.items():
        assert cell.preserved
        cell.value = val
    return values.w_void

@expose("place-enabled?")
def do_is_place_enabled(args):
    return values.w_false

@expose("make-continuation-prompt-tag", [default(values.W_Symbol, None)])
def mcpt(s):
    from pycket.interpreter import Gensym
    s = Gensym.gensym("cm") if s is None else s
    return values.W_ContinuationPromptTag(s)

@expose("gensym", [default(values.W_Symbol, values.W_Symbol.make("g"))])
def gensym(init):
    from pycket.interpreter import Gensym
    return Gensym.gensym(init.utf8value)


@expose("keyword<?", [values.W_Keyword, values.W_Keyword])
def keyword_less_than(a_keyword, b_keyword):
    return values.W_Bool.make(a_keyword.value < b_keyword.value)

@expose("build-path")
def build_path(args):
    # this is terrible
    r = ""
    for a in args:
        if isinstance(a, values.W_Bytes):
            r = r + str(a.value)
        elif isinstance(a, values_string.W_String):
            r = r + a.as_str_utf8()
        elif isinstance(a, values.W_Path):
            r = r + a.path
        else:
            raise SchemeException("bad input to build-path: %s" % a)
    return values.W_Path(r)

@expose("current-environment-variables", [])
def cur_env_vars():
    return values.W_EnvVarSet()

@expose("environment-variables-ref", [values.W_EnvVarSet, values.W_Bytes])
def env_var_ref(set, name):
    return values.w_false

@expose("raise", [values.W_Object, default(values.W_Object, values.w_true)])
def do_raise(v, barrier):
    # TODO:
    raise SchemeException("uncaught exception: %s" % v.tostring())

@expose("raise-argument-error",
        [values.W_Symbol, values_string.W_String, values.W_Object])
def raise_arg_err(name, expected, v):
    raise SchemeException("%s: expected %s but got %s" % (
        name.utf8value, expected.as_str_utf8(), v.tostring()))

@expose("raise-arguments-error")
def raise_arg_err(args):
    name = args[0]
    assert isinstance(name, values.W_Symbol)
    message = args[1]
    assert isinstance(message, values_string.W_String)
    from rpython.rlib.rstring import StringBuilder
    error_msg = StringBuilder()
    error_msg.append(name.utf8value)
    error_msg.append(": ")
    error_msg.append(message.as_str_utf8())
    error_msg.append("\n")
    i = 2
    while i + 1 < len(args):
        field = args[i]
        assert isinstance(field, values_string.W_String)
        v = args[i+1]
        assert isinstance(v, values.W_Object)
        error_msg.append("%s: %s\n" % (field.as_str_utf8(), v.tostring()))
        i += 2
    raise SchemeException(error_msg.build())

define_nyi("error-escape-handler", False, [default(values.W_Object, None)])

@expose("find-system-path", [values.W_Symbol], simple=False)
def find_sys_path(sym, env, cont):
    from pycket.interpreter import return_value
    v = env.toplevel_env().globalconfig.lookup(sym.utf8value)
    if v:
        return return_value(values.W_Path(v), env, cont)
    else:
        raise SchemeException("unknown system path %s" % sym.utf8value)

@expose("find-main-collects", [])
def find_main_collects():
    return values.w_false

@expose("module-path-index-join", [values.W_Object, values.W_Object])
def mpi_join(a, b):
    return values.W_ModulePathIndex()

# Loading

# FIXME: Proper semantics.
@expose("load", [values_string.W_String], simple=False)
def load(lib, env, cont):
    from pycket.expand import ensure_json_ast_run, load_json_ast_rpython
    lib_name = lib.tostring()
    json_ast = ensure_json_ast_run(lib_name)
    if json_ast is None:
        raise SchemeException(
            "can't gernerate load-file for %s " % lib.tostring())
    #ast = load_json_ast_rpython(json_ast)
    raise NotImplementedError(
        "would crash anyway when trying to interpret the Module")
    #return ast, env, cont

@expose("current-load-relative-directory", [])
def cur_load_rel_dir():
    return values.w_false

@expose("current-directory", [])
def cur_dir():
    return values.W_Path(os.getcwd())

w_unix_sym = values.W_Symbol.make("unix")
w_windows_sym = values.W_Symbol.make("windows")
w_macosx_sym = values.W_Symbol.make("macosx")

def detect_platform():
    from sys import platform
    if platform == "darwin":
        return w_macosx_sym
    elif platform in ['win32', 'cygwin']:
        return w_windows_sym
    else:
        return w_unix_sym

w_system_sym = detect_platform()

w_os_sym = values.W_Symbol.make("os")
w_os_so_suffix = values.W_Symbol.make("so-suffix")
w_unix_so_suffix = values.W_Bytes.from_string(".so")

@expose("system-type", [default(values.W_Symbol, w_os_sym)])
def system_type(w_what):
    if w_what is w_os_sym:
        return w_system_sym
    if w_what is w_os_so_suffix:
        return w_unix_so_suffix
    raise SchemeException("unexpected system-type symbol %s" % w_what.utf8value)


@expose("system-path-convention-type", [])
def system_path_convetion_type():
    if w_system_sym is w_windows_sym:
        return w_windows_sym
    else:
        return w_unix_sym

@expose("collect-garbage", [])
@jit.dont_look_inside
def do_collect_garbage():
    from rpython.rlib import rgc
    rgc.collect()
    return values.w_void

@continuation
def vec2val_cont(vals, vec, n, s, l, env, cont, new_vals):
    from pycket.interpreter import return_multi_vals, check_one_val
    new  = check_one_val(new_vals)
    vals[n] = new
    if s+n+1 == l:
        return return_multi_vals(values.Values.make(vals), env, cont)
    else:
        return vec.vector_ref(values.W_Fixnum.make(s+n+1), env, vec2val_cont(vals, vec, n+1, s, l, env, cont))


@expose("vector->values", [values_vector.W_Vector,
                           default(values.W_Fixnum, values.W_Fixnum.make(0)),
                           default(values.W_Fixnum, None)],
        simple=False)
def vector_to_values(v, start, end, env, cont):
    from pycket.interpreter import return_multi_vals
    l = end.value if end else v.length()
    s = start.value
    if s == l:
        return return_multi_vals(values.Values.make([]), env, cont)
    else:
        vals = [None] * (l - s)
        return v.vector_ref(values.W_Fixnum.make(s), env, vec2val_cont(vals, v, 0, s, l, env, cont))
