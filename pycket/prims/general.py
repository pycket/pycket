#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os
import sys
import time
#import struct
from pycket import impersonators as imp
from pycket import values, values_string
from pycket.cont import continuation, loop_label, call_cont
from pycket.arity import Arity
from pycket import values_parameter
from pycket import values_struct
from pycket import values_regex
from pycket import vector as values_vector
from pycket.error import SchemeException, UserException
from pycket.foreign import W_CPointer, W_CType
from pycket.hash.equal import W_EqualHashTable, W_EqualAlwaysHashTable
from pycket.hash.base import W_HashTable
from pycket.hash.simple import (W_EqImmutableHashTable, W_EqvImmutableHashTable, W_EqMutableHashTable, W_EqvMutableHashTable, make_simple_immutable_table)
from pycket.prims.expose import (unsafe, default, expose, expose_val, prim_env,
                                 procedure, define_nyi, subclass_unsafe, make_procedure)
from pycket.prims.primitive_tables import *
from pycket.prims import string
from pycket.racket_paths import racket_sys_paths
from pycket.env import w_global_config
from rpython.rlib         import jit, objectmodel, unroll, rgc
from rpython.rlib.rsre    import rsre_re as re

# import for side effects
from pycket.prims import control
from pycket.prims import continuation_marks
from pycket.prims import char
from pycket.prims import box
from pycket.prims import equal as eq_prims
from pycket.prims import foreign
from pycket.prims import hash
from pycket.prims import impersonator
from pycket.prims import input_output
from pycket.prims import logging
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

def make_dummy_char_pred(name):
    @expose(name, [values.W_Character], simple=True)
    def predicate_(a):
        return values.w_false
    predicate_.__name__ +=  name

def make_pred_eq(name, val):
    typ = type(val)
    @expose(name, [values.W_Object], simple=True)
    def pred_eq(a):
        return values.W_Bool.make(a is val)

for args in [
        ("output-port?", values.W_OutputPort),
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
        ("variable-reference?", values.W_VariableReference),
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
        ("hash?", W_HashTable),
        ("cpointer?", W_CPointer),
        ("ctype?", W_CType),
        ("continuation-prompt-tag?", values.W_ContinuationPromptTag),
        ("logger?", values.W_Logger),
        ("log-receiver?", values.W_LogReciever),
        ("evt?", values.W_Evt),
        ("unquoted-printing-string?", values.W_UnquotedPrintingString),
        ("port?", values.W_Port),
        ("security-guard?", values.W_SecurityGuard),
        # FIXME
        ("will-executor?", values.W_WillExecutor),
        ("bytes-converter?", values.W_Impossible),
        ("fsemaphore?", values.W_Impossible),
        ("thread-group?", values.W_Impossible),
        ("udp?", values.W_Impossible),
        ("extflonum?", values.W_ExtFlonum),
        ("custodian-box?", values.W_Impossible),
        ("custodian?", values.W_Impossible),
        ("future?", values.W_Impossible),
        ]:
    make_pred(*args)

for args in [
        ("void?", values.w_void),
        ("false?", values.w_false),
        ("null?", values.w_null),
        ]:
    make_pred_eq(*args)

@expose("hash-weak?", [values.W_Object], simple=True)
def hash_weah_huh(obj):
    # FIXME
    return values.w_false

@expose("hash-strong?", [values.W_Object], simple=True)
def hash_strong_huh(obj):
    # FIXME: /pypy/rpython/rlib/rweakref.py
    return values.W_Bool.make(isinstance(obj, W_HashTable))

@expose("hash-ephemeron?", [values.W_Object], simple=True)
def hash_strong_huh(obj):
    # FIXME
    return values.w_false

@expose("hash-equal?", [values.W_Object], simple=True)
def hash_eq(obj):
    inner = obj
    if isinstance(obj, imp.W_ImpHashTable) or isinstance(obj, imp.W_ChpHashTable):
        inner = obj.get_proxied()
    return values.W_Bool.make(isinstance(inner, W_EqualHashTable))

@expose("hash-equal-always?", [values.W_Object], simple=True)
def hash_eq(obj):
    inner = obj
    if isinstance(obj, imp.W_ImpHashTable) or isinstance(obj, imp.W_ChpHashTable):
        inner = obj.get_proxied()
    return values.W_Bool.make(isinstance(inner, W_EqualAlwaysHashTable))

@expose("hash-eq?", [values.W_Object], simple=True)
def hash_eq(obj):
    inner = obj
    if isinstance(obj, imp.W_ImpHashTable) or isinstance(obj, imp.W_ChpHashTable):
        inner = obj.get_proxied()
    eq_mutable = isinstance(inner, W_EqMutableHashTable)
    eq_immutable = isinstance(inner, W_EqImmutableHashTable)
    return values.W_Bool.make(eq_mutable or eq_immutable)

@expose("hash-eqv?", [values.W_Object], simple=True)
def hash_eqv(obj):
    inner = obj
    if isinstance(obj, imp.W_ImpHashTable) or isinstance(obj, imp.W_ChpHashTable):
        inner = obj.get_proxied()
    eqv_mutable = isinstance(inner, W_EqvMutableHashTable)
    eqv_immutable = isinstance(inner, W_EqvImmutableHashTable)
    return values.W_Bool.make(eqv_mutable or eqv_immutable)

def struct_port_huh(w_struct):
    w_in, w_out = struct_port_prop_huh(w_struct)
    return (w_in is not None) or (w_out is not None)

def struct_port_prop_huh(w_struct):
    w_type = w_struct.struct_type()
    in_property = out_property = None
    for property in w_type.properties:
        w_property, w_value = property
        if w_property is values_struct.w_prop_input_port:
            in_property = w_value
        elif w_property is values_struct.w_prop_output_port:
            out_property = w_value

    return in_property, out_property

def struct_input_port_huh(w_struct):
   w_in, w_out = struct_port_prop_huh(w_struct)
   return w_in is not None

def struct_output_port_huh(w_struct):
    w_in, w_out = struct_port_prop_huh(w_struct)
    return w_out is not None

@expose("input-port?", [values.W_Object], simple=True)
def input_port_huh(a):
    if isinstance(a, values.W_InputPort):
        return values.w_true
    elif isinstance(a, values_struct.W_Struct):
        if struct_input_port_huh(a):
            return values.w_true
    return values.w_false

@expose("datum-intern-literal", [values.W_Object])
def datum_intern_literal(v):
    return v

@expose("byte?", [values.W_Object])
def byte_huh(val):
    if isinstance(val, values.W_Fixnum):
        return values.W_Bool.make(0 <= val.value <= 255)
    return values.w_false

@expose("regexp?", [values.W_Object])
def regexp_huh(r):
    if isinstance(r, values_regex.W_Regexp) or isinstance(r, values_regex.W_PRegexp):
        return values.w_true
    return values.w_false

@expose("pregexp?", [values.W_Object])
def pregexp_huh(r):
    if isinstance(r, values_regex.W_PRegexp):
        return values.w_true
    return values.w_false

@expose("byte-regexp?", [values.W_Object])
def byte_regexp_huh(r):
    if isinstance(r, values_regex.W_ByteRegexp) or isinstance(r, values_regex.W_BytePRegexp):
        return values.w_true
    return values.w_false

@expose("byte-pregexp?", [values.W_Object])
def byte_pregexp_huh(r):
    if isinstance(r, values_regex.W_BytePRegexp):
        return values.w_true
    return values.w_false

@expose("true-object?", [values.W_Object])
def true_object_huh(val):
    if val is values.w_true:
        return values.w_true
    return values.w_false

@expose("procedure?", [values.W_Object])
def procedurep(n):
    return values.W_Bool.make(n.iscallable())

@expose("syntax-original?", [values.W_Object], only_old=True)
def syntax_original(v):
    return values.w_false

@expose("syntax-tainted?", [values.W_Object], only_old=True)
def syntax_tainted(v):
    return values.w_false

@expose("syntax-source-module", [values.W_Object, default(values.W_Object, values.w_false)], only_old=True)
def syntax_source_module(stx, src):
    # XXX Obviously not correct
    return values.W_ResolvedModulePath(values.W_Symbol.make("fake symbol"))

@expose("srcloc->string", [values.W_Object])
def srcloc_to_string(obj):
    return values.w_false

expose_val("null", values.w_null)
expose_val("true", values.w_true)
expose_val("false", values.w_false)
expose_val("break-enabled-key", values.break_enabled_key)
expose_val("exception-handler-key", values.exn_handler_key)

# FIXME: need stronger guards for all of these
for name in ["prop:evt",
             "prop:impersonator-of",
             "prop:method-arity-error"]:
    expose_val(name, values_struct.W_StructProperty(
        values.W_Symbol.make(name), values.w_false))

for name in ["exn:srclocs",
             "custom-print-quotable"]:
    prop = values_struct.W_StructProperty(values.W_Symbol.make(name), values.w_false)
    expose_val("prop:"+name, prop)
    expose_val(name+"?", values_struct.W_StructPropertyPredicate(prop))
    expose_val(name+"-accessor", values_struct.W_StructPropertyAccessor(prop))



expose_val("prop:authentic", values_struct.w_prop_authentic)
expose_val("prop:sealed", values_struct.w_prop_sealed)
expose_val("prop:object-name", values_struct.w_prop_object_name)

expose_val("prop:procedure", values_struct.w_prop_procedure)
expose_val("prop:checked-procedure", values_struct.w_prop_checked_procedure)
expose_val("prop:arity-string", values_struct.w_prop_arity_string)
expose_val("prop:incomplete-arity", values_struct.w_prop_incomplete_arity)
expose_val("prop:custom-write", values_struct.w_prop_custom_write)
expose_val("prop:equal+hash", values_struct.w_prop_equal_hash)
expose_val("prop:chaperone-unsafe-undefined",
           values_struct.w_prop_chaperone_unsafe_undefined)
expose_val("prop:set!-transformer", values_struct.w_prop_set_bang_transformer, only_old=True)
expose_val("prop:rename-transformer", values_struct.w_prop_rename_transformer, only_old=True)
expose_val("prop:expansion-contexts", values_struct.w_prop_expansion_contexts, only_old=True)
expose_val("prop:output-port", values_struct.w_prop_output_port)
expose_val("prop:input-port", values_struct.w_prop_input_port)

@continuation
def check_cont(proc, v, v1, v2, app, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    val = check_one_val(_vals)
    if val is not values.w_false:
        return v.ref_with_extra_info(1, app, env, cont)
    return proc.call([v, v1, v2], env, cont)

@continuation
def receive_first_field(proc, v, v1, v2, app, env, cont, _vals):
    from pycket.interpreter import check_one_val
    first_field = check_one_val(_vals)
    return first_field.call([v1, v2], env,
                            check_cont(proc, v, v1, v2, app, env, cont))

@expose("checked-procedure-check-and-extract",
        [values_struct.W_StructType, values.W_Object, procedure,
         values.W_Object, values.W_Object], simple=False, extra_info=True)
@jit.unroll_safe
def do_checked_procedure_check_and_extract(type, v, proc, v1, v2, env, cont, calling_app):
    from pycket.interpreter import check_one_val, return_value
    if isinstance(v, values_struct.W_RootStruct):
        struct_type = jit.promote(v.struct_type())
        if type.has_subtype(struct_type):
            offset = struct_type.get_offset(type)
            assert offset != -1
            return v.ref_with_extra_info(offset, calling_app, env,
                    receive_first_field(proc, v, v1, v2, calling_app, env, cont))
    return proc.call([v, v1, v2], env, cont)

################################################################
# printing

@expose("system-library-subpath", [default(values.W_Object, values.w_false)])
def sys_lib_subpath(mode):
    # Pycket is 64bit only a.t.m.
    if w_system_sym == w_windows_sym:
        return values.W_Path(r"win32\\x86_64")
    elif w_system_sym == w_macosx_sym:
        return values.W_Path("x86_64-macosx")
    else:
        # FIXME: pretend all unicies are linux for now
        return values.W_Path("x86_64-linux")

@expose("primitive-closure?", [values.W_Object])
def prim_clos(v):
    return values.w_false

################################################################
# built-in struct types

def define_struct(name, w_super=values.w_null, fields=[]):
    immutables = range(len(fields))
    symname = values.W_Symbol.make(name)

    w_struct_type = values_struct.W_StructType.make_simple(
        w_name=symname,
        w_super_type=w_super,
        init_field_count=len(fields),
        auto_field_count=0,
        immutables=immutables)
    expose_val("struct:" + name, w_struct_type)
    expose_val(name, w_struct_type.constructor)
    # this is almost always also provided
    expose_val("make-" + name, w_struct_type.constructor)
    expose_val(name + "?", w_struct_type.predicate)
    struct_acc = w_struct_type.accessor
    for field, field_name in enumerate(fields):
        w_name =  values.W_Symbol.make(field_name)
        acc = values_struct.W_StructFieldAccessor(struct_acc, field, w_name)
        expose_val(name + "-" + field_name, acc)
    return w_struct_type

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

for args in [ ("char-symbolic?",),
              ("char-graphic?",),
              ("char-blank?",),
              ("char-iso-control?",),
              ("char-punctuation?",),
              ("char-upper-case?",),
              ("char-title-case?",),
              ("char-lower-case?",),
              ]:
    make_dummy_char_pred(*args)


for args in [ ("subprocess?",),
              ("file-stream-port?",),
              ("terminal-port?",),
              ("byte-ready?",),
              ("char-ready?",),
              ("handle-evt?",),
              ("thread?",),
              ("thread-running?",),
              ("thread-dead?",),
              ("semaphore-try-wait?",),
              ("link-exists?",),
              ("chaperone-channel",),
              ("impersonate-channel",),
              ]:
    define_nyi(*args)

@expose("unsafe-make-place-local", [values.W_Object])
def unsafe_make_place_local(v):
    return values.W_MBox(v)

@expose("unsafe-place-local-ref", [values.W_MBox], simple=False)
def unsafe_make_place_local(p, env, cont):
    return p.unbox(env, cont)

@expose("unsafe-place-local-set!", [values.W_MBox, values.W_Object], simple=False)
def unsafe_make_place_local(p, v, env, cont):
    return p.set_box(v, env, cont)

@expose("set!-transformer?", [values.W_Object], only_old=True)
def set_bang_transformer(v):
    if isinstance(v, values.W_AssignmentTransformer):
        return values.w_true
    elif isinstance(v, values_struct.W_RootStruct):
        w_property = v.struct_type().read_property(
            values_struct.w_prop_set_bang_transformer)
        return values.W_Bool.make(w_property is not None)
    else:
        return values.w_false


@expose("object-name", [values.W_Object])
def object_name(v):
    if isinstance(v, values.W_Prim):
        return v.name

    elif isinstance(v, values_regex.W_AnyRegexp) or isinstance(v, values.W_Port):
        return v.obj_name()

    return values_string.W_String.fromstr_utf8(v.tostring()) # XXX really?

@expose("find-main-config", [])
def find_main_config():
    return values.w_false

@expose("version", [])
def version():
    from pycket.env import w_version
    version = w_version.get_version()
    if version == '':
        version = "old-pycket"
    return values_string.W_String.fromascii("unknown version" if version is None else version)


@continuation
def sem_post_cont(sem, env, cont, vals):
    sem.post()
    from pycket.interpreter import return_multi_vals
    return return_multi_vals(vals, env, cont)

@expose("call-with-semaphore", simple=False, extra_info=True)
def call_with_sem(args, env, cont, extra_call_info):
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
    return f.call_with_extra_info(new_args, env, sem_post_cont(sem, env, cont), extra_call_info)

c_thread = values.W_Thread()

@expose("current-thread", [])
def current_thread():
    return c_thread

# FIXME : implementation
@expose("current-memory-use", [default(values.W_Object, values.w_false)])
def current_memory_use(mode):
    # mode is : (or/c #f 'cumulative custodian?)

    return values.W_Fixnum(1)

@expose("semaphore-post", [values.W_Semaphore])
def sem_post(s):
    s.post()

@expose("semaphore-wait", [values.W_Semaphore])
def sem_wait(s):
    s.wait()

@expose("procedure-rename", [procedure, values.W_Object])
def procedure_rename(p, n):
    return p

@expose("procedure->method", [procedure])
def procedure_to_method(proc):
    # TODO provide a real implementation
    return proc

@jit.unroll_safe
def make_arity_list(arity, extra=None):
    jit.promote(arity)
    acc = values.w_null
    if extra is not None:
        acc = values.W_Cons.make(extra, acc)
    for item in reversed(arity.arity_list):
        i = values.W_Fixnum(item)
        acc = values.W_Cons.make(i, acc)
    return acc

@continuation
def proc_arity_cont(arity, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    val = check_one_val(_vals)
    if not arity.arity_list:
        return return_value(val, env, cont)
    result = make_arity_list(arity, val)
    return return_value(result, env, cont)

def arity_to_value(arity, env, cont):
    from pycket.interpreter import return_value
    if arity.at_least != -1:
        val = [values.W_Fixnum(arity.at_least)]
        constructor = arity_at_least.constructor
        return constructor.call(val, env, proc_arity_cont(arity, env, cont))
    if len(arity.arity_list) == 1:
        item = values.W_Fixnum(arity.arity_list[0])
        return return_value(item, env, cont)
    result = make_arity_list(arity)
    return return_value(result, env, cont)

@expose("procedure-arity", [procedure], simple=False)
@jit.unroll_safe
def do_procedure_arity(proc, env, cont):
    arity = proc.get_arity()
    return arity_to_value(arity, env, cont)


@expose("procedure-arity-mask", [procedure], simple=True)
@jit.unroll_safe
def do_procedure_arity_mask(proc):
    arity = proc.get_arity()
    return arity.arity_bits()

@make_procedure("default-read-handler",[values.W_InputPort, default(values.W_Object, None)], simple=False)
def default_read_handler(ip, src, env, cont):
    # default to the "read" and "read-syntax" defined in the expander linklet
    if src is None:
        return prim_env[values.W_Symbol.make("read")].call([ip], env, cont)
    else:
        return prim_env[values.W_Symbol.make("read-syntax")].call([ip, src], env, cont)

@continuation
def get_read_handler_cont(env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    ip = check_one_val(_vals)
    assert isinstance(ip, values.W_InputPort)
    if ip.get_read_handler():
        return return_value(ip.get_read_handler(), env, cont)
    else:
        return return_value(default_read_handler, env, cont)

@expose("port-read-handler", [values.W_Object, default(values.W_Procedure, None)], simple=False)
def do_port_read_handler(ip, proc, env, cont):
    from pycket.interpreter import return_value
    if not isinstance(ip, values.W_InputPort):
        assert isinstance(ip, values_struct.W_Struct)
        st = ip.struct_type()
        return st.accessor.call([ip, values.W_Fixnum(0)], env, get_read_handler_cont(env, cont))
    if proc is None:
        #get
        if ip.get_read_handler():
            return return_value(ip.get_read_handler(), env, cont)
        else:
            return return_value(default_read_handler, env, cont)
    else:
        #set
        if proc is default_read_handler:
            ip.set_read_handler(default_read_handler)
        else:
            ip.set_read_handler(proc)

        return return_value(values.w_void, env, cont)

@expose("procedure-arity?", [values.W_Object])
@jit.unroll_safe
def do_is_procedure_arity(n):
    if isinstance(n, values.W_Fixnum):
        return values.W_Bool.make(n.value >= 0)

    elif (isinstance(n, values_struct.W_RootStruct) and
          n.struct_type() is arity_at_least):
        return values.w_true

    elif isinstance(n, values.W_List) and n.is_proper_list():
        for item in values.from_list_iter(n):
            if not (isinstance(item, values.W_Fixnum) or
                (isinstance(item, values_struct.W_RootStruct) and
                item.struct_type() is arity_at_least)):
                return values.w_false
        return values.w_true

    return values.w_false

@expose("procedure-arity-includes?",
        [procedure, values.W_Integer, default(values.W_Object, values.w_false)])
def procedure_arity_includes(proc, k, kw_ok):
    if kw_ok is values.w_false and isinstance(proc, values_struct.W_RootStruct):
        w_prop_val = proc.struct_type().read_property(values_struct.w_prop_incomplete_arity)
        if w_prop_val is not None:
            return values.w_false
    if isinstance(k, values.W_Integer):
        try:
            k_val = k.toint()
        except OverflowError:
            pass
        else:
            arity = proc.get_arity(promote=True)
            return values.W_Bool.make(arity.arity_includes(k_val))
    return values.w_false

@expose("procedure-result-arity", [procedure], simple=False)
def procedure_result_arity(proc, env, cont):
    from pycket.interpreter import return_multi_vals
    arity = proc.get_result_arity()
    if arity is None:
        return return_multi_vals(values.w_false, env, cont)
    return arity_to_value(arity, env, cont)

@expose("procedure-reduce-arity", [procedure, values.W_Object, default(values.W_Object, None)])
def procedure_reduce_arity(proc, arity, e):
    # FIXME : this code is all wrong
    #assert isinstance(arity, Arity)
    #proc.set_arity(arity)
    return proc

@expose("procedure-reduce-arity-mask", [procedure, values.W_Fixnum, default(values.W_Object, values.w_false), default(values.W_Symbol, values.W_Symbol.make("racket"))])
def procedure_reduce_arity_mask(proc, mask, name, realm):
    import math
    return proc # FIXME: do this without mutation

    v = mask.value
    # turn the given mask into an arity
    if v < 0:
        # it's an at least value
        ar_value = int(math.log(abs(v))/math.log(2))
        # for some reason the 2 argument log doesn't exist
        ar = Arity([], ar_value)
    else:
        ar_value = int(math.log(v)/math.log(2))
        ar = Arity([ar_value], -1)

    # FIXME: what if the mask represents a list? see math_arity_cont
    # FIXME: mutation is wrong!
    proc.set_arity(ar)
    return proc

@expose("procedure-struct-type?", [values_struct.W_StructType])
def do_is_procedure_struct_type(struct_type):
    return values.W_Bool.make(struct_type.prop_procedure is not None)

@expose("procedure-extract-target", [procedure], simple=False)
def do_procedure_extract_target(proc, env, cont):
    from pycket.interpreter import return_value
    if not isinstance(proc, values_struct.W_RootStruct):
        return return_value(values.w_false, env, cont)
    struct_type = proc.struct_type()
    prop_procedure = struct_type.prop_procedure
    if isinstance(prop_procedure, values.W_Fixnum):
        idx = prop_procedure.value
        return struct_type.accessor.access(proc, idx, env, cont)
    return return_value(values.w_false, env, cont)

@expose("variable-reference-constant?",
        [values.W_VariableReference], simple=False)
def varref_const(varref, env, cont):
    from pycket.interpreter import return_value
    return return_value(values.W_Bool.make(not(varref.varref.is_mutable(env))),
                        env, cont)

@expose("variable-reference->resolved-module-path",
        [values.W_VariableReference], only_old=True)
def varref_rmp(varref):
    return values.W_ResolvedModulePath(values.W_Path(varref.varref.path))

@expose("variable-reference->module-source",  [values.W_VariableReference], only_old=True)
def varref_ms(varref):
    # FIXME: not implemented
    return values.W_Symbol.make("dummy_module")

@expose("variable-reference->module-path-index", [values.W_VariableReference], only_old=True)
def varref_to_mpi(ref):
    from pycket.interpreter import ModuleVar
    if not isinstance(ref, ModuleVar):
        return values.w_false
    return values.W_ModulePathIndex()

@expose("variable-reference->module-base-phase", [values.W_VariableReference], only_old=True)
def varref_to_mbp(ref):
    # XXX Obviously not correct
    return values.W_Fixnum.ZERO

@expose("resolved-module-path-name", [values.W_ResolvedModulePath], only_old=True)
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

@expose("module-path?", [values.W_Object], only_old=True)
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
def time_apply_cont(initial, initial_user, initial_gc, env, cont, vals):
    from pycket.interpreter import return_multi_vals
    final = time.time()
    final_gc = current_gc_time()
    final_user = time.clock()
    ms = values.W_Fixnum(int((final - initial) * 1000))
    ms_gc = values.W_Fixnum(int((final_gc - initial_gc)))
    ms_user = values.W_Fixnum(int((final_user - initial_user) * 1000))
    vals_w = vals.get_all_values()
    results = values.Values.make([values.to_list(vals_w),
                                  ms_user, ms, ms_gc])
    return return_multi_vals(results, env, cont)

@jit.dont_look_inside
def current_gc_time():
    if objectmodel.we_are_translated():
        memory = rgc.get_stats(rgc.TOTAL_GC_TIME)
    else:
        memory = 0
    return memory

@expose("time-apply", [procedure, values.W_List], simple=False, extra_info=True)
def time_apply(a, args, env, cont, extra_call_info):
    initial = time.time()
    initial_user = time.clock()
    initial_gc = current_gc_time()
    return  a.call_with_extra_info(values.from_list(args),
                                   env, time_apply_cont(initial, initial_user, initial_gc, env, cont),
                                   extra_call_info)

@expose("apply", simple=False, extra_info=True)
def apply(args, env, cont, extra_call_info):
    if len(args) < 2:
        raise SchemeException("apply expected at least 2 arguments, given %s" % len(args))
    fn = args[0]
    if not fn.iscallable():
        raise SchemeException("apply expected a procedure, got something else")
    lst = args[-1]
    try:
        fn_arity = fn.get_arity(promote=True)
        if fn_arity is Arity.unknown or fn_arity.at_least == -1:
            unroll_to = 3
        elif fn_arity.arity_list:
            unroll_to = fn_arity.arity_list[-1]
        else:
            unroll_to = fn_arity.at_least + 7
        rest = values.from_list(lst, unroll_to=unroll_to, force=True)
    except SchemeException:
        raise SchemeException(
            "apply expected a list as the last argument, got something else")
    args_len = len(args) - 1
    assert args_len >= 0
    others = args[1:args_len]
    new_args = others + rest
    return fn.call_with_extra_info(new_args, env, cont, extra_call_info)

@expose("make-semaphore", [default(values.W_Fixnum, values.W_Fixnum.ZERO)])
def make_semaphore(n):
    return values.W_Semaphore(n.value)

@expose("semaphore-peek-evt", [values.W_Semaphore])
def sem_peek_evt(s):
    return values.W_SemaphorePeekEvt(s)

@expose("not", [values.W_Object])
def notp(a):
    return values.W_Bool.make(a is values.w_false)

@jit.elidable
def elidable_length(lst):
    n = 0
    while isinstance(lst, values.W_Cons):
        n += 1
        lst = lst.cdr()
    return n

@objectmodel.always_inline
def unroll_pred(lst, idx, unroll_to=0):
    if not jit.we_are_jitted():
        return False
    return not jit.isvirtual(lst) and idx > unroll_to

@jit.unroll_safe
def virtual_length(lst, unroll_to=0):
    n = 0
    while isinstance(lst, values.W_Cons):
        if unroll_pred(lst, n, unroll_to):
            return elidable_length(lst) + n
        n += 1
        lst = lst.cdr()
    return n

@expose("length", [values.W_List])
def length(a):
    if not a.is_proper_list():
        raise SchemeException("length: not given a proper list (either cyclic or not null terminated)")
    return values.W_Fixnum(virtual_length(a, unroll_to=2))

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


@expose("memq", [values.W_Object, values.W_List])
def memq(w_o, w_l):
    while isinstance(w_l, values.W_Cons):
        if eq_prims.eqp_logic(w_o, w_l.car()):
            return w_l
        w_l = w_l.cdr()
    return values.w_false

@expose("memv", [values.W_Object, values.W_List])
def memv(w_o, w_l):
    while isinstance(w_l, values.W_Cons):
        if w_o.eqv(w_l.car()):
            return w_l
        w_l = w_l.cdr()
    return values.w_false


@expose("cons", [values.W_Object, values.W_Object])
def do_cons(a, b):
    return values.W_Cons.make(a, b)

def make_list_eater(name):
    """
    For generating car, cdr, caar, cadr, etc...
    """
    spec     = name[1:-1]
    unrolled = unroll.unrolling_iterable(reversed(spec))

    contract = "pair?"

    for letter in spec[1::-1]:
        if letter == 'a':
            contract = "(cons/c %s any/c)" % contract
        elif letter == 'd':
            contract = "(cons/c any/c %s)" % contract
        else:
            assert False, "Bad list eater specification"

    @expose(name, [values.W_Object])
    def process_list(_lst):
        lst = _lst
        for letter in unrolled:
            if not isinstance(lst, values.W_Cons):
                raise SchemeException("%s: expected %s given %s" % (name, contract, _lst))
            if letter == 'a':
                lst = lst.car()
            elif letter == 'd':
                lst = lst.cdr()
            else:
                assert False, "Bad list eater specification"
        return lst
    process_list.__name__ = "do_" + name
    return process_list

def list_eater_names(n):
    names = []
    for i in range(n):
        names = [n + 'a' for n in names] + [n + 'd' for n in names] + ['a', 'd']
    return ["c%sr" % name for name in names]

for name in list_eater_names(4):
    make_list_eater(name)

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

@expose("map", simple=False, arity=Arity.geq(2))
def do_map(args, env, cont):
    # XXX this is currently not properly jitted
    if len(args) < 2:
        raise SchemeException("map expected at least two argument, got %s"%len(args))
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

@expose("for-each", simple=False, arity=Arity.geq(2))
@jit.unroll_safe
def for_each(args, env, cont):
    from pycket.interpreter import return_value
    if len(args) < 2:
        raise SchemeException("for-each: expected at least a procedure and a list")
    f = args[0]
    if not f.iscallable():
        raise SchemeException("for-each: expected a procedure, but got %s" % f)
    ls = args[1:]
    for l in ls:
        if not l.is_proper_list():
            raise SchemeException("for-each: expected a list, but got %s" % l)
    return for_each_loop(f, ls, env, cont)

@loop_label
@jit.unroll_safe
def for_each_loop(func, args, env, cont):
    from pycket.interpreter import return_value
    nargs = jit.promote(len(args))
    heads = [None] * nargs
    tails = [None] * nargs
    for i in range(nargs):
        arg = args[i]
        if arg is values.w_null:
            for v in args:
                if v is not values.w_null:
                    raise SchemeException("for-each: all lists must have same size")
            return return_value(values.w_void, env, cont)
        assert isinstance(arg, values.W_Cons)
        heads[i] = arg.car()
        tails[i] = arg.cdr()
    return func.call(heads, env,
            for_each_cont(func, tails, env, cont))

@continuation
def for_each_cont(func, tails, env, cont, _vals):
    return for_each_loop(func, tails, env, cont)

@expose("andmap", simple=False, arity=Arity.geq(2))
def andmap(args, env, cont):
    from pycket.interpreter import return_value
    if len(args) < 2:
        raise SchemeException("andmap: expected at least a procedure and a list")
    f = args[0]
    if not f.iscallable():
        raise SchemeException("andmap: expected a procedure, but got %s"%f)
    ls = args[1:]
    for l in ls:
        if not isinstance(l, values.W_List):
            raise SchemeException("andmap: expected a list, but got %s"%l)
    return return_value(values.w_void, env, andmap_cont(f, ls, env, cont))

@continuation
def andmap_cont(f, ls, env, cont, vals):
    # XXX this is currently not properly jitted
    from pycket.interpreter import return_value, check_one_val
    val = check_one_val(vals)
    if val is values.w_false:
        return return_value(val, env, cont)
    for l in ls:
        if l is values.w_null:
            return return_value(values.w_true, env, cont)
    cars = [l.car() for l in ls]
    cdrs = [l.cdr() for l in ls]
    return f.call(cars, env, andmap_cont(f, cdrs, env, cont))

@expose("ormap", simple=False, arity=Arity.geq(2))
def ormap(args, env, cont):
    from pycket.interpreter import return_value
    if len(args) < 2:
        raise SchemeException("ormap: expected at least a procedure and a list")
    f = args[0]
    if not f.iscallable():
        raise SchemeException("ormap: expected a procedure, but got %s"%f)
    ls = args[1:]
    for l in ls:
        if not isinstance(l, values.W_List):
            raise SchemeException("ormap: expected a list, but got %s"%l)
    return return_value(values.w_false, env, ormap_cont(f, ls, env, cont))

@continuation
def ormap_cont(f, ls, env, cont, vals):
    # XXX this is currently not properly jitted
    from pycket.interpreter import return_value, check_one_val
    val = check_one_val(vals)
    if val is not values.w_false:
        return return_value(val, env, cont)
    for l in ls:
        if l is values.w_null:
            return return_value(values.w_false, env, cont)
    cars = [l.car() for l in ls]
    cdrs = [l.cdr() for l in ls]
    return f.call(cars, env, ormap_cont(f, cdrs, env, cont))

@expose("append", arity=Arity.geq(0))
@jit.look_inside_iff(
    lambda l: jit.loop_unrolling_heuristic(l, len(l), values.UNROLLING_CUTOFF))
def append(lists):
    if not lists:
        return values.w_null
    acc = lists[-1]
    for i in range(len(lists) - 2, -1, -1):
        curr = lists[i]
        if not curr.is_proper_list():
            raise SchemeException("append: expected proper list")
        acc = append_two(curr, acc)
    return acc

def append_two(l1, l2):
    first = None
    last  = None
    while isinstance(l1, values.W_Cons):
        v = l1.clone()
        if first is None:
            first = v
        else:
            last._unsafe_set_cdr(v)
        last = v
        l1 = l1.cdr()

    if last is None:
        return l2
    last._unsafe_set_cdr(l2)
    return first

@expose("reverse", [values.W_List])
def reverse(w_l):
    acc = values.w_null
    while isinstance(w_l, values.W_Cons):
        val, w_l = w_l.car(), w_l.cdr()
        acc = values.W_Cons.make(val, acc)

    if w_l is not values.w_null:
        raise SchemeException("reverse: not given proper list")

    return acc

@expose("void", arity=Arity.geq(0))
def do_void(args):
    return values.w_void

@expose("make-ephemeron", [values.W_Object] * 2)
def make_ephemeron(key, val):
    return values.W_Ephemeron(key, val)

@expose("ephemeron-value",
        [values.W_Ephemeron, default(values.W_Object, values.w_false)])
def ephemeron_value(ephemeron, default):
    v = ephemeron.get()
    return v if v is not None else default

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

@expose("list-pair?", [values.W_Object])
def list_pair(v):
    return values.W_Bool.make(isinstance(v, values.W_Cons) and v.is_proper_list())

def enter_list_ref_iff(lst, pos):
    if jit.isconstant(lst) and jit.isconstant(pos):
        return True
    return jit.isconstant(pos) and pos <= 16

@jit.look_inside_iff(enter_list_ref_iff)
def list_ref_impl(lst, pos):
    if pos < 0:
        raise SchemeException("list-ref: negative index")
    for i in range(pos):
        lst = lst.cdr()
        if not isinstance(lst, values.W_Cons):
            raise SchemeException("list-ref: index out of range")
    return lst.car()

@expose("list-ref", [values.W_Cons, values.W_Fixnum])
def list_ref(lst, pos):
    return list_ref_impl(lst, pos.value)

@expose("unsafe-list-ref", [subclass_unsafe(values.W_Cons), values.W_Fixnum])
def unsafe_list_ref(lst, pos):
    return list_ref_impl(lst, pos.value)

@expose("unsafe-list-tail", [subclass_unsafe(values.W_Object), values.W_Fixnum])
def unsafe_list_tail(lst, pos):
    return list_tail_impl(lst, pos)

@expose("list-tail", [values.W_Object, values.W_Fixnum])
def list_tail(lst, pos):
    return list_tail_impl(lst, pos)

def list_tail_impl(lst, pos):
    start_pos = pos.value

    while start_pos > 0:
        if not isinstance(lst, values.W_Cons):
            msg = "index too large for list" if lst is values.w_null else "index reaches a non-pair"
            raise SchemeException("list-tail : %s\n -- lst : %s\n -- index : %s\n" % (msg, lst.tostring(), start_pos))

        lst = lst.cdr()
        start_pos -= 1

    return lst

@expose("assoc", [values.W_Object, values.W_List, default(values.W_Object, values.w_false)])
def assoc(v, lst, is_equal):
    if is_equal is not values.w_false:
        raise SchemeException("assoc: using a custom equal? is not yet implemented")

    while isinstance(lst, values.W_Cons):
        c = lst.car()
        if not isinstance(lst, values.W_Cons):
            raise SchemeException("assoc: non-pair found in list: %s in %s" % (c.tostring(), lst.tostring()))
        cc = c.car()
        if v.equal(cc):
            return c
        lst = lst.cdr()

    return values.w_false

@expose("current-seconds", [])
def current_seconds():
    tick = int(time.time())
    return values.W_Fixnum(tick)

@expose("current-inexact-milliseconds", [])
def curr_millis():
    return values.W_Flonum(time.time() * 1000.0)

@expose("seconds->date", [values.W_Fixnum])
def seconds_to_date(s):
    # TODO: Proper implementation
    return values.w_false

def _error(args, is_user=False):
    reason = ""
    if len(args) == 1:
        sym = args[0]
        reason = "error: %s" % sym.tostring()
    else:
        first_arg = args[0]
        if isinstance(first_arg, values_string.W_String):
            from rpython.rlib.rstring import StringBuilder
            msg = StringBuilder()
            msg.append(first_arg.tostring())
            v = args[1:]
            for item in v:
                msg.append(" %s" % item.tostring())
            reason = msg.build()
        else:
            src = first_arg
            form = args[1]
            v = args[2:]
            assert isinstance(src, values.W_Symbol)
            assert isinstance(form, values_string.W_String)
            reason = "%s: %s" % (
                src.tostring(), input_output.format(form, v, "error"))
    if is_user:
        raise UserException(reason)
    else:
        raise SchemeException(reason)

@expose("error", arity=Arity.geq(1))
def error(args):
    return _error(args, False)

@expose("raise-user-error", arity=Arity.geq(1))
def error(args):
    return _error(args, True)

@expose("raise-arity-error", arity=Arity.geq(2))
def raise_arity_error(args):
    return _error(args, False)

@expose("raise-result-arity-error", arity=Arity.geq(3))
def raise_result_arity_error(args):
    return _error(args, False)


@expose("list->vector", [values.W_List])
def list2vector(l):
    return values_vector.W_Vector.fromelements(values.from_list(l))

# FIXME: make this work with chaperones/impersonators
@expose("vector->list", [values.W_MVector], simple=False)
def vector2list(v, env, cont):
    from pycket.interpreter import return_value
    if isinstance(v, values_vector.W_Vector):
        # Fast path for unproxied vectors
        result = values.vector_to_improper(v, values.w_null)
        return return_value(result, env, cont)
    return vector_to_list_loop(v, v.length() - 1, values.w_null, env, cont)

@loop_label
def vector_to_list_loop(vector, idx, acc, env, cont):
    from pycket.interpreter import return_value
    if idx < 0:
        return return_value(acc, env, cont)
    return vector.vector_ref(idx, env,
            vector_to_list_read_cont(vector, idx, acc, env, cont))

@continuation
def vector_to_list_read_cont(vector, idx, acc, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    val = check_one_val(_vals)
    acc = values.W_Cons.make(val, acc)
    return vector_to_list_loop(vector, idx - 1, acc, env, cont)

# Unsafe pair ops
@expose("unsafe-car", [subclass_unsafe(values.W_Cons)])
def unsafe_car(p):
    return p.car()

@expose("unsafe-mcar", [subclass_unsafe(values.W_MCons)])
def unsafe_mcar(p):
    return p.car()

@expose("unsafe-cdr", [subclass_unsafe(values.W_Cons)])
def unsafe_cdr(p):
    return p.cdr()

@expose("unsafe-mcdr", [subclass_unsafe(values.W_MCons)])
def unsafe_mcdr(p):
    return p.cdr()

@continuation
def struct_port_loc_cont(input_huh, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_multi_vals
    pr = check_one_val(_vals)

    if not isinstance(pr, values.W_Port):
        if input_huh:
            # empty string input port is used for prop:input-port
            pr = values.W_StringInputPort("")
        else:
            # a port that discards all data is used for prop:output-port
            pr = values.W_StringOutputPort()

    assert isinstance(pr, values.W_Port)
    lin = pr.get_line()
    col = pr.get_column()
    pos = pr.get_position()
    return return_multi_vals(values.Values.make([lin, col, pos]), env, cont)


@expose("port-next-location", [values.W_Object], simple=False)
def port_next_loc(p, env, cont):
    from pycket.interpreter import return_multi_vals
    lin = col = pos = values.w_false
    if isinstance(p, values_struct.W_Struct):
        i, o = struct_port_prop_huh(p)
        if (i is None) and (o is None):
            raise SchemeException("given struct doesn't have neither prop:input-port nor prop:output-port")

        if i:
            if isinstance(i, values.W_InputPort):
                lin = i.get_line()
                col = i.get_column()
                pos = i.get_position()
            elif isinstance(i, values.W_Fixnum):
                port_index = i.value
                return p.struct_type().accessor.call([p, values.W_Fixnum(port_index)], env, struct_port_loc_cont(True, env, cont))
            else:
                raise SchemeException("invalid value %s for prop:input-port of the given struct : %s" % (i, p.tostring()))
        elif o:
            if isinstance(o, values.W_OutputPort):
                lin = o.get_line()
                col = o.get_column()
                pos = o.get_position()
            elif isinstance(o, values.W_Fixnum):
                port_index = o.value
                return p.struct_type().accessor.call([p, values.W_Fixnum(port_index)], env, struct_port_loc_cont(False, env, cont))
            else:
                raise SchemeException("invalid value %s for prop:output-port of the given struct : %s" % (o, p.tostring()))
    else:
        assert isinstance(p, values.W_Port)
        lin = p.get_line()
        col = p.get_column()
        pos = p.get_position()

    return return_multi_vals(values.Values.make([lin, col, pos]), env, cont)

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

@expose("symbol<?", arity=Arity.geq(1))
def symbol_lt(args):
    name = "symbol<?"
    if len(args) < 2:
        raise SchemeException(name + ": requires at least 2 arguments")
    head = args[0]
    if not isinstance(head, values.W_Symbol):
        raise SchemeException(name + ": not given a string")
    for i in range(1, len(args)):
        t = args[i]
        if not isinstance(t, values.W_Symbol):
            raise SchemeException(name + ": not given a string")
        # FIXME: shouldn't need to convert to W_String
        # but this is much easier than recreating the logic

        if string.symbol_to_string_impl(head).cmp(string.symbol_to_string_impl(t)) >= 0:
            return values.w_false
        head = t
    return values.w_true


@expose("immutable?", [values.W_Object])
def immutable(v):
    return values.W_Bool.make(v.immutable())

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
    for cell, val in v.assoc.iteritems():
        assert cell.preserved
        cell.value = val
    return values.w_void

@expose("place-enabled?")
def do_is_place_enabled(args):
    return values.w_false

@expose("gensym", [default(values.W_Object, values.W_Symbol.make("g"))])
def gensym(init):
    from pycket.interpreter import Gensym
    if not isinstance(init, values.W_Symbol) and not isinstance(init, values_string.W_String):
        raise SchemeException("gensym exptected a string or symbol but got : %s" % init.tostring())

    gensym_key = init.tostring()
    return Gensym.gensym(gensym_key)

@expose("keyword<?", [values.W_Keyword, values.W_Keyword])
def keyword_less_than(a_keyword, b_keyword):
    return values.W_Bool.make(a_keyword.value < b_keyword.value)

initial_env_vars = values.W_EnvVarSet({}, True)

expose_val("current-environment-variables", values_parameter.W_Parameter(initial_env_vars))

@expose("environment-variables-ref", [values.W_EnvVarSet, values.W_Bytes])
def env_var_ref(set, name):
    r = set.get(name.as_str())
    if r is None:
        return values.w_false
    else:
        return values.W_Bytes.from_string(r)

@expose("environment-variables-set!", [values.W_EnvVarSet, values.W_Bytes, values.W_Bytes, default(values.W_Object, None)])
def env_var_ref(set, name, val, fail):
    return set.set(name.as_str(), val.as_str())

@expose("make-environment-variables")
def make_env_var(args):
    return values.W_EnvVarSet({}, False)

@expose("environment-variables-names", [values.W_EnvVarSet])
def env_var_names(set):
    names = set.get_names()
    return values.to_list([values.W_Bytes.from_string(n) for n in names])

@expose("check-for-break", [])
def check_for_break():
    return values.w_false

@expose("find-system-path", [values.W_Symbol], simple=True)
def find_sys_path(kind):
    return racket_sys_paths.get_path(kind)

@expose("find-main-collects", [])
def find_main_collects():
    return values.w_false

@expose("module-path-index-join",
        [values.W_Object, values.W_Object, default(values.W_Object, None)], only_old=True)
def mpi_join(a, b, c):
    return values.W_ModulePathIndex()

@expose("module-path-index-resolve",
        [values.W_ModulePathIndex], only_old=True)
def mpi_resolve(a):
    return values.W_ResolvedModulePath(values.W_Path("."))

# Loading

# FIXME: Proper semantics.
@expose("load", [values_string.W_String], simple=False, only_old=True)
def load(lib, env, cont):
    from pycket.expand import ensure_json_ast_run
    lib_name = lib.tostring()
    json_ast = ensure_json_ast_run(lib_name)
    if json_ast is None:
        raise SchemeException(
            "can't gernerate load-file for %s " % lib.tostring())
    #ast = load_json_ast_rpython(json_ast)
    raise NotImplementedError(
        "would crash anyway when trying to interpret the Module")
    #return ast, env, cont

expose_val("current-load-relative-directory", values_parameter.W_Parameter(values.w_false))
expose_val("current-write-relative-directory", values_parameter.W_Parameter(values.w_false))

initial_security_guard = values.W_SecurityGuard()

expose_val("current-security-guard", values_parameter.W_Parameter(initial_security_guard))

@expose("make-security-guard", [values.W_SecurityGuard, values.W_Procedure, values.W_Procedure, default(values.W_Procedure, values.w_false)])
def make_security_guard(parent, file, network, link):
    return values.W_SecurityGuard()

@expose("unsafe-make-security-guard-at-root")
def unsafe_make_sec_guard(args):
    return values.W_SecurityGuard()

@make_procedure("current-directory-guard", [values.W_Object], simple=False)
def current_directory_guard(path, env, cont):
    from pycket.interpreter import return_value
    # "cd"s at the os level
    if not (isinstance(path, values_string.W_String) or isinstance(path, values.W_Path)):
        raise SchemeException("current-directory: exptected a path-string? as argument 0, but got : %s" % path.tostring())
    path_str = input_output.extract_path(path)

    # if path is a complete-path?, set it
    if path_str[0] == os.path.sep:
        new_current_dir = path_str
    else: # relative to the current one
        current_dir = current_directory_param.get(cont)
        current_path_str = input_output.extract_path(current_dir)
        # let's hope that there's no symbolic links etc.
        new_current_dir = os.path.normpath(os.path.sep.join([current_path_str, path_str]))

    try:
        os.chdir(new_current_dir)
    except OSError:
        raise SchemeException("path doesn't exist : %s" % path_str)

    out_port = input_output.current_out_param.get(cont)
    assert isinstance(out_port, values.W_OutputPort)
    out_port.write("; now in %s\n" % new_current_dir)

    return return_value(values.W_Path(new_current_dir), env, cont)

current_directory_param = values_parameter.W_Parameter(values.W_Path(os.getcwd()), current_directory_guard)
expose_val("current-directory", current_directory_param)

w_unix_sym = values.W_Symbol.make("unix")
w_windows_sym = values.W_Symbol.make("windows")
w_macosx_sym = values.W_Symbol.make("macosx")

_platform = sys.platform

def detect_platform():
    if _platform == "darwin":
        return w_macosx_sym
    elif _platform in ['win32', 'cygwin']:
        return w_windows_sym
    else:
        return w_unix_sym

w_system_sym = detect_platform()

w_os_sym = values.W_Symbol.make("os")
w_os_so_suffix = values.W_Symbol.make("so-suffix")
w_os_so_mode_sym = values.W_Symbol.make("so-mode")
w_fs_change_mode = values.W_Symbol.make("fs-change")
w_local_mode = values.W_Symbol.make("local")
w_unix_so_suffix = values.W_Bytes.from_string(".so")

w_word_sym = values.W_Symbol.make("word")
w_link_sym = values.W_Symbol.make("link")
w_vm_sym = values.W_Symbol.make("vm")
w_gc_sym = values.W_Symbol.make("gc")
w_machine_sym = values.W_Symbol.make("machine")
w_cross_sym = values.W_Symbol.make("cross")

w_fs_supported = values.W_Symbol.make("supported")
w_fs_scalable = values.W_Symbol.make("scalable")
w_fs_low_latency = values.W_Symbol.make("low-latency")
w_fs_file_level = values.W_Symbol.make("file-level")
w_target_machine_sym = values.W_Symbol.make("target-machine")

def system_type(w_what):
    # os
    if w_what is w_os_sym:
        return w_system_sym

    # word
    if w_what is w_word_sym:
        #return values.W_Fixnum(8*struct.calcsize("P"))
        return values.W_Fixnum(64)

    # vm
    if w_what is w_vm_sym:
        return values.W_Symbol.make("pycket")

    # gc
    if w_what is w_gc_sym:
        return values.W_Symbol.make("3m") # ??

    # link
    #
    # 'static (Unix)
    # 'shared (Unix)
    # 'dll (Windows)
    # 'framework (Mac OS)
    if w_what is w_link_sym:
        return values.W_Symbol.make("static")

    # machine
    if w_what is w_machine_sym:
        return values_string.W_String.make("further details about the current machine in a platform-specific format")

    # so-suffix
    if w_what is w_os_so_suffix:
        return w_unix_so_suffix

    # so-mode
    if w_what is w_os_so_mode_sym:
        return w_local_mode

    # fs-change
    if w_what is w_fs_change_mode:
        from pycket.prims.vector import vector
        w_f = values.w_false
        # FIXME: Is there a way to get this info from sys or os?
        if w_system_sym is w_unix_sym:
            return vector([w_fs_supported, w_fs_scalable, w_f, w_fs_file_level])
        else:
            return vector([w_f, w_f, w_f, w_f])

    # cross
    if w_what is w_cross_sym:
        return values.W_Symbol.make("infer")

    # cross
    if w_what is w_target_machine_sym:
        return values.W_Symbol.make("pycket")

    raise SchemeException("unexpected system-type symbol '%s" % w_what.utf8value)

expose("system-type", [default(values.W_Symbol, w_os_sym)])(system_type)

def system_path_convention_type():
    if w_system_sym is w_windows_sym:
        return w_windows_sym
    else:
        return w_unix_sym

expose("system-path-convention-type", [])(system_path_convention_type)

@expose("bytes->path", [values.W_Bytes, default(values.W_Symbol, system_path_convention_type())])
def bytes_to_path(bstr, typ):
    # FIXME : ignores the type, won't work for windows
    return values.W_Path(bstr.as_str())

major_gc_sym = values.W_Symbol.make("major")
minor_gc_sym = values.W_Symbol.make("minor")
incremental_gc_sym = values.W_Symbol.make("incremental")

@expose("collect-garbage", [default(values.W_Symbol, major_gc_sym)])
@jit.dont_look_inside
def do_collect_garbage(request):
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
        return vec.vector_ref(s+n+1, env, vec2val_cont(vals, vec, n+1, s, l, env, cont))


@expose("vector->values", [values_vector.W_Vector,
                           default(values.W_Fixnum, values.W_Fixnum.ZERO),
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
        return v.vector_ref(s, env, vec2val_cont(vals, v, 0, s, l, env, cont))

class ReaderGraphBuilder(object):

    def __init__(self):
        self.state = {}

    def reader_graph_loop_cons(self, v):
        assert isinstance(v, values.W_Cons)
        p = values.W_WrappedConsMaybe(values.w_unsafe_undefined, values.w_unsafe_undefined)
        self.state[v] = p
        car = self.reader_graph_loop(v.car())
        cdr = self.reader_graph_loop(v.cdr())
        p._car = car
        p._cdr = cdr
        # FIXME: should change this to say if it's a proper list now ...
        return p

    def reader_graph_loop_vector(self, v):
        assert isinstance(v, values_vector.W_Vector)
        len = v.length()
        p = values_vector.W_Vector.fromelement(values.w_false, len)
        self.state[v] = p
        for i in range(len):
            vi = v.ref(i)
            p.set(i, self.reader_graph_loop(vi))
        return p

    def reader_graph_loop_struct(self, v):
        assert isinstance(v, values_struct.W_Struct)

        type = v.struct_type()
        if not type.isprefab:
            return v

        size = v._get_size_list()
        p = values_struct.W_Struct.make_n(size, type)
        self.state[v] = p
        for i in range(size):
            val = self.reader_graph_loop(v._ref(i))
            p._set_list(i, val)

        return p

    def reader_graph_loop_proxy(self, v):
        assert v.is_proxy()
        inner = self.reader_graph_loop(v.get_proxied())
        p = v.replace_proxied(inner)
        self.state[v] = p
        return p

    def reader_graph_loop_equal_hash(self, v):
        from pycket.hash.equal import W_EqualHashTable
        assert isinstance(v, W_EqualHashTable)
        empty = v.make_empty()
        self.state[v] = empty
        for key, val in v.hash_items():
            key = self.reader_graph_loop(key)
            val = self.reader_graph_loop(val)
            empty._set(key, val)
        return empty

    def reader_graph_loop(self, v):
        assert v is not None
        from pycket.hash.equal import W_EqualHashTable
        if v in self.state:
            return self.state[v]
        if v.is_proxy():
            return self.reader_graph_loop_proxy(v)
        if isinstance(v, values.W_Cons):
            return self.reader_graph_loop_cons(v)
        if isinstance(v, values_vector.W_Vector):
            return self.reader_graph_loop_vector(v)
        if isinstance(v, values_struct.W_Struct):
            return self.reader_graph_loop_struct(v)
        if isinstance(v, W_EqualHashTable):
            return self.reader_graph_loop_equal_hash(v)
        if isinstance(v, values.W_Placeholder):
            return self.reader_graph_loop(v.value)
        # XXX FIXME: doesn't handle stuff
        return v

@expose("make-reader-graph", [values.W_Object])
@jit.dont_look_inside
def make_reader_graph(v):
    from rpython.rlib.nonconst import NonConstant
    builder = ReaderGraphBuilder()
    if NonConstant(False):
        # XXX JIT seems be generating questionable code when the argument of
        # make-reader-graph is a virtual cons cell. The car and cdr fields get
        # set by the generated code after the call, causing reader_graph_loop to
        # crash. I suspect the problem has to do with the translators effect analysis.
        # Example:
        # p29 = new_with_vtable(descr=<SizeDescr 24>)
        # p31 = call_r(ConstClass(make_reader_graph), p29, descr=<Callr 8 r EF=5>)
        # setfield_gc(p29, p15, descr=<FieldP pycket.values.W_WrappedCons.inst__car 8 pure>)
        # setfield_gc(p29, ConstPtr(ptr32), descr=<FieldP pycket.values.W_WrappedCons.inst__cdr 16 pure>)
        if isinstance(v, values.W_WrappedCons):
            print(v._car.tostring())
            print(v._cdr.tostring())
    return builder.reader_graph_loop(v)

@expose("procedure-specialize", [procedure])
def procedure_specialize(proc):
    from pycket.ast_visitor import copy_ast
    # XXX This is the identity function simply for compatibility.
    # Another option is to wrap closures in a W_PromotableClosure, which might
    # get us a similar effect from the RPython JIT.
    if not isinstance(proc, values.W_Closure1AsEnv):
        return proc
    code = copy_ast(proc.caselam)
    vals = proc._get_full_list()
    new_closure = values.W_Closure1AsEnv.make(vals, code, proc._prev)
    return proc

@expose("processor-count", [])
def processor_count():
    return values.W_Fixnum.ONE

cached_values = {}

@continuation
def thunk_cont(index, env, cont, _vals):
    from pycket.interpreter import check_one_val, return_value
    val = check_one_val(_vals)
    cached_values[index] = val
    return return_value(val, env, cont)

@expose("cache-configuration", [values.W_Fixnum, values.W_Object], simple=False)
def cache_configuration(index, proc, env, cont):
    from pycket.interpreter import return_value

    if index in cached_values:
        return return_value(cached_values[index], env, cont)

    return proc.call([], env, thunk_cont(index, env, cont))

@expose("make-readtable", [values.W_Object, values.W_Character, values.W_Symbol, procedure], only_old=True)
def make_readtable(parent, char, sym, proc):
    print("making readtable", [parent, char, sym, proc])
    return values.W_ReadTable(parent, char, sym, proc)

@expose("read/recursive", only_old=True)
def read_recursive(args):
    return values.w_false

def make_stub_predicates(names):
    for name in names:
        message = "%s: not yet implemented" % name
        @expose(name, [values.W_Object])
        def predicate(obj):
            if not objectmodel.we_are_translated():
                print(message)
            return values.w_false
        predicate.__name__ = "stub_predicate(%s)" % name

def make_stub_predicates_no_linklet():
    STUB_PREDICATES_NO_LINKLET = ["namespace-anchor?",
                                  "rename-transformer?",
                                  "readtable?",
                                  "liberal-define-context?",
                                  "compiled-expression?",
                                  "special-comment?",
                                  "internal-definition-context?",
                                  "namespace?",
                                  "compiled-module-expression?"]
    make_stub_predicates(STUB_PREDICATES_NO_LINKLET)

if not w_global_config.is_expander_loaded():
    make_stub_predicates_no_linklet()

@expose("unsafe-start-atomic", [])
def unsafe_start_atomic():
    return values.w_void

@expose("unsafe-start-breakable-atomic", [])
def unsafe_start_atomic():
    return values.w_void

@expose("unsafe-end-breakable-atomic", [])
def unsafe_start_atomic():
    return values.w_void

@expose("unsafe-end-atomic", [])
def unsafe_start_atomic():
    return values.w_void

@expose("__dummy-function__", [])
def __dummy__():
    from rpython.rlib.rbigint  import ONERBIGINT
    from rpython.rlib.runicode import str_decode_utf_8
    ex = ONERBIGINT.touint()
    print(ex)


@expose("primitive-table", [values.W_Object])
def primitive_table(v):
    if v not in select_prim_table:
        return values.w_false

    if v in prim_table_cache:
        return prim_table_cache[v]

    expose_env = {}
    for prim_name_sym in select_prim_table[v]:
        if prim_name_sym in prim_env:
            expose_env[prim_name_sym] = prim_env[prim_name_sym]

    table = make_simple_immutable_table(W_EqImmutableHashTable,
                                        expose_env.keys(),
                                        expose_env.values())

    prim_table_cache[v] = table
    return table

@expose("unquoted-printing-string", [values_string.W_String])
def up_string(s):
    return values.W_UnquotedPrintingString(s)

@expose("unquoted-printing-string-value", [values.W_UnquotedPrintingString])
def ups_val(v):
    return v.string


# Any primitive on Pycket can use "w_global_config.is_debug_active()"
# to control debug outputs (or breakpoints in the interpreter) (with
# an even greater output control with the console_log with verbosity
# levels)
@expose("pycket:activate-debug", [])
def activate_debug():
    w_global_config.activate_debug()

@expose("pycket:deactivate-debug", [])
def activate_debug():
    w_global_config.deactivate_debug()

@expose("pycket:is-debug-active", [])
def debug_status():
    return values.W_Bool.make(w_global_config.is_debug_active())

# Maybe we should do it with just one Racket level parameter
@expose("pycket:get-verbosity", [])
def get_verbosity():
    lvl = w_global_config.get_config_val('verbose')
    return values.W_Fixnum(lvl)

@expose("pycket:set-verbosity", [values.W_Fixnum])
def set_verbosity(v):
    w_global_config.set_config_val('verbose', v.value)

@expose("pycket:activate-keyword", [values.W_Symbol])
def activate_debug_keyword(v):
    w_global_config.activate_keyword(v.variable_name())

@expose("pycket:deactivate-keyword", [values.W_Symbol])
def deactivate_debug_keyword(v):
    w_global_config.deactivate_keyword(v.variable_name())

@expose("pycket:report-undefined-prims", [])
def report_undefined_prims():
    from pycket.prims.primitive_tables import report_undefined_prims
    report_undefined_prims()

addr_sym = values.W_Symbol.make("mem-address")

@expose("pycket:print", [values.W_Object, default(values.W_Symbol, addr_sym)])
def pycket_print(o, sym):
    from pycket.util import console_log
    if sym is addr_sym:
        console_log("PYCKET:PRINT : %s" % o, debug=True)
    else:
        console_log("PYCKET:PRINT : %s" % o.tostring(), debug=True)

@expose("pycket:eq?", [values.W_Object, values.W_Object])
def pycket_eq(o1, o2):
    return values.W_Bool.make(o1 is o2)

expose_val("error-print-width", values_parameter.W_Parameter(values.W_Fixnum.make(256)))

@expose("banner", [])
def banner():
    from pycket.env import w_version
    version = w_version.get_version()
    return values_string.W_String.make("Welcome to Pycket %s.\n"%version)

executable_yield_handler = values_parameter.W_Parameter(do_void.w_prim)

expose_val("executable-yield-handler", executable_yield_handler)

current_load_extension = values_parameter.W_Parameter(do_void.w_prim)
expose_val("current-load-extension", current_load_extension)

@expose("system-language+country", [])
def lang_country():
    return values_string.W_String.make("en_US.UTF-8")


@expose("unsafe-add-post-custodian-shutdown", [values.W_Object])
def add_post(p):
    return values.w_void


@expose("make-will-executor", [])
def make_will_exec():
    return values.W_WillExecutor()

@expose("will-register", [values.W_WillExecutor, values.W_Object, values.W_Object])
def will_register(w, v, p):
    return values.w_void

@expose("will-execute", [values.W_WillExecutor])
def will_exec(w):
    return values.w_void

@expose("will-try-execute", [values.W_WillExecutor, default(values.W_Object, values.w_false)])
def will_exec(w, v):
    return v

@expose("thread", [values.W_Object])
def thread(p):
    return values.W_Thread()

@expose("thread/suspend-to-kill", [values.W_Object])
def thread_susp(p):
    return values.W_Thread()

@expose("make-channel", [])
def make_channel():
    return values.W_Channel()

@expose("primitive-lookup", [values.W_Symbol], simple=True)
def primitive_lookup(sym):
    return prim_env.get(sym, values.w_false)

@expose("get-installation-name", [default(values.W_Object, None)])
def get_installation_name(config):
    return values_string.W_String.make("development")