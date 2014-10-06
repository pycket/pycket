#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os
import time
from pycket import impersonators as imp
from pycket import values
from pycket.cont import continuation, loop_label, call_cont
from pycket import cont
from pycket import values_struct
from pycket import vector as values_vector
from pycket.error import SchemeException
from pycket.prims.expose import unsafe, default, expose, expose_val, procedure, make_call_method
from rpython.rlib import jit
from rpython.rlib.rsre import rsre_re as re

# import for side effects
from pycket.prims import continuation_marks
from pycket.prims import equal as eq_prims
from pycket.prims import hash
from pycket.prims import impersonator
from pycket.prims import numeric
from pycket.prims import random
from pycket.prims import string
from pycket.prims import undefined
from pycket.prims import vector
from pycket.prims import input_output

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
        ("string?", values.W_String),
        ("symbol?", values.W_Symbol),
        ("boolean?", values.W_Bool),
        ("inspector?", values_struct.W_StructInspector),
        ("struct-type?", values_struct.W_StructType),
        ("struct-constructor-procedure?", values_struct.W_StructConstructor),
        ("struct-predicate-procedure?", values_struct.W_StructPredicate),
        ("struct-accessor-procedure?", values_struct.W_StructAccessor),
        ("struct-mutator-procedure?", values_struct.W_StructMutator),
        ("struct-type-property?", values_struct.W_StructProperty),
        ("struct-type-property-accessor-procedure?", values_struct.W_StructPropertyAccessor),
        ("box?", values.W_Box),
        ("regexp?", values.W_Regexp),
        ("pregexp?", values.W_PRegexp),
        ("byte-regexp?", values.W_ByteRegexp),
        ("byte-pregexp?", values.W_BytePRegexp),
        ("variable-reference?", values.W_VariableReference),
        ("syntax?", values.W_Syntax),
        ("thread-cell?", values.W_ThreadCell),
        ("thread-cell-values?", values.W_ThreadCellValues),
        ("semaphore?", values.W_Semaphore),
        ("semaphore-peek-evt?", values.W_SemaphorePeekEvt),
        ("path?", values.W_Path),
        ("arity-at-least?", values.W_ArityAtLeast),
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
        ("impersonator-property-accessor-procedure?", imp.W_ImpPropertyAccessor),
        ("impersonator-property?", imp.W_ImpPropertyDescriptor),
        ("parameter?", values.W_Parameter),
        ("parameterization?", values.W_Parameterization),
        # FIXME: Assumes we only have eq-hashes
        ("hash?", values.W_HashTable),
        ("hash-eq?", values.W_HashTable),
        ("hash-eqv?", values.W_HashTable),
        ("hash-equal?", values.W_HashTable),
        ("hash-weak?", values.W_HashTable)
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
    if isinstance(val, values.W_Bignum):
        # XXX this should never be reachable
        try:
            v = val.value.toint()
            return values.W_Bool.make(0 <= v <= 255)
        except OverflowError:
            return values.w_false
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
def syntax_to_datum(v):
    return v.val

@expose("compiled-module-expression?", [values.W_Object])
def compiled_module_expression(v):
    return values.w_false

expose_val("null", values.w_null)
expose_val("true", values.w_true)
expose_val("false", values.w_false)
expose_val("exception-handler-key", values.exn_handler_key)
expose_val("parameterization-key", values.parameterization_key)
expose_val("print-mpair-curly-braces", values.W_Parameter(values.w_false))
expose_val("print-pair-curly-braces", values.W_Parameter(values.w_false))

# FIXME: need stronger guards for all of these
for name in ["prop:evt",
             "prop:output-port",
             "prop:impersonator-of",
             "prop:method-arity-error",
             "prop:exn:srclocs",
             "prop:custom-print-quotable",
             "prop:incomplete-arity"]:
    expose_val(name, values_struct.W_StructProperty(values.W_Symbol.make(name), values.w_false))

expose_val("prop:procedure", values_struct.w_prop_procedure)
expose_val("prop:checked-procedure", values_struct.w_prop_checked_procedure)
expose_val("prop:arity-string", values_struct.w_prop_arity_string)
expose_val("prop:custom-write", values_struct.w_prop_custom_write)
expose_val("prop:equal+hash", values_struct.w_prop_equal_hash)
expose_val("prop:chaperone-unsafe-undefined", values_struct.w_prop_chaperone_unsafe_undefined)

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
    return first_field.call([v1, v2], env, check_cont(proc, v, v1, v2, env, cont))

@expose("checked-procedure-check-and-extract",
        [values_struct.W_StructType, values.W_Object, procedure,
         values.W_Object, values.W_Object], simple=False)
def do_checked_procedure_check_and_extract(type, v, proc, v1, v2, env, cont):
    if isinstance(v, values_struct.W_Struct):
        st = v.struct_type()
        if st is type:
            return v.ref(v.struct_type(), 0, env,
                    receive_first_field(proc, v, v1, v2, env, cont))
    return proc.call([v, v1, v2], env, cont)

################################################################
# printing

@expose("current-logger", [])
def current_logger():
    return values.current_logger

@expose("make-logger", [values.W_Symbol, values.W_Logger])
def make_logger(name, parent):
    return values.W_Logger()

@expose("make-parameter", [values.W_Object, default(values.W_Object, values.w_false)])
def make_parameter(init, guard):
    return values.W_Parameter(init, guard)

@expose("system-library-subpath", [default(values.W_Object, values.w_false)])
def sys_lib_subpath(mode):
    return values.W_Path("x86_64-linux") # FIXME

@expose("primitive-closure?", [values.W_Object])
def prim_clos(v):
    return values.w_false

@expose("char->integer", [values.W_Character])
def char2int(c):
    return values.W_Fixnum(ord(c.value))

@expose("string->list", [values.W_String])
def string_to_list(s):
    return values.to_list([values.W_Character(i) for i in s.value])


################################################################
# built-in struct types

def define_struct(name, super=values.w_null, fields=[]):
    struct_type, struct_constr, struct_pred, struct_acc, struct_mut = \
        values_struct.W_StructType.make_simple(values.W_Symbol.make(name),
            super, values.W_Fixnum(len(fields)), values.W_Fixnum(0),
            values.w_false, values.w_null, values.w_false).make_struct_tuple()
    expose_val("struct:" + name, struct_type)
    expose_val(name, struct_constr)
    # this is almost always also provided
    expose_val("make-" + name, struct_constr)
    expose_val(name + "?", struct_pred)
    for field, field_name in enumerate(fields):
        acc = values_struct.W_StructFieldAccessor(struct_acc, values.W_Fixnum(field), values.W_Symbol.make(field_name))
        expose_val(name + "-" + field_name, acc)
    return struct_type

exn = define_struct("exn", values.w_null, ["message", "continuation-marks"])
exn_fail = define_struct("exn:fail", exn)
exn_fail_contract = define_struct("exn:fail:contract", exn_fail)
exn_fail_contract_arity = define_struct("exn:fail:contract:arity", exn_fail)
exn_fail_contract_divide_by_zero = define_struct("exn:fail:contract:divide-by-zero", exn_fail)
exn_fail_contract_non_fixnum_result = define_struct("exn:fail:contract:non-fixnum-result", exn_fail)
exn_fail_contract_continuation = define_struct("exn:fail:contract:continuation", exn_fail)
exn_fail_contract_variable = define_struct("exn:fail:contract:variable", exn_fail, ["id"])
exn_fail_syntax = define_struct("exn:fail:syntax", exn_fail, ["exprs"])
exn_fail_syntax_unbound = define_struct("exn:fail:syntax:unbound", exn_fail_syntax)
exn_fail_syntax_missing_module = define_struct("exn:fail:syntax:missing-module", exn_fail_syntax, ["path"])
exn_fail_read = define_struct("exn:fail:read", exn_fail, ["srclocs"])
exn_fail_read_eof = define_struct("exn:fail:read:eof", exn_fail_read)
exn_fail_read_non_char = define_struct("exn:fail:read:non-char", exn_fail_read)
exn_fail_fs = define_struct("exn:fail:filesystem", exn_fail)
exn_fail_fs_exists = define_struct("exn:fail:filesystem:exists", exn_fail_fs)
exn_fail_fs_version = define_struct("exn:fail:filesystem:version", exn_fail_fs)
exn_fail_fs_errno = define_struct("exn:fail:filesystem:errno", exn_fail_fs, ["errno"])
exn_fail_fs_missing_module = define_struct("exn:fail:filesystem:missing-module", exn_fail_fs, ["path"])
exn_fail_network = define_struct("exn:fail:network", exn_fail)
exn_fail_network_errno = define_struct("exn:fail:network:errno", exn_fail_network, ["errno"])
exn_fail_out_of_memory = define_struct("exn:fail:out-of-memory", exn_fail)
exn_fail_unsupported = define_struct("exn:fail:unsupported", exn_fail)
exn_fail_user = define_struct("exn:fail:user", exn_fail)
exn_break = define_struct("exn:break", exn)
exn_break_hang_up = define_struct("exn:break:hang-up", exn_break)
exn_break_terminate = define_struct("exn:break:terminate", exn_break)

srcloc = define_struct("srcloc", fields=["source", "line", "column", "position", "span"])
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
date_star_struct = define_struct("date*", date_struct, fields=["nanosecond",
                                                               "time-zone-name"])

def define_nyi(name, args=None):
    @expose(name, args, nyi=True)
    def nyi(args):
        pass

for args in [ ("subprocess?",),
              ("file-stream-port?",),
              ("terminal-port?",),
              ("byte-ready?",),
              ("char-ready?",),
              ("bytes-converter?",),
              ("char-alphabetic?",),
              ("char-numeric?",),
              ("char-symbolic?",),
              ("char-graphic?",),
              ("char-whitespace?",),
              ("char-blank?",),
              ("char-iso-control?",),
              ("char-punctuation?",),
              ("char-upper-case?",),
              ("char-title-case?",),
              ("char-lower-case?",),
              ("compiled-expression?",),
              ("custom-write?",),
              ("custom-print-quotable?",),
              ("liberal-define-context?",),
              ("handle-evt?",),
              ("procedure-struct-type?",),
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
              ("set!-transformer?",),
              ("rename-transformer?",),
              ("identifier?",),
              ("port?",),
              ("sequence?",),
              ("namespace-anchor?",),
              ("chaperone-channel",),
              ("impersonate-channel",),
              ]:
    define_nyi(*args)

@expose("object-name", [values.W_Object])
def object_name(v):
    return values.W_String(v.tostring())

@expose("namespace-variable-value", [values.W_Symbol,
    default(values.W_Object, values.w_true),
    default(values.W_Object, values.w_true),
    default(values.W_Object, None)])
def namespace_variable_value(sym, use_mapping, failure_thunk, namespace):
    return values.w_void

@expose("find-main-config", [])
def find_main_config():
    return values.w_false

@expose("version", [])
def version():
    from .. import interpreter
    version = interpreter.GlobalConfig.lookup("version")
    return values.W_String("unknown version" if version is None else version)

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

@expose("arity-at-least", [values.W_Fixnum])
def arity_at_least(n):
    return values.W_ArityAtLeast(n.value)

@expose("arity-at-least-value", [values.W_ArityAtLeast])
def arity_at_least(a):
    return values.W_Fixnum(a.val)

@expose("procedure-rename", [procedure, values.W_Object])
def procedure_rename(p, n):
    return p

@expose("procedure-arity", [procedure])
def arity_at_least(n):
    # FIXME
    return values.W_ArityAtLeast(0)

@expose("procedure-arity?", [values.W_Object])
def arity_at_least_p(n):
    if isinstance(n, values.W_Fixnum):
        if n.value >= 0:
            return values.w_true
    elif isinstance(n, values.W_ArityAtLeast):
        return values.w_true
    elif isinstance(n, values.W_List):
        for item in values.from_list(n):
            if not (isinstance(item, values.W_Fixnum) or isinstance(item, values.W_ArityAtLeast)):
                return values.w_false
        return values.w_true
    return values.w_false

@expose("procedure-arity-includes?", [procedure, values.W_Number, default(values.W_Object, values.w_false)])
@jit.unroll_safe
def procedure_arity_includes(p, n, w_kw_ok):
    # for now, ignore kw_ok
    if not isinstance(n, values.W_Fixnum):
        return values.w_false # valid arities are always small integers
    n_val = n.value
    (ls, at_least) = p.get_arity()
    for i in ls:
        if n_val == i: return values.w_true
    if at_least != -1 and n_val >= at_least:
        return values.w_true
    return values.w_false

@expose("variable-reference-constant?", [values.W_VariableReference], simple=False)
def varref_const(varref, env, cont):
    from pycket.interpreter import return_value
    return return_value(values.W_Bool.make(not(varref.varref.is_mutable(env))), env, cont)

@expose("variable-reference->resolved-module-path",  [values.W_VariableReference])
def varref_rmp(varref):
    return values.W_ResolvedModulePath(values.W_Path(varref.varref.path))

@expose("variable-reference->module-source",  [values.W_VariableReference])
def varref_ms(varref):
    # FIXME: not implemented
    return values.W_Symbol.make("dummy_module")

@expose("resolved-module-path-name", [values.W_ResolvedModulePath])
def rmp_name(rmp):
    return rmp.name

@expose("module-path?", [values.W_Object])
def module_pathp(v):
    if isinstance(v, values.W_Symbol):
        # FIXME: not always right
        return values.w_true
    if isinstance(v, values.W_Path):
        return values.w_true
    # FIXME
    return values.w_false

@expose("values")
def do_values(args_w):
    return values.Values.make(args_w)

@expose("call-with-values", [procedure] * 2, simple=False)
def call_with_values (producer, consumer, env, cont):
    # FIXME: check arity
    return producer.call([], env, call_cont(consumer, env, cont))

@continuation
def time_apply_cont(initial, env, cont, vals):
    from pycket.interpreter import return_multi_vals
    final = time.clock()
    ms = values.W_Fixnum(int((final - initial) * 1000))
    vals_l = vals._get_full_list()
    results = values.Values.make([values.to_list(vals_l), ms, ms, values.W_Fixnum(0)])
    return return_multi_vals(results, env, cont)

@expose("continuation-prompt-available?")
def cont_prompt_avail(args):
    return values.w_false

# FIXME: this is a data type
@expose("continuation-prompt-tag?")
def cont_prompt_tag(args):
    return values.w_false

@expose(["call/cc", "call-with-current-continuation",
         "call/ec", "call-with-escape-continuation"],
        [procedure], simple=False)
def callcc(a, env, cont):
    return a.call([values.W_Continuation(cont)], env, cont)

@expose("time-apply", [procedure, values.W_List], simple=False)
def time_apply(a, args, env, cont):
    initial = time.clock()
    return a.call(values.from_list(args), env, time_apply_cont(initial, env, cont))

@expose("apply", simple=False)
def apply(args, env, cont):
    if not args:
        raise SchemeException("apply expected at least one argument, got 0")
    fn = args[0]
    if not fn.iscallable():
        raise SchemeException("apply expected a procedure, got something else")
    lst = args[-1]
    if not listp_loop(lst):
        raise SchemeException("apply expected a list as the last argument, got something else")
    args_len = len(args)-1
    assert args_len >= 0
    others = args[1:args_len]
    new_args = others + values.from_list(lst)
    return fn.call(new_args, env, cont)

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
        if isinstance(a, values.W_Null):
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

@expose("cadr")
def do_cadr(args):
    return do_car([do_cdr(args)])

@expose("cddr")
def do_cddr(args):
    return do_cdr([do_cdr(args)])

@expose("caddr")
def do_caddr(args):
    return do_car([do_cdr([do_cdr(args)])])

@expose("cadddr")
def do_cadddr(args):
    return do_car([do_cdr([do_cdr([do_cdr(args)])])])

@expose("cdr", [values.W_Cons])
def do_cdr(a):
    return a.cdr()

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

@expose("for-each", [procedure, values.W_List], simple=False)
def for_each(f, l, env, cont):
    from pycket.interpreter import return_value
    return return_value(values.w_void, env, for_each_cont(f, l, env, cont))

@continuation
def for_each_cont(f, l, env, cont, vals):
    from pycket.interpreter import return_value
    if l is values.w_null:
        return return_value(values.w_void, env, cont)
    return f.call([l.car()], env, for_each_cont(f, l.cdr(), env, cont))


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

@expose("make-inspector", [default(values_struct.W_StructInspector, None)])
def do_make_instpector(inspector):
    return values_struct.W_StructInspector.make(inspector)

@expose("make-sibling-inspector", [default(values_struct.W_StructInspector, None)])
def do_make_sibling_instpector(inspector):
    return values_struct.W_StructInspector.make(inspector, True)

@expose("current-inspector")
def do_current_instpector(args):
    return values_struct.current_inspector

@expose("struct?", [values.W_Object])
def do_is_struct(v):
    return values.W_Bool.make(isinstance(v, values_struct.W_RootStruct) and
                              not v.struct_type().isopaque)

def is_struct_info(v):
    if isinstance(v, values.W_Cons):
        struct_info = values.from_list(v)
        if len(struct_info) == 6:
            if not isinstance(struct_info[0], values_struct.W_StructType) and\
                struct_info[0] is not values.w_false:
                return False
            if not isinstance(struct_info[1], values_struct.W_StructConstructor) and\
                struct_info[1] is not values.w_false:
                return False
            if not isinstance(struct_info[2], values_struct.W_StructPredicate) and\
                struct_info[2] is not values.w_false:
                return False
            accessors = struct_info[3]
            if isinstance(accessors, values.W_Cons):
                for accessor in values.from_list(accessors):
                    if not isinstance(accessor, values_struct.W_StructFieldAccessor):
                        if accessor is not values.w_false and\
                            accessor is values.from_list(accessors)[-1]:
                            return False
            else:
                return False
            mutators = struct_info[4]
            if isinstance(mutators, values.W_Cons):
                for mutator in values.from_list(mutators):
                    if not isinstance(mutator, values_struct.W_StructFieldAccessor):
                        if mutator is not values.w_false and\
                          mutator is values.from_list(mutators)[-1]:
                          return False
            else:
                return False
            if not isinstance(struct_info[5], values_struct.W_StructType) and\
                not isinstance(struct_info[5], values.W_Bool):
                return False
            return True
        return False
    elif isinstance(v, values.W_Prim):
        if v.name == "make-struct-info":
            return True
    # TODO: it can be also:
    # 1. a structure with the prop:struct-info property
    # 2. a structure type derived from struct:struct-info or
    # with prop:struct-info and wrapped with make-set!-transformer
    return False

@expose("struct-info?", [values.W_Object])
def do_is_struct_info(v):
    return values.W_Bool.make(is_struct_info(v))

@expose("checked-struct-info?", [values.W_Object])
def do_is_checked_struct_info(v):
    if isinstance(v, values.W_Prim):
        if v.name == "make-struct-info":
            # TODO: only when no parent type is specified or
            # the parent type is also specified through a transformer binding to such a value
            return values.w_true
    return values.w_false

@expose("make-struct-info", [procedure])
def do_make_struct_info(thunk):
    # FIXME: return values.W_Prim("make-struct-info", thunk.call)
    return values.w_void

@expose("extract-struct-info", [values.W_Object], simple=False)
def do_extract_struct_info(v, env, cont):
    assert is_struct_info(v)
    from pycket.interpreter import return_value
    if isinstance(v, values.W_Cons):
        return return_value(v, env, cont)
    elif isinstance(v, values.W_Prim):
        return v.call([], env, cont)
    else:
        # TODO: it can be also:
        # 1. a structure with the prop:struct-info property
        # 2. a structure type derived from struct:struct-info or
        # with prop:struct-info and wrapped with make-set!-transformer
        return return_value(values.w_void, env, cont)

@expose("struct-info", [values_struct.W_RootStruct])
def do_struct_info(struct):
    # TODO: if the current inspector does not control any
    # structure type for which the struct is an instance then return w_false
    struct_type = struct.struct_type() if True else values.w_false
    skipped = values.w_false
    return values.Values.make([struct_type, skipped])

@expose("struct-type-info", [values_struct.W_StructType])
def do_struct_type_info(struct_type):
    return values.Values.make(struct_type.struct_type_info())

@expose("struct-type-make-constructor", [values_struct.W_StructType])
def do_struct_type_make_constructor(struct_type):
    # TODO: if the type for struct-type is not controlled by the current inspector,
    # the exn:fail:contract exception should be raised
    return struct_type.constr

@expose("struct-type-make-predicate", [values_struct.W_StructType])
def do_struct_type_make_predicate(struct_type):
    # TODO: if the type for struct-type is not controlled by the current inspector,
    #the exn:fail:contract exception should be raised
    return struct_type.pred

@expose("make-struct-type",
        [values.W_Symbol, values.W_Object, values.W_Fixnum, values.W_Fixnum,
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_null),
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_null),
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_false)], simple=False)
def do_make_struct_type(name, super_type, init_field_cnt, auto_field_cnt,
        auto_v, props, inspector, proc_spec, immutables, guard, constr_name, env, cont):
    if not (isinstance(super_type, values_struct.W_StructType) or super_type is values.w_false):
        raise SchemeException("make-struct-type: expected a struct-type? or #f")
    return values_struct.W_StructType.make(name, super_type, init_field_cnt,
        auto_field_cnt, auto_v, props, inspector, proc_spec, immutables,
        guard, constr_name, env, cont)

@expose("make-struct-field-accessor",
        [values_struct.W_StructAccessor, values.W_Fixnum, default(values.W_Symbol, None)])
def do_make_struct_field_accessor(accessor, field, field_name):
    return values_struct.W_StructFieldAccessor(accessor, field, field_name)

@expose("make-struct-field-mutator",
        [values_struct.W_StructMutator, values.W_Fixnum, default(values.W_Symbol, None)])
def do_make_struct_field_mutator(mutator, field, field_name):
    return values_struct.W_StructFieldMutator(mutator, field, field_name)

@expose("struct->vector", [values_struct.W_RootStruct])
def expose_struct2vector(struct):
    return values_struct.struct2vector(struct)

@expose("prefab-struct-key", [values.W_Object])
def do_prefab_struct_key(v):
    if not (isinstance(v, values_struct.W_Struct) and v._type.isprefab):
        return values.w_false
    prefab_key = values_struct.W_PrefabKey.from_struct_type(v._type)
    return prefab_key.short_key()

@expose("make-prefab-struct")
def do_make_prefab_struct(args):
    assert len(args) > 1
    key = args[0]
    vals = args[1:]
    return values_struct.W_Struct.make_prefab(key, vals)

@expose("prefab-key->struct-type", [values.W_Object, values.W_Fixnum])
def expose_prefab_key2struct_type(w_key, field_count):
    return values_struct.W_StructType.make_prefab(
      values_struct.W_PrefabKey.from_raw_key(w_key, field_count.value))

@expose("prefab-key?", [values.W_Object])
def do_prefab_key(v):
    return values_struct.W_PrefabKey.is_prefab_key(v)

@expose("make-struct-type-property", [values.W_Symbol,
                                      default(values.W_Object, values.w_false),
                                      default(values.W_List, values.w_null),
                                      default(values.W_Object, values.w_false)])
def mk_stp(sym, guard, supers, _can_imp):
    can_imp = False
    if guard is values.W_Symbol.make("can-impersonate"):
        guard = values.w_false
        can_imp = True
    if _can_imp is not values.w_false:
        can_imp = True
    prop = values_struct.W_StructProperty(sym, guard, supers, can_imp)
    return values.Values.make([prop,
                               values_struct.W_StructPropertyPredicate(prop),
                               values_struct.W_StructPropertyAccessor(prop)])

@expose("number->string", [values.W_Number])
def num2str(a):
    return values.W_String(a.tostring())

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

@expose("weak-box-value", [values.W_WeakBox, default(values.W_Object, values.w_false)])
def weak_box_value(wb, default):
    v = wb.get()
    return v if v is not None else default

@expose("make-ephemeron", [values.W_Object] * 2)
def make_ephemeron(key, val):
    return values.W_Ephemeron(key, val)

@expose("ephemeron-value", [values.W_Ephemeron, default(values.W_Object, values.w_false)])
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

# my kingdom for a tail call
def listp_loop(v):
    while True:
        if v is values.w_null: return True
        if isinstance(v, values.W_Cons):
            v = v.cdr()
            continue
        return False

@expose("list?", [values.W_Object])
def consp(v):
    return values.W_Bool.make(listp_loop(v))

@expose("list-ref", [values.W_Cons, values.W_Fixnum])
def list_ref(lst, pos):
    return values.from_list(lst)[pos.value]

@expose("current-inexact-milliseconds", [])
def curr_millis():
    return values.W_Flonum(time.clock()*1000)

@expose("error")
def error(args):
    if len(args) == 1:
        sym = args
        assert isinstance(sym, values.W_Symbol)
        raise SchemeException("error: %s"%sym.tostring())
    else:
        first_arg = args[0]
        if isinstance(first_arg, values.W_String):
            from rpython.rlib.rstring import StringBuilder
            msg = StringBuilder()
            msg.append(first_arg.tostring())
            v = args[1:]
            for item in v:
                msg.append(" %s"%item.tostring())
            raise SchemeException(msg.build())
        else:
            src = first_arg
            form = args[1]
            v = args[2:]
            assert isinstance(src, values.W_Symbol)
            assert isinstance(form, values.W_String)
            raise SchemeException("%s: %s"%(src.tostring(), input_output.format(form, v)))

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

@expose("vector->immutable-vector", [values_vector.W_Vector])
def vector2immutablevector(v):
    # FIXME: it should be immutable
    return v

# FIXME: make that a parameter
@expose("current-command-line-arguments", [], simple=False)
def current_command_line_arguments(env, cont):
    from pycket.interpreter import return_value
    w_v = values_vector.W_Vector.fromelements(
            env.toplevel_env().commandline_arguments)
    return return_value(w_v, env, cont)

# Unsafe struct ops
@expose("unsafe-struct-ref", [values.W_Object, unsafe(values.W_Fixnum)])
def unsafe_struct_ref(v, k):
    while isinstance(v, imp.W_ChpStruct) or isinstance(v, imp.W_ImpStruct):
        v = v.inner
    assert isinstance(v, values_struct.W_Struct)
    assert 0 <= k.value <= v.struct_type().total_field_cnt
    return v._ref(k.value)

@expose("unsafe-struct-set!", [values.W_Object, unsafe(values.W_Fixnum), values.W_Object])
def unsafe_struct_set(v, k, val):
    while isinstance(v, imp.W_ChpStruct) or isinstance(v, imp.W_ImpStruct):
        v = v.inner
    assert isinstance(v, values_struct.W_Struct)
    assert 0 <= k.value < v.struct_type().total_field_cnt
    return v._set(k.value, val)

@expose("unsafe-struct*-ref", [values_struct.W_Struct, unsafe(values.W_Fixnum)])
def unsafe_struct_star_ref(v, k):
    assert 0 <= k.value < v.struct_type().total_field_cnt
    return v._ref(k.value)

@expose("unsafe-struct*-set!", [values_struct.W_Struct, unsafe(values.W_Fixnum), values.W_Object])
def unsafe_struct_star_set(v, k, val):
    assert 0 <= k.value <= v.struct_type().total_field_cnt
    return v._set(k.value, val)

# Unsafe pair ops
@expose("unsafe-car", [values.W_Cons])
def unsafe_car(p):
    return p.car()

@expose("unsafe-cdr", [values.W_Cons])
def unsafe_cdr(p):
    return p.cdr()

@expose("path-string?", [values.W_Object])
def path_stringp(v):
    # FIXME: handle zeros in string
    return values.W_Bool.make(isinstance(v, values.W_String) or isinstance(v, values.W_Path))

@expose("complete-path?", [values.W_Object])
def complete_path(v):
    # FIXME: stub
    return values.w_false

@expose("path->bytes", [values.W_Path])
def path2bytes(p):
    return values.W_Bytes(p.path)

@expose("port-next-location", [values.W_Object], simple=False)
def port_next_loc(p, env, cont):
    from pycket.interpreter import return_multi_vals
    return return_multi_vals(values.Values.make([values.w_false] * 3), env, cont)

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



@expose("symbol->string", [values.W_Symbol])
def symbol_to_string(v):
    return values.W_String(v.value)

@expose("string->symbol", [values.W_String])
def string_to_symbol(v):
    return values.W_Symbol.make(v.value)

@expose("string->number", [values.W_String])
def str2num(w_s):
    from rpython.rlib import rarithmetic, rfloat, rbigint
    from rpython.rlib.rstring import ParseStringError, ParseStringOverflowError

    s = w_s.value
    try:
        if "." in s:
            return values.W_Flonum(rfloat.string_to_float(s))
        else:
            try:
                return values.W_Fixnum(rarithmetic.string_to_int(s, base=0))
            except ParseStringOverflowError:
                return values.W_Bignum(rbigint.rbigint.fromstr(s))
    except ParseStringError as e:
        return values.w_false

@expose("string->unreadable-symbol", [values.W_String])
def string_to_unsymbol(v):
    return values.W_Symbol.make_unreadable(v.value)

@expose("string->immutable-string", [values.W_String])
def string_to_immutable_string(string):
    if string.immutable():
        return string
    return values.W_String(string.value, immutable=True)

@expose("symbol-unreadable?", [values.W_Symbol])
def sym_unreadable(v):
    if v.unreadable:
        return values.w_true
    return values.w_false

@expose("symbol-interned?", [values.W_Symbol])
def string_to_symbol(v):
    return values.W_Bool.make(v.is_interned())

@expose("string->uninterned-symbol", [values.W_String])
def string_to_symbol(v):
    return values.W_Symbol(v.value)

@expose("string->bytes/locale", [values.W_String,
                                 default(values.W_Object, values.w_false),
                                 default(values.W_Integer, values.W_Fixnum(0)),
                                 default(values.W_Integer, None)])
def string_to_bytes_locale(str, errbyte, start, end):
    # FIXME: This ignores the locale
    return values.W_Bytes(str.value)

@expose("integer->char", [values.W_Fixnum])
def integer_to_char(v):
    return values.W_Character(unichr(v.value))

@expose("immutable?", [values.W_Object])
def immutable(v):
    return values.W_Bool.make(v.immutable())

@expose("eval-jit-enabled", [])
def jit_enabled():
    return values.w_true

@expose("make-thread-cell", [values.W_Object, default(values.W_Bool, values.w_false)])
def make_thread_cell(v, pres):
    return values.W_ThreadCell(v, False if pres is values.w_false else True)

@expose("thread-cell-ref", [values.W_ThreadCell])
def thread_cell_ref(cell):
    return cell.value

@expose("thread-cell-set!", [values.W_ThreadCell, values.W_Object])
def thread_cell_set(cell, v):
    cell.value = v
    return values.w_void

@expose("current-preserved-thread-cell-values", [default(values.W_ThreadCellValues, None)])
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

@expose("extend-parameterization", [values.W_Object, values.W_Object, values.W_Object])
def extend_paramz(paramz, key, val):
    if not isinstance(key, values.W_Parameter):
        raise SchemeException("Not a parameter")
    if isinstance(paramz, values.W_Parameterization):
        return paramz.extend([key], [val])
    else:
        return paramz # This really is the Racket behavior

def call_with_parameterization(f, args, paramz, env, cont):
    cont.update_cm(values.parameterization_key, paramz)
    return f.call(args, env, cont)

@expose("call-with-parameterization", [values.W_Object, values.W_Parameterization], simple=False)
def call_w_paramz(f, paramz, env, cont):
    return call_with_parameterization(f, [], paramz, env, cont)

def call_with_extended_paramz(f, args, keys, vals, env, cont):
    paramz = cont.get_mark_first(values.parameterization_key)
    paramz_new = paramz.extend(keys, vals)
    return call_with_parameterization(f, args, paramz_new, env, cont)



@expose("gensym", [default(values.W_Symbol, values.W_Symbol.make("g"))])
def gensym(init):
    from pycket.interpreter import Gensym
    return Gensym.gensym(init.value)

@expose("regexp-match", [values.W_Object, values.W_Object]) # FIXME: more error checking
def regexp_match(r, o):
    assert isinstance(r, values.W_AnyRegexp) or isinstance(r, values.W_String) or isinstance(r, values.W_Bytes)
    assert isinstance(o, values.W_String) or isinstance(o, values.W_Bytes)
    return values.w_false # Back to one problem

@expose("regexp-match?", [values.W_Object, values.W_Object]) # FIXME: more error checking
def regexp_matchp(r, o):
    assert isinstance(r, values.W_AnyRegexp) or isinstance(r, values.W_String) or isinstance(r, values.W_Bytes)
    assert isinstance(o, values.W_String) or isinstance(o, values.W_Bytes)
    # ack, this is wrong
    return values.w_true # Back to one problem

@expose("keyword<?", [values.W_Keyword, values.W_Keyword])
def keyword_less_than(a_keyword, b_keyword):
    return values.W_Bool.make(a_keyword.value < b_keyword.value)

@expose("build-path")
def build_path(args):
    # this is terrible
    r = ""
    for a in args:
        if isinstance(a, values.W_Bytes):
            r = r + a.value
        elif isinstance(a, values.W_String):
            r = r + a.value
        elif isinstance(a, values.W_Path):
            r = r + a.path
        else:
            raise SchemeException("bad input to build-path: %s"%a)
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

@expose("raise-argument-error", [values.W_Symbol, values.W_String, values.W_Object])
def raise_arg_err(sym, str, val):
    raise SchemeException("%s: expected %s but got %s"%(sym.value, str.value, val.tostring()))

@expose("find-system-path", [values.W_Symbol])
def find_sys_path(sym):
    from .. import interpreter
    v = interpreter.GlobalConfig.lookup(sym.value)
    if v:
        return values.W_Path(v)
    else:
        raise SchemeException("unknown system path %s"%sym.value)

@expose("system-type", [default(values.W_Symbol, values.W_Symbol.make("os"))])
def system_type(sym):
    if sym is values.W_Symbol.make("os"):
        # FIXME: make this work on macs
        return values.W_Symbol.make("unix")
    raise SchemeException("unexpected system-type symbol %s"%sym.value)

@expose("find-main-collects", [])
def find_main_collects():
    return values.w_false

@expose("module-path-index-join", [values.W_Object, values.W_Object])
def mpi_join(a, b):
    return values.W_ModulePathIndex()

# Loading

# FIXME: Proper semantics.
@expose("load", [values.W_String], simple=False)
def load(lib, env, cont):
    from pycket.expand import ensure_json_ast_load, load_json_ast_rpython
    lib_name = lib.tostring()
    json_ast = ensure_json_ast_load(lib_name)
    if json_ast is None:
        raise SchemeException("can't gernerate load-file for %s " % lib.tostring())
    ast = load_json_ast_rpython(json_ast)
    raise NotImplementedError("would crash anyway when trying to interpret the Module")
    #return ast, env, cont
    
@expose("current-load-relative-directory", [])
def cur_load_rel_dir():
    return values.w_false

@expose("current-directory", [])
def cur_dir():
    return values.W_Path(os.getcwd())

# Byte stuff
@expose("make-bytes", [values.W_Fixnum, default(values.W_Object, values.W_Fixnum(0))])
def make_bytes(length, byte):
    # assert byte_huh(byte) is values.w_true
    if isinstance(byte, values.W_Fixnum):
        v = byte.value
    elif isinstance(byte, values.W_Bignum):
        try:
            v = byte.value.toint()
        except OverflowError:
            assert False
    else:
        assert False
    assert 0 <= v <= 255
    bstr = chr(v) * length.value
    return values.W_Bytes(bstr, immutable=False)
