#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket              import values, values_string
from pycket.cont         import continuation, loop_label, call_cont, Prompt
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, procedure, make_procedure

def find_continuation_prompt(tag, cont):
    while cont is not None:
        if isinstance(cont, Prompt) and cont.tag is tag:
            return cont
        cont = cont.get_previous_continuation()
    return None

@expose("continuation-prompt-available?", [values.W_ContinuationPromptTag, default(values.W_Continuation, None)], simple=False)
def cont_prompt_avail(tag, continuation, env, cont):
    from pycket.interpreter import return_value
    if continuation is not None:
        cont = continuation.cont
    prompt = find_continuation_prompt(tag, cont)
    available = values.W_Bool.make(prompt is not None)
    return return_value(available, env, cont)

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

@expose(["call/cc", # Racket < 6.2.900.10
        "call-with-current-continuation",
         "call/ec", # Racket < 6.2.900.10
         "call-with-escape-continuation"],
        [procedure], simple=False, extra_info=True)
def callcc(a, env, cont, extra_call_info):
    return a.call_with_extra_info([values.W_Continuation(cont)], env, cont, extra_call_info)

@expose("make-continuation-prompt-tag", [default(values.W_Symbol, None)])
def make_continuation_prompt_tag(s):
    from pycket.interpreter import Gensym
    s = Gensym.gensym("cm") if s is None else s
    return values.W_ContinuationPromptTag(s)

@expose("default-continuation-prompt-tag", [])
def default_continuation_prompt_tag():
    return values.w_default_continuation_prompt_tag

@expose("abort-current-continuation", simple=False)
def abort_current_continuation(args, env, cont):
    from pycket.interpreter import return_multi_vals
    if len(args) < 1:
        raise SchemeException("abort-current-continuation: expected 1 or more args")
    tag, args = args[0], args[1:]
    if not isinstance(tag, values.W_ContinuationPromptTag):
        raise SchemeException("abort-current-continuation: expected prompt-tag for argument 0")
    prompt = find_continuation_prompt(tag, cont)
    if prompt is not None:
        handler = prompt.handler
        cont    = prompt.get_previous_continuation()
        assert cont is not None
        return handler.call(args, env, cont)
    raise SchemeException("abort-current-continuation: no such prompt exists")

@make_procedure("default-continuation-prompt-handler", [procedure], simple=False)
def default_continuation_prompt_handler(proc, env, cont):
    return proc.call([], env, cont)

@expose("call-with-continuation-prompt", simple=False)
def call_with_continuation_prompt(args, env, cont):
    if len(args) < 1:
        raise SchemeException("call-with-continuation-prompt: not given enough values")
    idx     = 1
    fun     = args[0]
    tag     = values.w_default_continuation_prompt_tag
    handler = default_continuation_prompt_handler
    if idx < len(args):
        tag = args[idx]
        idx += 1
    if idx < len(args):
        if handler is not values.w_false:
            handler = args[idx]
        idx += 1
    args = args[idx:]
    if not fun.iscallable():
        raise SchemeException("call-with-continuation-prompt: not given callable function")
    if not handler.iscallable():
        raise SchemeException("call-with-continuation-prompt: not given callable handler")
    cont = Prompt(tag, handler, env, cont)
    return fun.call(args, env, cont)

@expose("raise", [values.W_Object, default(values.W_Object, values.w_true)], simple=False)
def raise_exception(v, barrier, env, cont):
    # TODO: Handle case where barrier is not #t
    assert barrier is values.w_true

    handler = None
    while cont is not None:
        handler = cont.find_cm(values.exn_handler_key)
        if handler is not None:
            break
        cont = cont.get_previous_continuation()
    else:
        raise SchemeException("uncaught exception:\n %s" % v.tostring())

    if not handler.iscallable():
        raise SchemeException("provided handler is not callable")

    assert cont is not None
    return handler.call([v], env, cont)

@expose("raise-argument-error")
def raise_arg_err(args):
    nargs = len(args)
    if nargs < 3:
        raise SchemeException("raise-argument-error: expected at least 3 arguments")
    name = args[0]
    if not isinstance(name, values.W_Symbol):
        raise SchemeException("raise-argument-error: expected symbol as the first argument")
    expected = args[1]
    if not isinstance(expected, values_string.W_String):
        raise SchemeException("raise-argument-error: expected string as the second argument")
    if nargs == 3:
        # case 1
        v = args[2]
        raise SchemeException("%s: expected %s but got %s" % (
            name.utf8value, expected.as_str_utf8(), v.tostring()))
    else:
        # case 2
        bad_v = args[2]
        if not isinstance(bad_v, values.W_Fixnum):
            raise SchemeException("raise-argument-error: expected number as the third argument")
        value = bad_v.value
        if value >= nargs - 3:
            raise SchemeException("raise-argument-error: out of bounds number as the third argument")
        v = args[value + 3]
        # FIXME: actually print the other arguments
        raise SchemeException("%s: contract violation\n  expected: %s\n  given: %s\n argument position: %s"%(
             name.utf8value, expected.as_str_utf8(), v.tostring(), value + 1))

@expose("raise-arguments-error")
def raise_args_err(args):
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

@expose("raise-type-error", [values.W_Symbol, values_string.W_String, values.W_Object])
def raise_type_error(name, expected, v):
    raise SchemeException("%s: expected %s in %s" % (name.tostring(), expected.tostring(), v.tostring()))

