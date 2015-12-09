#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket              import values
from pycket.cont         import continuation, loop_label, call_cont, Prompt
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, procedure, make_procedure

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

def find_continuation_prompt(tag, cont):
    while cont is not None:
        if isinstance(cont, Prompt) and cont.tag is tag:
            return cont
        cont = cont.get_previous_continuation()
    return None

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
        return handler.call(args, env, prompt)
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

