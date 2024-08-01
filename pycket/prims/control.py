#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket                    import values, values_parameter, values_string, values_struct, prims
from pycket.arity              import Arity
from pycket.cont               import continuation, loop_label, call_cont, Barrier, Cont, NilCont, Prompt
from pycket.error              import SchemeException
from pycket.argument_parser    import ArgParser, EndOfInput
from pycket.prims.expose       import default, expose, expose_val, procedure, make_procedure
from pycket.prims.plumber      import current_plumber_param
from pycket.util               import add_copy_method
from rpython.rlib              import jit
from rpython.rlib.objectmodel  import specialize

@add_copy_method(copy_method="_clone")
class DynamicWindValueCont(Cont):

    _immutable_fields_ = ['pre', 'post']

    def __init__(self, pre, post, env, cont):
        Cont.__init__(self, env, cont)
        self.pre  = pre
        self.post = post

    def has_unwind(self):
        return True

    def unwind(self, env, cont):
        return self.post.call([], env, cont)

    def has_rewind(self):
        return True

    def rewind(self, env, cont):
        return self.pre.call([], env, cont)

    def plug_reduce(self, _vals, env):
        cont = dynamic_wind_post_cont(_vals, env, self.prev)
        return self.post.call([], env, cont)

@continuation
def call_handler_cont(proc, args, env, cont, _vals):
    return proc.call(args, env, cont)

@continuation
def post_build_exception(env, cont, _vals):
    from pycket.interpreter import check_one_val
    barrier = values.w_true
    val = check_one_val(_vals)
    return raise_exception(val, barrier, env, cont)

def convert_runtime_exception(exn, env, cont):
    from pycket               import values_string
    from pycket.values        import W_ContinuationMarkSet
    from pycket.prims.control import raise_exception
    message = values_string.W_String.fromstr_utf8(exn.msg)
    marks   = W_ContinuationMarkSet(cont, values.w_default_continuation_prompt_tag)
    cont    = post_build_exception(env, cont)
    args    = exn.extra_args

    return exn.get_exn_type().constructor.call([message, marks] + args, env, cont)

def convert_os_error(exn, env, cont):
    from pycket               import values_string
    from pycket.values        import W_ContinuationMarkSet
    from pycket.prims.control import raise_exception
    message = values_string.W_String.fromstr_utf8(exn.strerror)
    marks   = W_ContinuationMarkSet(cont, values.w_default_continuation_prompt_tag)
    cont    = post_build_exception(env, cont)

    return prims.general.exn_fail.constructor.call([message, marks], env, cont)

@jit.unroll_safe
def scan_continuation(curr, prompt_tag, look_for=None, escape=False):
    """
    Segment a continuation based on a given continuation-prompt-tag.
    The head of the continuation, up to and including the desired continuation
    prompt is reversed (in place), and the tail is returned un-altered.

    The hint value |look_for| is used to determine when the continuation being
    installed is a prefix of the extant continuation.
    In this case, installing the continuation is much simpler, as the expensive
    merge operation needed to find common substructure is the two continuation is
    not needed.
    """
    handlers = False
    xs = []
    while isinstance(curr, Cont):
        if curr is look_for:
            return None, handlers
        handlers |= isinstance(curr, DynamicWindValueCont)
        xs.append(curr)
        if isinstance(curr, Prompt) and curr.tag is prompt_tag:
            break
        curr = curr.prev
        if not escape and not jit.isvirtual(curr):
            return _scan_continuation(curr, prompt_tag, look_for, xs, handlers)
    return xs, handlers

@jit.elidable
def _scan_continuation(curr, prompt_tag, look_for, xs, handlers):
    """
    Variant of scan_continuation which is elidable.
    scan_continuation switchs to using this function when all the virtual
    continuation frames are exhausted.
    """
    while isinstance(curr, Cont):
        if curr is look_for:
            return None, handlers
        handlers |= isinstance(curr, DynamicWindValueCont)
        xs.append(curr)
        if isinstance(curr, Prompt) and curr.tag is prompt_tag:
            break
        curr = curr.prev
    return xs, handlers

@jit.elidable
def find_merge_point(c1, c2):
    i = len(c1) - 1
    j = len(c2) - 1
    while i >= 0 and j >= 0 and c1[i] is c2[j]:
        i -= 1
        j -= 1
    unwind = []
    rewind = []

    r1 = c1[-1] if i == len(c1) - 1 else c1[i+1]
    r2 = c2[-1] if j == len(c2) - 1 else c2[j+1]

    while i >= 0:
        if isinstance(c1[i], DynamicWindValueCont):
            unwind.append(c1[i])
        i -= 1
    while j >= 0:
        if isinstance(c2[j], DynamicWindValueCont):
            rewind.append(c2[j])
        j -= 1
    return r1, r2, unwind, rewind

@jit.elidable
def find_handlers(cont, target):
    result = None
    while cont is not target:
        assert isinstance(cont, Cont)
        if isinstance(cont, DynamicWindValueCont):
            if not result:
                result = []
            result.append(cont)
        cont = cont.prev
    return result

@jit.unroll_safe
def install_continuation_fast_path(current_cont, args, has_handlers, env, cont):
    from pycket.interpreter import return_multi_vals, return_void

    if not has_handlers:
        args = values.Values.make(args)
        return return_multi_vals(args, env, cont)

    unwind = find_handlers(current_cont, cont)
    cont = return_args_cont(args, env, cont)
    unwind = [x for x in reversed(unwind)]
    cont = do_unwind_cont(unwind, env, cont)
    return return_void(env, cont)


@jit.unroll_safe
def install_continuation(cont, prompt_tag, args, env, current_cont, extend=False, escape=False):
    from pycket.interpreter import return_multi_vals, return_void

    # Find the common merge point for two continuations
    # The extend option controls whether or not we remove frames from the
    # existing continuation, or simply stack the new continuation on top.
    # This is what differentiates call-with-current-continuation from
    # call-with-composable-continuation.
    if extend:
        _   , rewind = find_continuation_prompt(prompt_tag, cont, direction='rewind')
        base, unwind = current_cont, None
        stop         = None
    else:
        head1, handlers = scan_continuation(
                current_cont, prompt_tag, look_for=cont, escape=escape)
        if head1 is None:
            return install_continuation_fast_path(current_cont, args, handlers, env, cont)
        head2, _ = scan_continuation(cont, prompt_tag)
        base, stop, unwind, rewind = find_merge_point(head1, head2)

    # Append the continuations at the appropriate prompt
    if base is not None:
        cont = cont.append(base, stop=stop, upto=prompt_tag)

    # Fast path if no unwinding is required (avoids continuation allocation)
    if not unwind and not rewind:
        args = values.Values.make(args)
        return return_multi_vals(args, env, cont)

    # NOTE: All the continuations pushed here expect no arguments
    # They simply wrap functions which we could call directly, but using
    # continuations to perform the desired sequencing is easier.
    cont = return_args_cont(args, env, cont)
    if rewind:
        cont = do_rewind_cont(rewind, env, cont)
    if unwind:
        unwind = [x for x in reversed(unwind)]
        cont = do_unwind_cont(unwind, env, cont)
    return return_void(env, cont)

@continuation
def return_args_cont(args, env, cont, _vals):
    from pycket.interpreter import return_multi_vals
    args = values.Values.make(args)
    return return_multi_vals(args, env, cont)

@continuation
def do_unwind_cont(frames, env, cont, _vals):
    return unwind_frames(frames, env, cont)

@continuation
def do_rewind_cont(frames, env, cont, _vals):
    return rewind_frames(frames, env, cont)

@jit.elidable
@specialize.arg(2)
def find_continuation_prompt(tag, cont, direction=None):
    wind_frames = [] if direction is not None else None
    while cont is not None:
        if isinstance(cont, Prompt) and cont.tag is tag:
            if direction is not None:
                return cont, wind_frames
            return cont
        if direction is not None and getattr(cont, "has_" + direction)():
            assert isinstance(cont, Cont)
            wind_frames.append(cont)
        cont = cont.get_previous_continuation()
    if direction is not None:
        return None, wind_frames
    return None

# NOTE do_unwind_frame and do_rewind_frame expect to be invoked with return_void
def do_unwind_frames(frames, index, env, final_cont):
    from pycket.interpreter import return_void
    if index >= len(frames):
        return return_void(env, final_cont)
    frame = frames[index]
    ctxt  = unwind_next_frame_cont(frames, index, final_cont, env, frame.prev)
    return frame.unwind(env, ctxt)

def do_rewind_frames(frames, index, env, final_cont):
    from pycket.interpreter import return_void
    if index < 0:
        return return_void(env, final_cont)
    frame = frames[index]
    ctxt = rewind_next_frame_cont(frames, index, final_cont, env, frame.prev)
    return frame.rewind(env, ctxt)

@continuation
def unwind_next_frame_cont(frames, index, final_cont, env, cont, _vals):
    return do_unwind_frames(frames, index + 1, env, final_cont)

@continuation
def rewind_next_frame_cont(frames, index, final_cont, env, cont, _vals):
    return do_rewind_frames(frames, index - 1, env, final_cont)

def unwind_frames(frames, env, final_cont):
    return do_unwind_frames(frames, 0, env, final_cont)

def rewind_frames(frames, env, final_cont):
    return do_rewind_frames(frames, len(frames) - 1, env, final_cont)

@expose("continuation-prompt-available?", [values.W_ContinuationPromptTag, default(values.W_Continuation, None)], simple=False)
def cont_prompt_avail(tag, continuation, env, cont):
    from pycket.interpreter import return_value
    if continuation is not None:
        cont = continuation.cont
    prompt = find_continuation_prompt(tag, cont)
    available = values.W_Bool.make(prompt is not None)
    return return_value(available, env, cont)

@continuation
def dynamic_wind_pre_cont(value, pre, post, env, cont, _vals):
    cont = DynamicWindValueCont(pre, post, env, cont)
    return value.call([], env, cont)

@continuation
def dynamic_wind_value_cont(post, env, cont, _vals):
    return post.call([], env, dynamic_wind_post_cont(_vals, env, cont))

@continuation
def dynamic_wind_post_cont(val, env, cont, _vals):
    from pycket.interpreter import return_multi_vals
    return return_multi_vals(val, env, cont)

@expose("dynamic-wind", [procedure, procedure, procedure], simple=False)
def dynamic_wind(pre, value, post, env, cont):
    return pre.call([], env, dynamic_wind_pre_cont(value, pre, post, env, cont))

@expose(["call/cc", "call-with-current-continuation"],
        [procedure, default(values.W_ContinuationPromptTag, None)],
        simple=False, extra_info=True)
def callcc(proc, prompt_tag, env, cont, extra_call_info):
    kont = [values.W_Continuation(cont, prompt_tag)]
    return proc.call_with_extra_info(kont, env, cont, extra_call_info)

@continuation
def call_with_escape_continuation_cont(env, cont, _vals):
    # Does not do anything currently. Solely to ensure call/ec does not invoke
    # its procedure in tail position
    from pycket.interpreter import return_multi_vals
    return return_multi_vals(_vals, env, cont)

@expose(["call/ec", "call-with-escape-continuation"],
        [procedure, default(values.W_ContinuationPromptTag, None)],
        simple=False, extra_info=True)
def call_with_escape_continuation(proc, prompt_tag, env, cont, extra_call_info):
    assert prompt_tag is None, "NYI"
    cont = call_with_escape_continuation_cont(env, cont)
    return proc.call_with_extra_info([values.W_EscapeContinuation(cont)],
                                     env, cont, extra_call_info)

@expose("call-with-composable-continuation",
        [procedure, default(values.W_ContinuationPromptTag, values.w_default_continuation_prompt_tag)],
        simple=False, extra_info=True)
def call_with_composable_continuation(proc, prompt_tag, env, cont, extra_call_info):
    kont = [values.W_ComposableContinuation(cont, prompt_tag)]
    return proc.call_with_extra_info(kont, env, cont, extra_call_info)

@expose("make-continuation-prompt-tag", [default(values.W_Symbol, None)])
def make_continuation_prompt_tag(sym):
    return values.W_ContinuationPromptTag(sym)

@expose("default-continuation-prompt-tag", [])
def default_continuation_prompt_tag():
    return values.w_default_continuation_prompt_tag

@expose("unsafe-root-continuation-prompt-tag", [])
def unsafe_root_configuraion_prompt_tag():
    return values.w_root_continuation_prompt_tag

def abort_current_continuation(args, env, cont):
    from pycket.interpreter import return_multi_vals
    if not args:
        raise SchemeException("abort-current-continuation: expected 1 or more args")
    tag, args = args[0], args[1:]
    if not isinstance(tag, values.W_ContinuationPromptTag):
        raise SchemeException("abort-current-continuation: expected prompt-tag for argument 0")

    prompt, frames = find_continuation_prompt(tag, cont, direction='unwind')
    if prompt is None:
        raise SchemeException("abort-current-continuation: no such prompt exists")
    handler = prompt.handler
    cont    = prompt.get_previous_continuation()
    assert cont is not None
    if handler is None:
        if not args:
            raise SchemeException("abort-current-continuation: expected thunk as argument 1")
        cont = Prompt(tag, None, env, cont)
        handler = args[0]
        args = []
    if frames:
        cont = call_handler_cont(handler, args, env, cont)
        return unwind_frames(frames, env, cont)
    return handler.call(args, env, cont)

expose("abort-current-continuation", simple=False)(abort_current_continuation)

@expose("force-exit", [values.W_Object])
def force_exit(v):
    return _force_exit(v)

def _force_exit(v):
    from pycket.error import ExitException

    if isinstance(v, values.W_Fixnum) and (0 <= v.value <= 255):
        raise ExitException(v)
    else:
        raise ExitException(values.W_Fixnum(0))

@continuation
def force_exit_cont(v, env, cont, _vals):
    return _force_exit(v)

@make_procedure("initial-exit-handler", [values.W_Object], arity=Arity.ZERO, simple=False)
def initial_exit_handler(v, env, cont):
    from pycket.prims.plumber import do_plumber_flush_all
    root_plumber = current_plumber_param.get(cont) ## ??
    return do_plumber_flush_all(root_plumber, env , force_exit_cont(v, env, cont))

exit_handler_param = values_parameter.W_Parameter(initial_exit_handler)
expose_val("exit-handler", exit_handler_param)

@make_procedure("error-value-string-handler", [values.W_Object, values.W_Fixnum])
def error_value_string_handler(v, repr_length):
    # FIXME: truncate it with the given representation length
    return values_string.W_String.make(v.tostring())

error_val_string_handler_param = values_parameter.W_Parameter(error_value_string_handler)
expose_val("error-value->string-handler", error_val_string_handler_param)

@continuation
def exit_cont(env, cont, _vals):
    from pycket.interpreter import return_value
    return return_value(values.w_void, env, cont)

@expose("exit", [default(values.W_Object, values.w_true)], simple=False)
def exit(v, env, cont):
    exit_handler = exit_handler_param.get(cont)
    return exit_handler.call([v], env, exit_cont(env, cont))

@make_procedure("default-error-escape-handler", [], simple=False)
def default_error_escape_handler(env, cont):
    from pycket.prims.general import do_void
    args = [values.w_default_continuation_prompt_tag, do_void.w_prim]
    return abort_current_continuation(args, env, cont)

error_escape_handler_param = values_parameter.W_Parameter(default_error_escape_handler)

expose_val("error-escape-handler", error_escape_handler_param)

def is_exn(v):
    from pycket.prims.general import exn
    return (isinstance(v, values_struct.W_Struct) and (exn.has_subtype(v.struct_type())))

def is_user_exn(v):
    from pycket.prims.general import exn_fail_user
    return (isinstance(v, values_struct.W_Struct) and (exn_fail_user.has_subtype(v.struct_type())))

def get_exn_message(exn, env, cont):
    offset = exn.struct_type().get_offset(exn.struct_type())
    original_field_num = 0 # this is for the message field in exceptions
    message_field_index = values.W_Fixnum(original_field_num-offset)

    return exn.struct_type().accessor.call([exn, message_field_index], env, display_escape_cont(exn, env, cont))

def display_stack_trace(port, cont):
    from pycket.prims.continuation_marks import cms_context
    context = cms_context.w_prim.call_interpret([values.W_ContinuationMarkSet(cont, values.w_default_continuation_prompt_tag)])
    if isinstance(context, values.W_Cons):
        port.write("Error Trace:\n")
        total_frames_to_show = 10
        count = 0
        while isinstance(context, values.W_Cons):
            if count >= total_frames_to_show:
                break
            port.write("-- %s\n" % context.car().tostring()[:1000])
            context = context.cdr()
            count += 1

@make_procedure("default-error-display-handler", [values_string.W_String, values.W_Object], simple=False)
def default_error_display_handler(msg, exn_object, env, cont):
    from pycket.prims.input_output import current_error_param, return_void
    port = current_error_param.get(cont)

    assert isinstance(port, values.W_OutputPort)
    if is_exn(exn_object):
        port.write("%s : %s\n" % (exn_object.struct_type().name.tostring(), msg.tostring()))
    else:
        port.write("exception : %s\n" % (msg.tostring()))

    if not is_user_exn(exn_object):
        display_stack_trace(port, cont)
    return return_void(env, cont)

error_display_handler_param = values_parameter.W_Parameter(default_error_display_handler)

expose_val("error-display-handler", error_display_handler_param)

@continuation
def display_escape_cont(exn, env, cont, _vals):
    from pycket.interpreter import check_one_val
    message = check_one_val(_vals)

    display_handler = error_display_handler_param.get(cont) # parameterize this to default first
    escape_handler = error_escape_handler_param.get(cont) # this one too

    # display, then escape
    return display_handler.call([message, exn], env, call_handler_cont(escape_handler, [], env, cont))

@make_procedure("default-uncaught-exception-handler", [values.W_Object], simple=False)
def default_uncaught_exception_handler(exn, env, cont):
    from pycket.env import w_global_config
    w_global_config.set_error_exit(exn)
    # racket/src/cs/rumble/error.ss

    #FIXME : handle Breaks
    if is_exn(exn):
        return get_exn_message(exn, env, display_escape_cont(exn, env, cont))
    else:
        from pycket.interpreter import return_value
        return return_value(values_string.W_String.make(exn.tostring()), env, display_escape_cont(exn, env, cont))

uncaught_exception_handler_param = values_parameter.W_Parameter(default_uncaught_exception_handler)

expose_val("uncaught-exception-handler", uncaught_exception_handler_param)

@make_procedure("default-continuation-prompt-handler", [procedure], simple=False)
def default_continuation_prompt_handler(proc, env, cont):
    return proc.call([], env, cont)

# @expose("call-with-exception-handler", [procedure, procedure], simple=False)
# def call_with_exception_handler(f, thunk, env, cont):
#     #FIXME

@expose("call-with-continuation-prompt", simple=False, arity=Arity.geq(1), extra_info=True)
def call_with_continuation_prompt(args, env, cont, extra_call_info):
    if not args:
        raise SchemeException("call-with-continuation-prompt: not given enough values")
    parser  = ArgParser("call-with-continuation-prompt", args)
    tag     = values.w_default_continuation_prompt_tag
    handler = values.w_false
    fun     = parser.expect(values.W_Object)

    try:
        tag     = parser.expect(values.W_ContinuationPromptTag)
        handler = parser.expect(values.W_Object)
    except EndOfInput:
        pass

    args = parser.expect_many(values.W_Object)
    if not fun.iscallable():
        raise SchemeException("call-with-continuation-prompt: not given callable function")
    if handler is not values.w_false and not handler.iscallable():
        raise SchemeException("call-with-continuation-prompt: not given callable handler")
    if handler is values.w_false:
        handler = None
    cont = Prompt(tag, handler, env, cont)
    return fun.call_with_extra_info(args, env, cont, extra_call_info)

@expose("call-with-continuation-barrier", [procedure], simple=False, extra_info=True)
def call_with_continuation_barrier(proc, env, cont, calling_app):
    # TODO: Implementation
    cont = Barrier(env, cont)
    return proc.call_with_extra_info([], env, cont, calling_app)

def raise_exception(v, barrier, env, cont):
    # TODO: Handle case where barrier is not #t
    assert barrier is values.w_true

    handler = None
    k = cont
    while k is not None:
        handler, next_cont = k.find_cm(values.exn_handler_key)
        if handler is not None:
            break
        k = next_cont if next_cont else k.get_previous_continuation()
    else:
        raise SchemeException("uncaught exception:\n %s" % v.tostring())

    if not handler.iscallable():
        raise SchemeException("provided handler is not callable")

    return handler.call([v], env, cont)

expose("raise", [values.W_Object, default(values.W_Object, values.w_true)], simple=False)(raise_exception)

def raise_arg_err(args):
    """raise_arg_err is used to expose:

    raise-argument-error, raise-argument-error*
    """
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

expose("raise-argument-error",  arity=Arity.geq(3))(raise_arg_err)

@expose("raise-argument-error*", arity=Arity.geq(4))
def raise_arg_err_star(args):
    """(raise-argument-error* name realm expected v) → any

    name : symbol?
    realm : symbol?
    expected : string?
    v : any/c

    Like raise-argument-error, but using the given realm for error-message adjustments.
    Ignoring the realm argument in Pycket. TODO(cderici - 8/1/2024): Can be added if a proper error adjustment is needed/implemented.
    """
    # Call raise_arg_err with all the arguments except the second arg (realm)
    return raise_arg_err(args[:1] + args[2:])

def raise_args_err(args):
    """
    raise_args_err is used to expose:

    raise-arguments-error, raise-arguments-error*
    """

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

expose("raise-arguments-error", arity=Arity.geq(2))(raise_args_err)

@expose("raise-arguments-error*", arity=Arity.geq(3))
def raise_args_err_star(args):
    """(raise-arguments-error*	name
                                realm
                                message
                                field
                                v ...
                                ...) → any
    name : symbol?
    realm : symbol?
    message : string?
    field : string?
    v : any/c

    Like raise-arguments-error, but using the given realm for error-message adjustments.
    """
    # Call raise_args_err with all the arguments except the second arg (realm)
    return raise_args_err(args[:1] + args[2:])

@expose("raise-mismatch-error", arity=Arity.geq(3))
def raise_mismatch_err(args):
    name = args[0]
    assert isinstance(name, values.W_Symbol)
    message = args[1]
    assert isinstance(message, values_string.W_String)
    v = args[2]
    assert isinstance(v, values.W_Object)
    from rpython.rlib.rstring import StringBuilder
    error_msg = StringBuilder()
    error_msg.append(name.utf8value)
    error_msg.append(": ")
    error_msg.append(message.as_str_utf8())
    error_msg.append(v.tostring())
    i = 3
    while i + 1 < len(args):
        message = args[i]
        assert isinstance(message, values_string.W_String)
        error_msg.append(message.as_str_utf8())
        v = args[i+1]
        assert isinstance(v, values.W_Object)
        error_msg.append(v.tostring())
        i += 2
    raise SchemeException(error_msg.build())

@expose("raise-type-error", [values.W_Symbol, values_string.W_String, values.W_Object])
def raise_type_error(name, expected, v):
    raise SchemeException("%s: expected %s in %s" % (name.tostring(), expected.tostring(), v.tostring()))
