#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket                    import values, values_parameter, values_string
from pycket.arity              import Arity
from pycket.cont               import continuation, loop_label, call_cont, Barrier, Cont, NilCont, Prompt
from pycket.error              import SchemeException
from pycket.argument_parser    import ArgParser, EndOfInput
from pycket.prims.expose       import default, expose, expose_val, procedure, make_procedure
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
    from pycket.prims.general import exn_fail, exn_fail_user
    from pycket.values        import W_ContinuationMarkSet
    from pycket.prims.control import raise_exception
    message = values_string.W_String.fromstr_utf8(exn.msg)
    marks   = W_ContinuationMarkSet(cont, values.w_default_continuation_prompt_tag)
    cont    = post_build_exception(env, cont)
    if exn.is_user():
        return exn_fail_user.constructor.call([message, marks], env, cont)
    else:
        return exn_fail.constructor.call([message, marks], env, cont)

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

@make_procedure("default-error-escape-handler", [], simple=False)
def default_error_escape_handler(env, cont):
    from pycket.prims.general import do_void
    args = [values.w_default_continuation_prompt_tag, do_void.w_prim]
    return abort_current_continuation(args, env, cont)

expose_val("error-escape-handler", values_parameter.W_Parameter(default_error_escape_handler))

@make_procedure("default-continuation-prompt-handler", [procedure], simple=False)
def default_continuation_prompt_handler(proc, env, cont):
    return proc.call([], env, cont)

@expose("call-with-continuation-prompt", simple=False, arity=Arity.geq(1))
def call_with_continuation_prompt(args, env, cont):
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
    return fun.call(args, env, cont)

@expose("call-with-continuation-barrier", [procedure], simple=False, extra_info=True)
def call_with_continuation_barrier(proc, env, cont, calling_app):
    # TODO: Implementation
    cont = Barrier(env, cont)
    return proc.call_with_extra_info([], env, cont, calling_app)

def raise_exception(v, barrier, env, cont):
    # TODO: Handle case where barrier is not #t
    assert barrier is values.w_true

    handler = None
    while cont is not None:
        handler, _ = cont.find_cm(values.exn_handler_key)
        if handler is not None:
            break
        cont = cont.get_previous_continuation()
    else:
        raise SchemeException("uncaught exception:\n %s" % v.tostring())

    if not handler.iscallable():
        raise SchemeException("provided handler is not callable")

    assert cont is not None
    return handler.call([v], env, cont)

expose("raise", [values.W_Object, default(values.W_Object, values.w_true)], simple=False)(raise_exception)

@expose("raise-argument-error", arity=Arity.geq(3))
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

@expose("raise-arguments-error", arity=Arity.geq(2))
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


