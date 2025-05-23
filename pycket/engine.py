from pycket                 import arity, values, base, values_parameter
from pycket.argument_parser import ArgParser, EndOfInput
from pycket.error           import SchemeException
from pycket.prims.expose    import expose
from pycket.cont            import Prompt, continuation

"""
Engines in the rumble layer are so much more tightly coupled with the
higher levels (i.e. thread linklet) than anything else in the general
self-hosting setup.

The protocols for invoking engines, as well as the mechanism in which
they're controlled (in the control-flow sense) are all mandated by the
design.

For example, the only primitive that explicitly invokes an engine is the
call-with-engine-completion. The procedure P that is passed to the
call-with-engine-completion (at the Racket level) has to invoke an engine
(which always means calling the engine like a function with three parameteres,
so the host is forced to represent an engine as a callable entity).
There are also implicit requirements for the arguments to the engine invocation.
For instance, the complete-or-expire (the 3rd parameter) has to call P's argument
in tail-position.

All these need to be reverse engineered because these are internal
stuff that don't have any user level documentation.

See https://github.com/racket/racket/blob/master/racket/src/cs/rumble/engine.ss
for engines in Racket CS's rumble layer.
"""


"""
An engine is represented by a procedure that takes three arguments, where the
procedure must be tail-called either within `call-with-engine-completion` or
in an engine call's `complete-or-expire` callback:
  * ticks: number of ticks to run before exire
  * prefix: thunk to invoke just before continuing (counts toward ticks)
  * complete-or-expire: callback that accepts 3 arguments,
     - engine or #f: an engine if the original `thunk` has not yet returned
     - list or #f: a list if the original `thunk` has returned values
     - remining-ticks: a number of ticks leftover due to complete or `(engine-block)`
    where the callback must end by tail-calling another engine procedure or
    the procedure provided by `call-with-engine-completion`

"""

current_engine_complete_or_expire_param = values_parameter.W_Parameter(values.w_false)

"""
We ignore the w_ticks here (the _ argument) because
Pycket currently doesn't care about preemption at the Racket
level. So every TICK is considered infinite (i.e. we
always have a non-zero remaining ticks)
"""
current_engine_ticks_param = values_parameter.W_Parameter(values.W_Fixnum.MAX_INTERNED)


class W_Engine(values.W_Procedure):
    # TODO: we could derive it from W_PromotableClosure if
    # we keep it pure
    #
    # Keeping it pure requires refraining from putting dynamic stuff
    # (e.g. complete-or-expire, or ticks) inside the object,
    # which is why we keep some Racket level parameters for the
    # "current engine". (Also so that when the metacontinuations
    # are introduced here, the scaffolding--that weaves the engine through
    # the continuation prompts--is ready to go)

    _attrs_ = _immutable_fields_ = ["w_thunk", "cell_state", "w_pmompt_tag", "w_abort_handler"]
    def __init__(self, w_thunk, w_prompt_tag, w_abort_handler, w_cell, w_empty_conf_huh):
        self.w_thunk = w_thunk # callable
        self.w_prompt_tag = w_prompt_tag
        self.w_abort_handler = w_abort_handler
        self.cell_state = w_cell
        self.empty_conf_huh = w_empty_conf_huh

    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

    @continuation
    def engine_return_cont(self, env, cont, _vals):

        # _vals are what the engine's thunk produced
        results = _vals.get_all_values()
        # results is a python list of w_objects

        return do_engine_return(results, env, cont)

    @continuation
    def engine_invoke_prefix_cont(self, calling_app, env, cont, _):

        # values produced by the thunk will be passed into the
        # engine-return, which will invoke the w_complete_or_expire
        # with a possible new engine, possibly some results,
        # and the remaining ticks
        cont = self.engine_return_cont(env, cont)

        # call engine's thunk with prompt tag and handler
        # (passed to the engine at make-engine)
        cont = Prompt(self.w_prompt_tag, self.w_abort_handler, env, cont)

        return self.w_thunk.call_with_extra_info([], env, cont, calling_app)

    def call_with_extra_info(self, args, env, cont, calling_app):
        ENGINE_ARG_EXPECT = """
Expected three arguments for engine invocation:
    ticks (fixnum), prefix (function), complete-or-expire (function)
Given:
%s
"""
        if not args:
            raise SchemeException(ENGINE_ARG_EXPECT, args)
        parser = ArgParser("invoke engine", args)

        w_ticks                 = values.W_Fixnum.MAX_INTERNED
        w_prefix                = values.w_false
        w_complete_or_expire    = values.w_false

        try:
            w_ticks                 = parser.expect(values.W_Fixnum)
            w_prefix                = parser.expect(base.W_Object)
            w_complete_or_expire    = parser.expect(base.W_Object)
        except EndOfInput:
            pass

        args = parser.expect_many(values.W_Object)
        if not w_prefix.iscallable or not w_complete_or_expire.iscallable:
            raise SchemeException(ENGINE_ARG_EXPECT, args)

        # we might also save here:
        #   -- current_engine_cell_state

        # Set the current complete_or_expire
        complete_or_expire_p_cell = current_engine_complete_or_expire_param.get_cell(cont)
        complete_or_expire_p_cell.set(w_complete_or_expire)

        # Set current ticks
        current_ticks_p_cell = current_engine_ticks_param.get_cell(cont)
        current_ticks_p_cell.set(w_ticks)


        cont = self.engine_invoke_prefix_cont(calling_app, env, cont)

        # call prefix
        return w_prefix.call_with_extra_info(args, env, cont, calling_app)

    def tostring(self):
        return "#<engine>"


@expose("make-engine", 
        [values.W_Object,                   # thunk -- callable, can return any number of values 
         values.W_ContinuationPromptTag,    # prompt-tag -- prompt to wrap around call to 'thunk'
         values.W_Object,                   # abort-handler -- handler for that prompt
         values.W_ThreadCell,               # init-break-enabled-cell -- default break-enable cell
         values.W_Bool,                     # empty-config? -- whether to clone the current parameterization
         ],
        simple=False)
def make_engine(w_thunk, w_prompt_tag, w_abort_handler, w_init_break_enabled_cell, w_empty_config_huh):

    assert w_thunk.iscallable()
    assert w_abort_handler.iscallable()

    return W_Engine(w_thunk, w_prompt_tag, w_abort_handler, w_init_break_enabled_cell, w_empty_config_huh)

# call-with-engine-completion: proc -> any
# It's for capturing the current metacontinuation as an engine runner.
# Calls the proc with a procedure to be tail-called from an engine's
# complete-or-expire callback to return to the metacontinuation.
#
# The proc is a function that invokes an engine (grep thread.linklet to see
# an actual call).
#
# The proc's argument is something like a postfix function, to be (tail-)called
# within the complete-or-expire that the engine is given when it is invoked
#
# call-with-engine-completion calls the proc. So the proc's argument is
# a functionn that does whatever the host needs. For Pycket, it can be identity
# for now.
@expose("call-with-engine-completion", [values.W_Object], simple=False)
def call_with_engine_completion(w_proc, env, cont):
    from pycket.racket_entry import get_primitive

    assert w_proc.iscallable()

    w_values = get_primitive("values")
    # TODO: For future when metacontinuations are introduced:
    # This call needs to be in a function that receives the current
    # metacontinuation (i.e. call-with-current-metacontinuation arg)
    # And instead of the primitive "values", we need a function that
    # applies the values to that metacontinuation (i.e. apply-meta-continuation)
    # See the racket/racket/src/cs/rumble/engine.ss
    return w_proc.call([w_values], env, cont)


# engine-timeout: -> T
# Called *inside* the running engine when its time-slice expires.
# Never returns, unwind to the caller of engine-run
# (engine-timeout)
# engine.complete_or_expire(new_engine, #f, 0)


# engine-block: -> T
# Called when the thread must wait on an external event.
# Yield with blocking.
# (engine-block)
# engine.complete_or_expire(new_engine, #f, remaining_ticks)


# engine-return: any ... -> T
# Terminates the engine and delivers its final values to the scheduler.
# (engine-return val ...)
# engine.complete_or_expire(#f, results, remaining_ticks)
def do_engine_return(results, env, cont):
    # get the current_complette or expire
    w_complete_or_expire = current_engine_complete_or_expire_param.get_cell(cont).get()
    w_remaining_ticks = current_engine_ticks_param.get_cell(cont).get()

    args = [values.w_false, results, w_remaining_ticks]

    return w_complete_or_expire.call_with_extra_info(args, env, cont, None)

expose("engine-return", simple=False, arity=arity.Arity.unknown)(do_engine_return)
