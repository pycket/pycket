from pycket                 import arity, values, base, values_parameter
from pycket.argument_parser import ArgParser, EndOfInput
from pycket.error           import SchemeException
from pycket.prims.expose    import expose
from pycket.cont            import Prompt, continuation


current_engine_complete_or_expire_param = values_parameter.W_Parameter(values.w_false)

"""
We ignore the w_ticks here (the _ argument) because
Pycket currently doesn't care about preemption at the Racket
level. So every TICK is considered infinite (i.e. we
always have a non-zero remaining ticks)
"""
current_engine_ticks_param = values_parameter.W_Parameter(values.W_Fixnum.MAX_INTERNED)


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
class W_Engine(values.W_Procedure):
    # TODO: we could derive it from W_PromotableClosure if
    # we keep it pure
    #
    # Keeping it pure requires not putting dynamic stuff
    # (e.g. complete-or-expire, or ticks) inside the object
    # Which is why we keep Racket level parameters to keep track
    # of the "current engine". (Also so that when the metacontinuations
    # are introduced, the scaffolding--that weaves the engine through
    # the continuation prompts--is ready)

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

"""
(make-engine
    proc_11
    (default-continuation-prompt-tag)
    #f
    (if (let-values (((or-part_59) initial?_0))
            (if or-part_59 or-part_59 at-root?_0))
            break-enabled-default-cell
        (current-break-enabled-cell))
    at-root?_0)
"""
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
