from pycket                 import arity, values, base, values_parameter
from pycket.argument_parser import ArgParser, EndOfInput
from pycket.error           import SchemeException
from pycket.prims.expose    import default, expose
from pycket.cont            import Prompt, continuation

from rpython.rlib           import rgc

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
current_engine_ticks_param = values_parameter.W_Parameter(values.W_Fixnum.ONE)

# HACK: At engine timeouts and bloks, we need to create a new engine
# that'll continuae where we left off when invoked. In Pycket, engines
# run to completion every time. In the case where engine-timeout or
# engine-block is explicitly called (possibly with something like kill-thread),
# we return the same engine that was running.
# This parameter is to be able to find that engine in the continuation.
# When we introduce metacontinuations, then we'll be able to use them
# to actually continue
current_engine_param = values_parameter.W_Parameter(values.w_false)


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

    _attrs_ = ["w_thunk", "w_prompt_tag", "w_abort_handler",  "cell_state", "empty_conf_huh", "w_cont"]
    _immutable_fields_ = ["w_thunk", "w_prompt_tag", "w_abort_handler",  "cell_state", "empty_conf_huh"]

    def __init__(self, w_thunk, w_prompt_tag, w_abort_handler, w_cell, w_empty_conf_huh, w_cont=None):
        self.w_thunk = w_thunk # callable
        self.w_prompt_tag = w_prompt_tag
        self.w_abort_handler = w_abort_handler
        self.cell_state = w_cell
        self.empty_conf_huh = w_empty_conf_huh
        self.w_cont = w_cont

    def inject_cont(self, new_cont):
        self.w_cont = new_cont

    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

    @continuation
    def engine_return_cont(self, env, cont, _vals):

        # _vals are what the engine's thunk produced
        results = _vals.get_all_values()
        # get_all_values returns a python list of w_objects

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
        handler = self.w_abort_handler if self.w_abort_handler.iscallable() else None
        cont = Prompt(self.w_prompt_tag, handler, env, cont)

        return self.w_thunk.call_with_extra_info([], env, cont, calling_app)

    def call_with_extra_info(self, args, env, cont, calling_app):
        ENGINE_ARG_EXPECT = """
Expected three arguments for engine invocation:
    ticks (fixnum), prefix (function), complete-or-expire (function)
Given:
%s
"""
        if not args:
            raise SchemeException(ENGINE_ARG_EXPECT % args)
        parser = ArgParser("invoke engine", args)

        w_ticks                 = values.W_Fixnum.ONE
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
            raise SchemeException(ENGINE_ARG_EXPECT % args)

        # we might also save here:
        #   -- current_engine_cell_state

        # Set the current complete_or_expire
        complete_or_expire_p_cell = current_engine_complete_or_expire_param.get_cell(cont)
        complete_or_expire_p_cell.set(w_complete_or_expire)

        # Set current ticks
        current_ticks_p_cell = current_engine_ticks_param.get_cell(cont)
        current_ticks_p_cell.set(w_ticks)

        # Set current engine
        current_engine_p_cell = current_engine_param.get_cell(cont)
        current_engine_p_cell.set(self)

        if self.w_cont and self.w_cont is not values.w_false:
            cont = self.w_cont

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
         ])
def make_engine(w_thunk, w_prompt_tag, w_abort_handler, w_init_break_enabled_cell, w_empty_config_huh):

    assert w_thunk.iscallable()

    # Returns a callable engine that when invoked with 3 parameters (ticks, prefix, complete-or-expire),
    # calls the prefix first, continues with the thunk, and then continues with the complete-or-expire with
    # the results from the thunk (continues in a continuation sense).

    # In RacketCS's engines in the rumble layer, there's an additional 'create-engine' abstraction, that
    # allows the host to provide a function that'll in turn call the prefix (that's given to the engine),
    # after setting up that the engine applies the results (of complete-or-expire) to the metacontinuation.
    # In Pycket, we can ignore all that for now because we don't worry about the metacontinuations just yet.
    # So in our case, we treat the function that's supplied to the 'create-engine' as eta equivalent to
    # prefix, so no need for the explicit 'create-engine'.

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
@expose("engine-timeout", [], simple=False)
def engine_timeout(env, cont):
    # This should never be called in Pycket.
    return do_engine_block(True, env, cont)

# engine-block: -> T
# Called when the thread must wait on an external event.
# Yield with blocking.
# (engine-block)
# engine.complete_or_expire(new_engine, #f, remaining_ticks)
@expose("engine-block", [], simple=False)
def engine_block(env, cont):
    return do_engine_block(False, env, cont)


def do_engine_block(with_timeout_huh, env, cont):
     # get the current_complette or expire
    w_complete_or_expire = current_engine_complete_or_expire_param.get_cell(cont).get()
    w_remaining_ticks = current_engine_ticks_param.get_cell(cont).get()
    w_current_engine = current_engine_param.get_cell(cont).get()

    remaining_ticks = values.W_Fixnum.ZERO if with_timeout_huh else w_remaining_ticks

    w_current_engine.inject_cont(cont)
    w_new_engine = w_current_engine

    args = [w_new_engine, values.w_false, remaining_ticks]

    return w_complete_or_expire.call_with_extra_info(args, env, cont, None)


# engine-return: any ... -> T
# Terminates the engine and delivers its final values to the scheduler.
# (engine-return val ...)
# engine.complete_or_expire(#f, results, remaining_ticks)
def do_engine_return(_results, env, cont):
    # get the current_complette or expire
    w_complete_or_expire = current_engine_complete_or_expire_param.get_cell(cont).get()
    w_remaining_ticks = current_engine_ticks_param.get_cell(cont).get()

    # results has to be a W_Cons of some sort
    results = values.to_list(_results)

    args = [values.w_false, results, w_remaining_ticks]

    return w_complete_or_expire.call_with_extra_info(args, env, cont, None)

expose("engine-return", simple=False, arity=arity.Arity.unknown)(do_engine_return)


@expose("make-mutex", [])
def make_mutex():
    return values.W_Semaphore(1)

@expose("mutex-acquire", [values.W_Semaphore])
def mutex_acquire(m):
    m.wait()

@expose("mutex-release", [values.W_Semaphore])
def mutex_release(m):
    m.post()

@expose("get-initial-place", [])
def get_initial_place():
    return values.w_false

@expose("set-reachable-size-increments-callback!", [values.W_Object])
def set_reachable_size_increments_callback_bang(proc):
    return values.w_void

@expose("set-custodian-memory-use-proc!", [values.W_Object])
def set_custodian_memory_use_proc_bang(proc):
    return values.w_void

@expose("set-immediate-allocation-check-proc!", [values.W_Object])
def set_immediate_allocation_check_proc_bang(proc):
    return values.w_void

@expose("set-break-enabled-transition-hook!", [values.W_Object])
def set_break_enabled_transition_hook_bang(proc):
    return values.w_void

@expose("set-ctl-c-handler!", [values.W_Object])
def set_ctl_c_handler(proc):
    # FIXME: integrate with either rpython or Racket exit
    return values.w_void

@expose(["disable-interrupts", "enable-interrupts"], [])
def disable_enable_interrupts():
    return values.w_void

@expose("poll-async-callbacks", [])
def poll_async_callbacks():
    return values.w_null

@expose("current-place-roots", [])
def current_place_roots():
    return values.w_null


# rgc.FinalizerQueue is a queue that RPython GC drives
# finalizer_trigger is called whenever a major collection
# occurs that involves the registered objects (in our case W_Object)
# We use this to flip the will procedure state in a will
# executor registered for that value
# see https://doc.pypy.org/en/stable/discussion/finalizer-order.html
class WillFinalizerQueue(rgc.FinalizerQueue):

    base_class = values.W_Object

    def __init__(self, ready_set):
        self.ready_set = ready_set

    def finalizer_trigger(self):
        w_obj = self.next_dead()
        while w_obj:
            self.ready_set[w_obj] = None
            w_obj = self.next_dead()

# If not translated, none of the values will ever be ready, so
# this whole thing will be a big no-op
class W_WillExecutor(values.W_Object):
    errorname = "will-executor"

    def __init__(self):
        # W_Object in set means it's will is ready to be executed
        self.ready_set = {} # w_val : None

        self.will_procs = {} # W_Object: W_Object (callable)

        self.queue = WillFinalizerQueue(self.ready_set)

    def have_any_ready_will(self):
        return len(self.ready_set) != 0

    def register_proc(self, w_value, w_proc):
        self.will_procs[w_value] = w_proc

    def try_execute_will_for(self, w_value):
        res = None
        if w_value in self.ready_set:
            res = self.will_procs[w_value].call_interpret([w_value])
        res = res if res else values.w_false
        return res

    def execute_ready_wills(self):
        results = []

        # FIXME: I don't think anyone will call/cc in a will proc
        # but the right way of doing this is threading through a
        # continuation loop to invoke these procedures one after another
        for w_val in self.ready_set:
            # FIXME: check the order in which the procedures are executed
            results.append(self.will_procs[w_val].call_interpret([w_val]))

        return values.Values.make(results)

WILL_EXECUTORS = []

@expose("make-will-executor", [values.W_Object])
def make_will_executor(notify):
    e = W_WillExecutor()
    WILL_EXECUTORS.append(e)
    return e

@expose("make-late-will-executor", [values.W_Object, default(values.W_Object, values.w_false)])
def make_late_will_executor(notify, keep_huh):
    e = W_WillExecutor()
    WILL_EXECUTORS.append(e)
    return e

@expose("will-executor?", [values.W_Object])
def will_executor(v):
    return values.W_Bool.make(isinstance(v, W_WillExecutor))

@expose("will-register", [W_WillExecutor, values.W_Object, values.W_Object])
def will_register(w_executor, w_v, w_proc):
    w_executor.register_proc(w_v, w_proc)

@expose("will-try-execute", [W_WillExecutor, default(values.W_Object, values.w_false)])
def will_try_execute(w_executor, w_v):
    return w_executor.try_execute_will_for(w_v)

@expose("poll-will-executors", [])
def poll_will_executors():

    done = False
    while not done:
        done = True
        for executor in WILL_EXECUTORS:
            if executor.have_any_ready_will():
                executor.execute_ready_wills()
                done = False
