from pycket                 import arity, values, base, values_parameter
from pycket.prims           import general as genprims
from pycket.argument_parser import ArgParser, EndOfInput
from pycket.error           import SchemeException
from pycket.prims.control   import default_uncaught_exception_handler
from pycket.prims.expose    import default, expose
from pycket.cont            import Prompt, continuation
from pycket.env             import w_global_config

from rpython.rlib           import rgc, rtime

from pycket.util            import console_log

ENGINE_DEBUG = False
ENGINE_VERBOSITY = 4

get_current_mc = w_global_config.get_current_mc
set_current_mc = w_global_config.set_current_mc

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
For instance, the complete-or-expire (the 3rd parameter) has to call P's
own argument in tail-position.

All these need to be reverse engineered because these are internal
stuff that don't have any user level documentation.

"""


"""
See https://github.com/racket/racket/blob/master/racket/src/cs/rumble/engine.ss
for engines in Racket CS's rumble layer.

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

See comment in make-engine down below for Pycket's rendition of this.
"""

current_engine_complete_or_expire_param = values_parameter.W_Parameter(values.w_false)

"""
We ignore the w_ticks here (the _ argument) because
Pycket currently doesn't care about preemption at the Racket
level. So every TICK is considered infinite (i.e. we
always have a non-zero remaining ticks)
"""
TICKS = 100000
current_engine_ticks_param = values_parameter.W_Parameter(values.W_Fixnum(TICKS))

# This is actually to for getting the "current" w_complete_or_expire
current_engine_param = values_parameter.W_Parameter(values.w_false)

w_engine_default_cont_tag = values.W_ContinuationPromptTag("engine")

def make_new_engine_prompt_tag():
    from pycket.interpreter import Gensym
    return values.W_ContinuationPromptTag(Gensym.gensym("engine-tag"))

class W_Engine(values.W_Procedure):
    # TODO: we could derive it from W_PromotableClosure if
    # we keep it pure
    #
    # Keeping it pure requires refraining from putting dynamic stuff
    # (e.g. complete-or-expire, or ticks) inside the object,
    # which is why we keep some Racket level parameters for the
    # "current engine".

    _attrs_ = ["saves", "w_thunk", "w_prompt_tag", "w_abort_handler",  "cell_state", "w_empty_conf_huh", "w_cont"]
    _immutable_fields_ = ["saves", "w_thunk", "w_prompt_tag", "w_abort_handler",  "cell_state", "w_empty_conf_huh"]

    def __init__(self, saves, w_thunk, w_prompt_tag, w_abort_handler, w_cell, w_empty_conf_huh, w_cont=None):
        self.saves = saves # Python list - MetaContinuation captured at suspension
        self.w_thunk = w_thunk # callable
        self.w_prompt_tag = w_prompt_tag
        self.w_abort_handler = w_abort_handler
        self.cell_state = w_cell
        self.w_empty_conf_huh = w_empty_conf_huh
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

        console_log("calling engine return with _results: %s -- %s" % ([r.tostring() for r in results], self), ENGINE_VERBOSITY, ENGINE_DEBUG)

        return do_engine_return(results, env, cont)

    @continuation
    def engine_initial_start_cont(self, calling_app, env, cont, _):

        # values produced by the thunk will be passed into the
        # engine-return, which will invoke the w_complete_or_expire
        # and the remaining ticks
        cont = self.engine_return_cont(env, cont)

        # call engine's thunk with prompt tag and handler
        # (passed to the engine at make-engine)
        handler = self.w_abort_handler if self.w_abort_handler.iscallable() else None
        cont = Prompt(self.w_prompt_tag, handler, env, cont)

        cont.update_cm(values.exn_handler_key, default_uncaught_exception_handler)
        # TODO: check self.w_empty_conf_huh:
        cont.update_cm(values.parameterization_key, values_parameter.top_level_config)

        console_log("engine %s invoking w_thunk" % self, ENGINE_VERBOSITY, ENGINE_DEBUG)
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

        w_ticks                 = values.W_Fixnum(TICKS)
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

        # restore the saved metacontinuation
        set_current_mc(self.saves)

        # Two paths can invoke an engine:
        #   - initial start: we create the engine, give it a thunk, and let it rip
        #   - resumption: when suspending, we create a new engine, we don't give it a thunk,
        #               we plug_reduce into the host resume_k within the metacont frame
        #               saved when this engine is suspended

        if self.w_thunk is not values.w_false:
            # This is an initial start of an engine,
            # i.e. not resuming from a suspend

            # Set the current complete_or_expire
            complete_or_expire_p_cell = current_engine_complete_or_expire_param.get_cell(cont)
            complete_or_expire_p_cell.set(w_complete_or_expire)

            # Set current ticks
            current_ticks_p_cell = current_engine_ticks_param.get_cell(cont)
            current_ticks_p_cell.set(w_ticks)

            # Set current engine
            current_engine_p_cell = current_engine_param.get_cell(cont)
            current_engine_p_cell.set(self)


            cont.update_cm(values.exn_handler_key, default_uncaught_exception_handler)
            # TODO: check self.w_empty_conf_huh:
            cont.update_cm(values.parameterization_key, values_parameter.top_level_config)

            console_log("engine %s is started" % self, ENGINE_VERBOSITY, ENGINE_DEBUG)

            cont = self.engine_initial_start_cont(calling_app, env, cont)
        else:
            # We're resuming
            saves = self.saves
            # Replace the current continuation with the one that we saved
            # in the metacontinuation
            cont = saves.pop().resume_k()
            console_log("engine %s is resumed" % self, ENGINE_VERBOSITY, ENGINE_DEBUG)

         # call prefix
        console_log("engine %s is calling prefix" % self, ENGINE_VERBOSITY, ENGINE_DEBUG)
        return w_prefix.call_with_extra_info(args, env, cont, calling_app)

    def tostring(self):
        return "#<engine>"

class W_MetaContFrame(values.W_Object):
    """
        https://github.com/racket/racket/blob/master/racket/src/cs/rumble/control.ss
    """
    _attrs_ = ["w_tag", "w_resume_k"]
    _immutable_fields_ = _attrs_

    def __init__(self, w_tag, w_resume_k):
        self.w_tag          = w_tag
        # resume_k is cont captured by host's call/cc
        self.w_resume_k     = w_resume_k

        # self.w_handler      = w_handler
        # self.w_marks        = w_marks
        # self.w_winders      = w_winders
        # self.w_mark_splice  = w_mark_splice
        # self.w_mark_chain   = w_mark_chain
        # self.w_traces       = w_traces
        # self.w_cc_guard     = w_cc_guard
        # self.w_avail_cache  = w_avail_cache

    def resume_k(self):
        return self.w_resume_k

EMPTY_MC = []

def current_mc_is_empty():
    return get_current_mc() == []

def pop_from_current_mc():
    _current_mc = get_current_mc()
    frame = _current_mc.pop()
    set_current_mc(_current_mc)
    return frame

def push_to_current_mc(frame):
    _current_mc = get_current_mc()
    _current_mc.append(frame)
    set_current_mc(_current_mc)

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
    # the results from the thunk (continues in a continuation sense). That's the initial start of an engine.

    # Whenever an engine is suspended (via timeout or block), we make a new engine that's supposed to
    # continue where it's left off whenever it's invoked with another 3 parameters. This time, we don't
    # care about any thunks etc, we continue where we left off by calling the prefix, then plugging the
    # result into the continuation that we saved when the previous version of this engine is suspended.

    # In RacketCS's engines in the rumble layer, there's an additional 'create-engine' abstraction, that
    # allows the host to provide a function that'll in turn call the prefix (that's given to the engine),
    # after setting up that the engine applies the results (of complete-or-expire) to the metacontinuation.
    # In Pycket, we can ignore all that for now because we don't operate with closures.
    # So in our case, we treat the function that's supplied to the 'create-engine' as eta equivalent to
    # prefix, so no need for the explicit 'create-engine'.

    eng = W_Engine(EMPTY_MC, w_thunk, w_prompt_tag, w_abort_handler, w_init_break_enabled_cell, w_empty_config_huh)

    console_log("make-engine made %s" % eng, ENGINE_VERBOSITY, ENGINE_DEBUG)
    return eng


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
# a function that does whatever the host needs. For Pycket, the only extra thing
# it does other than passing values through is that it reinstalls the metacont
# that's captured at the time the call-with-engine-completion is called.

class DoneGetBack(values.W_Procedure):

    _attrs_ = _immutable_fields_ = ["w_cont", "captured_mc"]

    def __init__(self, w_cont, captured_mc):
        self.w_cont = w_cont
        self.captured_mc = captured_mc

    def call_with_extra_info(self, args, env, cont, _):
        vals = values.Values.make(args)

        # captured_mc can be empty
        resume_k = self.captured_mc.pop().resume_k()
        set_current_mc(self.captured_mc)

        return resume_k.plug_reduce(vals, env)

@expose("call-with-engine-completion", [values.W_Object], simple=False)
def call_with_engine_completion(w_proc, env, cont):

    assert w_proc.iscallable()

    # from pycket.racket_entry import get_primitive
    # w_values = get_primitive("values")

    # w_proc has the code that invokes an engine
    # the result of that proc has to return to cont
    # (actually the metacont)
    saved = get_current_mc()
    set_current_mc([])

    done = DoneGetBack(cont, saved)

    console_log("call-with-engine-completion", ENGINE_VERBOSITY, ENGINE_DEBUG)
    return w_proc.call([done], env, cont)

# engine-timeout: -> T
# Called *inside* the running engine when its time-slice expires.
# Never returns, unwind to the caller of engine-run
# (engine-timeout)
# engine.complete_or_expire(new_engine, #f, 0)
@expose("engine-timeout", [], simple=False)
def engine_timeout(env, cont):
    # This should never be called in Pycket.
    console_log("engine-timeout", ENGINE_VERBOSITY, ENGINE_DEBUG)
    return do_engine_block(True, env, cont)

# engine-block: -> T
# Called when the thread must wait on an external event.
# Yield with blocking.
# (engine-block)
# engine.complete_or_expire(new_engine, #f, remaining_ticks)
@expose("engine-block", [], simple=False)
def engine_block(env, cont):
    console_log("engine-block", ENGINE_VERBOSITY, ENGINE_DEBUG)
    return do_engine_block(False, env, cont)

WAKE_HANDLE = values.W_Symbol.make("pycket-wakeup-handle")
wakeables = {}

def do_engine_block(with_timeout_huh, env, cont):
     # get the current_complete or expire
    w_complete_or_expire = current_engine_complete_or_expire_param.get_cell(cont).get()
    w_remaining_ticks = current_engine_ticks_param.get_cell(cont).get()
    w_ce = current_engine_param.get_cell(cont).get()

    remaining_ticks = values.W_Fixnum.ZERO if with_timeout_huh else w_remaining_ticks

    """
    Here we need to:
        - get the _CURRENT_MC
        - make a new W_MetaContFrame (resume_k <- cont)
        - cons that onto _CURRENT_MT
        - Put that into a new W_Engine as saves
    """
    # Capture the current meta-continuation
    current_mc = get_current_mc()

    # Add a new frame
    new_tag = make_new_engine_prompt_tag()
    new_mc_frame = W_MetaContFrame(new_tag, cont)
    current_mc.append(new_mc_frame)

    # Make a new engine
    w_new_engine = W_Engine(current_mc,
                            values.w_false, # thunk
                            new_tag,
                            genprims.do_void.w_prim,
                            values.W_ThreadCell(values.w_false, False),
                            values.w_true)

    console_log("do_engine_block - current-engine: %s -- new-engine: %s" % (w_ce, w_new_engine), ENGINE_VERBOSITY, ENGINE_DEBUG)

    set_current_mc([])

    args = [w_new_engine, values.w_false, remaining_ticks]

    # Block is called within an engine invocation, so this should
    # probably abort to the nearest engine prompt and then call
    # the complete_or_expire with the new engine.

    from pycket.prims.control import find_continuation_prompt
    # abort until nearest engine prompt
    assert isinstance(w_ce, W_Engine)
    cont = find_continuation_prompt(w_ce.w_prompt_tag, cont)

    return w_complete_or_expire.call_with_extra_info(args, env, cont, None)

@expose("get-wakeup-handle", [], simple=True)
def get_wakeup_handle():
    return WAKE_HANDLE

@expose("wakeup", [values.W_Object])
def wakeup(_):
    # TODO: put a semaphore in the wakeables in sleep,
    # and post it here
    console_log("wakeup called", ENGINE_VERBOSITY, ENGINE_DEBUG)
    return values.w_void

# engine-return: any ... -> T
# Terminates the engine and delivers its final values to the scheduler.
# (engine-return val ...)
# engine.complete_or_expire(#f, results, remaining_ticks)
def do_engine_return(_results, env, cont):

    # get the current_complette or expire
    w_complete_or_expire = current_engine_complete_or_expire_param.get_cell(cont).get()
    w_remaining_ticks = current_engine_ticks_param.get_cell(cont).get()

    w_ce = current_engine_param.get_cell(cont).get()
    console_log("do_engine_return - current complete_or_expire belongs to engine: %s" % (w_ce), ENGINE_VERBOSITY, ENGINE_DEBUG)
    # results has to be a W_Cons of some sort
    results = values.to_list(_results)

    args = [values.w_false, results, w_remaining_ticks]
    set_current_mc([])

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

@expose("sleep", [default(values.W_Number, values.W_Fixnum(0))])
def racket_sleep(w_secs):
    console_log("sleep", ENGINE_VERBOSITY, ENGINE_DEBUG)

    if isinstance(w_secs, values.W_Flonum):
        secs = w_secs.value
    elif isinstance(w_secs, values.W_Fixnum):
        secs = float(w_secs.value)
    elif isinstance(w_secs, values.W_Bignum):
        secs = w_secs.value.tofloat()
    else:
        raise SchemeException("expected real? in sleep, given:" % w_secs)

    if secs < 0.0:
        secs = 0.0

    rtime.sleep(secs)

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
