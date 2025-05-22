from pycket                 import values
from pycket.prims.expose    import expose

"""
;; An engine is represented by a procedure that takes three arguments, where the
;; procedure must be tail-called either within `call-with-engine-completion` or
;; in an engine call's `complete-or-expire` callback:
;;   * ticks: number of ticks to run before exire
;;   * prefix: thunk to invoke just before continuing (counts toward ticks)
;;   * complete-or-expire: callback that accepts 3 arguments,
;;      - engine or #f: an engine if the original `thunk` has not yet returned
;;      - list or #f: a list if the original `thunk` has returned values
;;      - remining-ticks: a number of ticks leftover due to complete or `(engine-block)`
;;     where the callback must end by tail-calling another engine procedure or
;;     the procedure provided by `call-with-engine-completion`
"""
class W_Engine(values.W_Procedure):
    def __init__(self, cell_state, w_thunk, w_proc):
        self.w_thunk = w_thunk
        self.cell_state = cell_state
        self.w_proc = w_proc

    def call(self, args, env, cont):
        return self.call_with_extra_info(args, env, cont, None)

    def call_with_extra_info(self, args, env, cont, calling_app):
        # args : ticks, prefix, complete_or_expire

        # we might save here:
        #   -- current_engine_complete_or_expire
        #   -- current_engine_cell_state
        pass

        self.w_proc.call_whataver(prefix, env, cont)

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
    # Check thunk callable

    # Actually starts a computation... by
    #   -- calls the 
    pass


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

