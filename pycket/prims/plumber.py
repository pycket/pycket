#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket import values, values_parameter
from pycket.prims.expose import (expose, expose_val, default, make_procedure, procedure)
from pycket.cont import continuation, loop_label

class W_PlumberFlushHandle(values.W_Object):

    def __init__(self, plumber):
        self.plumber = plumber

    def get_plumber(self):
        return self.plumber

    def set_plumber(self, plumber):
        self.plumber = plumber

class W_Plumber(values.W_Object):

    def __init__(self, callbacks={}, weak_callbacks={}):
        self.callbacks = callbacks # hash table of handles -> callbacks
        self.weak_callbacks = weak_callbacks # same, but weak refences

    def get_callbacks(self):
        return self.callbacks

    def get_weak_callbacks(self):
        return self.weak_callbacks

    def set_callback(self, h, proc):
        self.callbacks[h] = proc

    def set_weak_callback(self, h, proc):
        self.weak_callbacks[h] = proc

    def remove_handle(self, handle):
        if handle in self.callbacks:
            del(self.callbacks[handle])

        if handle in self.weak_callbacks:
            del(self.weak_callbacks[handle])

current_plumber_param = values_parameter.W_Parameter(W_Plumber())
expose_val("current-plumber", current_plumber_param)

@expose("make-plumber", [])
def make_plumber():
    return W_Plumber()

@expose("plumber-add-flush!", [W_Plumber, procedure, default(values.W_Bool, values.w_false)])
def plumber_add_flush_bang(p, proc, is_weak):

    # create a new handle
    h = W_PlumberFlushHandle(p)

    # put the new handle into p's callbacks with the given proc
    if is_weak is values.w_true:
        p.set_weak_callback(h, proc)
    else:
        p.set_callback(h, proc)

    return h

@continuation
def plumber_flush_loop_cont(handlers_callbacks, index, env, cont, _vals):
    from pycket.interpreter import return_value

    if index >= len(handlers_callbacks):
        return return_value(values.w_void, env, cont)
    else:
        return plumber_flush_loop(handlers_callbacks, index, env, cont)

@loop_label
def plumber_flush_loop(handlers_callbacks, index, env, cont):

    current_h = handlers_callbacks[index][0]
    current_proc = handlers_callbacks[index][1]

    return current_proc.call([current_h], env, plumber_flush_loop_cont(handlers_callbacks, index + 1, env, cont))

@expose("plumber-flush-all", [W_Plumber], simple=False)
def plumber_flush_all(p, env, cont):
    return do_plumber_flush_all(p, env, cont)

def do_plumber_flush_all(p, env, cont):
    from pycket.interpreter import return_value

    callbacks = p.get_callbacks()
    weak_callbacks = p.get_weak_callbacks()

    handlers_callbacks = [None]*(len(callbacks) + len(weak_callbacks))
    index = 0
    for h, proc in callbacks.iteritems():
        handlers_callbacks[index] = [h, proc]
        index += 1

    for h, proc in weak_callbacks.iteritems():
        handlers_callbacks[index] = [h, proc]
        index += 1

    if not handlers_callbacks:
        return return_value(values.w_void, env, cont)

    return plumber_flush_loop(handlers_callbacks, 0, env, cont)

@expose("plumber-flush-handle-remove!", [W_PlumberFlushHandle])
def plumber_flush_handle_remove_bang(h):
    p = h.get_plumber()
    # remove the given handle from it's plumber's callbacks
    p.remove_handle(h)

    return values.w_void
