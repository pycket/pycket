#! /usr/bin/env python
# -*- coding: utf-8 -*-

from pycket              import impersonators as imp
from pycket              import values
from pycket              import vector
from pycket.cont         import call_cont, Cont
from pycket.error        import SchemeException
from pycket.prims.expose import default, expose, make_callable_label, procedure

# Can use this to promote a get_cmk operation to a callable function.
CMKSetToListHandler = make_callable_label([values.W_Object])

@expose("current-continuation-marks",
        [default(values.W_ContinuationPromptTag, values.w_default_continuation_prompt_tag)],
        simple=False)
def current_cont_marks(prompt_tag, env, cont):
    from pycket.interpreter import return_value
    return return_value(values.W_ContinuationMarkSet(cont, prompt_tag), env, cont)

@expose("continuation-marks",
        [values.W_Continuation,
         default(values.W_ContinuationPromptTag, values.w_default_continuation_prompt_tag)])
def continuation_marks(cont, prompt_tag):
    # TODO Prompt tag
    return values.W_ContinuationMarkSet(cont.cont, prompt_tag)

@expose("continuation-mark-set->list",
        [values.W_ContinuationMarkSet, values.W_Object,
         default(values.W_ContinuationPromptTag, None)],
        simple=False)
def cms_list(cms, mark, prompt_tag, env, cont):
    from pycket.interpreter   import return_value
    from pycket.prims.general import map_loop
    if isinstance(mark, values.W_ContinuationMarkKey):
        func  = CMKSetToListHandler(mark.get_cmk)
        marks = cms.cont.get_marks(imp.get_base_object(mark),
                                   upto=[prompt_tag, cms.prompt_tag])
        return map_loop(func, [marks], env, cont)
    marks = cms.cont.get_marks(mark, upto=[prompt_tag, cms.prompt_tag])
    return return_value(marks, env, cont)

def get_marks_all(cont, keys, not_found, upto=[]):
    results = vector.W_Vector.fromelement(not_found, len(keys))
    while True:
        if cont is None:
            return values.w_null
        found = False
        next  = None
        for i, key in enumerate(keys):
            value, _ = cont.find_cm(key)
            if value is not None:
                found = True
            else:
                value = not_found
            results.set(i, value)
        cont = cont.get_previous_continuation(upto=upto)
        if found:
            break
    rest = get_marks_all(cont, keys, not_found, upto=upto)
    return values.W_Cons.make(results, rest)

@expose("continuation-mark-set->list*",
        [values.W_ContinuationMarkSet,
         values.W_List,
         default(values.W_Object, values.w_false),
         default(values.W_ContinuationPromptTag, values.w_default_continuation_prompt_tag)])
def continuation_mark_set_to_list_star(mark_set, key_list, none_v, prompt_tag):
    cont = mark_set.cont
    keys = values.from_list(key_list)
    return get_marks_all(cont, keys, none_v, upto=[prompt_tag])

def is_ast_cont_with_surrounding_lambda(k):
    from pycket import interpreter as i
    cs = [i.LetCont,
          i.LetrecCont,
          i.BeginCont,
          i.Begin0Cont,
          i.Begin0BodyCont,
          i.WCMKeyCont,
          i.WCMValCont]
    # the ones having the method "get_next_executed_ast"
    for c in cs:
        if isinstance(k, c):
            a = k.get_ast()
            if isinstance(a, i.AST) and a.surrounding_lambda:
                return True
    return False

@expose("continuation-mark-set->context", [values.W_ContinuationMarkSet])
def cms_context(marks):
    from pycket.values_string import W_String
    # TODO: Pycket does not have a mark to denote context. We need to fix that.

    k = marks.cont
    n = 0
    # find out the length
    while isinstance(k, Cont):
        if is_ast_cont_with_surrounding_lambda(k):
            n += 1
        k = k.get_previous_continuation()

    # second traversal saves us from reversing it later
    ls = [None]*n
    k = marks.cont
    i = n-1
    while isinstance(k, Cont):
        if is_ast_cont_with_surrounding_lambda(k):
            surrounding_lam = k.get_ast().surrounding_lambda
            lam_str = W_String.make(surrounding_lam.tostring())
            ls[i] = values.W_Cons.make(lam_str, values.w_false)
            i -= 1
        k = k.get_previous_continuation()

    return values.to_list(ls)

@expose("continuation-mark-set-first",
        [values.W_Object,
         values.W_Object,
         default(values.W_Object, values.w_false),
         default(values.W_Object, values.w_default_continuation_prompt_tag)],
        simple=False)
def cms_first(cms, key, missing, prompt_tag, env, cont):
    from pycket.interpreter import return_value
    is_cmk = isinstance(key, values.W_ContinuationMarkKey)
    m = imp.get_base_object(key) if is_cmk else key

    if prompt_tag is values.w_default_continuation_prompt_tag and \
       (key is values.break_enabled_key or key is values.parameterization_key):
        prompt_tag = values.w_root_continuation_prompt_tag

    if cms is values.w_false:
        the_cont = cont
        v = cont.get_mark_first(m, upto=[prompt_tag])
    elif isinstance(cms, values.W_ContinuationMarkSet):
        the_cont = cms.cont
        v = cont.get_mark_first(m, upto=[prompt_tag, cms.prompt_tag])
    else:
        raise SchemeException("Expected #f or a continuation-mark-set")
    val = v if v is not None else missing
    if is_cmk:
        return key.get_cmk(val, env, cont)
    return return_value(val, env, cont)

@expose("make-continuation-mark-key", [default(values.W_Symbol, None)])
def mk_cmk(s):
    from pycket.interpreter import Gensym
    s = Gensym.gensym("cm") if s is None else s
    return values.W_ContinuationMarkKey(s)

@expose("call-with-immediate-continuation-mark",
        [values.W_Object, procedure, default(values.W_Object, values.w_false)],
        simple=False,
        extra_info=True)
def cwicm(key, proc, default, env, cont, extra_call_info):
    lup, _ = cont.find_cm(key)
    val = default if lup is None else lup
    if isinstance(key, values.W_ContinuationMarkKey):
        return key.get_cmk(val, env, call_cont(proc, env, cont))
    return proc.call_with_extra_info([val], env, cont, extra_call_info)
