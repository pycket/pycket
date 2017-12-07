#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from pycket.values import W_Object, w_true, w_false, w_null, W_Cons, W_List, W_Fixnum, W_Number, W_Symbol
from pycket.vector import W_Vector
from pycket.prims.expose import expose, default
from pycket.prims.general import make_pred
from pycket.cont import continuation
from pycket.interpreter import check_one_val, return_value

class W_Correlated(W_Object):

    def __init__(self, obj, srcloc, props):
        self.obj = obj # W_Object
        self.srcloc = srcloc # (or correlated? #f '(line column position span) (vector line column position any))
        self.props = props

    def get_obj(self):
        return self.obj

    def get_srcloc(self):
        return self.srcloc

    def get_props(self):
        return self.props

    def extend_prop(self, key, val):
        self.props[key] = val

    def tostring(self):
        return "#<correlated:%s>" % self.get_obj().tostring()

# [core:correlated? syntax?]
make_pred("syntax?", W_Correlated)

# [core:correlated-e syntax-e]
@expose("syntax-e", [W_Correlated])
def correlated_e(c):
    return c.get_obj()

def correlated_to_datum_inner(e):
    if isinstance(e, W_Correlated):
        return correlated_to_datum_inner(e.get_obj())
    elif isinstance(e, W_List):
        a = correlated_to_datum_inner(e.car())
        d = correlated_to_datum_inner(e.cdr())
        if a is e.car() and d is e.cdr():
            return e
        else:
            return W_Cons.make(a,d)      
    else:
        return e

# [core:correlated->datum syntax->datum]
@expose("syntax->datum", [W_Object])
def correlated_to_datum(e):
    return correlated_to_datum_inner(e)


@continuation
def datum_to_corr_cont(datum, env, cont, _vals):
    srcloc_object = check_one_val(_vals)
    return return_value(W_Correlated(datum, srcloc_object, {}), env, cont)

# [core:datum->correlated datum->syntax]
@expose("datum->syntax", [W_Object, W_Object, default(W_Object, None)], simple=False)
def datum_to_correlated(ignored, datum, _srcloc, env, cont):

    if isinstance(datum, W_Correlated):
        return return_value(datum, env, cont)
    else:
        from pycket.prims.general import srcloc
        srcloc_const = srcloc.constructor
        if isinstance(_srcloc, W_Vector):
            #unerase = _srcloc.get_strategy().unerase
            #vector_contents = unerase(_srcloc.storage)
            v_ref = _srcloc.get_strategy().ref
            return srcloc_const.call([v_ref(_srcloc, 0),
                                      v_ref(_srcloc, 1),
                                      v_ref(_srcloc, 2),
                                      v_ref(_srcloc, 3),
                                      v_ref(_srcloc, 4)],
                                     env, datum_to_corr_cont(datum, env, cont))
        elif isinstance(_srcloc, W_List):
            return srcloc_const.call([_srcloc.car(),
                                      _srcloc.cdr().car(),
                                      _srcloc.cdr().cdr().car(),
                                      _srcloc.cdr().cdr().cdr().car(),
                                      _srcloc.cdr().cdr().cdr().cdr().car()],
                                     env, datum_to_corr_cont(datum, env, cont))
        elif isinstance(_srcloc, W_Correlated):
            raise Exception("FIXME NYI datum->syntax _srcloc is a correlated")
        else:
            if _srcloc is not w_false:
                raise Exception("FIXME, unhandled srcloc type %s" % _srcloc.tostring())
            srcloc = _srcloc
            return return_value(W_Correlated(datum, srcloc, {}), env, cont)

# [core:correlated-source syntax-source]
@expose("syntax-source", [W_Correlated])
def correlated_source(c):
    if c.get_srcloc() is w_false:
        return w_false
    return c.get_srcloc()._ref(0)

# [core:correlated-line syntax-line]
@expose("syntax-line", [W_Correlated])
def correlated_line(c):
    if c.get_srcloc() is w_false:
        return w_false
    return c.get_srcloc()._ref(1)


# [core:correlated-column syntax-column]
@expose("syntax-column", [W_Correlated])
def correlated_column(c):
    if c.get_srcloc() is w_false:
        return w_false
    return c.get_srcloc()._ref(2)

# [core:correlated-position syntax-position]
@expose("syntax-position", [W_Correlated])
def correlated_position(c):
    if c.get_srcloc() is w_false:
        return w_false
    return c.get_srcloc()._ref(3)

# [core:correlated-span syntax-span]
@expose("syntax-span", [W_Correlated])
def correlated_line(c):
    if c.get_srcloc() is w_false:
        return w_false
    return c.get_srcloc()._ref(4)

# [core:correlated-property-symbol-keys syntax-property-symbol-keys]))
@expose("syntax-property-symbol-keys", [W_Correlated])
def correlated_property_symbol_keys(c):
    acc = w_null
    for k,v in c.get_props().iteritems():
        acc = W_Cons.make(k, acc)
    return acc

# [core:correlated-property syntax-property]
@expose("syntax-property", [W_Correlated, W_Object, default(W_Object, None)])
def correlated_property(stx, key, v):
    if v is None:
        # getting
        return stx.get_props()[key]
    else:
        # setting
        stx.extend_prop(key, v)
        return stx
