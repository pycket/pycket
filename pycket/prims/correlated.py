#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from pycket.values import W_Object, w_true, w_false, w_null, W_Cons, W_List, W_Fixnum 
from pycket.prims.expose import expose, default, define_nyi
from pycket.error import SchemeException
from pycket.prims.general import make_pred

# FIXME: W_Srcloc expose srcloc function

class W_Correlated(W_Object):

    def __init__(self, obj, srcloc, props):
        self.obj = obj # W_Object
        self.srcloc = srcloc # (or correlated? #f '(line column position span) (vector line column position any))
        self.props = props

    def get_srcloc_field(self, field):
        if field not in self.srcloc.keys():
            raise SchemeException("Invalid key : %s" % field)
        return self.srcloc[field]

    def get_obj(self):
        return self.obj

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

# [core:datum->correlated datum->syntax]
@expose("datum->syntax", [W_Object, W_Object, default(W_Object, None)])
def datum_to_correlated(ignored, datum, srcloc):
    if isinstance(datum, W_Correlated):
        return datum
    else:
        # FIXME : obviously wrong, create Srcloc class
        srcloc = {"source":W_Fixnum(1),
                  "line":W_Fixnum(2),
                  "column":W_Fixnum(3),
                  "position":W_Fixnum(4),
                  "span":W_Fixnum(5)}
        return W_Correlated(datum, srcloc, {})

# [core:correlated-source syntax-source]
@expose("syntax-source", [W_Correlated])
def correlated_source(c):
    return c.get_srcloc_field("source")

# [core:correlated-line syntax-line]
@expose("syntax-line", [W_Correlated])
def correlated_line(c):
    return c.get_srcloc_field("line")

# [core:correlated-column syntax-column]
@expose("syntax-column", [W_Correlated])
def correlated_column(c):
    return c.get_srcloc_field("column")

# [core:correlated-position syntax-position]
@expose("syntax-position", [W_Correlated])
def correlated_position(c):
    return c.get_srcloc_field("position")

# [core:correlated-span syntax-span]
@expose("syntax-span", [W_Correlated])
def correlated_span(c):
    return c.get_srcloc_field("span")

# [core:correlated-property-symbol-keys syntax-property-symbol-keys]))
@expose("syntax-property-symbol-keys", [W_Correlated])
def correlated_property_symbol_keys(c):
    acc = w_null
    for k,v in c.props.iteritems():
        acc = W_Cons.make(k, acc)
    return acc

# [core:correlated-property syntax-property]
@expose("syntax-property", [W_Correlated, W_Object, default(W_Object, None)])
def correlated_property(stx, key, v):
    from pycket.interpreter import return_value
    if v is None:
        # getting
        return stx.props[key]
    else:
        # setting
        stx.extend_prop(key, v)
        return stx
