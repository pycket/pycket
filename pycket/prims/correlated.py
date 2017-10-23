#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

from pycket.values import W_Object, w_true, w_false, w_null, W_Cons, W_List, W_Fixnum 
from pycket.prims.expose import expose, default, define_nyi
from pycket.error import SchemeException
from pycket.prims.general import make_pred

# FIXME: W_Srcloc expose srcloc function

class Correlated(W_Object):

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


# [core:correlated? syntax?]
make_pred("syntax?", Correlated)

# [core:correlated-e syntax-e]
@expose("syntax-e", [Correlated])
def correlated_e(c):
    return c.get_obj()

def correlated_to_datum_inner(e):
    if isinstance(e, Correlated):
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
@expose("datum->syntax", [W_Object, default(W_Object, None)])
def datum_to_correlated(v, srcloc):
    if isinstance(v, Correlated):
        return v
    else:
        # FIXME : obviously wrong, create Srcloc class
        srcloc = {"source":W_Fixnum(1),
                  "line":W_Fixnum(2),
                  "column":W_Fixnum(3),
                  "position":W_Fixnum(4),
                  "span":W_Fixnum(5)}
        return Correlated(v, srcloc, {})

# [core:correlated-source syntax-source]
@expose("syntax-source", [Correlated])
def correlated_source(c):
    return c.get_srcloc_field("source")

# [core:correlated-line syntax-line]
@expose("syntax-line", [Correlated])
def correlated_line(c):
    return c.get_srcloc_field("line")

# [core:correlated-column syntax-column]
@expose("syntax-column", [Correlated])
def correlated_column(c):
    return c.get_srcloc_field("column")

# [core:correlated-position syntax-position]
@expose("syntax-position", [Correlated])
def correlated_position(c):
    return c.get_srcloc_field("position")

# [core:correlated-span syntax-span]
@expose("syntax-span", [Correlated])
def correlated_span(c):
    return c.get_srcloc_field("span")

# [core:correlated-property-symbol-keys syntax-property-symbol-keys]))
@expose("syntax-property-symbol-keys", [Correlated])
def correlated_property_symbol_keys(c):
    acc = w_null
    for k,v in c.props.iteritems():
        acc = W_Cons.make(k, acc)
    return acc

# [core:correlated-property syntax-property]
# FIXME
define_nyi("syntax-property")
