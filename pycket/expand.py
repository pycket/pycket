import json
import subprocess
import os

from pycket.interpreter import *
from pycket import values

fn = os.path.join(os.path.dirname(__file__), "expand_racket.rkt")

def expand(s):
    process = subprocess.Popen(
        "racket %s" % (fn, ),
        shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    (data, err) = process.communicate(s)
    if err:
        raise Exception("Racket produced an error")
    return json.loads(data)
    

def to_formals(json):
    if "improper" in json:
        regular, last = json["improper"]
        return [values.W_Symbol.make(str(x["lexical"])) for x in regular], values.W_Symbol.make(str(last["lexical"]))
    elif isinstance(json, list):
        return [values.W_Symbol.make(str(x["lexical"])) for x in json], None
    elif "lexical" in json:
        return [], values.W_Symbol.make(str(json["lexical"]))
    assert 0


def to_bindings(json):
    def to_binding(j):
        fmls, rest = to_formals(j[0])
        assert not rest
        assert len(fmls) == 1
        return (fmls[0], _to_ast(j[1])) # this is bad for multiple values
    l  = [to_binding(x) for x in json]
    if not l: return l,l
    return zip(*l)

def mksym(j):
    for i in ["toplevel", "lexical", "module"]:
        if i in j:
            return values.W_Symbol.make(str(j[i]))
    assert 0

def _to_ast(json):
    if isinstance(json, list):
        if json[0] == {"module": "begin"}:
            return Begin([_to_ast(x) for x in json[1:]])
        if json[0] == {"module": "#%expression"}:
            return _to_ast(json[1])
        if json[0] == {"module": "#%app"}:
            return App(_to_ast(json[1]), [_to_ast(x) for x in json[2:]]).let_convert()
        if json[0] == {"module": "if"}:
            return If(_to_ast(json[1]), _to_ast(json[2]),  _to_ast(json[3])).let_convert()
        if json[0] == {"module": "quote"}:
            return Quote(to_value(json[1]))
        if json[0] == {"module": "lambda"}:
            fmls, rest = to_formals(json[1])
            return Lambda(fmls, rest, [_to_ast(x) for x in json[2:]])
        if json[0] == {"module": "letrec-values"}:
            vars, rhss = to_bindings(json[1])
            return make_letrec(list(vars), list(rhss), [_to_ast(x) for x in json[2:]])
        if json[0] == {"module": "let-values"}:
            vars, rhss = to_bindings(json[1])
            return make_let(list(vars), list(rhss), [_to_ast(x) for x in json[2:]])
        if json[0] == {"module": "set!"}:
            j = json[1]
            if "lexical" in j:
                return SetBang(CellRef(values.W_Symbol.make(str(j["lexical"]))), _to_ast(json[2]))
            if "toplevel" in j:
                return SetBang(ToplevelVar(values.W_Symbol.make(str(j["toplevel"]))), _to_ast(json[2]))
            else:
                assert 0
        if json[0] == {"module": "#%top"}:
            assert 0
            return CellRef(values.W_Symbol.make(str(json[1]["symbol"])))
        if json[0] == {"module": "define-values"}:
            fmls = [mksym(x) for x in json[1]]
            assert len(fmls) == 1
            return Define(fmls[0],_to_ast(json[2]))
        if json[0] == {"module": "quote-syntax"}:
            raise Exception("quote-syntax is unsupported")
        if json[0] == {"module": "begin0"}:
            raise Exception("begin0 is unsupported")
        if json[0] == {"module": "with-continuation-mark"}:
            raise Exception("with-continuation-mark is unsupported")
        if json[0] == {"module": "#%variable-reference"}:
            raise Exception("#%variable-reference is unsupported")
        if json[0] == {"module": "case-lambda"}:
            raise Exception("case-lambda is unsupported")
        if json[0] == {"module": "define-syntaxes"}:
            return Begin([])
        assert 0
    if isinstance(json, dict):
        if "module" in json:
            return ModuleVar(values.W_Symbol.make(str(json["module"])))
        if "lexical" in json:
            return LexicalVar(values.W_Symbol.make(str(json["lexical"])))
        if "toplevel" in json:
            return ToplevelVar(values.W_Symbol.make(str(json["toplevel"])))
        assert 0
    assert 0

def to_ast(json):
    ast = _to_ast(json)
    return ast.assign_convert({})

def to_value(json):
    if json is False:
        return values.w_false
    if json is True:
        return values.w_true
    if isinstance(json, dict):
        if "vector" in json:
            return values.W_Vector([to_value(v) for v in json["vector"]])
        if "integer" in json:
            return values.W_Fixnum(int(json["integer"]))
        if "real" in json:
            return values.W_Flonum(float(json["real"]))
        if "string" in json:
            return values.W_String(str(json["string"]))
        if "improper" in json:
            return values.to_improper([to_value(v) for v in json["improper"][0]],
                                      to_value(json["improper"][1]))
        for i in ["toplevel", "lexical", "module"]:
            if i in json:
                return values.W_Symbol.make(str(json[i]))
    if isinstance(json, list):
        return values.to_list([to_value(j) for j in json])
    assert 0
        
        
