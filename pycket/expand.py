import json
import subprocess
import os

fn = os.path.join(os.path.dirname(__file__), "expand_racket.rkt")

def expand(s):
    process = subprocess.Popen(
        "racket %s" % (fn, ),
        shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    (data, err) = process.communicate(s)
    if err:
        raise Exception("Racket produced an error")
    return json.loads(data)
    

def to_formals (json):
    if "improper" in json:
        regular, last = json["improper"]
        return [values.W_Symbol.make(str(x["symbol"])) for x in regular], values.W_Symbol.make(str(last["symbol"]))
    elif isinstance (json, list):
        return [values.W_Symbol.make(str(x["symbol"])) for x in json], None
    elif "symbol" in json:
        return [], values.W_Symbol.make(str(json["symbol"]))
    assert 0


def to_bindings(json):
    def to_binding(j):
        fmls, rest = to_formals(j[0])
        assert not rest
        assert len (fmls) == 1
        return (fmls[0], _to_ast(j[1])) # this is bad for multiple values
    l  = [to_binding(x) for x in json]
    if not l: return l,l
    return zip(*l)

def _to_ast(json):
    if isinstance(json, list):
        if json[0] == {"symbol": "begin"}:
            return Begin([_to_ast(x) for x in json[1:]])
        if json[0] == {"symbol": "#%expression"}:
            return _to_ast(json[1])
        if json[0] == {"symbol": "#%app"}:
            return App(_to_ast(json[1]), [_to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "if"}:
            return If(_to_ast(json[1]), _to_ast(json[2]),  _to_ast(json[3]))
        if json[0] == {"symbol": "quote"}:
            return Quote(to_value(json[1]))
        if json[0] == {"symbol": "lambda"}:
            fmls, rest = to_formals(json[1])
            return Lambda(fmls, rest, [_to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "letrec-values"}:
            vars, rhss = to_bindings(json[1])
            return Letrec(list(vars), list(rhss), [_to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "let-values"}:
            vars, rhss = to_bindings(json[1])
            return Let(vars, rhss, [_to_ast(x) for x in json[2:]])
        if json[0] == {"symbol": "set!"}:
            return SetBang(values.W_Symbol.make(str(json[1]["symbol"])), _to_ast(json[2]))
        if json[0] == {"symbol": "#%top"}:
            return CellRef(values.W_Symbol.make(str(json[1]["symbol"])))
        if json[0] == {"symbol": "define-values"}:
            fmls, rest = to_formals(json[1])
            assert not rest
            assert len(fmls) == 1
            return Define(fmls[0],_to_ast(json[2]))
        if json[0] == {"symbol": "quote-syntax"}:
            raise Exception ("quote-syntax is unsupported")
        if json[0] == {"symbol": "begin0"}:
            raise Exception ("begin0 is unsupported")
        if json[0] == {"symbol": "with-continuation-mark"}:
            raise Exception ("with-continuation-mark is unsupported")
        if json[0] == {"symbol": "#%variable-reference"}:
            raise Exception ("#%variable-reference is unsupported")
        if json[0] == {"symbol": "case-lambda"}:
            raise Exception ("case-lambda is unsupported")
        assert 0
    if isinstance(json, dict):
        if "symbol" in json:
            return Var(values.W_Symbol.make(str(json["symbol"])))
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
    if isinstance (json, dict):
        if "vector" in json:
            return values.W_Vector(json["vector"])
        if "integer" in json:
            return values.W_Fixnum(int(json["integer"]))
        if "real" in json:
            return values.W_Flonum(float(json["real"]))
        else: assert 0
        
