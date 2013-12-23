
import subprocess
import os

from rpython.rlib import rfile
import pycket.json as pycket_json
from pycket.interpreter import *
from pycket import values
from pycket import vector


#### ========================== Utility functions

def readfile(fname):
    "NON_RPYTHON"
    f = open(fname)
    s = f.read()
    f.close()
    return s

def readfile_rpython(fname):
    f = rfile.create_file(fname, "r")
    s = f.read()
    f.close()
    return s


#### ========================== Functions for expanding code to json

fn = os.path.join(os.path.dirname(__file__), "expand_racket.rkt")

def expand_string(s):
    "NON_RPYTHON"
    process = subprocess.Popen(
        "racket %s" % (fn, ),
        shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    (data, err) = process.communicate(s)
    if len(data) == 0:
        raise Exception("Racket did not produce output. Probably racket is not installed, or it could not parse the input.")
    if err:
        raise Exception("Racket produced an error")
    return data

def expand(s):
    data = expand_string(s)
    return pycket_json.loads(data)


#### ========================== Functions for parsing json to an AST

def parse_ast(json_string):
    json = pycket_json.loads(json_string)
    return to_ast(json)

def load_json_ast(fname):
    data = readfile(fname)
    return parse_ast(data)

def load_json_ast_rpython(fname):
    data = readfile_rpython(fname)
    return parse_ast(data)

def to_ast(json):
    ast = _to_ast(json)
    return ast.assign_convert({})


#### ========================== Implementation functions

def to_formals(json):
    if json.is_object:
        if "improper" in json.value_object:
            improper_arr = json.value_object["improper"]
            regular, last = improper_arr.value_array
            regular_symbols = [values.W_Symbol.make(x.value_object["lexical"].value_string) for x in regular.value_array]
            last_symbol = values.W_Symbol.make(last.value_object["lexical"].value_string)
            return regular_symbols, last_symbol
        elif "lexical" in json.value_object:
            return [], values.W_Symbol.make(json.value_object["lexical"].value_string)
    elif json.is_array:
        return [values.W_Symbol.make(x.value_object["lexical"].value_string) for x in json.value_array], None
    assert 0

def to_bindings(json):
    def to_binding(arr):
        fmls, rest = to_formals(arr[0])
        assert not rest
        assert len(fmls) == 1
        return (fmls[0], _to_ast(arr[1])) # this is bad for multiple values
    l  = [to_binding(x.value_array) for x in json.value_array]
    if not l: return l,l
    return zip(*l)

def mksym(json):
    j = json.value_object
    for i in ["toplevel", "lexical", "module"]:
        if i in j:
            return values.W_Symbol.make(j[i].value_string)
    assert 0, json.tostring()

def _to_ast(json):
    if json.is_array:
        arr = json.value_array
        if "module" in arr[0].value_object:
            ast_elem = arr[0].value_object["module"].value_string
            if ast_elem == "begin":
                return Begin([_to_ast(x) for x in arr[1:]])
            if ast_elem == "#%expression":
                return _to_ast(arr[1])
            if ast_elem == "#%app":
                return App(_to_ast(arr[1]), [_to_ast(x) for x in arr[2:]]).let_convert()
            if ast_elem == "if":
                return If(_to_ast(arr[1]), _to_ast(arr[2]), _to_ast(arr[3])).let_convert()
            if ast_elem == "quote":
                return Quote(to_value(arr[1]))
            if ast_elem == "lambda":
                fmls, rest = to_formals(arr[1])
                return Lambda(fmls, rest, [_to_ast(x) for x in arr[2:]])
            if ast_elem == "letrec-values":
                vars, rhss = to_bindings(arr[1])
                return make_letrec(list(vars), list(rhss), [_to_ast(x) for x in arr[2:]])
            if ast_elem == "let-values":
                vars, rhss = to_bindings(arr[1])
                return make_let(list(vars), list(rhss), [_to_ast(x) for x in arr[2:]])
            if ast_elem == "set!":
                target = arr[1].value_object
                if "lexical" in target:
                    assert target["lexical"].is_string
                    return SetBang(CellRef(values.W_Symbol.make(target["lexical"].value_string)), _to_ast(arr[2]))
                if "toplevel" in target:
                    assert target["toplevel"].is_string
                    return SetBang(ToplevelVar(values.W_Symbol.make(target["toplevel"].value_string)), _to_ast(arr[2]))
                assert 0
            if ast_elem == "#%top":
                assert 0
                return CellRef(values.W_Symbol.make(arr[1].value_object["symbol"].value_string))
            if ast_elem == "define-values":
                fmls = [mksym(x) for x in arr[1]]
                assert len(fmls) == 1
                return Define(fmls[0], _to_ast(arr[2]))
            if ast_elem == "quote-syntax":
                raise Exception("quote-syntax is unsupported")
            if ast_elem == "begin0":
                raise Exception("begin0 is unsupported")
            if ast_elem == "with-continuation-mark":
                raise Exception("with-continuation-mark is unsupported")
            if ast_elem == "#%variable-reference":
                raise Exception("#%variable-reference is unsupported")
            if ast_elem == "case-lambda":
                raise Exception("case-lambda is unsupported")
            if ast_elem == "define-syntaxes":
                return Begin([])
        assert 0, "Unexpected ast-element element: %s" % arr[0].tostring()
    if json.is_object:
        obj = json.value_object
        if "module" in obj:
            return ModuleVar(values.W_Symbol.make(obj["module"].value_string))
        if "lexical" in obj:
            return LexicalVar(values.W_Symbol.make(obj["lexical"].value_string))
        if "toplevel" in obj:
            return ToplevelVar(values.W_Symbol.make(obj["toplevel"].value_string))
    assert 0, "Unexpected json object: %s"%json_obj.tostring()

def to_value(json):
    if json.is_bool:
        if json.value_bool is False:
            print "Using false"
            return values.w_false
        elif json.value_bool is True:
            print "Using true"
            return values.w_true
    if json.is_object:
        # The json-object should only contain one element
        obj = json.value_object
        if "vector" in obj:
            return vector.W_Vector.fromelements([to_value(v) for v in obj["vector"].value_array])
        if "integer" in obj:
            return values.W_Fixnum(int(obj["integer"].value_string))
        if "real" in obj:
            return values.W_Flonum(float(obj["real"].value_float))
        if "string" in obj:
            return values.W_String(str(obj["string"].value_string))
        if "improper" in obj:
            improper = obj["improper"].value_array
            return values.to_improper([to_value(v) for v in improper[0].value_array], to_value(improper[1]))
        for i in ["toplevel", "lexical", "module"]:
            if i in obj:
                return values.W_Symbol.make(obj[i].value_string)
    if json.is_array:
        return values.to_list([to_value(j) for j in json.value_array])
    assert 0, "Unexpected json value: %s" % json.tostring()
