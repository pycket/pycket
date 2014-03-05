#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
import os

from rpython.rlib import streamio
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
    f = streamio.open_file_as_stream(fname)
    s = f.readall()
    f.close()
    return s


#### ========================== Functions for expanding code to json

fn = os.path.join(os.path.dirname(__file__), "expand_racket.rkt")

def expand_string(s, wrap=False, stdlib=False):
    "NON_RPYTHON"
    from subprocess import Popen, PIPE

    cmd = "racket %s %s --stdin --stdout %s" % (
        fn, "" if stdlib else "--no-stdlib", "" if wrap else "--no-wrap")
    process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE)
    (data, err) = process.communicate(s)
    if len(data) == 0:
        raise Exception("Racket did not produce output. Probably racket is not installed, or it could not parse the input.")
    if err:
        raise Exception("Racket produced an error")
    return data


def expand_file(fname):
    "NON_RPYTHON"
    from subprocess import Popen, PIPE

    cmd = "racket %s --stdout %s" % (fn, fname)
    process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE)
    (data, err) = process.communicate()
    if len(data) == 0:
        raise Exception("Racket did not produce output. Probably racket is not installed, or it could not parse the input.")
    if err:
        raise Exception("Racket produced an error")
    return data

def expand(s, wrap=False):
    data = expand_string(s,wrap)
    return pycket_json.loads(data)


def expand_file_to_json(rkt_file, json_file, stdlib=True, wrap=True):
    if not os.access(rkt_file, os.R_OK):
        raise ValueError("Cannot access file %s" % rkt_file)
    try:
        os.remove(json_file)
    except IOError:
        pass
    except OSError:
        pass
    cmd = "racket %s %s%s--output %s %s" % (
        fn, "" if stdlib else "--no-stdlib ", "" if wrap else "--no-wrap ",
        json_file, rkt_file)
    # print cmd
    err = os.system(cmd)
    if err != 0:
        raise Exception("Racket produced an error")
    return json_file


def expand_code_to_json(code, json_file, stdlib=True, wrap=True):
    from rpython.rlib.rfile import create_popen_file
    try:
        os.remove(json_file)
    except IOError:
        pass
    except OSError:
        pass
    cmd = "racket %s %s%s--output %s --stdin" % (
        fn, "" if stdlib else "--no-stdlib ", "" if wrap else "--no-wrap ",
        json_file)
    # print cmd
    pipe = create_popen_file(cmd, "w")
    pipe.write(code)
    err = os.WEXITSTATUS(pipe.close())
    if err != 0:
        raise Exception("Racket produced an error")
    return json_file


def needs_update(file_name, json_name):
    try:
        file_mtime = os.stat(file_name).st_mtime
        if os.access(json_name, os.F_OK):
            if not file_mtime < os.stat(json_name).st_mtime:
                return False
    except OSError:
        pass
    return True


def _json_name(file_name):
    return file_name + '.json'

def ensure_json_ast_run(file_name, stdlib=True, wrap=True):
    json = _json_name(file_name)
    if needs_update(file_name, json):
        return expand_file_to_json(file_name, json, stdlib, wrap)
    else:
        return json

def ensure_json_ast_load(file_name, stdlib=True, wrap=False):
    return ensure_json_ast_run(file_name, stdlib, wrap)

def ensure_json_ast_eval(code, file_name, stdlib=True, wrap=True):
    json = _json_name(file_name)
    if needs_update(file_name, json):
        return expand_code_to_json(code, json, stdlib, wrap)
    else:
        return json


#### ========================== Functions for parsing json to an AST

def load_json_ast(fname):
    data = readfile(fname)
    return parse_ast(data)

def load_json_ast_rpython(fname):
    data = readfile_rpython(fname)
    return parse_ast(data)

def parse_ast(json_string):
    json = pycket_json.loads(json_string)
    return to_ast(json)

def to_ast(json):
    ast = _to_ast(json)
    return ast.assign_convert({}, None)


#### ========================== Implementation functions

DO_DEBUG_PRINTS = False
def dbgprint(funcname, json):
    # This helped debugging segfaults
    if DO_DEBUG_PRINTS:
        print "Entering %s with: %s" % (funcname, json.tostring())

def to_formals(json):
    dbgprint("to_formals", json)
    if json.is_object:
        if "improper" in json.value_object():
            improper_arr = json.value_object()["improper"]
            regular, last = improper_arr.value_array()
            regular_symbols = [values.W_Symbol.make(x.value_object()["lexical"].value_string()) for x in regular.value_array()]
            last_symbol = values.W_Symbol.make(last.value_object()["lexical"].value_string())
            return regular_symbols, last_symbol
        elif "lexical" in json.value_object():
            return [], values.W_Symbol.make(json.value_object()["lexical"].value_string())
    elif json.is_array:
        return [values.W_Symbol.make(x.value_object()["lexical"].value_string()) for x in json.value_array()], None
    assert 0

def to_bindings(json):
    dbgprint("to_bindings", json)
    varss = []
    rhss = []
    for v in json.value_array():
        arr = v.value_array()
        fmls, rest = to_formals(arr[0])
        assert not rest
        rhs = _to_ast(arr[1]) 
        varss.append(fmls)
        rhss.append(rhs)
    return varss, rhss

def mksym(json):
    dbgprint("mksym", json)
    j = json.value_object()
    for i in ["toplevel", "lexical", "module"]:
        if i in j:
            return values.W_Symbol.make(j[i].value_string())
    assert 0, json.tostring()

def _to_ast(json):
    dbgprint("_to_ast", json)
    if json.is_array:
        arr = json.value_array()
        if "module" in arr[0].value_object():
            ast_elem = arr[0].value_object()["module"].value_string()
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
                return make_lambda(fmls, rest, [_to_ast(x) for x in arr[2:]])
            if ast_elem == "letrec-values":
                body = [_to_ast(x) for x in arr[2:]]
                if len(arr[1].value_array()) == 0:
                    return Begin.make(body)
                else:
                    vs, rhss = to_bindings(arr[1])
                    for v in vs:
                        for var in v:
                            assert isinstance(var, values.W_Symbol)
                    assert isinstance(rhss[0], AST)
                    return make_letrec(list(vs), list(rhss), body)
            if ast_elem == "let-values":
                body = [_to_ast(x) for x in arr[2:]]
                if len(arr[1].value_array()) == 0:
                    return Begin.make(body)
                else:
                    vs, rhss = to_bindings(arr[1])
                    for v in vs:
                        for var in v:
                            assert isinstance(var, values.W_Symbol)
                    for r in rhss:
                        assert isinstance(r, AST)
                    return make_let(list(vs), list(rhss), body)
            if ast_elem == "set!":
                target = arr[1].value_object()
                if "lexical" in target:
                    assert target["lexical"].is_string
                    return SetBang(CellRef(values.W_Symbol.make(target["lexical"].value_string())), _to_ast(arr[2]))
                if "toplevel" in target:
                    assert target["toplevel"].is_string
                    return SetBang(ToplevelVar(values.W_Symbol.make(target["toplevel"].value_string())), _to_ast(arr[2]))
                assert 0
            if ast_elem == "#%top":
                assert 0
                return CellRef(values.W_Symbol.make(arr[1].value_object()["symbol"].value_string()))
            if ast_elem == "define-values":
                fmls = [mksym(x) for x in arr[1].value_array()]
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
                return Quote(values.w_void)
        assert 0, "Unexpected ast-element element: %s" % arr[0].tostring()
    if json.is_object:
        obj = json.value_object()
        if "module" in obj:
            return ModuleVar(values.W_Symbol.make(obj["module"].value_string()))
        if "lexical" in obj:
            return LexicalVar(values.W_Symbol.make(obj["lexical"].value_string()))
        if "toplevel" in obj:
            return ToplevelVar(values.W_Symbol.make(obj["toplevel"].value_string()))
    assert 0, "Unexpected json object: %s" % json.tostring()

def to_value(json):
    dbgprint("to_value", json)
    if json is pycket_json.json_false:
        return values.w_false
    elif json is pycket_json.json_true:
        return values.w_true
    if json.is_object:
        # The json-object should only contain one element
        obj = json.value_object()
        if "vector" in obj:
            return vector.W_Vector.fromelements([to_value(v) for v in obj["vector"].value_array()])
        if "integer" in obj:
            return values.W_Fixnum(int(obj["integer"].value_string()))
        if "real" in obj:
            return values.W_Flonum(float(obj["real"].value_float()))
        if "char" in obj:
            return values.W_Character(unichr(int(obj["char"].value_string())))
        if "string" in obj:
            return values.W_String(str(obj["string"].value_string()))
        if "improper" in obj:
            improper = obj["improper"].value_array()
            return values.to_improper([to_value(v) for v in improper[0].value_array()], to_value(improper[1]))
        for i in ["toplevel", "lexical", "module"]:
            if i in obj:
                return values.W_Symbol.make(obj[i].value_string())
    if json.is_array:
        return values.to_list([to_value(j) for j in json.value_array()])
    assert 0, "Unexpected json value: %s" % json.tostring()
