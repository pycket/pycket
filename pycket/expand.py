#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
import os
import sys

from rpython.rlib import streamio
from rpython.rlib.rbigint import rbigint
from rpython.rlib.objectmodel import specialize
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

fn = "-l pycket/expand --"

def expand_string(s):
    "NON_RPYTHON"
    from subprocess import Popen, PIPE

    cmd = "racket %s --stdin --stdout " % (fn)
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

# Call the Racket expander and read its output from STDOUT rather than producing an
# intermediate (possibly cached) file.
def expand_file_rpython(rkt_file):
    from rpython.rlib.rfile import create_popen_file
    cmd = "racket %s --stdout %s 2>&1" % (fn, rkt_file)
    if not os.access(rkt_file, os.R_OK):
        raise ValueError("Cannot access file %s" % rkt_file)
    pipe = create_popen_file(cmd, "r")
    out = pipe.read()
    err = os.WEXITSTATUS(pipe.close())
    if err != 0:
        raise Exception("Racket produced an error and said '%s'" % out)
    return out

# Expand and load the module without generating intermediate JSON files.
def expand_to_ast(fname):
    data = expand_file_rpython(fname)
    return _to_module(pycket_json.loads(data)).assign_convert(variable_set(), None)

def expand(s, wrap=False, stdlib=False):
    data = expand_string(s)
    return pycket_json.loads(data)

def expand_file_to_json(rkt_file, json_file):
    from rpython.rlib.rfile import create_popen_file
    if not os.access(rkt_file, os.R_OK):
        raise ValueError("Cannot access file %s" % rkt_file)
    try:
        os.remove(json_file)
    except IOError:
        pass
    except OSError:
        pass
    cmd = "racket %s --output %s %s 2>&1" % (
        fn,
        json_file, rkt_file)
    # print cmd
    pipe = create_popen_file(cmd, "r")
    out = pipe.read()
    err = os.WEXITSTATUS(pipe.close())
    if err != 0:
        raise Exception("Racket produced an error and said '%s'" % out)
    return json_file


def expand_code_to_json(code, json_file, stdlib=True, mcons=False, wrap=True):
    from rpython.rlib.rfile import create_popen_file
    try:
        os.remove(json_file)
    except IOError:
        pass
    except OSError:
        pass
    cmd = "racket %s --output %s --stdin" % (
        fn,
        json_file)
    # print cmd
    pipe = create_popen_file(cmd, "w")
    pipe.write("#lang s-exp pycket%s"%(" #:stdlib" if stdlib else ""))
    pipe.write(code)
    err = os.WEXITSTATUS(pipe.close())
    if err != 0:
        raise Exception("Racket produced an error we failed to record")
    return json_file


def needs_update(file_name, json_name):
    try:
        file_mtime = os.stat(file_name).st_mtime
        if os.access(json_name, os.F_OK):
            if file_mtime < os.stat(json_name).st_mtime:
                return False
    except OSError:
        pass
    return True


def _json_name(file_name):
    return file_name + '.json'

def ensure_json_ast_run(file_name):
    json = _json_name(file_name)
    if needs_update(file_name, json):
        return expand_file_to_json(file_name, json)
    else:
        return json

def ensure_json_ast_load(file_name):
    return ensure_json_ast_run(file_name)

def ensure_json_ast_eval(code, file_name, stdlib=True, mcons=False, wrap=True):
    json = _json_name(file_name)
    if needs_update(file_name, json):
        return expand_code_to_json(code, json, stdlib, mcons, wrap)
    else:
        return json


#### ========================== Functions for parsing json to an AST

def load_json_ast(fname):
    data = readfile(fname)
    return _to_module(pycket_json.loads(data)).assign_convert(variable_set(), None)

def load_json_ast_rpython(fname):
    data = readfile_rpython(fname)
    return _to_module(pycket_json.loads(data)).assign_convert(variable_set(), None)

def parse_ast(json_string):
    json = pycket_json.loads(json_string)
    return to_ast(json)

def parse_module(json_string):
    json = pycket_json.loads(json_string)
    return _to_module(json).assign_convert(variable_set(), None)


def to_ast(json):
    ast = _to_ast(json)
    return ast.assign_convert(variable_set(), None)


#### ========================== Implementation functions

DO_DEBUG_PRINTS = False

@specialize.argtype(1)
def dbgprint(funcname, json):
    # This helped debugging segfaults
    if DO_DEBUG_PRINTS:
        if isinstance(json, pycket_json.JsonBase):
            s = json.tostring()
        else:
            s = "[" + ", ".join([j.tostring() for j in json]) + "]"
        print "Entering %s with: %s" % (funcname, s)

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

def to_bindings(arr):
    dbgprint("to_bindings", arr)
    varss = []
    rhss = []
    for v in arr:
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

def _to_module(json):
    v = json.value_object()
    if "body-forms" in v:
        return Module(v["module-name"].value_string(), 
                      [_to_ast(x) for x in v["body-forms"].value_array()])
    else:
        assert 0

# A global table listing all the module files that have been loaded.
# A module need only be loaded once.
# Modules (aside from builtins like #%kernel) are listed in the table
# as paths to their implementing files which are assumed to be normalized.
class ModTable(object):
    table = {"#%kernel" : None}

    @staticmethod
    def add_module(fname):
        ModTable.table[fname] = None

    @staticmethod
    def has_module(fname):
        return fname in ModTable.table

def _to_require(fname):
    if ModTable.has_module(fname):
        return Quote(values.w_void)
    ModTable.add_module(fname)
    module = expand_to_ast(fname) # _expand_and_load(fname)
    return Require(fname, module)

def _expand_and_load(fname):
    return load_json_ast(ensure_json_ast_run(fname))

def to_lambda(arr):
    fmls, rest = to_formals(arr[0])
    return make_lambda(fmls, rest, [_to_ast(x) for x in arr[1:]])

def _to_ast(json):
    dbgprint("_to_ast", json)
    if json.is_array:
        arr = json.value_array()
        if "module" in arr[0].value_object() and arr[0].value_object()["source-module"].value_string() == "#%kernel":
            ast_elem = arr[0].value_object()["source-name"].value_string()
            if ast_elem == "begin":
                return Begin([_to_ast(x) for x in arr[1:]])
            if ast_elem == "#%expression":
                return _to_ast(arr[1])
            if ast_elem == "lambda":
                return CaseLambda([to_lambda(arr[1:])])
            if ast_elem == "set!":
                target = arr[1].value_object()
                var = None
                if "module" in target:
                    var = ModCellRef(values.W_Symbol.make(target["module"].value_string()), 
                                     target["source-module"].value_string() 
                                     if target["source-module"].is_string else
                                     None,
                                     values.W_Symbol.make(target["source-name"].value_string()))
                if "lexical" in target:
                    var = CellRef(values.W_Symbol.make(target["lexical"].value_string()))
                if "toplevel" in target:
                    var = ToplevelVar(values.W_Symbol.make(target["toplevel"].value_string()))
                return SetBang(var, _to_ast(arr[2]))
            if ast_elem == "#%top":
                assert 0
                return CellRef(values.W_Symbol.make(arr[1].value_object()["symbol"].value_string()))
            if ast_elem == "define-values":
                fmls = [mksym(x) for x in arr[1].value_array()]
                assert len(fmls) == 1
                return DefineValues(fmls, _to_ast(arr[2]))
            if ast_elem == "quote-syntax":
                raise Exception("quote-syntax is unsupported")
            if ast_elem == "with-continuation-mark":
                raise Exception("with-continuation-mark is unsupported")
            if ast_elem == "#%variable-reference":
                raise Exception("#%variable-reference is unsupported")
            if ast_elem == "case-lambda":
                lams = [to_lambda(v.value_array()) for v in arr[1:]]
                return CaseLambda(lams)
            if ast_elem == "define-syntaxes":
                return Quote(values.w_void)
            # The parser now ignores `#%require` AST nodes.
            # The actual file to include is now generated by expander
            # as an object that is handled below.
            if ast_elem == "#%require":
                return Quote(values.w_void)
            if ast_elem == "#%provide":
                return Quote(values.w_void)
        assert 0, "Unexpected ast-element element: %s" % arr[0].tostring()
    if json.is_object:
        obj = json.value_object()
        if "require" in obj:
            path = obj["require"].value_string()
            return _to_require(path)
        if "begin0" in obj:
            fst = _to_ast(obj["begin0"])
            rst = [_to_ast(x) for x in obj["begin0-rest"].value_array()]
            if len(rst) == 0:
                return fst
            else:
                return Begin0.make(fst, rst)
        if "letrec-bindings" in obj:
            body = [_to_ast(x) for x in obj["letrec-body"].value_array()]
            bindings = obj["letrec-bindings"].value_array()
            if len(bindings) == 0:
                return Begin.make(body)
            else:
                vs, rhss = to_bindings(bindings)
                for v in vs:
                    for var in v:
                        assert isinstance(var, values.W_Symbol)
                assert isinstance(rhss[0], AST)
                return make_letrec(list(vs), list(rhss), body)
        if "let-bindings" in obj:
            body = [_to_ast(x) for x in obj["let-body"].value_array()]
            bindings = obj["let-bindings"].value_array()
            if len(bindings) == 0:
                return Begin.make(body)
            else:
                vs, rhss = to_bindings(bindings)
                for v in vs:
                    for var in v:
                        assert isinstance(var, values.W_Symbol)
                assert isinstance(rhss[0], AST)
                return make_let(list(vs), list(rhss), body)
        if "operator" in obj:
            return App(_to_ast(obj["operator"]), [_to_ast(x) for x in obj["operands"].value_array()]).let_convert()
        if "test" in obj:
            return If(_to_ast(obj["test"]), _to_ast(obj["then"]), _to_ast(obj["else"])).let_convert()
        if "quote" in obj:
            return Quote(to_value(obj["quote"]))
        if "module" in obj:
            return ModuleVar(values.W_Symbol.make(obj["module"].value_string()), 
                             obj["source-module"].value_string() 
                             if obj["source-module"].is_string else
                             None,
                             values.W_Symbol.make(obj["source-name"].value_string()))
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
            val = rbigint.fromdecimalstr(obj["integer"].value_string())
            try:
                return values.W_Fixnum(int(val.toint()))
            except OverflowError:
                return values.W_Bignum(val)
        if "real" in obj:
            return values.W_Flonum(float(obj["real"].value_float()))
        if "char" in obj:
            return values.W_Character(unichr(int(obj["char"].value_string())))
        if "string" in obj:
            return values.W_String(str(obj["string"].value_string()))
        if "keyword" in obj:
            return values.W_Keyword.make(str(obj["keyword"].value_string()))
        if "improper" in obj:
            improper = obj["improper"].value_array()
            return values.to_improper([to_value(v) for v in improper[0].value_array()], to_value(improper[1]))
        for i in ["toplevel", "lexical", "module"]:
            if i in obj:
                return values.W_Symbol.make(obj[i].value_string())
    if json.is_array:
        return values.to_list([to_value(j) for j in json.value_array()])
    assert 0, "Unexpected json value: %s" % json.tostring()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        print parse_module(expand_file(sys.argv[1])).tostring()
