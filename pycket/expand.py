#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
import os
import sys

from rpython.rlib import streamio
from rpython.rlib.rbigint import rbigint
from rpython.rlib.objectmodel import specialize, we_are_translated
from rpython.rlib.rstring import ParseStringError, ParseStringOverflowError
from rpython.rlib.rarithmetic import string_to_int
from pycket import pycket_json
from pycket.error import SchemeException
from pycket.interpreter import *
from pycket import values, values_string
from pycket import values_regex
from pycket import vector
from pycket import values_struct
from pycket import values_hash

class ExpandException(SchemeException):
    pass

class PermException(SchemeException):
    pass

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


current_racket_proc = None

def expand_string(s, reuse=True, srcloc=True):
    "NON_RPYTHON"
    global current_racket_proc
    from subprocess import Popen, PIPE

    cmd = "racket %s --loop --stdin --stdout %s" % (fn, "" if srcloc else "--omit-srcloc")
    if current_racket_proc and reuse and current_racket_proc.poll() is None:
        process = current_racket_proc
    else:
        process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE)
        if reuse:
            current_racket_proc = process
    if reuse:
        process.stdin.write(s.encode("utf-8"))
        ## I would like to write something so that Racket sees EOF without
        ## closing the file. But I can't figure out how to do that. It
        ## must be possible, though, because bash manages it.
        #process.stdin.write(chr(4))
        process.stdin.write("\n\0\n")
        process.stdin.flush()
        #import pdb; pdb.set_trace()
        data = process.stdout.readline()
    else:
        (data, err) = process.communicate(s)
    if len(data) == 0:
        raise ExpandException("Racket did not produce output. Probably racket is not installed, or it could not parse the input.")
    # if err:
    #     raise ExpandException("Racket produced an error")
    return data

def expand_file(fname):
    "NON_RPYTHON"
    from subprocess import Popen, PIPE

    cmd = "racket %s --stdout \"%s\"" % (fn, fname)
    process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE)
    (data, err) = process.communicate()
    if len(data) == 0:
        raise ExpandException("Racket did not produce output. Probably racket is not installed, or it could not parse the input.")
    if err:
        raise ExpandException("Racket produced an error")
    return data

# Call the Racket expander and read its output from STDOUT rather than producing an
# intermediate (possibly cached) file.
def expand_file_rpython(rkt_file):
    from rpython.rlib.rfile import create_popen_file
    cmd = "racket %s --stdout \"%s\" 2>&1" % (fn, rkt_file)
    if not os.access(rkt_file, os.R_OK):
        raise ValueError("Cannot access file %s" % rkt_file)
    pipe = create_popen_file(cmd, "r")
    out = pipe.read()
    err = os.WEXITSTATUS(pipe.close())
    if err != 0:
        raise ExpandException("Racket produced an error and said '%s'" % out)
    return out

def expand_file_cached(rkt_file, modtable):
    try:
        json_file = ensure_json_ast_run(rkt_file)
    except PermException:
        return expand_to_ast(rkt_file, modtable)
    return load_json_ast_rpython(json_file, modtable)

# Expand and load the module without generating intermediate JSON files.
def expand_to_ast(fname, modtable):
    data = expand_file_rpython(fname)
    return _to_module(pycket_json.loads(data), modtable).assign_convert_module()

def expand(s, wrap=False, stdlib=False):
    data = expand_string(s)
    return pycket_json.loads(data)

def wrap_for_tempfile(fn):
    def wrap(rkt_file, json_file):
        "NOT_RPYTHON"
        try:
            os.remove(json_file)
        except IOError:
            pass
        except OSError:
            pass
        from tempfile import mktemp
        tmp_json_file = mktemp(suffix='.json',
                               prefix=json_file[:json_file.rfind('.')])
        out = fn(rkt_file, tmp_json_file)
        assert tmp_json_file == out
        os.rename(tmp_json_file, json_file)
        return json_file

    wrap.__name__ = fn.__name__
    return wrap


def expand_file_to_json(rkt_file, json_file):
    if not we_are_translated():
        return wrap_for_tempfile(_expand_file_to_json)(rkt_file, json_file)
    return _expand_file_to_json(rkt_file, json_file)

def _expand_file_to_json(rkt_file, json_file):
    from rpython.rlib.rfile import create_popen_file
    if not os.access(rkt_file, os.R_OK):
        raise ValueError("Cannot access file %s" % rkt_file)
    if not os.access(rkt_file, os.W_OK):
        # we guess that this means no permission to write the json file
        raise PermException(rkt_file)
    try:
        os.remove(json_file)
    except IOError:
        pass
    except OSError:
        pass
    print "Expanding %s to %s" % (rkt_file, json_file)
    cmd = "racket %s --output \"%s\" \"%s\" 2>&1" % (
        fn,
        json_file, rkt_file)
    # print cmd
    pipe = create_popen_file(cmd, "r")
    out = pipe.read()
    err = os.WEXITSTATUS(pipe.close())
    if err != 0:
        raise ExpandException("Racket produced an error and said '%s'" % out)
    return json_file

def expand_code_to_json(code, json_file, stdlib=True, mcons=False, wrap=True):
    from rpython.rlib.rfile import create_popen_file
    try:
        os.remove(json_file)
    except IOError:
        pass
    except OSError:
        pass
    cmd = "racket %s --output \"%s\" --stdin" % (
        fn,
        json_file)
    # print cmd
    pipe = create_popen_file(cmd, "w")
    pipe.write("#lang s-exp pycket%s" % (" #:stdlib" if stdlib else ""))
    pipe.write(code)
    err = os.WEXITSTATUS(pipe.close())
    if err != 0:
        raise ExpandException("Racket produced an error we failed to record")
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

def ensure_json_ast_eval(code, file_name, stdlib=True, mcons=False, wrap=True):
    json = _json_name(file_name)
    if needs_update(file_name, json):
        return expand_code_to_json(code, json, stdlib, mcons, wrap)
    else:
        return json


#### ========================== Functions for parsing json to an AST

def load_json_ast(fname):
    data = readfile(fname)
    return _to_module(pycket_json.loads(data), modtable).assign_convert_module()

def load_json_ast_rpython(fname, modtable):
    data = readfile_rpython(fname)
    return _to_module(pycket_json.loads(data), modtable).assign_convert_module()

def parse_ast(json_string):
    json = pycket_json.loads(json_string)
    modtable = ModTable()
    return to_ast(json, modtable)

def parse_module(json_string):
    json = pycket_json.loads(json_string)
    modtable = ModTable()
    return _to_module(json, modtable).assign_convert_module()

def to_ast(json, modtable):
    ast = _to_ast(json, modtable)
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
            # a list
            s = "[" + ", ".join([j.tostring() for j in json]) + "]"
        print "Entering %s with: %s" % (funcname, s)

def to_formals(json):
    dbgprint("to_formals", json)
    make = values.W_Symbol.make
    lex  = lambda x : x.value_object()["lexical"].value_string()
    if json.is_object:
        obj = json.value_object()
        if "improper" in obj:
            improper_arr = obj["improper"]
            regular, last = improper_arr.value_array()
            regular_symbols = [make(lex(x)) for x in regular.value_array()]
            last_symbol = make(lex(last))
            return regular_symbols, last_symbol
        elif "lexical" in obj:
            return [], make(obj["lexical"].value_string())
    elif json.is_array:
        arr = json.value_array()
        return [make(lex(x)) for x in arr], None
    assert 0

def to_bindings(arr, modtable):
    dbgprint("to_bindings", arr)
    varss = []
    rhss = []
    for v in arr:
        varr = v.value_array()
        fmls = [values.W_Symbol.make(x.value_string()) for x in varr[0].value_array()]
        rhs = _to_ast(varr[1], modtable)
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

def _to_module(json, modtable):
    # YYY
    v = json.value_object()
    if "body-forms" in v:
        config = {}
        if "config" in v:
            for (k, _v) in v["config"].value_object().iteritems():
                config[k] = _v.value_string()

        lang = [parse_require([b.value_string()], modtable) for b in v["language"].value_array()]
        body = [_to_ast(x, modtable) for x in v["body-forms"].value_array()]
        return Module(v["module-name"].value_string(), lang + body, config)
    else:
        assert 0

void = Quote(values.w_void)

# A table listing all the module files that have been loaded.
# A module need only be loaded once.
# Modules (aside from builtins like #%kernel) are listed in the table
# as paths to their implementing files which are assumed to be normalized.
class ModTable(object):

    def __init__(self):
        self.table = {}
        self.current_modules = []

    def add_module(self, fname, module):
        #print "Adding module '%s'\n\t\tbecause of '%s'" % (fname, self.current_module or "")
        self.table[fname] = module

    def push(self, fname):
        self.current_modules.append(fname)

    def pop(self):
        if not self.current_modules:
            raise SchemeException("No current module")
        self.current_modules.pop()

    def current_mod(self):
        if not self.current_modules:
            return None
        return self.current_modules[-1]

    def has_module(self, fname):
        return fname.startswith("#%") or fname in self.table

    def lookup(self, fname):
        if fname.startswith("#%"):
            return None
        return self.table[fname]

def _to_require(fname, modtable, path=None):
    if modtable.has_module(fname):
        mod = modtable.lookup(fname)
        if mod is None:
            return void
        return Require(fname, modtable.lookup(fname))
    modtable.push(fname)
    modtable.add_module(fname, None)
    module = expand_file_cached(fname, modtable)
    modtable.add_module(fname, module)
    modtable.pop()
    return Require(fname, module, path=path)

def parse_require(path, modtable):
    fname, subs = path[0], path[1:]
    if fname in [".", ".."]:
        return Require(modtable.current_mod(), None, path=subs)
    return _to_require(fname, modtable, path=subs)
    assert 0, "malformed require"

def get_srcloc(o):
    pos = o["position"].value_int() if "position" in o else -1
    source = o["source"] if "source" in o else None
    if source and source.is_object:
        v = source.value_object()
        if "%p" in v:
            sourcefile = v["%p"].value_string()
        elif "quote" in v:
            sourcefile = v["quote"].value_string()
        else:
            assert 0
    else:
        sourcefile = None
    return (pos, sourcefile)

def to_lambda(o, modtable):
    fmls, rest = to_formals(o["lambda"])
    pos, sourcefile = get_srcloc(o)
    return make_lambda(fmls, rest, [_to_ast(x, modtable) for x in o["body"].value_array()],
                       pos, sourcefile)

def parse_path(arr):
    assert len(arr) >= 1
    srcmod, path = arr[0], arr[1:]
    # Relative module names go into the path.
    # None value for the srcmod indicate the current module
    if srcmod in [".", ".."]:
        path   = arr
        srcmod = None
    return srcmod, path

def _to_ast(json, modtable):
    # YYY
    dbgprint("_to_ast", json)
    if json.is_array:
        arr = json.value_array()
        rator = arr[0].value_object()
        if "source-name" in rator and (("source-module" not in rator) or (rator["source-module"].value_string() == "#%kernel")):
            ast_elem = rator["source-name"].value_string()
            if ast_elem == "begin":
                return Begin([_to_ast(x, modtable) for x in arr[1:]])
            if ast_elem == "#%expression":
                return _to_ast(arr[1], modtable)
            if ast_elem == "set!":
                target = arr[1].value_object()
                var = None
                if "source-name" in target:
                    srcname = values.W_Symbol.make(target["source-name"].value_string())
                    if "source-module" in target:
                        if target["source-module"].is_array:
                            path_arr = [t.value_string() for t in target["source-module"].value_array()]
                            srcmod, path = parse_path(path_arr)
                        else:
                            srcmod = path = None
                    else:
                        srcmod = "#%kernel"
                        path   = None
                    modname = values.W_Symbol.make(target["module"].value_string()) if "module" in target else srcname
                    var = ModuleVar(modname, srcmod, srcname, path)
                elif "lexical" in target:
                    var = CellRef(values.W_Symbol.make(target["lexical"].value_string()))
                elif "toplevel" in target:
                    var = ToplevelVar(values.W_Symbol.make(target["toplevel"].value_string()))
                return SetBang(var, _to_ast(arr[2], modtable))
            if ast_elem == "#%top":
                assert 0
                return CellRef(values.W_Symbol.make(arr[1].value_object()["symbol"].value_string()))
            if ast_elem == "begin-for-syntax":
                return void
            if ast_elem == "define-syntaxes":
                return void
            # The parser now ignores `#%require` AST nodes.
            # The actual file to include is now generated by expander
            # as an object that is handled below.
            if ast_elem == "#%require":
                return void
            if ast_elem == "#%provide":
                return void
        assert 0, "Unexpected ast-element element: %s" % json.tostring()
    if json.is_object:
        obj = json.value_object()
        if "require" in obj:
            paths = obj["require"].value_array()
            requires = []
            for path in paths:
                path = [o.value_string() for o in path.value_array()]
                if not path:
                    continue
                requires.append(parse_require(path, modtable))
            return Begin.make(requires) if requires else void
        if "begin0" in obj:
            fst = _to_ast(obj["begin0"], modtable)
            rst = [_to_ast(x, modtable) for x in obj["begin0-rest"].value_array()]
            if len(rst) == 0:
                return fst
            else:
                return Begin0.make(fst, rst)

        if "wcm-key" in obj:
            return WithContinuationMark(_to_ast(obj["wcm-key"], modtable),
                                        _to_ast(obj["wcm-val"], modtable),
                                        _to_ast(obj["wcm-body"], modtable))
        if "define-values" in obj:
            binders = obj["define-values"].value_array()
            display_names = obj["define-values-names"].value_array()
            fmls = [values.W_Symbol.make(x.value_string()) for x in binders]
            disp_syms = [values.W_Symbol.make(x.value_string()) for x in display_names]
            body = _to_ast(obj["define-values-body"], modtable)
            return DefineValues(fmls, body, disp_syms)
        if "letrec-bindings" in obj:
            body = [_to_ast(x, modtable) for x in obj["letrec-body"].value_array()]
            bindings = obj["letrec-bindings"].value_array()
            if len(bindings) == 0:
                return Begin.make(body)
            else:
                vs, rhss = to_bindings(bindings, modtable)
                for v in vs:
                    for var in v:
                        assert isinstance(var, values.W_Symbol)
                assert isinstance(rhss[0], AST)
                return make_letrec(list(vs), list(rhss), body)
        if "let-bindings" in obj:
            body = [_to_ast(x, modtable) for x in obj["let-body"].value_array()]
            bindings = obj["let-bindings"].value_array()
            if len(bindings) == 0:
                return Begin.make(body)
            else:
                vs, rhss = to_bindings(bindings, modtable)
                for v in vs:
                    for var in v:
                        assert isinstance(var, values.W_Symbol)
                assert isinstance(rhss[0], AST)
                return make_let(list(vs), list(rhss), body)
        if "variable-reference" in obj:
            if obj["variable-reference"].is_bool: # assumes that only boolean here is #f
                return VariableReference(None, modtable.current_mod())
            else:
                return VariableReference(_to_ast(obj["variable-reference"], modtable), modtable.current_mod())
        if "lambda" in obj:
                return CaseLambda([to_lambda(obj, modtable)])
        if "case-lambda" in obj:
                lams = [to_lambda(v.value_object(), modtable) for v in obj["case-lambda"].value_array()]
                return CaseLambda(lams)
        if "operator" in obj:
            return App.make_let_converted(_to_ast(obj["operator"], modtable), [_to_ast(x, modtable) for x in obj["operands"].value_array()])
        if "test" in obj:
            return If.make_let_converted(_to_ast(obj["test"], modtable), _to_ast(obj["then"], modtable), _to_ast(obj["else"], modtable))
        if "quote" in obj:
            return Quote(to_value(obj["quote"]))
        if "quote-syntax" in obj:
            return QuoteSyntax(to_value(obj["quote-syntax"]))
        if "source-name" in obj:
            srcname = obj["source-name"].value_string()
            modname = obj["module"].value_string() if "module" in obj else None
            srcsym = values.W_Symbol.make(srcname)
            modsym = values.W_Symbol.make(modname) if modname else srcsym
            if "source-module" in obj:
                if obj["source-module"].is_array:
                    path_arr = [p.value_string() for p in obj["source-module"].value_array()]
                    srcmod, path = parse_path(path_arr)
                else:
                    srcmod = path = None
            else:
                srcmod = "#%kernel"
                path   = None
            return ModuleVar(modsym, srcmod, srcsym, path=path)
        if "lexical" in obj:
            return LexicalVar(values.W_Symbol.make(obj["lexical"].value_string()))
        if "toplevel" in obj:
            return ToplevelVar(values.W_Symbol.make(obj["toplevel"].value_string()))
        if "module-name" in obj:
            # stub for now
            return _to_module(json, modtable)
    assert 0, "Unexpected json object: %s" % json.tostring()

INF = values.W_Flonum(float("inf"))
NEGINF = values.W_Flonum(-float("inf"))
NAN = values.W_Flonum(float("nan"))

def _to_num(json):
    assert json.is_object
    obj = json.value_object()
    if "real" in obj:
        r = obj["real"]
        return values.W_Flonum.make(r.value_float())
    if "real-part" in obj:
        r = obj["real-part"]
        i = obj["imag-part"]
        return values.W_Complex.make(_to_num(r), _to_num(i))
    if "numerator" in obj:
        n = obj["numerator"]
        d = obj["denominator"]
        return values.W_Rational.make(_to_num(n), _to_num(d))
    if "extended-real" in obj:
        rs = obj["extended-real"].value_string()
        if rs == "+inf.0":
            return INF
        if rs == "-inf.0":
            return NEGINF
        if rs == "+nan.0":
            return NAN
    if "integer" in obj:
        rs = obj["integer"].value_string()
        try:
            return values.W_Fixnum.make(string_to_int(rs))
        except ParseStringOverflowError:
            val = rbigint.fromdecimalstr(rs)
            return values.W_Bignum(val)
    assert False

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
            return vector.W_Vector.fromelements([to_value(v) for v in obj["vector"].value_array()], immutable=True)
        if "struct" in obj:
            key = to_value(obj["prefab-key"])
            fields = [to_value(v) for v in obj["struct"].value_array()]
            return values_struct.W_Struct.make_prefab(key, fields)
        if "box" in obj:
            return values.W_IBox(to_value(obj["box"]))
        if "number" in obj:
            return _to_num(obj["number"])
        if "path" in obj:
            return values.W_Path(obj["path"].value_string())
        if "char" in obj:
            return values.W_Character.make(unichr(int(obj["char"].value_string())))
        if "hash-keys" in obj and "hash-vals" in obj:
            return values_hash.W_EqualHashTable(
                    [to_value(i) for i in obj["hash-keys"].value_array()],
                    [to_value(i) for i in obj["hash-vals"].value_array()])
        if "regexp" in obj:
            return values_regex.W_Regexp(obj["regexp"].value_string())
        if "byte-regexp" in obj:
            return values_regex.W_ByteRegexp(obj["byte-regexp"].value_string())
        if "pregexp" in obj:
            return values_regex.W_PRegexp(obj["pregexp"].value_string())
        if "byte-regexp" in obj:
            return values_regex.W_BytePRegexp(obj["byte-pregexp"].value_string())
        if "bytes" in obj:
            return values.W_Bytes.from_string(str(obj["bytes"].value_string()))
        if "string" in obj:
            return values_string.W_String.make(str(obj["string"].value_string()))
        if "keyword" in obj:
            return values.W_Keyword.make(str(obj["keyword"].value_string()))
        if "improper" in obj:
            improper = obj["improper"].value_array()
            return values.to_improper([to_value(v) for v in improper[0].value_array()], to_value(improper[1]))
        for i in ["toplevel", "lexical", "module", "source-name"]:
            if i in obj:
                return values.W_Symbol.make(obj[i].value_string())
    if json.is_array:
        return values.to_list([to_value(j) for j in json.value_array()])
    assert 0, "Unexpected json value: %s" % json.tostring()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        print parse_module(expand_file(sys.argv[1])).tostring()
