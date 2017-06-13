#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
import os
import rpath
import sys

from rpython.rlib import streamio
from rpython.rlib.rbigint import rbigint
from rpython.rlib.objectmodel import specialize, we_are_translated
from rpython.rlib.rstring import ParseStringError, ParseStringOverflowError
from rpython.rlib.rarithmetic import string_to_int
from rpython.rlib.unroll import unrolling_iterable
from pycket import pycket_json
from pycket.error import SchemeException
from pycket.interpreter import *
from pycket import values, values_string
from pycket import values_regex
from pycket import vector
from pycket import values_struct
from pycket.hash.equal import W_EqualHashTable



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

_FN = "-l pycket/expand --"
_BE = "-l pycket/zo-expand --"

module_map = {}

current_racket_proc = None

def expand_string(s, reuse=True, srcloc=True, byte_option=False, tmp_file_name=False):
    "NON_RPYTHON"
    global current_racket_proc
    from subprocess import Popen, PIPE

    if not byte_option:
        cmd = "racket %s --loop --stdin --stdout %s" % (_FN, "" if srcloc else "--omit-srcloc")
    else:
        tmp_module = tmp_file_name + '.rkt'
        cmd = "racket -l pycket/zo-expand -- --test --stdout %s" % tmp_module

    if current_racket_proc and reuse and current_racket_proc.poll() is None:
        process = current_racket_proc
    else:
        process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE)
        if reuse:
            current_racket_proc = process
    if reuse:
        if not byte_option:
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

    cmd = "racket %s --stdout \"%s\"" % (_FN, fname)
    process = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE)
    data, err = process.communicate()
    if len(data) == 0:
        raise ExpandException("Racket did not produce output. Probably racket is not installed, or it could not parse the input.")
    if err:
        raise ExpandException("Racket produced an error")
    return data

# Call the Racket expander and read its output from STDOUT rather than producing an
# intermediate (possibly cached) file.
def expand_file_rpython(rkt_file, lib=_FN):
    from rpython.rlib.rfile import create_popen_file
    cmd = "racket %s --stdout \"%s\" 2>&1" % (lib, rkt_file)
    if not os.access(rkt_file, os.R_OK):
        raise ValueError("Cannot access file %s" % rkt_file)
    pipe = create_popen_file(cmd, "r")
    out = pipe.read()
    err = os.WEXITSTATUS(pipe.close())
    if err != 0:
        raise ExpandException("Racket produced an error and said '%s'" % out)
    return out

def expand(s, wrap=False, stdlib=False):
    data = expand_string(s)
    return pycket_json.loads(data)

def wrap_for_tempfile(func):
    def wrap(rkt_file, json_file, byte_flag=False):
        "NOT_RPYTHON"
        try:
            os.remove(json_file)
        except IOError:
            pass
        except OSError:
            pass
        from tempfile import mktemp
        json_file = os.path.abspath(json_file)
        tmp_json_file = mktemp(suffix='.json',
                               prefix=json_file[:json_file.rfind('.')])
        out = func(rkt_file, tmp_json_file, byte_flag) # this may be a problem in the future if the given func doesn't expect a third arg (byte_flag)
        assert tmp_json_file == out
        os.rename(tmp_json_file, json_file)
        return json_file

    wrap.__name__ = func.__name__
    return wrap

def expand_file_to_json(rkt_file, json_file, byte_flag=False):

    if not we_are_translated():
        return wrap_for_tempfile(_expand_file_to_json)(rkt_file, json_file, byte_flag)

    return _expand_file_to_json(rkt_file, json_file, byte_flag)

def _expand_file_to_json(rkt_file, json_file, byte_flag=False, multi_flag=False):
    lib = _BE if byte_flag else _FN

    assert not (byte_flag and multi_flag)

    dbgprint("_expand_file_to_json", "", lib=lib, filename=rkt_file)

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

    if multi_flag:
        print "Complete expansion for %s into %s" % (rkt_file, json_file)
        cmd = "racket %s --complete-expansion --output \"%s\" \"%s\" 2>&1" % (lib, json_file, rkt_file)
    else:
        if byte_flag:
            print "Transforming %s bytecode to %s" % (rkt_file, json_file)
        else:
            print "Expanding %s to %s" % (rkt_file, json_file)
            
        cmd = "racket %s --output \"%s\" \"%s\" 2>&1" % (lib, json_file, rkt_file)
        

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
    cmd = "racket %s --output \"%s\" --stdin" % (_FN, json_file)
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

def ensure_json_ast_run(file_name, byte_flag=False):
    json = _json_name(file_name)
    dbgprint("ensure_json_ast_run", json, filename=file_name)
    if needs_update(file_name, json):
        return expand_file_to_json(file_name, json, byte_flag)
    else:
        return json

def ensure_json_ast_eval(code, file_name, stdlib=True, mcons=False, wrap=True):
    json = _json_name(file_name)
    if needs_update(file_name, json):
        return expand_code_to_json(code, json, stdlib, mcons, wrap)
    else:
        return json


#### ========================== Functions for parsing json to an AST

def parse_ast(json_string):
    json = pycket_json.loads(json_string)
    modtable = ModTable()
    return to_ast(json, modtable)

def finalize_module(mod):
    from pycket.interpreter    import Context
    from pycket.assign_convert import assign_convert
    mod = Context.normalize_term(mod)
    mod = assign_convert(mod)
    mod.clean_caches()
    return mod

def parse_module(json_string, bytecode_expand=False):
    json = pycket_json.loads(json_string)
    modtable = ModTable()
    reader = JsonLoader(bytecode_expand)
    module = reader.to_module(json)
    return finalize_module(module)

#### ========================== Implementation functions

DO_DEBUG_PRINTS = False

@specialize.argtype(1)
def dbgprint(funcname, json, lib="", filename=""):
    # This helped debugging segfaults
    if DO_DEBUG_PRINTS:
        if isinstance(json, pycket_json.JsonBase):
            s = json.tostring()
        else:
            # a list
            s = "[" + ", ".join([j.tostring() for j in json]) + "]"
        print "Entering %s with: json - %s | lib - %s | filename - %s " % (funcname, s, lib, filename)

def get_lexical(x):
    assert x.is_object
    x = x.value_object()["lexical"]
    assert x.is_string
    x = x.value_string()
    return values.W_Symbol.make(x)

def to_formals(json):
    dbgprint("to_formals", json)
    make = values.W_Symbol.make
    if json.is_object:
        obj = json.value_object()
        if "improper" in obj:
            improper_arr = obj["improper"]
            regular, last = improper_arr.value_array()
            regular_symbols = [get_lexical(x) for x in regular.value_array()]
            last_symbol = get_lexical(last)
            return regular_symbols, last_symbol
        elif "lexical" in obj:
            return [], make(obj["lexical"].value_string())
    elif json.is_array:
        arr = json.value_array()
        return [get_lexical(x) for x in arr], None
    assert 0

def mksym(json):
    dbgprint("mksym", json)
    j = json.value_object()
    for i in ["toplevel", "lexical", "module"]:
        if i in j:
            return values.W_Symbol.make(j[i].value_string())
    assert 0, json.tostring()

# A table listing all the module files that have been loaded.
# A module need only be loaded once.
# Modules (aside from builtins like #%kernel) are listed in the table
# as paths to their implementing files which are assumed to be normalized.
class ModTable(object):
    _attrs_ = ["table", "current_modules"]
    _immutable_fields_ = ["table", "current_modules"]

    def __init__(self):
        self.table = {}
        self.current_modules = []

    def add_module(self, fname, module):
        self.table[fname] = module

    def push(self, fname):
        self.current_modules.append(fname)

    def pop(self):
        assert self.current_modules, "malformed JSON"
        return self.current_modules.pop()

    def current_mod(self):
        if not self.current_modules:
            return None
        return self.current_modules[-1]

    @staticmethod
    def builtin(fname):
        return fname.startswith("#%")

    def has_module(self, fname):
        return ModTable.builtin(fname) or fname in self.table

    def lookup(self, fname):
        if fname.startswith("#%"):
            return None
        return self.table.get(fname, None)

    def enter_module(self, fname):
        # Pre-emptive pushing to prevent recursive expansion due to submodules
        # which reference the enclosing module
        self.push(fname)
        self.add_module(fname, None)

    def exit_module(self, fname, module):
        self.add_module(fname, module)
        assert self.pop() == fname

def shorten_submodule_path(path):
    if path is None:
        return None
    acc = []
    for p in path:
        if p == ".":
            continue
        if p == "..":
            assert acc, "Malformed submodule path"
            acc.pop()
        else:
            acc.append(p)
    return acc[:]

class SourceInfo(object):

    _immutable_ = True

    def __init__(self, position, line, column, span, sourcefile):
        self.position = position
        self.line = line
        self.column = column
        self.span = span
        self.sourcefile = sourcefile

JSON_TYPES = unrolling_iterable(['string', 'int', 'float', 'object', 'array'])

@specialize.arg(2)
def getkey(obj, key, type, throws=False):
    result = obj.get(key, None)
    if result is None:
        if throws:
            raise KeyError
        return -1 if type == 'i' else None
    for t in JSON_TYPES:
        if type == t or type == t[0]:
            if not getattr(result, "is_" + t):
                raise ValueError("cannot decode key '%s' with value %s as type %s" %
                                 (key, result.tostring(), t))
            return getattr(result, "value_" + t)()
    assert False

def get_srcloc(o):
    position = getkey(o, "position", type='i')
    line     = getkey(o, "line", type='i')
    column   = getkey(o, "column", type='i')
    span     = getkey(o, "span", type='i')

    sourcefile = None
    source = getkey(o, "source", type='o')
    if source is not None:
        sourcefile = (getkey(source, "%p", type='s') or
                      getkey(source, "quote", type='s'))

    return SourceInfo(position, line, column, span, sourcefile)

def convert_path(path):
    return [p.value_string() for p in path]

def parse_path(p):
    assert len(p) >= 1
    arr = convert_path(p)
    srcmod, path = arr[0], arr[1:]
    # Relative module names go into the path.
    # None value for the srcmod indicate the current module
    if srcmod in (".", ".."):
        return None, arr
    if not ModTable.builtin(srcmod):
        srcmod = rpath.realpath(srcmod)
    return srcmod, path

class ModuleMap(object):

    def __init__(self, json_file_name):
        assert json_file_name is not None and json_file_name != ""
        data = readfile_rpython(rpath.realpath(os.path.abspath(json_file_name)))
        self.source_json = json_file_name
        self.mod_map = pycket_json.loads(data)
        ## TODO: validate the json

    def get_mod(self, mod_path):
        if not mod_path in self.mod_map.value_object():
            raise ValueError('Requested module - %s - is not in - %s.' %
                             (mod_path, self.source_json))

        return pycket_json.JsonObject(getkey(self.mod_map.value_object(), mod_path, type='o'))
    
class JsonLoader(object):

    _immutable_fields_ = ["modtable", "bytecode_expand", "multiple_modules"]

    def __init__(self, bytecode_expand=False, multiple_modules=False, module_mapper=None):
        self.modtable = ModTable()
        self.bytecode_expand = bytecode_expand
        self.multi_mod_flag = multiple_modules
        self.multi_mod_mapper = module_mapper

    def _lib_string(self):
        return _BE if self.bytecode_expand else _FN

    # Expand and load the module without generating intermediate JSON files.
    def expand_to_ast(self, fname):
        assert fname is not None
        fname = rpath.realpath(fname)
        data = expand_file_rpython(fname, self._lib_string())
        self.modtable.enter_module(fname)
        module = self.to_module(pycket_json.loads(data))
        module = finalize_module(module)
        self.modtable.exit_module(fname, module)
        return module

    def load_json_ast_rpython(self, modname, fname):
        assert modname is not None
        modname = rpath.realpath(modname)
        self.modtable.enter_module(modname)

        if self.multi_mod_flag:
            mod_ast = self.multi_mod_mapper.get_mod(modname)
            module = self.to_module(mod_ast)
        else:
            data = readfile_rpython(fname)
            module = self.to_module(pycket_json.loads(data))

        module = finalize_module(module)
        self.modtable.exit_module(modname, module)
        return module

    def expand_file_cached(self, rkt_file):
        dbgprint("expand_file_cached", "", lib=self._lib_string(), filename=rkt_file)
        # bypass if we already have module_map from the multi-ast-json
        if not self.multi_mod_flag:
            try:
                json_file = ensure_json_ast_run(rkt_file, self.bytecode_expand)
            except PermException:
                return self.expand_to_ast(rkt_file)
        else:
            json_file = _json_name(rkt_file)
        return self.load_json_ast_rpython(rkt_file, json_file)

    def to_bindings(self, arr):
        varss = [None] * len(arr)
        rhss  = [None] * len(arr)
        for i, v in enumerate(arr):
            varr = v.value_array()
            names, defs = varr[0].value_array(), varr[1]
            fmls = [values.W_Symbol.make(x.value_string()) for x in names]
            rhs = self.to_ast(varr[1])
            assert isinstance(rhs, AST)
            varss[i] = fmls
            rhss[i]  = rhs
        return varss, rhss

    def _to_lambda(self, lam):
        fmls, rest = to_formals(lam["lambda"])
        sourceinfo = get_srcloc(lam)
        body = [self.to_ast(x) for x in lam["body"].value_array()]
        return make_lambda(fmls, rest, body, sourceinfo)

    def _to_require(self, fname, path=None):
        path = shorten_submodule_path(path)
        modtable = self.modtable
        if modtable.builtin(fname):
            return VOID
        fname = rpath.realpath(fname)
        return Require(fname, self, path=path)

    def lazy_load(self, fname):
        modtable = self.modtable
        module = modtable.lookup(fname)
        if module is not None:
            return module
        return self.expand_file_cached(fname)

    def _parse_require(self, path):
        dbgprint("parse_require", path, self._lib_string(), path)
        fname, subs = path[0], path[1:]
        if fname in (".", ".."):
            # fname field is not used in this case, so we just give an idea of which
            # module we are in
            return Require(self.modtable.current_mod(), None, path=path)
        return self._to_require(fname, path=subs)

    def to_module(self, json):
        dbgprint("to_module", json, lib=self._lib_string(), filename="")

        # YYY
        obj = json.value_object()
        assert "body-forms" in obj, "got malformed JSON from expander"

        config = {}
        config_obj = getkey(obj, "config", type='o')
        if config_obj is not None:
            for k, v in config_obj.iteritems():
                config[k] = v.value_string()

            be_json = config.get("bytecode-expand", "false") == "true"
            if self.bytecode_expand != be_json:
                modname = getkey(obj, "module-name", type='s')
                raise ValueError('Byte-expansion is : %s, but "bytecode-expand" '
                                 'in json is : %s, in %s' %
                                 (self.bytecode_expand, be_json, modname))

        try:
            lang_arr = obj["language"].value_array()
        except KeyError:
            lang = None
        else:
            lang = self._parse_require([lang_arr[0].value_string()]) if lang_arr else None

        body = [self.to_ast(x) for x in getkey(obj, "body-forms", type='a')]
        name = getkey(obj, "module-name", type='s')
        return Module(name, body, config, lang=lang)

    @staticmethod
    def is_builtin_operation(rator):
        """ Sanity check for testing """
        if we_are_translated():
            return True
        if "source-name" not in rator:
            return False
        if "source-module" not in rator:
            return True
        if rator["source-module"].value_string() == "%kernel":
            return True
        return False

    def parse_if(self, cond, then, els):
        """ Avoid building branch of an if-expression when cond is a constant """
        cond = self.to_ast(cond)
        if not isinstance(cond, Quote):
            then = self.to_ast(then)
            els  = self.to_ast(els)
            return If(cond, then, els)
        branch = els if cond.w_val is values.w_false else then
        return self.to_ast(branch)

    def to_ast(self, json, get_req_mods_from_module_map=False):
        dbgprint("to_ast", json, lib=self._lib_string(), filename="")
        mksym = values.W_Symbol.make

        if json.is_array:
            arr = json.value_array()
            rator = arr[0].value_object()
            assert JsonLoader.is_builtin_operation(rator)
            ast_elem = rator["source-name"].value_string()
            if ast_elem == "begin":
                return Begin([self.to_ast(arr[i]) for i in range(1, len(arr))])
            if ast_elem == "#%expression":
                return self.to_ast(arr[1])
            if ast_elem == "set!":
                target = arr[1].value_object()
                var = None
                if "source-name" in target:
                    srcname = mksym(target["source-name"].value_string())
                    if "source-module" in target:
                        if target["source-module"].is_array:
                            path_arr = target["source-module"].value_array()
                            srcmod, path = parse_path(path_arr)
                        else:
                            srcmod = path = None
                    else:
                        srcmod = "#%kernel"
                        path   = None

                    modname = mksym(target["module"].value_string()) if "module" in target else srcname
                    var = ModuleVar(modname, srcmod, srcname, path)
                elif "lexical" in target:
                    var = CellRef(values.W_Symbol.make(target["lexical"].value_string()))
                else:
                    assert "toplevel" in target
                    var = ToplevelVar(mksym(target["toplevel"].value_string()))
                return SetBang(var, self.to_ast(arr[2]))
            if ast_elem == "#%top":
                assert 0
                return CellRef(mksym(arr[1].value_object()["symbol"].value_string()))
            # The parser now ignores `#%require` AST nodes.
            # The actual file to include is now generated by expander
            # as an object that is handled below.
            if ast_elem == "#%provide":
                return VOID
            assert 0, "Unexpected ast-element element: %s" % json.tostring()
        if json.is_object:
            obj = json.value_object()
            if "require" in obj:
                paths = obj["require"].value_array()
                requires = []
                for path in paths:
                    path = convert_path(path.value_array())
                    if not path:
                        continue
                    requires.append(self._parse_require(path))
                return Begin.make(requires) if requires else VOID
            if "begin0" in obj:
                fst = self.to_ast(obj["begin0"])
                rst = [self.to_ast(x) for x in obj["begin0-rest"].value_array()]
                if len(rst) == 0:
                    return fst
                else:
                    return Begin0.make(fst, rst)
            if "begin-for-syntax" in obj:
                body = [self.to_ast(x) for x in obj["begin-for-syntax"].value_array()]
                return BeginForSyntax(body)
            if "wcm-key" in obj:
                return WithContinuationMark(self.to_ast(obj["wcm-key"]),
                                            self.to_ast(obj["wcm-val"]),
                                            self.to_ast(obj["wcm-body"]))
            if "define-values" in obj:
                binders = obj["define-values"].value_array()
                display_names = obj["define-values-names"].value_array()
                fmls = [mksym(x.value_string()) for x in binders]
                disp_syms = [mksym(x.value_string()) for x in display_names]
                body = self.to_ast(obj["define-values-body"])
                return DefineValues(fmls, body, disp_syms)
            if "letrec-bindings" in obj:
                body = [self.to_ast(x) for x in obj["letrec-body"].value_array()]
                bindings = obj["letrec-bindings"].value_array()
                if len(bindings) == 0:
                    return Begin.make(body)
                else:
                    vs, rhss = self.to_bindings(bindings)
                    return make_letrec(list(vs), list(rhss), body)
            if "let-bindings" in obj:
                body = [self.to_ast(x) for x in obj["let-body"].value_array()]
                bindings = obj["let-bindings"].value_array()
                if len(bindings) == 0:
                    return Begin.make(body)
                else:
                    vs, rhss = self.to_bindings(bindings)
                    return make_let(vs, rhss, body)
            if "variable-reference" in obj:
                current_mod = self.modtable.current_mod()
                if obj["variable-reference"].is_bool: # assumes that only boolean here is #f
                    return VariableReference(None, current_mod)
                else:
                    var = self.to_ast(obj["variable-reference"])
                    return VariableReference(var, current_mod)
            if "lambda" in obj:
                    return CaseLambda([self._to_lambda(obj)])
            if "case-lambda" in obj:
                    lams = [self._to_lambda(v.value_object()) for v in obj["case-lambda"].value_array()]
                    return CaseLambda(lams)
            if "operator" in obj:
                rator = self.to_ast(obj["operator"])
                rands = [self.to_ast(x) for x in obj["operands"].value_array()]
                return App.make(rator, rands)
            if "test" in obj:
                return self.parse_if(obj["test"], obj["then"], obj["else"])
            if "quote" in obj:
                return Quote(to_value(obj["quote"]))
            if "quote-syntax" in obj:
                return QuoteSyntax(to_value(obj["quote-syntax"]))
            if "source-name" in obj:
                srcname = obj["source-name"].value_string()
                modname = obj["module"].value_string() if "module" in obj else None
                srcsym = mksym(srcname)
                modsym = mksym(modname) if modname else srcsym
                if "source-linklet" in obj:
                    source = obj["source-linklet"].value_object()["quote"].value_object()
                    if "toplevel" in source and source["toplevel"].value_string() == "self":
                        instance_number = -1
                    else:
                        instance_number = source_["number"].value_object()["integer"].value_string()
                        instance_number = string_to_int(instance_number)
                    return LinkletVar(mksym(srcname),instance_number)
                elif "source-module" in obj:
                    if obj["source-module"].is_array:
                        path_arr = obj["source-module"].value_array()
                        srcmod, path = parse_path(path_arr)
                    else:
                        srcmod = path = None
                else:
                    srcmod = "#%kernel"
                    path   = None
                return ModuleVar(modsym, srcmod, srcsym, path=path)
            if "lexical" in obj:
                return LexicalVar(mksym(obj["lexical"].value_string()))
            if "toplevel" in obj:
                return ToplevelVar(mksym(obj["toplevel"].value_string()))
            if "module-name" in obj:
                return self.to_module(json)
        assert 0, "Unexpected json object: %s" % json.tostring()

VOID = Quote(values.w_void)

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
            return values.W_Flonum.INF
        if rs == "-inf.0":
            return values.W_Flonum.NEGINF
        if rs == "+nan.0":
            return values.W_Flonum.NAN
    if "integer" in obj:
        rs = obj["integer"].value_string()
        try:
            return values.W_Fixnum.make(string_to_int(rs))
        except ParseStringOverflowError:
            val = rbigint.fromdecimalstr(rs)
            return values.W_Bignum(val)
    assert False

def decode_byte_array(arr):
    return [chr(i.value_int()) for i in arr.value_array()]

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
            return W_EqualHashTable(
                    [to_value(i) for i in obj["hash-keys"].value_array()],
                    [to_value(i) for i in obj["hash-vals"].value_array()],
                    immutable=True)
        if "regexp" in obj:
            return values_regex.W_Regexp(obj["regexp"].value_string())
        if "byte-regexp" in obj:
            arr = decode_byte_array(obj["byte-regexp"])
            return values_regex.W_ByteRegexp("".join(arr))
        if "pregexp" in obj:
            return values_regex.W_PRegexp(obj["pregexp"].value_string())
        if "byte-pregexp" in obj:
            arr = decode_byte_array(obj["byte-pregexp"])
            return values_regex.W_BytePRegexp("".join(arr))
        if "bytes" in obj:
            arr = decode_byte_array(obj["bytes"])
            return values.W_ImmutableBytes(arr)
        if "string" in obj:
            return values_string.W_String.make(str(obj["string"].value_string()))
        if "keyword" in obj:
            return values.W_Keyword.make(str(obj["keyword"].value_string()))
        if "improper" in obj:
            improper = obj["improper"].value_array()
            return values.to_improper([to_value(v) for v in improper[0].value_array()], to_value(improper[1]))
        if "void" in obj:
            return values.w_void
        for i in ["lexical", "module", "source-name", "toplevel"]:
            if i in obj:
                return values.W_Symbol.make(obj[i].value_string())
    if json.is_array:
        return values.to_list([to_value(j) for j in json.value_array()])
    assert 0, "Unexpected json value: %s" % json.tostring()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        print parse_module(expand_file(sys.argv[1])).tostring()
