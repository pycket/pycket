#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Assorted test helpers
#
import os
from tempfile import NamedTemporaryFile, gettempdir
from pycket.expand import expand, JsonLoader, expand_string, ModTable, parse_module
from pycket.pycket_json import loads
from pycket.interpreter import *
from pycket.env import ToplevelEnv
from pycket import values
from pycket.prims.linklet import *
from pycket.cont import continuation
from pycket.racket_entry import initiate_boot_sequence, namespace_require_kernel, read_eval_print_string, get_primitive
from pycket.values import to_list, w_false
from pycket.config import get_testing_config

import pytest
from pycket.linklet_test.utils import *
#
# basic runners
#

instantiate_linklet = get_primitive("instantiate-linklet")

# This is where all the work happens

if pytest.config.load_expander:
    # get the expander
    print("Loading and initializing the expander")
    initiate_boot_sequence(None, [])
    # load the '#%kernel
    print("(namespace-require '#%%kernel)")
    namespace_require_kernel(None)

delete_temp_files = True

def get_var_val(inst, id_str):
    # for getting uninterned symbols
    for k,v in inst.vars.iteritems():
        if id_str == k.tostring():
            return k, v.get_value_direct()
    raise Exception("Can't find the variable : %s in instance : %s" % (id_str, inst.tostring()))

def variables(inst):
    return get_instance_variable_names(inst) # W_Cons

def defines(inst, name_str):
    return inst.has_var(W_Symbol.make(name_str))

def get_val(inst, name_str):
    return inst.lookup_var_value(values.W_Symbol.make(name_str))

def check_val(inst, var_str, val):
    return get_val(inst, var_str).value == val

def inst(linkl, imports=[], target=None):
    if not target:
        target = w_false

    return instantiate_linklet.call_interpret([linkl, to_list(imports), target, w_false], get_testing_config())

def eval(linkl, target, imports=[], just_return=False):

    #result = linkl.instantiate(imports, None, target=target)
    result = instantiate_linklet.call_interpret([linkl, to_list(imports), target, w_false], get_testing_config())

    if just_return:
        return result, None

    if isinstance(result, values.W_Fixnum) or isinstance(result, values.W_Flonum):
        result = result.value
    elif isinstance(result, values_string.W_String):
        result = result.as_str_utf8()
    return result, target

def empty_target():
    # creates an empty target
    return make_instance("(linklet () ())")

def make_instance(linkl_str, imports=[]):
    instance = inst(make_linklet(linkl_str), imports)
    return instance

def make_linklet(linkl_str):
    #"(linklet () (x) (define-values (x) 4))"
    linkl_sexp = string_to_sexp(linkl_str)
    try:
        do_compile_linklet(linkl_sexp, values.W_Symbol.make("test_linklet_sexp"), w_false, w_false, w_false, ToplevelEnv(), NilCont())
    except Done, e:
        l = e.values # W_Linklet
        return l
    raise Exception("do_compile_linklet didn't raised a Done exception")

def run_linklet(w_linkl, v=None):
    # FIXME instantiate
    ov = w_linkl.instantiate([], None, prompt=False)
    assert isinstance(ov, values.W_Number) # FIXME: test for different types of results
    if v:
        assert ov.value == v
    #debug_out("\nHEY\n")
    return ov.value

# def run_ast(ast, v=None):
#     l = W_Linklet("test_linklet_ast", [], [], {}, [ast])
#     return run_linklet(l, v)

# use_expander is to request it to use expander
# it will just return True without running if the test is set to not use the expander
def run_expr(expr_str, v=None, use_expander=False, just_return=False):
    if pytest.config.use_expander:
        return run_string(expr_str, v, just_return)
    else:
        return run_sexp(expr_str, v, just_return)

def run_sexp(body_sexp_str, v=None, just_return=False):
    linkl_str = "(linklet () () %s)" % body_sexp_str
    l = make_linklet(linkl_str)
    result, _ = eval(l, empty_target(), just_return=just_return)

    if v and not just_return:
        assert result == v
    return result

def run_string(expr_str, v=None, just_return=False):
    ov = read_eval_print_string(expr_str, None, return_val=True)
    # FIXME: check for multiple results
    assert isinstance(ov, values.W_Object)
    if just_return:
        return ov
    # FIXME : unify this and the one in eval
    if isinstance(result, values.W_Fixnum) or isinstance(result, values.W_Flonum):
        result = result.value
    elif isinstance(result, values_string.W_String):
        result = result.as_str_utf8()

    if v:
        assert result == v
    return result

def execute(p, stdlib=False, extra=""):
    return run_expr(p,just_return=True)

#
# Combined checking
#
def check_all(*snippets_returning_true, **kwargs):
    code = []
    tail = []
    for i, snippet in enumerate(snippets_returning_true):
        code.append("  " * i + "(if %s" % snippet)
        tail.append("  " * (i + 1) + "%s)" % i)
    code.append("  " * (i + 1) + "#t")
    code = "\n".join(code) + "\n" + "\n".join(reversed(tail))
    print code
    res = execute(code, extra=kwargs.get("extra", ""))
    if res is not values.w_true:
        assert 0, "%s returned a non-true value" % snippets_returning_true[res.value]

def check_none(*snippets_returning_false, **kwargs):
    code = []
    tail = []
    for i, snippet in enumerate(snippets_returning_false):
        code.append("  " * i + "(if %s %s" % (snippet, i))
        tail.append("  " * (i + 1) + ")")
    code.append("  " * (i + 1) + "#t")
    code = "\n".join(code) + "\n" + "\n".join(reversed(tail))
    print code
    res = execute(code, extra=kwargs.get("extra", ""))
    if res is not values.w_false:
        assert 0, "%s returned a true value" % snippets_returning_false[res.value]



"""
--- OLD TEST HELPERS BELOW
"""

# def run_ast(ast):
#     env = ToplevelEnv()
#     env.globalconfig.load(ast)
#     mod = interpret_module(ast, env)
#     return mod

def expand_from_bytecode(m, srcloc):
    with NamedTemporaryFile(delete=delete_temp_files) as f:
        f.write(m.encode("utf-8"))
        f.seek(0)
        os.rename(f.name, f.name+'.rkt')
        s = expand_string(m.encode("utf-8"), reuse=False, srcloc=srcloc, byte_option=True, tmp_file_name=f.name)
        os.rename(f.name+'.rkt', f.name) # for automatic deletion to find it

        if delete_temp_files: # removing the compiled/*.dep && *.zo as well
            subs = f.name.split("/")
            parent = subs[:-1]
            parent.append('compiled')
            file_name = subs[-1]
            byteCodesPrefix = "/".join(parent) + "/" + file_name + "_rkt"
            os.remove(byteCodesPrefix + ".zo")
            os.remove(byteCodesPrefix + ".dep")

    return s

# def run_mod(m, stdlib=False, srcloc=True):
#     assert not stdlib

#     if not pytest.config.byte_option:
#         ast = parse_module(expand_string(m, srcloc=srcloc))
#     else:
#         ast = parse_module(expand_from_bytecode(m, srcloc), bytecode_expand=True)

#     return run_ast(ast)

def format_pycket_mod(s, stdlib=False, extra=""):
    # pycket handles the stdlib, various requires
    str = "#lang pycket%s\n%s\n%s"%(" #:stdlib" if stdlib else "", extra, s)
    return str

def run_mod_defs(m, extra="",stdlib=False, srcloc=True):
    str = format_pycket_mod(m, extra=extra, stdlib=stdlib)
    mod = run_mod(str, srcloc=srcloc)
    return mod

def run_mod_expr(e, v=None, stdlib=False, wrap=False, extra="", srcloc=False):
    # this (let () e) wrapping is needed if e is `(begin (define x 1) x)`, for example
    # FIXME: this should get moved into a language
    expr = "(let () %s)"%e if wrap else e
    defn = "(define #%%pycket-expr %s)"%expr
    mod = run_mod_defs(defn, stdlib=stdlib, extra=extra, srcloc=srcloc)
    ov = mod.defs[values.W_Symbol.make("#%pycket-expr")]
    if v:
        assert ov.equal(v)
    return ov

def run_fix(p, v=None, stdlib=False, extra=""):
    ov = run_mod_expr(p,stdlib=stdlib, extra=extra)
    assert isinstance(ov, values.W_Fixnum)
    if v is not None:
        assert ov.value == v
    return ov.value

def run_flo(p, v=None, stdlib=False, extra=""):
    ov = run_mod_expr(p,stdlib=stdlib, extra=extra)
    assert isinstance(ov, values.W_Flonum)
    if v is not None:
        assert ov.value == v
    return ov.value

def run(p, v=None, stdlib=False, extra=""):
    return run_mod_expr(p,v=v,stdlib=stdlib, extra=extra)

def run_top(p, v=None, stdlib=False, extra=""):
    return run_mod_expr(p,v=v,stdlib=stdlib, wrap=True, extra=extra)

def run_values(p, stdlib=False):
    e = "(call-with-values (lambda () %s) list)"%p
    v = run_mod_expr(e, stdlib=stdlib)
    return values.from_list(v)

def run_std(c, v):
    return run_top(c, v, stdlib=True)

def run_file(fname, *replacements, **kwargs):
    modname = os.path.join(os.path.dirname(__file__), fname)
    ast = parse_file(fname, *replacements, **kwargs)
    env = ToplevelEnv()
    env.globalconfig.load(ast)
    env.module_env.add_module(modname, ast)
    return interpret_module(ast, env)

def parse_file(fname, *replacements, **kwargs):
    abspath = kwargs.get("abspath", False)
    if not abspath:
        fname = os.path.join(os.path.dirname(__file__), fname)

    if kwargs.get("inplace", False):
        assert not replacements
        JsonLoader(bytecode_expand=False)
        # if not pytest.config.byte_option:
        #     reader = JsonLoader(bytecode_expand=False)
        # else:
        #     reader = JsonLoader(bytecode_expand=True)
        ast = reader.expand_to_ast(fname)
        return ast

    with file(fname) as f:
        s = f.read()
    for replace, with_ in replacements:
        assert s.count(replace) == 1
        s = s.replace(replace, with_)
    s = s.decode("utf-8")

    # if not pytest.config.byte_option:
    #     s = expand_string(s)
    #     ast = parse_module(s)
    # else:
    #     s = expand_from_bytecode(s, True)
    #     ast = parse_module(s, bytecode_expand=True)
    s = expand_string(s)
    ast = parse_module(s)

    return ast

def check_equal(*pairs_of_equal_stuff, **kwargs):
    code = []
    tail = []
    assert len(pairs_of_equal_stuff) % 2 == 0
    ind = 0
    for i in range(len(pairs_of_equal_stuff) // 2):
        a = pairs_of_equal_stuff[i * 2]
        b = pairs_of_equal_stuff[i * 2 + 1]
        ind += 1
        if isinstance(a, list):
            code.append("  " * ind + "(let ()")
            ind += 1
            code.extend(["  " * ind + x for x in a[:-1]])
            code.append( "  " * ind + ("(if (equal? %s %s)" % (a[-1], b)))
            ind += 1
            tail.append("  " * ind + "%s))" % i)
        else:
            snippet = "(equal? %s %s)" % (a, b)
            code.append("  " * ind + "(if %s" % snippet)
            tail.append("  " * (ind + 1) + "%s)" % i)
    code.append("  " * (ind + 1) + "#t")
    code = "\n".join(code) + "\n" + "\n".join(reversed(tail))
    try:
        print code
    except UnicodeEncodeError:
        print code.encode("ascii", 'replace')

    extra = kwargs.get("extra", "")
    res = execute(code, extra=extra)
    if res is not values.w_true:
        src = pairs_of_equal_stuff[res.value * 2]
        if isinstance(src, list):
            src = "(let ()\n" + "\n".join(src) + ")"
        wrong = execute(src, extra=extra)
        assert 0, u"%s is %s, which is different from %s" % (
            pairs_of_equal_stuff[res.value * 2], wrong.tostring(),
            pairs_of_equal_stuff[res.value * 2 + 1])
