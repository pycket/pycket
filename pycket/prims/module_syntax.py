#! /usr/bin/env python
# -*- coding: utf-8 -*-
from pycket              import values
from pycket.error        import SchemeException
from pycket.prims.expose import unsafe, default, expose, expose_val, procedure, make_call_method
from rpython.rlib        import jit



class SyntaxInfo(object):
    info_map = {}
    _immutable_fields_ = ['line', 'column', 'position', 'span',
                          'original', 'w_source', 'w_source_module']
    def __init__(self, line, column, position, span,
                 original, w_source, w_source_module):
        self.line = line
        self.column = column
        self.position = position
        self.span = span
        self.original = original
        self.w_source = w_source
        self.w_source_module = w_source_module

    @staticmethod
    @jit.elidable
    def get(obj):
        if obj in SyntaxInfo.info_map:
            return SyntaxInfo.info_map[obj]
        else:
            return None
    @staticmethod
    def set(obj, info):
        SyntaxInfo.info_map[obj] = info


@expose("syntax-original?", [values.W_Syntax])
def syntax_original(v):
    return values.w_false

@expose("syntax-tainted?", [values.W_Syntax])
def syntax_tainted(v):
    return values.w_false

@expose("syntax->datum", [values.W_Syntax])
def syntax_to_datum(v):
    return v.val


@expose("syntax-source-module",
        [values.W_Syntax, default(values.W_Bool, values.w_false)])
def syntax_source_module(w_stx, source):
    ast = w_stx.ast
    stx_info = SyntaxInfo.get(ast)
    if stx_info is not None:
        return stx_info.w_source_module
    else:
        return values.w_false


@expose("compiled-module-expression?", [values.W_Object])
def compiled_module_expression(v):
    return values.w_false

@expose("module-path-index-join", [values.W_Object, values.W_Object])
def mpi_join(a, b):
    #
    assert 0
    return values.W_ModulePathIndex()

@expose("module-path-index-resolve", [values.W_ModulePathIndex])
def mpi_resolve(w_mpi):
    if w_mpi.resolved is not None:
        return w_mpi.resolved
    else:
        return values.w_false

@expose("make-resolved-module-path", [values.W_Object])
def rmp_make(path):
    if isinstance(path, values.W_Path):
        return values.W_ResolvedModulePath.get(None, path, None)
    elif isinstance(path, values.W_Symbol):
        return values.W_ResolvedModulePath.get(path, None, None)
    # elif isinstance(path, values.W_List):
    #     return values.W_ResolvedModulePath.get(None, None, path)
    else:
        raise SchemeException("unknown path")

@expose("variable-reference-constant?", [values.W_VariableReference], simple=False)
def varref_const(varref, env, cont):
    from pycket.interpreter import return_value
    return return_value(values.W_Bool.make(not(varref.varref.is_mutable(env))), env, cont)

@expose("variable-reference->resolved-module-path",  [values.W_VariableReference])
def varref_rmp(varref):
    return values.W_ResolvedModulePath.get(None, values.W_Path(varref.varref.path), None)

@expose("variable-reference->module-source",  [values.W_VariableReference])
def varref_ms(varref):
    # FIXME: not implemented
    return values.W_Symbol.make("dummy_module")

@expose("resolved-module-path-name", [values.W_ResolvedModulePath])
def rmp_name(rmp):
    return rmp.resolved_module_path_name()

@expose("module-path?", [values.W_Object])
def module_pathp(v):
    if isinstance(v, values.W_Symbol):
        # FIXME: not always right
        return values.w_true
    if isinstance(v, values.W_Path):
        return values.w_true
    # FIXME
    return values.w_false
