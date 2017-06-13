
from pycket.interpreter import (
    App,
    Begin,
    Begin0,
    BeginForSyntax,
    CaseLambda,
    Cell,
    CellRef,
    DefineValues,
    If,
    Lambda,
    Let,
    Letrec,
    LexicalVar,
    Module,
    ModuleVar,
    LinkletVar,
    Quote,
    QuoteSyntax,
    Require,
    SetBang,
    ToplevelVar,
    VariableReference,
    WithContinuationMark,
    make_let,
    make_letrec,
)

from rpython.rlib.objectmodel import specialize

class ASTVisitor(object):
    """
    An abstract visitor class for the AST classes defined below.
    A subclass need only define handler functions for the relevant portions
    of the AST, as the default implementations in this class pass along the
    relevant data.
    """

    @specialize.argtype(0)
    def visit_cell(self, ast, *args):
        assert isinstance(ast, Cell)
        expr = ast.expr.visit(self, *args)
        return Cell(expr, need_cell_flags=ast.need_cell_flags)

    @specialize.argtype(0)
    def visit_quote(self, ast, *args):
        assert isinstance(ast, Quote)
        return ast

    @specialize.argtype(0)
    def visit_quote_syntax(self, ast, *args):
        assert isinstance(ast, QuoteSyntax)
        return ast

    @specialize.argtype(0)
    def visit_variable_reference(self, ast, *args):
        assert isinstance(ast, VariableReference)
        return ast

    @specialize.argtype(0)
    def visit_with_continuation_mark(self, ast, *args):
        assert isinstance(ast, WithContinuationMark)
        key   = ast.key.visit(self, *args)
        value = ast.value.visit(self, *args)
        body  = ast.body.visit(self, *args)
        return WithContinuationMark(key, value, body)

    @specialize.argtype(0)
    def visit_app(self, ast, *args):
        assert isinstance(ast, App)
        rator = ast.rator.visit(self, *args)
        rands = [a.visit(self, *args) for a in ast.rands]
        return App.make(rator, rands, ast.env_structure)

    @specialize.argtype(0)
    def visit_begin0(self, ast, *args):
        assert isinstance(ast, Begin0)
        first = ast.first.visit(self, *args)
        body  = [b.visit(self, *args) for b in ast.body]
        return Begin0.make(first, body)

    @specialize.argtype(0)
    def visit_begin(self, ast, *args):
        assert isinstance(ast, Begin)
        body = [b.visit(self, *args) for b in ast.body]
        return Begin.make(body)

    @specialize.argtype(0)
    def visit_begin_for_syntax(self, ast, *args):
        assert isinstance(ast, BeginForSyntax)
        return ast

    @specialize.argtype(0)
    def visit_cell_ref(self, ast, *args):
        assert isinstance(ast, CellRef)
        return ast

    @specialize.argtype(0)
    def visit_lexical_var(self, ast, *args):
        assert isinstance(ast, LexicalVar)
        return ast

    @specialize.argtype(0)
    def visit_module_var(self, ast, *args):
        assert isinstance(ast, ModuleVar)
        return ast

    @specialize.argtype(0)
    def visit_linklet_var(self, ast, *args):
        assert isinstance(ast, LinkletVar)
        return ast


    @specialize.argtype(0)
    def visit_toplevel_var(self, ast, *args):
        assert isinstance(ast, ToplevelVar)
        return ast

    @specialize.argtype(0)
    def visit_set_bang(self, ast, *args):
        assert isinstance(ast, SetBang)
        var = ast.var.visit(self, *args)
        rhs = ast.rhs.visit(self, *args)
        return SetBang(var, rhs)

    @specialize.argtype(0)
    def visit_if(self, ast, *args):
        assert isinstance(ast, If)
        tst = ast.tst.visit(self, *args)
        thn = ast.thn.visit(self, *args)
        els = ast.els.visit(self, *args)
        return If.make(tst, thn, els)

    @specialize.argtype(0)
    def visit_case_lambda(self, ast, *args):
        assert isinstance(ast, CaseLambda)
        lams = [l.visit(self, *args) for l in ast.lams]
        return CaseLambda(lams, recursive_sym=ast.recursive_sym, arity=ast._arity)

    @specialize.argtype(0)
    def visit_lambda(self, ast, *args):
        from pycket.interpreter import make_lambda
        assert isinstance(ast, Lambda)
        body = [b.visit(self, *args) for b in ast.body]
        return make_lambda(ast.formals, ast.rest, body, sourceinfo=ast.sourceinfo)

    @specialize.argtype(0)
    def visit_letrec(self, ast, *args):
        assert isinstance(ast, Letrec)
        rhss = [r.visit(self, *args) for r in ast.rhss]
        body = [b.visit(self, *args) for b in ast.body]
        vars = ast._rebuild_args()
        return make_letrec(vars, rhss, body)

    @specialize.argtype(0)
    def visit_let(self, ast, *args):
        assert isinstance(ast, Let)
        rhss = [r.visit(self, *args) for r in ast.rhss]
        body = [b.visit(self, *args) for b in ast.body]
        vars = ast._rebuild_args()
        return make_let(vars, rhss, body)

    @specialize.argtype(0)
    def visit_define_values(self, ast, *args):
        assert isinstance(ast, DefineValues)
        rhs = ast.rhs.visit(self, *args)
        return DefineValues(ast.names, rhs, ast.display_names)

    @specialize.argtype(0)
    def visit_module(self, ast, *args):
        """ Must not produce a new module AST """
        assert isinstance(ast, Module)
        for i, b in enumerate(ast.body):
            ast.body[i] = b.visit(self, *args)
        for i, r in enumerate(ast.requires):
            ast.requires[i] = r.visit(self, *args)
        return ast

    @specialize.argtype(0)
    def visit_require(self, ast, *args):
        assert isinstance(ast, Require)
        return ast

class CopyVisitor(ASTVisitor):

    def visit_variable_reference(self, ast):
        assert isinstance(ast, VariableReference)
        return VariableReference(ast.var, ast.path, ast.is_mut)

    def visit_quote(self, ast):
        assert isinstance(ast, Quote)
        return Quote(ast.w_val)

    def visit_lexical_var(self, ast):
        assert isinstance(ast, LexicalVar)
        return LexicalVar(ast.sym, ast.env_structure)

    def visit_module_var(self, ast):
        assert isinstance(ast, ModuleVar)
        var = ModuleVar(ast.sym, ast.srcmod, ast.srcsym, ast.path)
        var.modenv = ast.modenv
        var.w_value = ast.w_value
        return var

    def visit_cell_ref(self, ast):
        assert isinstance(ast, CellRef)
        return CellRef(ast.sym, ast.env_structure)

    def visit_let(self, ast):
        assert isinstance(ast, Let)
        body = [b.visit(self) for b in ast.body]
        rhss = [r.visit(self) for r in ast.rhss]
        result = Let(ast.args,
                     ast.counts,
                     rhss,
                     body,
                     ast.remove_num_envs)
        result.copy_body_pruning(ast)
        return result

    def visit_letrec(self, ast):
        assert isinstance(ast, Letrec)
        body = [b.visit(self) for b in ast.body]
        rhss = [r.visit(self) for r in ast.rhss]
        result = Letrec(ast.args,
                        ast.counts,
                        rhss,
                        body)
        result.copy_body_pruning(ast)
        return result

    def visit_begin(self, ast):
        assert isinstance(ast, Begin)
        body = [b.visit(self) for b in ast.body]
        result = Begin(body)
        result.copy_body_pruning(ast)
        return result

    def visit_begin0(self, ast):
        assert isinstance(ast, Begin0)
        fst = ast.first.visit(self)
        rst = [r.visit(self) for r in ast.body]
        result = Begin0(fst, rst)
        result.copy_body_pruning(ast)
        return result

def copy_ast(ast):
    visitor = CopyVisitor()
    return ast.visit(visitor)

