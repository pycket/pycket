
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
    Quote,
    QuoteSyntax,
    Require,
    SetBang,
    ToplevelVar,
    Var,
    VariableReference,
    WithContinuationMark,
    make_lambda,
    make_let,
    make_letrec,
)

class ASTVisitor(object):
    """
    An abstract visitor class for the AST classes defined below.
    A subclass need only define handler functions for the relevant portions
    of the AST, as the default implementations in this class pass along the
    relevant data.

    If visit_inplace == True then the stubbed visitors will return the original
    AST node rather than constructing a new one.
    This is faster if we just want to traverse and annotate the AST.
    """

    preserve_mutated_vars = False
    preserve_free_vars = False
    visit_inplace = False

    def visit_cell(self, ast, *args):
        assert isinstance(ast, Cell)
        if self.visit_inplace:
            return ast
        else:
            expr = ast.expr.visit(self, *args)
            return Cell(expr, need_cell_flags=ast.need_cell_flags)

    def visit_quote(self, ast, *args):
        assert isinstance(ast, Quote)
        return ast

    def visit_quote_syntax(self, ast, *args):
        assert isinstance(ast, QuoteSyntax)
        return ast

    def visit_variable_reference(self, ast, *args):
        assert isinstance(ast, VariableReference)
        return ast

    def visit_with_continuation_mark(self, ast, *args):
        assert isinstance(ast, WithContinuationMark)
        key   = ast.key.visit(self, *args)
        value = ast.value.visit(self, *args)
        body  = ast.body.visit(self, *args)
        if self.visit_inplace:
            return ast
        else:
            return WithContinuationMark(key, value, body)

    def visit_app(self, ast, *args):
        assert isinstance(ast, App)
        rator = ast.rator.visit(self, *args)
        if self.visit_inplace:
            for a in ast.rands:
                a.visit(self, *args)
            return ast
        else:
            rands = [a.visit(self, *args) for a in ast.rands]
            return App.make(rator, rands, ast.env_structure)

    def visit_begin0(self, ast, *args):
        assert isinstance(ast, Begin0)
        first = ast.first.visit(self, *args)
        body  = ast.body.visit(self, *args)
        if self.visit_inplace:
            return ast
        else:
            return Begin0.make(first, [body])

    def visit_begin(self, ast, *args):
        assert isinstance(ast, Begin)
        if self.visit_inplace:
            for b in ast.body:
                b.visit(self, *args)
            return ast
        else:
            body = [b.visit(self, *args) for b in ast.body]
            return Begin.make(body)

    def visit_begin_for_syntax(self, ast, *args):
        assert isinstance(ast, BeginForSyntax)
        return ast

    def visit_cell_ref(self, ast, *args):
        assert isinstance(ast, CellRef)
        return ast

    def visit_lexical_var(self, ast, *args):
        assert isinstance(ast, LexicalVar)
        return ast

    def visit_module_var(self, ast, *args):
        assert isinstance(ast, ModuleVar)
        return ast

    def visit_toplevel_var(self, ast, *args):
        assert isinstance(ast, ToplevelVar)
        return ast

    def visit_set_bang(self, ast, *args):
        assert isinstance(ast, SetBang)
        var = ast.var.visit(self, *args)
        rhs = ast.rhs.visit(self, *args)
        assert isinstance(var, Var)
        if self.visit_inplace:
            return ast
        else:
            return SetBang(var, rhs)

    def visit_if(self, ast, *args):
        assert isinstance(ast, If)
        tst = ast.tst.visit(self, *args)
        thn = ast.thn.visit(self, *args)
        els = ast.els.visit(self, *args)
        if self.visit_inplace:
            return ast
        else:
            return If.make(tst, thn, els)

    def visit_case_lambda(self, ast, *args):
        assert isinstance(ast, CaseLambda)
        if self.visit_inplace:
            for lam in ast.lams:
                lam.visit(self, *args)
            return ast
        else:
            lams = [lam.visit(self, *args) for lam in ast.lams]
            return CaseLambda(lams, recursive_sym=ast.recursive_sym, arity=ast._arity)

    def visit_lambda(self, ast, *args):
        assert isinstance(ast, Lambda)
        if self.visit_inplace:
            for b in ast.body:
                b.visit(self, *args)
            return ast
        else:
            body = [b.visit(self, *args) for b in ast.body]
            return make_lambda(ast.formals, ast.rest, body, sourceinfo=ast.sourceinfo)

    def visit_letrec(self, ast, *args):
        assert isinstance(ast, Letrec)
        if self.visit_inplace:
            for rhs in ast.rhss:
                rhs.visit(self, *args)
            for b in ast.body:
                b.visit(self, *args)
            return ast
        else:
            rhss = [r.visit(self, *args) for r in ast.rhss]
            body = [b.visit(self, *args) for b in ast.body]
            vars = ast._rebuild_args()
            return make_letrec(vars, rhss, body)

    def visit_let(self, ast, *args):
        assert isinstance(ast, Let)
        if self.visit_inplace:
            for rhs in ast.rhss:
                rhs.visit(self, *args)
            for b in ast.body:
                b.visit(self, *args)
            return ast
        else:
            rhss = [r.visit(self, *args) for r in ast.rhss]
            body = [b.visit(self, *args) for b in ast.body]
            vars = ast._rebuild_args()
            return make_let(vars, rhss, body)

    def visit_define_values(self, ast, *args):
        assert isinstance(ast, DefineValues)
        rhs = ast.rhs.visit(self, *args)
        if self.visit_inplace:
            return ast
        else:
            return DefineValues(ast.names, rhs, ast.display_names)

    def visit_module(self, ast, *args):
        """ Must not produce a new module AST """
        assert isinstance(ast, Module)
        for i, b in enumerate(ast.body):
            ast.body[i] = b.visit(self, *args)
        for i, r in enumerate(ast.requires):
            ast.requires[i] = r.visit(self, *args)
        return ast

    def visit_require(self, ast, *args):
        assert isinstance(ast, Require)
        return ast

