
from rpython.rlib.objectmodel import newlist_hint

from pycket.ast_visitor import ASTVisitor
from pycket.env         import SymList
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
    SymbolSet,
    SequencedBodyAST,
    ToplevelVar,
    VariableReference,
    WithContinuationMark,
    make_let,
    make_letrec,
    variable_set,
)
from pycket        import values
from pycket.values import W_Object, W_Prim, w_void

def compute_body_muts(node):
    assert isinstance(node, SequencedBodyAST)
    muts = variable_set()
    for b in node.body:
        muts.update(b.mutated_vars())
    return muts

def compute_body_frees(node):
    assert isinstance(node, SequencedBodyAST)
    frees = SymbolSet.EMPTY
    for b in node.body:
        frees = frees.union(b.free_vars())
    return frees

class AssignConvertVisitor(ASTVisitor):
    """
    This visitor performs assignment conversion of the Pycket AST, which is
    necessary in order to interpret the AST.

    This pass also performs something akin to liveness analysis, trying to reduce
    the sizes of environment frames by removing cells unreferenced by an expression.
    """

    @staticmethod
    def remove_var(set, key):
        try:
            del set[key]
        except KeyError:
            pass

    def visit_cell_ref(self, ast, vars, env_structure):
        assert isinstance(ast, CellRef)
        return CellRef(ast.sym, env_structure)

    def visit_lexical_var(self, ast, vars, env_structure):
        assert isinstance(ast, LexicalVar)
        if ast in vars:
            return CellRef(ast.sym, env_structure)
        else:
            return LexicalVar(ast.sym, env_structure)

    def visit_variable_reference(self, ast, vars, env_structure):
        assert isinstance(ast, VariableReference)
        v = ast.var
        if isinstance(v, LexicalVar) and v in vars:
            return VariableReference(v, ast.path, True)
        # top-level variables are always mutable
        if isinstance(v, ToplevelVar):
            return VariableReference(v, ast.path, True)
        else:
            return ast

    def visit_lambda(self, ast, vars, env_structure):
        assert isinstance(ast, Lambda)
        local_muts = compute_body_muts(ast)
        new_lets = []
        new_vars = vars.copy()
        for i in ast.args.elems:
            li = LexicalVar(i)
            self.remove_var(new_vars, li)
            if li in local_muts:
                new_lets.append(i)
        new_vars.update(local_muts)
        if new_lets:
            sub_env_structure = SymList(new_lets, ast.args)
        else:
            sub_env_structure = ast.args
        new_body = [b.visit(self, new_vars, sub_env_structure) for b in ast.body]
        if new_lets:
            cells = [Cell(LexicalVar(v, ast.args)) for v in new_lets]
            new_body = [Let(sub_env_structure, [1] * len(new_lets), cells, new_body)]
        return Lambda(ast.formals, ast.rest, ast.args, ast.frees, new_body,
                      ast.sourceinfo, env_structure, sub_env_structure)

    def visit_letrec(self, ast, vars, env_structure):
        assert isinstance(ast, Letrec)
        local_muts = compute_body_muts(ast)
        for b in ast.rhss:
            local_muts.update(b.mutated_vars())
        for v in ast.args.elems:
            lv = LexicalVar(v)
            local_muts[lv] = None
        new_vars = vars.copy()
        new_vars.update(local_muts)
        sub_env_structure = SymList(ast.args.elems, env_structure)
        new_rhss = [rhs.visit(self, new_vars, sub_env_structure) for rhs in ast.rhss]
        new_body = [b.visit(self, new_vars, sub_env_structure) for b in ast.body]
        return Letrec(sub_env_structure, ast.counts, new_rhss, new_body)

    def visit_let(self, ast, vars, env_structure):
        assert isinstance(ast, Let)
        sub_env_structure = SymList(ast.args.elems, env_structure)
        local_muts = compute_body_muts(ast)
        new_vars = vars.copy()
        new_vars.update(local_muts)
        ast, sub_env_structure, env_structures, remove_num_envs = ast._compute_remove_num_envs(
            new_vars, sub_env_structure)

        new_rhss = [None] * len(ast.rhss)
        offset = 0
        variables = ast.args.elems
        for i, rhs in enumerate(ast.rhss):
            new_rhs = rhs.visit(self, vars, env_structures[i])
            count = ast.counts[i]
            need_cell_flags = [LexicalVar(variables[offset+j]) in local_muts for j in range(count)]
            if True in need_cell_flags:
                new_rhs = Cell(new_rhs, need_cell_flags)
            new_rhss[i] = new_rhs
            offset += count

        body_env_structure = env_structures[-1]
        new_body = [b.visit(self, new_vars, body_env_structure) for b in ast.body]
        return Let(sub_env_structure, ast.counts, new_rhss, new_body,
                   remove_num_envs)

    def visit_define_values(self, ast, vars, env_structure):
        assert isinstance(ast, DefineValues)
        need_cell_flags = [(ModuleVar(i, None, i) in vars) for i in ast.names]
        if True in need_cell_flags:
            return DefineValues(ast.names,
                                Cell(ast.rhs.visit(self, vars, env_structure),
                                     need_cell_flags),
                                ast.display_names)
        else:
            return DefineValues(ast.names,
                                ast.rhs.visit(self, vars, env_structure),
                                ast.display_names)

    def visit_module(self, ast, vars, env_structure):
        """
        This method must return the original module due to the use of the shared
        module table.
        """
        assert isinstance(ast, Module)
        local_muts = ast.mod_mutated_vars()
        ast.body = [b.visit(self, local_muts, None) for b in ast.body]
        return ast

def assign_convert(ast, visitor=None):
    if visitor is None:
        visitor = AssignConvertVisitor()
    return ast.visit(visitor, variable_set(), None)

class Const(W_Object):
    _attrs_ = ['value']
    def __init__(self, value):
        self.value = value

    def const_value(self):
        """ Produces copies due to the mutable components of AST nodes. """
        val = self.value
        if isinstance(val, Quote):
            return Quote(val.w_val)
        if isinstance(val, ModuleVar):
            return ModuleVar(val.sym, val.srcmod, val.srcsym, val.path)
        return val

effect = 'e'
value = 'v'

def void():
    return Quote(w_void)

def resultof(expr):
    next = expr.resultof()
    while next is not expr:
        expr, next = next, next.resultof()
    if isinstance(expr, Quote):
        return expr.w_val
    if isinstance(expr, SetBang):
        return values.w_void
    return None

VALUES_SYMBOL = values.W_Symbol.make("values")
VALUES = ModuleVar(VALUES_SYMBOL, "#%kernel", VALUES_SYMBOL)

def zero_values():
    return App.make(VALUES, [])

class ConstantPropVisitor(ASTVisitor):
    preserve_mutated_vars = True

    def __init__(self, mod_mutated_vars):
        self.mod_mutated_vars = mod_mutated_vars
        # A mapping from the symbol representation of each lexical variable to
        # their discovered constant value, if any.
        # This representation is safe as each lexcal variable is guaranteed unique
        # by the expander.
        self.constant_bindings = {}

    def constant_binding(self, muts, sym, rhs):
        if LexicalVar(sym) in muts:
            return False
        if isinstance(rhs, Quote):
            return True
        if isinstance(rhs, ModuleVar) and rhs not in self.mod_mutated_vars:
            return True
        return False

    def visit_set_bang(self, ast, context):
        assert isinstance(ast, SetBang)
        var = ast.var
        rhs = ast.rhs.visit(self, 'v')
        return SetBang(var, rhs)

    def visit_if(self, ast, context):
        assert isinstance(ast, If)
        tst = ast.tst.visit(self, 'v')
        result = resultof(tst)
        if result is not None:
            case = ast.els if result is values.w_false else ast.thn
            return Begin.make([tst.visit(self, 'e'), case.visit(self, context)])
        thn = ast.thn.visit(self, context)
        els = ast.els.visit(self, context)
        return If(tst, thn, els)

    def visit_app(self, ast, context):
        rator = ast.rator.visit(self, 'v')
        rands = [r.visit(self, 'v') for r in ast.rands]
        w_prim = ast.get_prim_func(rator)
        if not isinstance(w_prim, W_Prim) or w_prim.unwrapped is None:
            return App.make(rator, rands)
        func = w_prim.unwrapped
        args = [None] * len(rands)
        for i, rand in enumerate(rands):
            if not isinstance(rand, Quote):
                return App.make(rator, rands)
            args[i] = rand.w_val
        if context == 'e':
            return void()
        try:
            return Quote(func(args))
        except Exception:
            # If anything goes wrong, just bail and deal with it at runtime
            return App.make(rator, rands)

    def visit_with_continuation_mark(self, ast, context):
        assert isinstance(ast, WithContinuationMark)
        key = ast.key.visit(self, 'v')
        value = ast.value.visit(self, 'v')
        body = ast.body.visit(self, context)
        return WithContinuationMark(key, value, body)

    def _visit_body(self, ast, context):
        assert isinstance(ast, SequencedBodyAST)
        new_body = [None] * len(ast.body)
        for i in range(len(ast.body) - 1):
            new_body[i] = ast.body[i].visit(self, 'e')
        new_body[-1] = ast.body[-1].visit(self, context)
        return new_body

    def visit_begin(self, ast, context):
        assert isinstance(ast, Begin)
        body = self._visit_body(ast, context)
        return Begin.make(body)

    def visit_begin0(self, ast, context):
        assert isinstance(ast, Begin0)
        first = ast.first.visit(self, context)
        body = ast.body.visit(self, 'e')
        return Begin0.make(first, [body])

    def visit_let(self, ast, context):
        assert isinstance(ast, Let)
        varss = ast._rebuild_args()
        body_muts = compute_body_muts(ast)
        body_frees = compute_body_frees(ast)

        max_len = len(ast.rhss)
        new_vars = newlist_hint(max_len)
        new_rhss = newlist_hint(max_len)
        for i, rhs in enumerate(ast.rhss):
            vars = varss[i]
            for var in vars:
                if body_frees.haskey(var):
                    rhs = rhs.visit(self, 'v')
                    break
            else:
                rhs = Begin.make([rhs, zero_values()]) if vars else rhs
                rhs = rhs.visit(self, 'e')
                vars = []
            if len(vars) == 1:
                sym, = vars
                if self.constant_binding(body_muts, sym, rhs):
                    self.constant_bindings[sym] = Const(rhs)
                    continue

            new_vars.append(vars)
            new_rhss.append(rhs)
        body = self._visit_body(ast, context)
        return make_let(new_vars[:], new_rhss[:], body)

    def visit_letrec(self, ast, context):
        assert isinstance(ast, Letrec)
        args = ast._rebuild_args()
        rhss = [r.visit(self, 'v') for r in ast.rhss]
        body = self._visit_body(ast, context)
        return make_letrec(args, rhss, body)

    def visit_case_lambda(self, ast, context):
        assert isinstance(ast, CaseLambda)
        if context == 'e':
            return void()
        return ASTVisitor.visit_case_lambda(self, ast, 'v')

    def visit_module_var(self, ast, context):
        if context == 'e':
            return void()
        return ASTVisitor.visit_module_var(self, ast, context)

    def visit_lexical_var(self, ast, context):
        assert isinstance(ast, LexicalVar)
        if context == 'e':
            return void()
        val = self.constant_bindings.get(ast.sym, None)
        if val is None:
            return ast
        assert isinstance(val, Const)
        return val.const_value()

    def visit_define_values(self, ast, context):
        assert isinstance(ast, DefineValues)
        assert context == 'v'
        rhs = ast.rhs.visit(self, 'v')
        return DefineValues(ast.names, ast.rhs, ast.display_names)

def constant_prop(ast, env=None):
    assert isinstance(ast, Module)
    if env is None:
        env = SymbolSet.EMPTY
    visitor = ConstantPropVisitor(ast.mod_mutated_vars())
    return ast.visit(visitor, 'v')

