
from rpython.rlib.objectmodel import newlist_hint, specialize

from pycket.ast_visitor import ASTVisitor
from pycket.error       import SchemeException
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
    SequencedBodyAST,
    SetBang,
    SymbolSet,
    SymbolSet,
    ToplevelVar,
    VariableReference,
    WithContinuationMark,
    make_let,
    make_letrec,
    var_eq,
    variable_set,
)
from pycket        import values
from pycket.values import W_Object, W_Prim, w_void

@specialize.argtype(0)
def compute_body_muts(node):
    if isinstance(node, SequencedBodyAST):
        body = node.body
    else:
        assert isinstance(node, list)
        body = node
    muts = variable_set()
    for b in body:
        muts.update(b.mutated_vars())
    return muts

@specialize.argtype(0)
def compute_body_frees(node):
    if isinstance(node, SequencedBodyAST):
        body = node.body
    else:
        assert isinstance(node, list)
        body = node
    frees = SymbolSet.EMPTY
    for b in body:
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
        need_cell_flags = [False] * len(ast.args.elems)
        new_vars = vars.copy()
        for i, var in enumerate(ast.args.elems):
            li = LexicalVar(var)
            self.remove_var(new_vars, li)
            need_cell_flags[i] = li in local_muts
        new_vars.update(local_muts)

        sub_env_structure = ast.args
        body_env_structures, body_remove_num_envs = self._visit_sequenced_body(
                ast, new_vars, sub_env_structure)
        new_body = [b.visit(self, new_vars, body_env_structures[i])
                    for i, b in enumerate(ast.body)]

        result = Lambda(ast.formals, ast.rest, ast.args, ast.frees, new_body,
                        ast.sourceinfo, env_structure, sub_env_structure)
        result.init_body_pruning(sub_env_structure, body_remove_num_envs)
        result.init_mutable_var_flags(need_cell_flags)
        return result

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

        body_env_structures, body_remove_num_envs = self._visit_sequenced_body(
                ast, new_vars, sub_env_structure)

        new_body = [b.visit(self, new_vars, body_env_structures[i])
                    for i, b in enumerate(ast.body)]
        letrec = Letrec(sub_env_structure, ast.counts, new_rhss, new_body)
        letrec.init_body_pruning(sub_env_structure, body_remove_num_envs)
        return letrec

    def visit_let(self, ast, vars, env_structure):
        assert isinstance(ast, Let)
        sub_env_structure = SymList(ast.args.elems, env_structure)
        local_muts = compute_body_muts(ast)
        new_vars = vars.copy()
        new_vars.update(local_muts)
        ast, sub_env_structure, env_structures, remove_num_envs = self._compute_remove_num_envs(
            ast, new_vars, sub_env_structure)

        new_rhss = [None] * len(ast.rhss)
        offset = 0
        variables = ast.args.elems
        need_cell_flags = [False] * len(ast.args.elems)
        for i, rhs in enumerate(ast.rhss):
            count = ast.counts[i]
            for j in range(count):
                var = variables[offset+j]
                need_cell_flags[offset+j] = LexicalVar(var) in local_muts
            new_rhss[i] = rhs.visit(self, vars, env_structures[i])
            offset += count

        body_env_structure = env_structures[-1]
        body_env_structures, body_remove_num_envs = self._visit_sequenced_body(
                ast, vars, body_env_structure)

        new_body = [b.visit(self, new_vars, body_env_structures[i])
                    for i, b in enumerate(ast.body)]
        result = Let(sub_env_structure, ast.counts, new_rhss, new_body,
                     remove_num_envs)
        result.init_body_pruning(body_env_structure, body_remove_num_envs)
        result.init_mutable_var_flags(need_cell_flags)
        return result

    def visit_begin(self, ast, vars, env_structure):
        assert isinstance(ast, Begin)
        body_structures, body_removes = self._visit_sequenced_body(
                ast, vars, env_structure)
        new_body = [None] * len(ast.body)
        new_body = [b.visit(self, vars, body_structures[i])
                    for i, b in enumerate(ast.body)]
        result = Begin(new_body)
        result.init_body_pruning(env_structure, body_removes)
        return result

    def _visit_sequenced_body(self, ast, vars, env_structure):
        from pycket import config
        assert isinstance(ast, SequencedBodyAST)

        if not config.prune_env or env_structure is None:
            return [env_structure] * len(ast.body), [0] * len(ast.body)
        remove_num_envs = [0] * len(ast.body)
        env_structures = [None] * len(ast.body)
        curr_remove = env_structure.depth_and_size()[0]
        for i in range(len(ast.body) - 1, -1, -1):
            free_vars = ast.body[i].free_vars()
            for var in free_vars:
                var_depth = env_structure.depth_of_var(var)[1]
                curr_remove = min(curr_remove, var_depth)
            next_structure = env_structure.drop_frames(curr_remove)
            env_structures[i] = next_structure
            remove_num_envs[i] = curr_remove
        return env_structures, remove_num_envs

    def visit_define_values(self, ast, vars, env_structure):
        assert isinstance(ast, DefineValues)
        need_cell_flags = [ModuleVar(i, None, i) in vars for i in ast.names]
        rhs = ast.rhs.visit(self, vars, env_structure)
        if True in need_cell_flags:
            rhs = Cell(rhs, need_cell_flags)
        return DefineValues(ast.names, rhs, ast.display_names)

    def visit_module(self, ast, vars, env_structure):
        """
        This method must return the original module due to the use of the shared
        module table.
        """
        assert isinstance(ast, Module)
        local_muts = ast.mod_mutated_vars()
        ast.body = [b.visit(self, local_muts, None) for b in ast.body]
        return ast

    def _compute_remove_num_envs(self, ast, new_vars, sub_env_structure):
        from pycket import config
        if not config.prune_env:
            remove_num_envs = [0] * (len(ast.rhss) + 1)
            env_structures = [sub_env_structure.prev] * len(ast.rhss)
            env_structures.append(sub_env_structure)
            return ast, sub_env_structure, env_structures, remove_num_envs

        # find out whether a smaller environment is sufficient for the body
        free_vars_not_from_let = compute_body_frees(ast)
        free_vars_not_from_let = free_vars_not_from_let.without_many(
                ast.args.elems)

        # at most, we can remove all envs, apart from the one introduced by let
        curr_remove = max_depth = sub_env_structure.depth_and_size()[0] - 1
        max_needed = 0
        free_vars_not_mutated = True
        for v in free_vars_not_from_let:
            depth = sub_env_structure.depth_of_var(v)[1] - 1
            curr_remove = min(curr_remove, depth)
            max_needed = max(max_needed, depth)
            free_vars_not_mutated &= LexicalVar(v) not in new_vars

        if curr_remove == 0:
            body_env_structure = sub_env_structure
        else:
            next_structure = sub_env_structure.prev.drop_frames(curr_remove)
            body_env_structure = SymList(ast.args.elems, next_structure)

        if (free_vars_not_mutated and max_needed == curr_remove and
                max_depth > max_needed):
            before_max_needed = sub_env_structure.drop_frames(max_needed + 2)
            if before_max_needed and before_max_needed.depth_and_size()[1] > 0:
                counts, new_lhs_vars, new_rhss = self._copy_live_vars(
                        ast, free_vars_not_from_let)
                body_env_structure = SymList(new_lhs_vars)
                sub_env_structure = SymList(new_lhs_vars, sub_env_structure.prev)
                ast = Let(body_env_structure, counts, new_rhss, ast.body)
                return self._compute_remove_num_envs(ast, new_vars, sub_env_structure)

        # The loops will modify all but the last element
        remove_num_envs = [curr_remove] * (len(ast.rhss) + 1)
        env_structures = [body_env_structure] * (len(ast.rhss) + 1)
        for i in range(len(ast.rhss) - 1, -1, -1):
            free_vars = ast.rhss[i].free_vars()
            for v in free_vars:
                var_depth = sub_env_structure.prev.depth_of_var(v)[1]
                curr_remove = min(curr_remove, var_depth)
            env_structures[i] = sub_env_structure.drop_frames(curr_remove + 1)
            remove_num_envs[i] = curr_remove
        return ast, sub_env_structure, env_structures, remove_num_envs

    @staticmethod
    def _copy_live_vars(ast, free_vars_not_from_let):
        # there is unneeded local env storage that we will never need
        # in the body. thus, make a copy of all local variables into
        # the current let, at the point where the variable is not longer
        # referenced in any of the right-hand-sides.
        vars_to_copy = free_vars_not_from_let

        # Find the last right hand side in which each variable to be
        # copied is referenced
        dead_after_sets = [[] for _ in ast.rhss]
        rhss = ast.rhss
        for var in free_vars_not_from_let:
            i = len(ast.rhss) - 1
            while i != 0 and not rhss[i].free_vars().haskey(var):
                i -= 1
            dead_after_sets[i].append(var)

        # Build the new args and rhss by interleaving the bindings with
        # the new copy operations
        new_lhs_vars = []
        new_rhss = []
        counts = []
        args = ast._rebuild_args()
        for i, rhs in enumerate(ast.rhss):
            new_lhs_vars.extend(dead_after_sets[i])
            new_lhs_vars.extend(args[i])

            new_rhss.extend([LexicalVar(v) for v in dead_after_sets[i]])
            new_rhss.append(rhs)

            counts.extend([1] * len(dead_after_sets[i]))
            counts.append(ast.counts[i])

        new_lhs_vars = new_lhs_vars[:]
        new_rhss = new_rhss[:]
        counts = counts[:]
        return counts, new_lhs_vars, new_rhss

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

# Expressions can be evaluated in value, predicate, or expression contexts.
# As a special case, we special case the context expecting exactly one value.
# The general values context 'v' expects an unknown number of results, making it
# the least precise context.
effect = 'e'
value = 'v'
value1 = '1'
pred = 'p'

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

def binding_context(args):
    if len(args) == 1:
        return '1'
    return 'v'

def expects(context):
    if context in ('e', 'p', '1'):
        return 1
    return -1

VALUES_SYMBOL = values.W_Symbol.make("values")
VALUES = ModuleVar(VALUES_SYMBOL, "#%kernel", VALUES_SYMBOL)

def zero_values():
    return App.make(VALUES, [])

def incontext(value, context):
    if context == 'e':
        return void()
    if context == 'p':
        return Quote(values.W_Bool.make(value is not values.w_false))
    assert context in ('v', '1')
    return Quote(value)

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

    def visit_quote(self, ast, context):
        if context in ('v', '1'):
            return ast
        if context == 'p' and isinstance(ast.w_val, values.W_Bool):
            return ast
        return incontext(ast.w_val, context)

    def visit_set_bang(self, ast, context):
        assert isinstance(ast, SetBang)
        var = ast.var
        rhs = ast.rhs.visit(self, '1')
        return SetBang(var, rhs)

    def visit_if(self, ast, context):
        assert isinstance(ast, If)
        tst = ast.tst.visit(self, 'p')
        result = resultof(tst)
        if result is not None:
            case = ast.els if result is values.w_false else ast.thn
            return Begin.make([tst.visit(self, 'e'), case.visit(self, context)])
        thn = ast.thn.visit(self, context)
        els = ast.els.visit(self, context)
        return If(tst, thn, els)

    def visit_app(self, ast, context):
        rator = ast.rator.visit(self, '1')
        rands = [r.visit(self, '1') for r in ast.rands]

        # If the context expects one value and this is an invocation of values,
        # then just return the first argument
        if (expects(context) == 1 and isinstance(rator, ModuleVar) and
            len(rands) == 1 and var_eq(rator, VALUES)):
            print "killed values:", ast.tostring()
            return rands[0]

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
            value = func(args)
            return incontext(value, context)
        except SchemeException:
            # If anything goes wrong, just bail and deal with it at runtime
            return App.make(rator, rands)

    def visit_with_continuation_mark(self, ast, context):
        assert isinstance(ast, WithContinuationMark)
        key = ast.key.visit(self, '1')
        value = ast.value.visit(self, '1')
        body = ast.body.visit(self, context)
        return WithContinuationMark(key, value, body)

    def _visit_body(self, body, context):
        assert isinstance(body, list)
        new_body = [None] * len(body)
        for i in range(len(body) - 1):
            new_body[i] = body[i].visit(self, 'e')
        new_body[-1] = body[-1].visit(self, context)
        return new_body

    def visit_begin(self, ast, context):
        assert isinstance(ast, Begin)
        body = self._visit_body(ast.body, context)
        return Begin.make(body)

    def visit_begin0(self, ast, context):
        assert isinstance(ast, Begin0)
        first = ast.first.visit(self, context)
        body = ast.body.visit(self, 'e')
        return Begin0.make(first, [body])

    def _visit_let(self, varss, rhss, body, context):
        body_muts = compute_body_muts(body)
        body_frees = compute_body_frees(body)

        max_len = len(rhss)
        new_vars = newlist_hint(max_len)
        new_rhss = newlist_hint(max_len)
        for i, rhs in enumerate(rhss):
            vars = varss[i]
            # If none of the bound variables are referenced, evaluate the
            # rhs for effect, rather than for value
            for var in vars:
                if body_frees.haskey(var):
                    rhs = rhs.visit(self, binding_context(vars))
                    break
            else:
                rhs = rhs.visit(self, 'e')
                if vars:
                    rhs = Begin.make([rhs, zero_values()])
                vars = []
            if len(vars) == 1:
                sym, = vars
                if self.constant_binding(body_muts, sym, rhs):
                    self.constant_bindings[sym] = Const(rhs)
                    continue

            new_vars.append(vars)
            new_rhss.append(rhs)
        body = self._visit_body(body, context)
        return make_let(new_vars[:], new_rhss[:], body)

    def visit_let(self, ast, context):
        assert isinstance(ast, Let)
        varss = ast._rebuild_args()
        return self._visit_let(varss, ast.rhss, ast.body, context)

    def visit_letrec(self, ast, context):
        assert isinstance(ast, Letrec)
        args = ast._rebuild_args()
        rhss = [None] * len(ast.rhss)
        for i, r in enumerate(ast.rhss):
            rhss[i] = r.visit(self, binding_context(args[i]))
        body = self._visit_body(ast.body, context)
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
        rhs = ast.rhs.visit(self, binding_context(ast.names))
        return DefineValues(ast.names, ast.rhs, ast.display_names)

def constant_prop(ast, env=None):
    assert isinstance(ast, Module)
    if env is None:
        env = SymbolSet.EMPTY
    visitor = ConstantPropVisitor(ast.mod_mutated_vars())
    return ast.visit(visitor, 'v')

