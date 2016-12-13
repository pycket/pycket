
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
    SequencedBodyAST,
    SymbolSet,
    ToplevelVar,
    VariableReference,
    WithContinuationMark,
    variable_set,
)

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

    @staticmethod
    def body_muts(node):
        assert isinstance(node, SequencedBodyAST)
        muts = variable_set()
        for b in node.body:
            muts.update(b.mutated_vars())
        return muts

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
        local_muts = self.body_muts(ast)
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

        body_env_structures, body_remove_num_envs = self._visit_sequenced_body(
                ast, new_vars, sub_env_structure)
        new_body = [b.visit(self, new_vars, body_env_structures[i])
                    for i, b in enumerate(ast.body)]

        if new_lets:
            cells = [Cell(LexicalVar(v, ast.args)) for v in new_lets]
            new_body = [Let(sub_env_structure, [1] * len(new_lets), cells, new_body)]
            new_body[0].init_body_pruning(sub_env_structure, body_remove_num_envs)
            result = Lambda(ast.formals, ast.rest, ast.args, ast.frees, new_body,
                            ast.sourceinfo, env_structure, sub_env_structure)
        else:
            result = Lambda(ast.formals, ast.rest, ast.args, ast.frees, new_body,
                            ast.sourceinfo, env_structure, sub_env_structure)
            result.init_body_pruning(sub_env_structure, body_remove_num_envs)
        return result

    def visit_letrec(self, ast, vars, env_structure):
        assert isinstance(ast, Letrec)
        local_muts = self.body_muts(ast)
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
        local_muts = self.body_muts(ast)
        new_vars = vars.copy()
        new_vars.update(local_muts)
        ast, sub_env_structure, env_structures, remove_num_envs = self._compute_remove_num_envs(
            ast, new_vars, sub_env_structure)

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
        body_env_structures, body_remove_num_envs = self._visit_sequenced_body(
                ast, vars, body_env_structure)

        new_body = [b.visit(self, new_vars, body_env_structures[i])
                    for i, b in enumerate(ast.body)]
        result = Let(sub_env_structure, ast.counts, new_rhss, new_body,
                     remove_num_envs)
        result.init_body_pruning(body_env_structure, body_remove_num_envs)
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
        assert isinstance(ast, SequencedBodyAST)
        if env_structure is None:
            return [None] * len(ast.body), [0] * len(ast.body)
        remove_num_envs = []
        env_structures  = []
        curr_remove = env_structure.depth_and_size()[0]
        for i in range(len(ast.body) - 1, -1, -1):
            free_vars = ast.body[i].free_vars()
            for var in free_vars:
                var_depth = env_structure.depth_of_var(var)[1]
                curr_remove = min(curr_remove, var_depth)
            next_structure = env_structure.drop_frames(curr_remove)
            env_structures.append(next_structure)
            remove_num_envs.append(curr_remove)
        env_structures.reverse()
        remove_num_envs.reverse()
        return env_structures[:], remove_num_envs[:]

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

        remove_num_envs = [curr_remove]
        env_structures = [body_env_structure]
        for i in range(len(ast.rhss) - 1, -1, -1):
            free_vars = ast.rhss[i].free_vars()
            for v in free_vars:
                var_depth = sub_env_structure.prev.depth_of_var(v)[1]
                curr_remove = min(curr_remove, var_depth)
            next_structure = sub_env_structure.drop_frames(curr_remove + 1)
            env_structures.append(next_structure)
            remove_num_envs.append(curr_remove)
        env_structures.reverse()
        remove_num_envs.reverse()
        return ast, sub_env_structure, env_structures, remove_num_envs[:]

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

