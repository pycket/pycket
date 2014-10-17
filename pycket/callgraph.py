from rpython.rlib import jit

class CallGraph(object):
    def __init__(self):
        self.calls = {}

    def register_call(self, lam, calling_app, cont=None):
        if jit.we_are_jitted():
            return
        if not calling_app:
            return
        calling_lam = calling_app.surrounding_lambda
        if not calling_lam:
            return
        subdct = self.calls.get(calling_lam, None)
        if subdct is None:
            self.calls[calling_lam] = subdct = {}
        if lam not in subdct:
            subdct[lam] = None
            if (not calling_lam.body[0].should_enter and
                    self.is_recursive(calling_lam, lam)):
                calling_lam.enable_jitting()
                cont_ast = None
                if cont:
                    cont_ast = cont.get_next_executed_ast()
                if cont_ast and cont_ast.surrounding_lambda is calling_lam:
                    # jit the down recursion
                    #print "jitting downrecursion", cont_ast.tostring()
                    cont_ast.should_enter = True

    def is_recursive(self, lam, starting_from=None):
        # quatratic in theory, hopefully not very bad in practice
        visited = {}
        if starting_from is None:
            starting_from = lam
        todo = self.calls.get(starting_from, {}).keys()
        while todo:
            current = todo.pop()
            if current is lam:
                return True
            if current in visited:
                continue
            todo.extend(self.calls.get(current, {}).keys())
            visited[current] = None
        return False

