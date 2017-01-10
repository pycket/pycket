from rpython.rlib import jit

# TODO: Find heavily executed lambdas that do not participate in a loop in the
# callgraph.

NOT_RECURSIVE = 0b00
LOOP_HEADER = 0b01
LOOP_PARTICIPANT = 0b11

def join_states(s1, s2):
    return s1 | s2

class Namer(object): #pragma: no cover

    def __init__(self):
        self.names   = {}
        self.counter = 0

    def nameof(self, val):
        name = self.names.get(val, None)
        if name is not None:
            return name
        name = "A_" + str(self.counter)
        self.counter += 1
        self.names[val] = name
        return name

class CallGraph(object):
    def __init__(self):
        self.calls     = {}
        self.recursive = {}

    def register_call(self, lam, calling_app, cont, env):
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
            lam_in_subdct = False
        else:
            lam_in_subdct = lam in subdct
        cont_ast = cont.get_next_executed_ast()
        config = env.pycketconfig()
        is_recursive = NOT_RECURSIVE
        if not lam_in_subdct:
            subdct[lam] = None
            is_recursive = self.is_recursive(calling_lam, lam)
            if is_recursive == LOOP_HEADER:
                calling_lam.enable_jitting()
        # It is possible to have multiple consuming continuations for a given
        # function body. This will attempt to mark them all.
        same_lambda = cont_ast and cont_ast.surrounding_lambda is calling_lam
        if same_lambda:
            # did not call is_recursive yet
            if lam_in_subdct:
                is_recursive = self.is_recursive(calling_lam, lam)
                if is_recursive != NOT_RECURSIVE:
                    cont_ast.set_should_enter()

    def is_recursive(self, lam, starting_from=None):
        # quatratic in theory, hopefully not very bad in practice
        status = self.recursive.get(lam, NOT_RECURSIVE)
        if status == LOOP_HEADER:
            return LOOP_HEADER
        if starting_from is None:
            starting_from = lam
        reachable = self.calls.get(starting_from, None)
        if reachable is None:
            return NOT_RECURSIVE
        init = Path(starting_from, None)
        todo = [(key, init) for key in reachable]
        visited = {}
        while todo:
            current, path = todo.pop()
            if current is lam:
                # all the lambdas in the path are recursive too
                for node in path:
                    status = self.recursive.get(node, NOT_RECURSIVE)
                    self.recursive[node] = join_states(status, LOOP_PARTICIPANT)
                self.recursive[lam] = LOOP_HEADER
                return LOOP_HEADER
            status = self.recursive.get(current, NOT_RECURSIVE)
            if status == LOOP_HEADER or current in visited:
                continue
            reachable = self.calls.get(current, None)
            if reachable is not None:
                path = Path(current, path)
                for key in reachable:
                    todo.append((key, path))
            visited[current] = None
        return NOT_RECURSIVE

    def write_dot_file(self, output): #pragma: no cover
        counter = 0
        output.write("digraph callgraph {\n")
        names = Namer()
        for node in self.calls.iterkeys():
            if node.body[0].should_enter:
                name = names.nameof(node)
                output.write(name)
                output.write(" [fillcolor=red,style=filled];\n")
            else:
                status = self.recursive.get(node, NOT_RECURSIVE)
                if status == LOOP_PARTICIPANT:
                    name = names.nameof(node)
                    output.write(name)
                    output.write(" [fillcolor=green,style=filled];\n")
        for src, subdct in self.calls.iteritems():
            for dst in subdct:
                srcname = names.nameof(src)
                dstname = names.nameof(dst)
                output.write(srcname)
                output.write(" -> ")
                output.write(dstname)
                if dst.body[0].should_enter:
                    output.write(" [color=blue]")
                output.write(";\n")
        output.write("}\n")

class Path(object):

    __slots__ = ('node', 'prev')

    def __init__(self, node, prev):
        self.node = node
        self.prev = prev

    def __iter__(self):
        while self is not None:
            yield self.node
            self = self.prev

    def __repr__(self):
        return "%s -> %s" % (self.node, self.prev)

