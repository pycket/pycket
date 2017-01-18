from rpython.rlib import jit, objectmodel

# TODO: Find heavily executed lambdas that do not participate in a loop in the
# callgraph.

# The callgraph classifies nodes into three possible categories
#   1. NOT_LOOP: nodes which do not occur in a loop at all
#   2. LOOP_PARTICIPANT: nodes which occur in a loop but are not loop headers
#   3. LOOP_HEADER: nodes occuring at the beginning of a loop, where we want
#                   tracing to start
# LOOP_PARTICIPANTs are used to find continuations which may be allocated in a loop
# by nodes other than the loop header.
# Ideally, for a given cyclic path through the callgraph, we should only need to
# mark a single node as a loop header which dominates all other nodes a loop.
#
# Further, we want to mark as few nodes as possible, as fewer loop headers reduces
# the work performed by the JIT. This also reduces potential collisions in the
# trace cache.

NOT_LOOP         = 0b00
LOOP_PARTICIPANT = 0b01
LOOP_HEADER      = 0b11

@objectmodel.always_inline
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

    @jit.not_in_trace
    def register_call(self, lam, calling_app, cont, env):
        if calling_app is None:
            return
        calling_lam = calling_app.surrounding_lambda
        if calling_lam is None:
            return
        subdct = self.calls.get(calling_lam, None)
        if subdct is None:
            self.calls[calling_lam] = subdct = {}
            lam_in_subdct = False
        else:
            lam_in_subdct = lam in subdct
        status = NOT_LOOP
        if not lam_in_subdct:
            subdct[lam] = None
            status = self.is_recursive(calling_lam, lam)
            if status == LOOP_HEADER:
                calling_lam.enable_jitting()
        # It is possible to have multiple consuming continuations for a given
        # function body. This will attempt to mark them all.
        cont_ast = cont.get_next_executed_ast()
        same_lambda = cont_ast and cont_ast.surrounding_lambda is calling_lam
        if same_lambda:
            # did not call is_recursive yet
            if lam_in_subdct:
                status = self.is_recursive(calling_lam, lam)
            if status != NOT_LOOP:
                cont_ast.set_should_enter()

    def add_participants(self, path):
        for node in path:
            status = self.recursive.get(node, NOT_LOOP)
            if status == NOT_LOOP:
                self.recursive[node] = LOOP_PARTICIPANT

    def status(self, node):
        return self.recursive.get(node, NOT_LOOP)

    def is_recursive(self, lam, starting_from=None):
        # quatratic in theory, hopefully not very bad in practice
        if self.status(lam) == LOOP_HEADER:
            return LOOP_HEADER
        # if starting_from is None:
        starting_from = lam
        reachable = self.calls.get(starting_from, None)
        if reachable is None:
            return NOT_LOOP
        path = Path(starting_from, None)
        todo = [(key, path) for key in reachable]
        visited = {}
        status = NOT_LOOP
        while todo:
            current, path = todo.pop()
            if current is lam:
                # all the lambdas in the path are recursive too
                self.add_participants(path)
                self.recursive[lam] = LOOP_HEADER
                status = LOOP_HEADER
            elif self.status(current) == LOOP_HEADER:
                self.add_participants(path)
            elif current not in visited:
                reachable = self.calls.get(current, None)
                if reachable is not None:
                    path = Path(current, path)
                    for key in reachable:
                        todo.append((key, path))
                visited[current] = None
        return status

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
                status = self.recursive.get(node, NOT_LOOP)
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

