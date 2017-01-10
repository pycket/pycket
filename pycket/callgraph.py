from rpython.rlib import jit

# TODO: Find heavily executed lambdas that do not participate in a loop in the
# callgraph.

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
        is_recursive = False
        if not lam_in_subdct:
            subdct[lam] = None
            if self.is_recursive(calling_lam, lam):
                is_recursive = True
                calling_lam.enable_jitting()
        # It is possible to have multiple consuming continuations for a given
        # function body. This will attempt to mark them all.
        same_lambda = cont_ast and cont_ast.surrounding_lambda is calling_lam
        if same_lambda:
            # did not call is_recursive yet
            if lam_in_subdct and self.is_recursive(calling_lam, lam):
                if self.is_recursive(calling_lam, lam):
                    cont_ast.set_should_enter()

    def is_recursive(self, lam, starting_from=None):
        # quatratic in theory, hopefully not very bad in practice
        if lam in self.recursive:
            return True
        if starting_from is None:
            starting_from = lam
        reachable = self.calls.get(starting_from, None)
        if reachable is None:
            return False
        init = Path(starting_from, None)
        todo = [(key, init) for key in reachable]
        visited = {}
        while todo:
            current, path = todo.pop()
            if current is lam:
                self.recursive[lam] = None
                # all the lambdas in the path are recursive too
                while path:
                    self.recursive[path.node] = None
                    path = path.prev
                return True
            if current in visited:
                continue
            reachable = self.calls.get(current, None)
            if reachable:
                path = Path(current, path)
                for key in reachable:
                    todo.append((key, path))
            visited[current] = None
        return False

    def write_dot_file(self, output): #pragma: no cover
        counter = 0
        output.write("digraph callgraph {\n")
        names = Namer()
        for node in self.calls.iterkeys():
            if node.body[0].should_enter:
                name = names.nameof(node)
                output.write(name)
                output.write(" [fillcolor=red,style=filled];\n")
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

    def __init__(self, node, prev):
        self.node = node
        self.prev = prev

    def __repr__(self):
        return "%s -> %s" % (self.node, self.prev)

