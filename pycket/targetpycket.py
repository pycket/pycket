from pycket.expand import expand, to_ast
from pycket.interpreter import interpret_one

asts = {}
with file("stdlib.sch") as f:
    stdlib = f.read()

def read(fname):
    with file(fname) as f:
        s = f.read()
    ast = to_ast(expand("(let () \n%s\n%s\n)"%(stdlib,s)))
    asts[fname] = ast

files = ["test/y.rktl", "test/puzzle.sch"]

for f in files: read(f)

def main(argv):
    ast = asts[argv[1]]
    val = interpret_one(ast)
    return 0

def target(*args):
    return main
