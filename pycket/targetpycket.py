import os.path
from pycket.expand import expand, to_ast
from pycket.interpreter import interpret_one

current_dir = os.path.dirname(__file__)

asts = {}
with file(os.path.join(current_dir, "stdlib.sch")) as f:
    stdlib = f.read()

def read(fname):
    with file(os.path.join(current_dir, fname)) as f:
        s = f.read()
    ast = to_ast(expand("(let () \n%s\n%s\n)"%(stdlib,s)))
    asts[fname] = ast

files = ["test/y.rktl", "test/puzzle.sch", "test/bubble.sch", "test/loop.rktl", "test/append.rktl", "test/nqueens.sch"]

for f in files: read(f)

def main(argv):
    ast = asts[argv[1]]
    val = interpret_one(ast)
    return 0

def target(*args):
    return main

if __name__ == '__main__':
    import sys
    main(sys.argv)
