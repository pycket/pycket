
# This file is intended to be compiled using rpython.
# If you want to execute pycket on top of a python interpreter, use runpycket.py instead.

from pycket.expand import load_json_ast_rpython
from pycket.interpreter import interpret_one

def main(argv):
    ast = load_json_ast_rpython(argv[1])
    val = interpret_one(ast)
    print val.tostring()
    return 0

def target(*args):
    return main

if __name__ == '__main__':
    import sys
    main(sys.argv)
