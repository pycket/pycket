
from pycket.expand import load_json_ast
from pycket.interpreter import interpret_one
from pycket import values

def main(argv):
    ast = load_json_ast(argv[1])
    val = interpret_one(ast)
    assert isinstance(val, values.Values)
    print val.tostring()
    return 0

def target(*args):
    return main

if __name__ == '__main__':
    import sys
    main(sys.argv)
