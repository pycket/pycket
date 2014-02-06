from pycket.expand import load_json_ast_rpython
from pycket.interpreter import interpret_one
from pycket.error import SchemeException

def main(argv):
    ast = load_json_ast_rpython(argv[1])
    try:
        val = interpret_one(ast)
    except SchemeException, e:
        print "ERROR:", e.msg
        raise # to see interpreter-level traceback
    else:
        print val.tostring()
        return 0

def target(*args):
    return main

if __name__ == '__main__':
    import sys
    main(sys.argv)
