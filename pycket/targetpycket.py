from pycket.expand import load_json_ast_rpython
from pycket.interpreter import interpret_one
from pycket.error import SchemeException

from rpython.rlib import jit

from rpython.rlib.objectmodel import we_are_translated

def main(argv):
    jit.set_param(None, "trace_limit", 20000)
    # XXX crappy argument handling
    try:
        index = argv.index("--jit")
    except ValueError:
        pass
    else:
        if index == len(argv) - 1:
            print "missing argument after --jit"
            return 2
        jitarg = argv[index + 1]
        del argv[index:index+2]
        jit.set_user_param(None, jitarg)

    if len(argv) != 2:
        print "need exactly one argument, the json file to run"
        return 3
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
    assert not we_are_translated()
    from rpython.translator.driver import TranslationDriver
    f, _ = target(TranslationDriver(), sys.argv)
    try:
        sys.exit(f(sys.argv))
    except SystemExit:
        pass
    except:
        import pdb, traceback
        _type, value, tb = sys.exc_info()
        traceback.print_exception(_type, value, tb)
        pdb.post_mortem(tb)
