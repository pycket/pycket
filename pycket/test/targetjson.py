
from pycket.json import loads

def main(argv):
    assert len(argv) > 1
    print loads(argv[1])
    return 0

def target(*args):
    return main
