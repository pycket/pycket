
from pycket.json import loads
from pycket.expand import readfile_rpython, readfile

def main(argv):
    assert len(argv) > 1
    data = readfile_rpython(argv[1])
    #data = readfile(argv[1])
    json = loads(data)
    print json
    print json.tostring()
    return 0

def target(*args):
    return main

if __name__ == '__main__':
    import sys
    main(sys.argv)
