
import os
from pycket.expand import expand_string, readfile

def write_to_file(expanded, targetfile):
    try:
        os.remove(targetfile)
    except IOError:
        pass
    except OSError:
        pass
    f = open(targetfile, "w")
    f.write(str(expanded))
    f.close()

def load_expanded(fname):
    stdlib = readfile("stdlib.sch")
    s = readfile(fname)
    code = "(let () \n%s\n%s\n)" % (stdlib, s)
    return expand_string(code)

def main(argv):
    if not(len(argv) == 3):
        print "Need two arguments: source file with pycket-code and target file to store json-output"
        return 1
    source = argv[1]
    target = argv[2]
    
    code = load_expanded(source)
    write_to_file(code, target)

if __name__ == '__main__':
    import sys
    main(sys.argv)
