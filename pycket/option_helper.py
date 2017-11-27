#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
from rpython.rlib import jit

def print_help(argv):
    print """Welcome to Pycket.
%s [<option> ...] <argument> ...
 File and expression options:
  -e <exprs>, --eval <exprs> : Evaluate <exprs>, prints results
  -f <file>, --load <file> : Like -e '(load "<file>")' without printing
  -t <file>, --require <file> : Like -e '(require (file "<file>"))'
  -l <path>, --lib <path> : Like -e '(require (lib "<path>"))'
  -r <file>, --script <file> : Same as -f <file> -N <file> --
  -u <file>, --require-script <file> : Same as -t <file> -N <file> --
  -g <json>, --eval-json <json> : Load and instantiate a linklet json
  -gg <json> : Like -g but stops after instantiating

 Interaction options:
  -i, --repl : Run interactive read-eval-print loop; implies -v
  -n, --no-lib : Skip `(require (lib "<init-lib>"))' for -i/-e/-f/-r
  -v, --version : Show version

 Configuration options:
  -N <file>, --name <file> : Sets `(find-system-path 'run-file)' to <file>
  --save-callgraph : save the jit output

 Meta options:
  --jit <jitargs> : Set RPython JIT options may be 'default', 'off',
                    or 'param=value,param=value' list
  -- : No argument following this switch is used as a switch
  -h, --help : Show this information and exits, ignoring other options
Default options:
 If only an argument is specified, it is loaded and evaluated
""" % (argv[0])

def parse_args(argv):
    config = {
        'repl' : False,
        'no-lib' : False,
        'version' : False,
        'stop' : False
    }
    names = {
        # 'file': "",
        # 'exprs': "",
    }
    args = []
    retval = -1
    i = 1
    to = len(argv)
    while i < to:
        if False:
            pass
        elif argv[i] == "--jit":
            if to <= i + 1:
                print "missing argument after --jit"
                retval = 2
                break
            i += 1
            jitarg = argv[i]
            jit.set_user_param(None, jitarg)
        elif argv[i] in ["-h", "--help", "/?", "-?", "/h", "/help"]:
            print_help(argv)
            if retval == -1:
                retval = 3
        elif argv[i] == "--":
            i += 1
            break
        elif argv[i] in ["-e", "--eval"]:
            if to <= i + 1:
                print "missing argument after %s" % argv[i]
                retval = 5
                break

            i += 1
            if 'exprs' in names:
                names['exprs'].append(argv[i])
            else:
                names['exprs'] = [argv[i]]

            retval = 0
        elif argv[i] in ["-f", "--load", "-r", "--script"]:
            if to <= i + 1:
                print "missing argument after %s" % argv[i]
                retval = 5
                break

            if argv[i] in ["r", "--script"]:
                names['run-file'] = [argv[i]]

            i += 1
            if 'load-file' in names:
                names['load-file'].append(argv[i])
            else:
                names['load-file'] = [argv[i]]

            retval = 0
        elif argv[i] in ["-t", "--require", "-u", "--require-script"]:
            if to <= i + 1:
                print "missing argument after %s" % argv[i]
                retval = 5
                break

            if argv[i] in ["-u", "--require-script"]:
                names['run-file'] = [argv[i]]

            i += 1
            if 'req-file' in names:
                names['req-file'].append(argv[i])
            else:
                names['req-file'] = [argv[i]]

            config['no-lib'] = True
            retval = 0
        elif argv[i] in ["-l", "--lib"]:
            if to <= i + 1:
                print "missing argument after %s" % argv[i]
                retval = 5
                break

            i += 1
            if 'req-lib' in names:
                names['req-lib'].append(argv[i])
            else:
                names['req-lib'] = [argv[i]]

            config['no-lib'] = True
            retval = 0
        elif argv[i] in ["-g", "-gg", "--eval-json"]:
            if argv[i] == "-gg":
                config['stop'] = True
            if to <= i + 1:
                print "missing argument after %s" % argv[i]
                retval = 5
                break

            i += 1
            if 'json-linklets' in names:
                names['json-linklets'].append(argv[i])
            else:
                names['json-linklets'] = [argv[i]]

            config['no-lib'] = True
            retval = 0
        elif argv[i] in ["-i", "--repl"]:
            config['repl'] = True
            config['version'] = True
            retval = 0
        elif argv[i] in ["-n", "--no-lib"]:
            config['no-lib'] = True
            if retval == -1:
                retval = 3
        elif argv[i] in ["-v", "--version"]:
            config['version'] = True
        elif argv[i] == "-N":
            if to <= i + 1:
                print "missing argument after -N"
                retval = 5
                break
            i += 1
            names['run-file'] = [argv[i]]
        elif argv[i] == '--save-callgraph':
            config['save-callgraph'] = True

        else:
            if '.rkt' in argv[i]:
                if 'req-file' in names:
                    names['req-file'].append(argv[i])
                else:
                    names['req-file'] = [argv[i]]
                names['run-file'] = [argv[i]]
                i += 1
                retval = 0
            else:
                print "Bad switch : %s" % argv[i]
                retval = 5
            break
        i += 1

    if retval == -1:
        config['repl'] = True
        config['version'] = True
        retval = 0

    if i <= to: #pending args
        args = argv[i:to]

    return config, names, args, retval

# EOF
