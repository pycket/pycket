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
  NYI -p <package> : Like -e '(require (planet "<package>")'
  NYI -r <file>, --script <file> : Same as -f <file> -N <file> --
  NYI -u <file>, --require-script <file> : Same as -t <file> -N <file> --
  -i, --repl : Run interactive read-eval-print loop; implies -v
  NYI -n, --no-lib : Skip `(require (lib "<init-lib>"))' for -i/-e/-f/-r
  NYI -v, --version : Show version

  DEL -b (-R) <file> : run pycket with bytecode expansion, optional -R flag enables recursive bytecode expansion
  DEL -c <file> : run pycket with complete expansion, expanding every dependent module and put everything into one single json. <file> can also be a json pre-generated with -c option, in this case pycket doesn't need to expand anything at all.
 Configuration options:
  --stdlib: Use Pycket's version of stdlib (only applicable for -e)
 Meta options:
  --jit <jitargs> : Set RPython JIT options may be 'default', 'off',
                    or 'param=value,param=value' list
  -- : No argument following this switch is used as a switch
  -h, --help : Show this information and exits, ignoring other options
Default options:
 If only an argument is specified, it is loaded and evaluated
""" % (argv[0])

_run = True
_eval = False
def parse_args(argv):
    config = {
        'stdlib': False,
        'racket': True,
#        'mcons': False,
        'mode': _run,
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
            return (None, None, None, 0)
        elif argv[i] == "--":
            i += 1
            break
        elif argv[i] == "--stdlib":
            config['stdlib'] = True
            i += 1
        elif argv[i] == "-e":
            if to <= i + 1:
                print "missing argument after -e"
                retval = 5
                break
            retval = 0
            config['mode'] = _eval
            i += 1
            names['exprs'] = argv[i]

        elif argv[i] == "-f":
            if to <= i + 1:
                print "missing argument after -r"
                retval = 5
                break
            retval = 0
            i += 1
            names['load_file'] = argv[i]
        elif argv[i] == "-i":
            retval = 0
            config['repl'] = True
            i += 1
        elif argv[i] in ["-u", "-t", "-l", "-p"]:
            arg = argv[i][1]
            stop = arg in ["u"]

            if to <= i + 1:
                print "missing argument after -%s" % arg
                retval = 5
                break
            # if arg == "r":
            #     suffix = "f"
            # elif arg == "u":
            #     suffix = "t"
            # else:
            #     suffix = arg

            i += 1
            if arg == "t":
                names['req_file'] = argv[i]
            elif arg == "l":
                names['req_lib'] = argv[i]

            retval = 0 # FIXME: temporary
            #i += 1
            # names['file'] = "%s.%s" % (argv[i], suffix)
            # names['exprs'] = script_exprs(arg, argv[i])
            # config['mode'] = _eval
            # retval = 0
            # if stop:
            #     i += 1
            #     break
        elif argv[i] == "-c":
            arg = argv[i][1]

            if to < i + 1:
                print "missing argument after -%s" % arg
                retval = 5
                break

            i += 1

            names['multiple-modules'] = "%s" % (argv[i])

            retval = 0
            
        elif argv[i] == "-b":
            arg = argv[i][1]

            if to <= i + 1:
                print "missing argument after -%s" % arg
                retval = 5
                break

            i += 1

            names['byte-expand'] = "%s" % (argv[i])

            retval = 0

        elif argv[i] == '--save-callgraph':
            config['save-callgraph'] = True

        else:
            if 'file' in names:
                break
            names['file'] = argv[i]
            retval = 0
        i += 1

    if config['stdlib'] and (config['mode'] is not _eval):
        retval = -1

    if retval == -1:
        print_help(argv)
        retval = 3

    if i <= to: #pending args
        args = argv[i:to]

    return config, names, args, retval

# EOF
