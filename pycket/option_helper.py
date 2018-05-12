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
  --kernel : ignore everything, only load the #%%kernel (for development purposes)
  -I <path> : Set <init-lib> to <path> (sets language)
  -X <dir>, --collects <dir> : Main collects at <dir> (or "" disables all)
  -G <dir>, --config <dir> : Main configuration directory at <dir>
  -A <dir>, --addon <dir> : Addon directory at <dir>
  -N <file>, --name <file> : Sets `(find-system-path 'run-file)' to <file>
  --save-callgraph : save the jit output

 Meta options:
  --verbose <level> : Print the debug logs. <level> : [0,1] (defaults to 0)
  --jit <jitargs> : Set RPython JIT options may be 'default', 'off',
                    or 'param=value,param=value' list
  -- : No argument following this switch is used as a switch
  -h, --help : Show this information and exits, ignoring other options
Default options:
 If only an argument is specified, it is loaded and evaluated
""" % (argv[0])

file_expr_opts = ["-e", "--eval",
                  "-f", "--load",
                  "-t", "--require",
                  "-l", "--lib",
                  "-r", "--script",
                  "-u", "--require-script",
                  "-g", "--eval-json",
                  "-gg"]
inter_opts = ["-i", "--repl",
              "-n", "--no-lib",
              "-v", "--version"]
conf_opts = ["--kernel",
             "-I",
             "-X", "--collects",
             "-G", "--config",
             "-A", "--addon",
             "-N", "--name",
             "--save-callgraph"]
meta_opts = ["--verbose", "--jit", "-h"]

all_opts = file_expr_opts + inter_opts + conf_opts + meta_opts

verbosity_levels = ['0','1']

def add_name(names, name, val, replace=False):
    if name not in names or replace:
        names[name] = [val]
    else:
        names[name].append(val)

def is_rkt_file(name_str):
    return ".rkt" in name_str

def parse_args(argv):
    INIT = -1
    RETURN_OK = 0
    MISSING_ARG = 5
    JUST_EXIT = 3

    config = {
        'repl' : False,
        'no-lib' : False,
        'version' : False,
        'stop' : False,
        'just_kernel' : False,
        'verbose' : False
    }
    names = {
        # 'file': "",
        # 'exprs': "",
    }
    args = []
    retval = INIT
    i = 1
    to = len(argv)
    while i < to:
        if False:
            pass

        elif argv[i] in ["-e", "--eval"]:
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after %s" % argv[i]
                retval = MISSING_ARG
                break

            i += 1
            add_name(names, 'exprs', argv[i])
            retval = RETURN_OK

        elif argv[i] in ["-f", "--load", "-r", "--script"]:
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after %s" % argv[i]
                retval = MISSING_ARG
                break

            if argv[i] in ["r", "--script"]:
                names['run-file'] = [argv[i]]

            i += 1
            add_name(names, 'load-file', argv[i])
            retval = RETURN_OK

        elif argv[i] in ["-t", "--require", "-u", "--require-script"]:
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after %s" % argv[i]
                retval = MISSING_ARG
                break

            if argv[i] in ["-u", "--require-script"]:
                names['run-file'] = [argv[i]]

            i += 1
            add_name(names, 'req-file', argv[i])

            config['no-lib'] = True
            retval = RETURN_OK

        elif argv[i] in ["-l", "--lib"]:
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after %s" % argv[i]
                retval = MISSING_ARG
                break

            i += 1
            add_name(names, 'req-lib', argv[i])

            config['no-lib'] = True
            retval = RETURN_OK

        elif argv[i] in ["-g", "-gg", "--eval-json"]:
            if argv[i] == "-gg":
                config['stop'] = True
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after %s" % argv[i]
                retval = MISSING_ARG
                break

            i += 1
            add_name(names, 'json-linklets', argv[i])

            config['no-lib'] = True
            retval = RETURN_OK

        #########################
        # Interaction Options
        #########################

        elif argv[i] in ["-i", "--repl"]:
            config['repl'] = True
            config['version'] = True
            retval = RETURN_OK

        elif argv[i] in ["-n", "--no-lib"]:
            config['no-lib'] = True
            if retval == INIT:
                retval = JUST_EXIT

        elif argv[i] in ["-v", "--version"]:
            config['version'] = True

        #########################
        # Configuration Options
        #########################

        elif argv[i] == "--kernel":
            config['just_kernel'] = True
            config['no-lib'] = True
            retval = RETURN_OK

        elif argv[i] == "-I":
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after -I"
                retval = MISSING_ARG
                break
            i += 1
            add_name(names, 'init-lib', argv[i])

        elif argv[i] in ["-X", "--collects"]:
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after %s" % argv[i]
                retval = MISSING_ARG
                break
            i += 1
            add_name(names, 'set-collects-dir', argv[i])

        elif argv[i] in ["-G", "--config"]:
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after %s" % argv[i]
                retval = MISSING_ARG
                break
            i += 1
            add_name(names, 'set-config-dir', argv[i])

        elif argv[i] in ["-A", "--addon"]:
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after %s" % argv[i]
                retval = MISSING_ARG
                break
            i += 1
            add_name(names, 'set-addon-dir', argv[i])

        elif argv[i] in ["-N", "--name"]:
            if to <= i + 1 or argv[i+1] in all_opts:
                print "missing argument after %s" % argv[i]
                retval = MISSING_ARG
                break
            i += 1
            add_name(names, 'set-run-file', argv[i])

        elif argv[i] == '--save-callgraph':
            config['save-callgraph'] = True

        #########################
        # Meta Options
        #########################

        elif argv[i] == "--verbose":
            config['verbose'] = True
            add_name(names, 'verbosity_level', '0')
            if to > i + 1 and not is_rkt_file(argv[i+1]) and argv[i+1] not in all_opts:
                vl = argv[i+1]
                if vl not in verbosity_levels:
                    print("--verbose <level> can only be one of : %s" % verbosity_levels)
                    retval = MISSING_ARG
                    break
                add_name(names, 'verbosity_level', vl, replace=True)
                i += 1

        elif argv[i] == "--jit":
            if to <= i + 1:
                print "missing argument after --jit"
                retval = 2
                break
            i += 1
            jitarg = argv[i]
            jit.set_user_param(None, jitarg)

        elif argv[i] == "--":
            i += 1
            break

        elif argv[i] in ["-h", "--help", "/?", "-?", "/h", "/help"]:
            print_help(argv)
            if retval == INIT:
                retval = JUST_EXIT

        else:
            if '.rkt' in argv[i]:
                add_name(names, 'req-file', argv[i])
                config['no-lib'] = True

                i += 1
                retval = RETURN_OK
            else:
                print "Bad switch : %s" % argv[i]
                retval = MISSING_ARG
            break
        i += 1

    if retval == INIT:
        config['repl'] = True
        config['version'] = True
        retval = RETURN_OK

    if i <= to: #pending args
        args = argv[i:to]

    return config, names, args, retval

# EOF
