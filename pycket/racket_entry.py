from pycket.prims.linklet import W_Linklet, to_rpython_list, do_compile_linklet, W_LinkletInstance
from pycket.interpreter import check_one_val, Done
from pycket.values import W_Symbol, W_WrappedConsProper, w_null, W_Object, Values, w_false, W_Path
from pycket.values_string import W_String
from pycket.vector import W_Vector
from pycket.expand import JsonLoader
from pycket.util import console_log
from pycket.prims.correlated import syntax_primitives

def locate_linklet(file_name):
    import os
    from pycket.error import SchemeException

    env_vars = os.environ.keys()
    if "PYTHONPATH" not in env_vars:
        raise SchemeException("For PyPy to work with Pycket, PYTHONPATH variable should be set in the environment.. See README")

    python_path = os.environ["PYTHONPATH"]
    py_paths = python_path.split(':')

    file_path = ""
    for p in py_paths:
        f_path = os.path.join(p, file_name)
        if os.path.exists(f_path):
            file_path = f_path
            break
        # Also try the UP (since PYTHONPATH may only point to the PyPy)
        up_file = os.path.join(p, "../" + file_name)
        if os.path.exists(up_file):
            file_path = up_file
            break
    else:
        raise SchemeException("Can't locate the : %s" % file_name)

    return file_path

def load_bootstrap_linklets(pycketconfig, debug=False):

    console_log("Loading the expander linklet...", debug)
    expander_file_path = locate_linklet("expander.rktl.linklet")

    # load the expander linklet
    expander_instance, sys_config = load_inst_linklet_json(expander_file_path, pycketconfig, debug)
    expander_instance.provide_all_exports_to_prim_env(excludes=syntax_primitives)

    console_log("Expander loading complete.", debug)

    return sys_config

def load_inst_linklet_json(json_file_name, pycketconfig, debug=False):

    console_log("Loading linklet from %s" % json_file_name, debug)
    linkl, sys_config = W_Linklet.load_linklet(json_file_name, JsonLoader())

    console_log("Instantiating %s ...."  % json_file_name, debug)
    linkl_instance = linkl.instantiate([], config=pycketconfig)

    console_log("DONE.", debug)
    return linkl_instance, sys_config

def set_path(kind_str, path_str):
    import os
    from pycket.racket_paths import racket_sys_paths

    if not os.path.exists(path_str):
        raise Exception("File not found : %s" % path_str)

    racket_sys_paths.set_path(W_Symbol.make(kind_str), W_Path(path_str))

def initiate_boot_sequence(pycketconfig, command_line_arguments, debug=False, set_run_file="", set_collects_dir="", set_config_dir="", set_addon_dir=""):
    from pycket.env import w_version

    sysconfig = load_bootstrap_linklets(pycketconfig, debug)

    v = sysconfig["version"]
    console_log("Setting the version to %s" % v, debug)
    w_version.set_version(v)

    # These need to be set before the boot sequence
    if set_run_file:
        console_log("Setting the 'run-file path to %s" % set_run_file, debug)
        set_path("run-file", set_run_file)

    if set_collects_dir:
        console_log("Setting the 'collects-dir path to %s" % set_collects_dir, debug)
        set_path("collects-dir", set_collects_dir)

    if set_config_dir:
        console_log("Setting the 'config-dir path to %s" % set_config_dir, debug)
        set_path("config-dir", set_config_dir)

    if set_addon_dir:
        console_log("Setting the 'addon-dir path to %s" % set_addon_dir, debug)
        set_path("addon-dir", set_addon_dir)

    # Set the cmd arguments for racket
    ccla = get_primitive("current-command-line-arguments")
    ccla.call_interpret([W_Vector.fromelements(command_line_arguments)], pycketconfig)

    # Run "boot" to set things like (current-module-name-resolver) (o/w it's going to stay as the
    # "core-module-name-resolver" which can't recognize modules like 'racket/base (anything other than
    # things like '#%kernel really)

    console_log("Entering Boot Sequence", debug)

    boot = get_primitive("boot")
    boot.call_interpret([], pycketconfig)

    flcl = get_primitive("find-library-collection-links")
    lib_coll_links = flcl.call_interpret([], pycketconfig)
    clcl = get_primitive("current-library-collection-links")
    clcl.call_interpret([lib_coll_links], pycketconfig)

    flcp = get_primitive("find-library-collection-paths")
    lib_coll_paths = flcp.call_interpret([], pycketconfig)
    clcp = get_primitive("current-library-collection-paths")
    clcp.call_interpret([lib_coll_paths], pycketconfig)

    # don't use compiled code
    ucfp = get_primitive("use-compiled-file-paths")
    ucfp.call_interpret([w_null], pycketconfig)

    console_log("...Boot Sequence Complete", debug)

    return 0

# temporary
def namespace_require_kernel(pycketconfig):

    namespace_require = get_primitive("namespace-require")

    kernel = W_WrappedConsProper.make(W_Symbol.make("quote"),
                                      W_WrappedConsProper.make(W_Symbol.make("#%kernel"), w_null))
    namespace_require.call_interpret([kernel], pycketconfig)

def racket_entry(names, config, pycketconfig, command_line_arguments):

    require_files, require_libs, load_files, expr_strs, init_library, is_repl, no_lib, set_run_file, set_collects_dir, set_config_dir, set_addon_dir, just_kernel, debug, version = get_options(names, config)

    initiate_boot_sequence(pycketconfig, command_line_arguments, debug, set_run_file, set_collects_dir, set_config_dir, set_addon_dir)

    if version:
        from pycket.env import w_version
        print("Welcome to Pycket v%s" % w_version.get_version())

    namespace_require = get_primitive("namespace-require")

    if just_kernel:
        console_log("Running on just the #%kernel", debug)
        namespace_require_kernel(pycketconfig)

    if not no_lib:
        init_lib = W_WrappedConsProper.make(W_Symbol.make("lib"),
                                            W_WrappedConsProper.make(W_String.make(init_library), w_null))
        console_log("(namespace-require %s) ..." % init_lib.tostring(), debug)

        namespace_require.call_interpret([init_lib], pycketconfig)
        console_log("Init lib : %s loaded..." % init_library, debug)

    if require_files: # -t
        for require_file in require_files:
            # (namespace-require '(file <file-string>))
            file_form = W_WrappedConsProper.make(W_Symbol.make("file"),
                                                 W_WrappedConsProper.make(W_String.make(require_file), w_null))
            namespace_require.call_interpret([file_form], pycketconfig)
    if require_libs: # -l
        for require_lib in require_libs:
            # (namespace-require '(lib <lib-sym>))
            lib_form = W_WrappedConsProper.make(W_Symbol.make("lib"),
                                                W_WrappedConsProper.make(W_String.make(require_lib),
                                                                         w_null))
            namespace_require.call_interpret([lib_form], pycketconfig)
    if load_files: # -f
        for load_file in load_files:
            # (load <file_name>)
            load = get_primitive("load")
            load.call_interpret([W_String.make(load_file)], pycketconfig)

    if expr_strs: # -e
        for expr_str in expr_strs:
            read_eval_print_string(expr_str, pycketconfig)
        
    if is_repl: # -i
        dynamic_require = get_primitive("dynamic-require")
        repl = dynamic_require.call_interpret([W_Symbol.make("racket/private/misc"),
                                               W_Symbol.make("read-eval-print-loop")],
                                              pycketconfig)
        repl.call_interpret([], pycketconfig)

    return 0


def racket_read(input_port, pycketconfig):
    read_prim = get_primitive("read")

    return check_one_val(read_prim.call_interpret([input_port], pycketconfig))

def racket_read_str(expr_str, pycketconfig):
    ois = get_primitive("open-input-string")

    str_port = check_one_val(ois.call_interpret([W_String.make(expr_str)], pycketconfig))

    return racket_read(str_port, pycketconfig)

def racket_read_file(file_name, pycketconfig):
    oif = get_primitive("open-input-file")

    in_port = oif.call_interpret([W_String.make(file_name)], pycketconfig)

    return racket_read(in_port, pycketconfig)

def racket_eval(sexp, pycketconfig):
    eval_prim = get_primitive("eval")
    return eval_prim.call_interpret([sexp], pycketconfig)

def racket_expand(sexp, pycketconfig):
    ex = get_primitive("expand")
    return check_one_val(ex.call_interpret([sexp], pycketconfig))

def racket_print(results, pycketconfig):
    pr = get_primitive("print")

    pr.call_interpret([W_String.make("\n\n")], pycketconfig)

    if isinstance(results, W_Object):
        # print single
        pr.call_interpret([results], pycketconfig)
    elif isinstance(results, Values):
        # print multiple values
        for r in results.get_all_values():
            pr.call_interpret([r], pycketconfig)
            pr.call_interpret([W_String.make("\n")], pycketconfig)
    else:
        raise Exception("Unsupoorted result value : %s" % results.tostring())
    
    pr.call_interpret([W_String.make("\n\n")], pycketconfig)

def read_eval_print_string(expr_str, pycketconfig, return_val=False):
    # read
    sexp = racket_read_str(expr_str, pycketconfig)

    # expand
    #expanded = racket_expand(sexp, pycketconfig)

    # eval
    results = racket_eval(sexp, pycketconfig)

    if return_val:
        return results

    # print
    racket_print(results, pycketconfig)

    return 0

def get_primitive(prim_name_str):
    from pycket.prims.expose import prim_env
    from pycket.error import SchemeException

    prim_sym = W_Symbol.make(prim_name_str)
    if not prim_sym in prim_env:
        raise SchemeException("Primitive not found : %s" % prim_name_str)

    return prim_env[prim_sym]

def get_options(names, config):

    require_files = names['req-file'] if 'req-file' in names else []
    require_libs = names['req-lib'] if 'req-lib' in names else []
    load_files = names['load-file'] if 'load-file' in names else []
    expr_strs = names['exprs'] if 'exprs' in names else []
    set_run_file = names['set-run-file'][0] if 'set-run-file' in names else ""
    set_collects_dir = names['set-collects-dir'][0] if 'set-collects-dir' in names else ""
    set_config_dir = names['set-config-dir'][0] if 'set-config-dir' in names else ""
    set_addon_dir = names['set-addon-dir'][0] if 'set-addon-dir' in names else ""

    init_library = names['init-lib'][0] if 'init-lib' in names else "racket/base" # racket/init
    is_repl = config['repl']
    no_lib = config['no-lib']
    just_kernel = config['just_kernel']
    debug = config['verbose']
    version = config['version']

    log_str = """Options :

require_files : %s
require_libs : %s
load_files : %s
expr_strs : %s
set-run-file : %s
set-collects-dir : %s
set-config-dir : %s
set-addon-dir : %s
init_library : %s
is_repl : %s
no_lib : %s
just-#%%kernel : %s
""" % (require_files,
       require_libs,
       load_files,
       expr_strs,
       set_run_file,
       set_collects_dir,
       set_config_dir,
       set_addon_dir,
       init_library,
       is_repl,
       no_lib,
       just_kernel)

    console_log(log_str, debug)

    return require_files, require_libs, load_files, expr_strs, init_library, is_repl, no_lib, set_run_file, set_collects_dir, set_config_dir, set_addon_dir, just_kernel, debug, version


# maybe we should move this to testhelpers
def run_linklet_file(file_name, pycketconfig, current_cmd_args):

    # CAUTION: doesn't check the format of the file, assumes a lot
    # Make sure Racket runs it without a problem

    # Assumptions:
    # 1) (define (instantiate-linklet ....) doesn't have any target
    # 2) it's gonna return after one targeted instantitation at the toplevel
    # 3) use (list) for empty list

    initiate_boot_sequence(pycketconfig, current_cmd_args)

    # Racket read the module
    module_sexp_ = racket_read_file(file_name, pycketconfig)

    # get the module body
    module_sexp = module_sexp_.cdr().cdr().cdr().cdr()
    exprs = to_rpython_list(module_sexp)

    from pycket.env import ToplevelEnv
    from pycket.interpreter import NilCont

    # Start going through the expressions at the toplevel

    w_linklets = {}
    instances = {}

    for expr in exprs:
        if "define" in expr.car().tostring():
            first_op = expr.cdr().cdr().car().car()
            if "compile-linklet" in first_op.tostring():
                """
                (define l-0
                  (compile-linklet
                    (datum->correlated
                      (quote
                        (linklet .......)))))
                """
                linkl_name = expr.cdr().car()
                linkl_sexp = expr.cdr().cdr().car().cdr().car().cdr().car().cdr().car() # Trust me :)
                # create the W_Linklet
                linkl = None
                try:
                    do_compile_linklet(linkl_sexp, linkl_name, w_false, w_false, w_false, ToplevelEnv(pycketconfig), NilCont())
                except Done, e:
                    linkl = e.values
                w_linklets[linkl_name] = linkl

            elif "instantiate-linklet" in first_op.tostring():
                # (define inst-0 (instantiate-linklet l-0 (list inst-1)))
                imp_names = to_rpython_list(expr.cdr().cdr().car().cdr().cdr().car().cdr())
                inst_name = expr.cdr().car()
                linkl_name = expr.cdr().cdr().car().cdr().car()

                if linkl_name not in w_linklets:
                    raise Exception("linklet is not yet defined : %s" % linkl_name.tostring())

                imp_insts = []
                for imp_name in imp_names:
                    if imp_name not in instances:
                        raise Exception("required instance is not instantiated : %s" % imp_name.tostring())
                    imp_insts.append(instances[imp_name])

                linkl = w_linklets[linkl_name]
                instances[inst_name] = linkl.instantiate(imp_insts, pycketconfig)
            else:
                raise Exception("I don't know yet: %s" % expr.tostring())

        elif "instantiate-linklet" in expr.car().tostring():
            # see it there's a target
            if expr.cdr().cdr().cdr() is w_null:
                # no target, don't care, well should we? #FIXME
                continue
            else:
                # (instantiate-linklet l-0 (list inst-1) target)
                linkl_name = expr.cdr().car()
                imp_names = to_rpython_list(expr.cdr().cdr().car().cdr())
                target_inst_name = expr.cdr().cdr().cdr().car()

                if linkl_name not in w_linklets:
                    raise Exception("linklet is not yet defined : %s" % linkl_name.tostring())

                linkl = w_linklets[linkl_name]

                imp_insts = []
                for imp_name in imp_names:
                    if imp_name not in instances:
                        raise Exception("required instance is not instantiated : %s" % imp_name.tostring())
                    imp_insts.append(instances[imp_name])

                if target_inst_name not in instances:
                    raise Exception("target instance is not instantiated : %s" % target_inst_name.tostring())
                target_inst = instances[target_inst_name]

                out_val = linkl.instantiate(imp_insts, pycketconfig, toplevel_eval=True, prompt=False, target=target_inst)
                racket_print(out_val, pycketconfig)
                return out_val
        else:
            raise Exception("I don't know yet: %s" % expr.tostring())
