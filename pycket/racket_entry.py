from pycket.prims.linklet import W_Linklet, to_rpython_list, do_compile_linklet, W_LinkletInstance
from pycket.interpreter import check_one_val, Done
from pycket.values import W_Symbol, W_WrappedConsProper, w_null, W_Object, Values, w_false
from pycket.values_string import W_String
from pycket.vector import W_Vector
from pycket.expand import JsonLoader
from pycket.util import console_log

DEBUG = True

def load_bootstrap_linklets(pycketconfig, debug=False):

    console_log("Loading the expander linklet...", debug)

    # load the expander linklet
    expander_instance, sys_config = load_inst_linklet_json("expander.rktl.linklet", pycketconfig, debug)
    expander_instance.provide_all_exports_to_prim_env()

    console_log("Expander loading complete.", debug)

    return sys_config

def load_inst_linklet_json(json_file_name, pycketconfig, debug=False):

    console_log("Loading linklet from %s" % json_file_name, debug)
    linkl, sys_config = W_Linklet.load_linklet(json_file_name, JsonLoader())

    console_log("Instantiating %s ...."  % json_file_name, debug)
    linkl_instance = linkl.instantiate([], config=pycketconfig)

    console_log("DONE.", debug)
    return linkl_instance, sys_config

def initiate_boot_sequence(pycketconfig, command_line_arguments, debug=False):

    sysconfig = load_bootstrap_linklets(pycketconfig, debug)

    # Set the cmd arguments for racket
    ccla = get_primitive("current-command-line-arguments")
    ccla.call_interpret([W_Vector.fromelements(command_line_arguments)], pycketconfig, sysconfig)

    # Run "boot" to set things like (current-module-name-resolver) (o/w it's going to stay as the
    # "core-module-name-resolver" which can't recognize modules like 'racket/base (anything other than
    # things like '#%kernel really)

    console_log("Entering Boot Sequence", debug)

    boot = get_primitive("boot")
    boot.call_interpret([], pycketconfig, sysconfig)

    flcl = get_primitive("find-library-collection-links")
    lib_coll_links = flcl.call_interpret([], pycketconfig, sysconfig)
    clcl = get_primitive("current-library-collection-links")
    clcl.call_interpret([lib_coll_links], pycketconfig, sysconfig)

    flcp = get_primitive("find-library-collection-paths")
    lib_coll_paths = flcp.call_interpret([], pycketconfig, sysconfig)
    clcp = get_primitive("current-library-collection-paths")
    clcp.call_interpret([lib_coll_paths], pycketconfig, sysconfig)

    # don't use compiled code
    ucfp = get_primitive("use-compiled-file-paths")
    ucfp.call_interpret([w_null], pycketconfig, sysconfig)

    console_log("...Boot Sequence Complete", debug)

    return sysconfig

# temporary
def namespace_require_kernel(namespace_require, pycketconfig, sysconfig):
    kernel = W_WrappedConsProper.make(W_Symbol.make("quote"),
                                      W_WrappedConsProper.make(W_Symbol.make("#%kernel"), w_null))
    namespace_require.call_interpret([kernel], pycketconfig, sysconfig)

def racket_entry(names, config, pycketconfig, command_line_arguments):

    require_files, require_libs, load_files, expr_strs, init_library, is_repl, no_lib, run_file_set, just_kernel, debug, version = get_options(names, config)

    sysconfig = initiate_boot_sequence(pycketconfig, command_line_arguments, debug)

    if version:
        v = sysconfig["version"]
        print("Welcome to Pycket v%s" % v)

    namespace_require = get_primitive("namespace-require")

    if run_file_set:
        import os
        if not os.path.isfile(run_file_set):
            raise Exception("File not found : %s" % run_file_set)
        sysconfig['run-file'] = run_file_set

    if just_kernel:
        console_log("Running on just the #%kernel", debug)
        namespace_require_kernel(namespace_require, pycketconfig, sysconfig)

    if not no_lib:
        init_lib = W_WrappedConsProper.make(W_Symbol.make("lib"),
                                            W_WrappedConsProper.make(W_String.make(init_library), w_null))
        console_log("(namespace-require %s) ..." % init_lib.tostring(), debug)

        namespace_require.call_interpret([init_lib], pycketconfig, sysconfig)
        console_log("Init lib : %s loaded..." % init_library, debug)

    if require_files: # -t
        for require_file in require_files:
            # (namespace-require '(file <file-string>))
            file_form = W_WrappedConsProper.make(W_Symbol.make("file"),
                                                 W_WrappedConsProper.make(W_String.make(require_file), w_null))
            namespace_require.call_interpret([file_form], pycketconfig, sysconfig)
    if require_libs: # -l
        for require_lib in require_libs:
            # (namespace-require '(lib <lib-sym>))
            lib_form = W_WrappedConsProper.make(W_Symbol.make("lib"),
                                                W_WrappedConsProper.make(W_String.make(require_lib),
                                                                         w_null))
            namespace_require.call_interpret([lib_form], pycketconfig, sysconfig)
    if load_files: # -f
        for load_file in load_files:
            # (load <file_name>)
            load = get_primitive("load")
            load.call_interpret([W_String.make(load_file)], pycketconfig, sysconfig)

    if expr_strs: # -e
        for expr_str in expr_strs:
            read_eval_print_string(expr_str, pycketconfig, sysconfig)
        
    if is_repl: # -i
        dynamic_require = get_primitive("dynamic-require")
        repl = dynamic_require.call_interpret([W_Symbol.make("racket/base"),
                                               W_Symbol.make("read-eval-print-loop")],
                                              pycketconfig, sysconfig)
        repl.call_interpret([], pycketconfig, sysconfig)

    return 0


def racket_read(input_port, pycketconfig, sysconfig):
    read_prim = get_primitive("read")

    return check_one_val(read_prim.call_interpret([input_port], pycketconfig, sysconfig))

def racket_read_str(expr_str, pycketconfig, sysconfig):
    ois = get_primitive("open-input-string")

    str_port = check_one_val(ois.call_interpret([W_String.make(expr_str)],
                                                pycketconfig, sysconfig))

    return racket_read(str_port, pycketconfig, sysconfig)

def racket_read_file(file_name, pycketconfig, sysconfig):
    oif = get_primitive("open-input-file")

    in_port = oif.call_interpret([W_String.make(file_name)], pycketconfig, sysconfig)

    return racket_read(in_port, pycketconfig, sysconfig)

def racket_eval(sexp, pycketconfig, sysconfig):
    eval_prim = get_primitive("eval")
    return eval_prim.call_interpret([sexp], pycketconfig, sysconfig)

def racket_expand(sexp, pycketconfig, sysconfig):
    ex = get_primitive("expand")
    return check_one_val(ex.call_interpret([sexp], pycketconfig, sysconfig))

def racket_print(results, pycketconfig, sysconfig):
    pr = get_primitive("print")

    pr.call_interpret([W_String.make("\n\n")], pycketconfig, sysconfig)

    if isinstance(results, W_Object):
        # print single
        pr.call_interpret([results], pycketconfig, sysconfig)
    elif isinstance(results, Values):
        # print multiple values
        for r in results.get_all_values():
            pr.call_interpret([r], pycketconfig, sysconfig)
            pr.call_interpret([W_String.make("\n")], pycketconfig, sysconfig)
    else:
        raise Exception("Unsupoorted result value : %s" % results.tostring())
    
    pr.call_interpret([W_String.make("\n\n")], pycketconfig, sysconfig)

def read_eval_print_string(expr_str, pycketconfig, sysconfig, return_val=False):
    # read
    sexp = racket_read_str(expr_str, pycketconfig, sysconfig)

    # expand
    #expanded = racket_expand(sexp, pycketconfig, sysconfig)

    # eval
    results = racket_eval(sexp, pycketconfig, sysconfig)

    if return_val:
        return results

    # print
    racket_print(results, pycketconfig, sysconfig)

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
    run_file_set = names['run-file'][0] if 'run-file' in names else ""

    init_library = names['init-lib'][0] if 'init-lib' in names else "racket/base" # racket/init
    is_repl = config['repl']
    no_lib = config['no-lib']
    just_kernel = config['just_kernel']
    debug = config['verbose']
    version = config['version']

    console_log("Options :\n \nrequire_files : %s\nrequire_libs : %s\nload_files : %s\nexpr_strs : %s\nrun_file_set : %s\ninit_library : %s\nis_repl : %s\nno_lib : %s\njust-#%%kernel : %s" % (require_files, require_libs, load_files, expr_strs, run_file_set, init_library, is_repl, no_lib, just_kernel), debug)
    return require_files, require_libs, load_files, expr_strs, init_library, is_repl, no_lib, run_file_set, just_kernel, debug, version


# maybe we should move this to testhelpers
def run_linklet_file(file_name, pycketconfig, current_cmd_args):

    # CAUTION: doesn't check the format of the file, assumes a lot
    # Make sure Racket runs it without a problem

    # Assumptions:
    # 1) (define (instantiate-linklet ....) doesn't have any target
    # 2) it's gonna return after one targeted instantitation at the toplevel
    # 3) use (list) for empty list

    sysconfig = initiate_boot_sequence(pycketconfig, current_cmd_args)

    # Racket read the module
    module_sexp_ = racket_read_file(file_name, pycketconfig, sysconfig)

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
                racket_print(out_val, pycketconfig, sysconfig)
                return out_val
        else:
            raise Exception("I don't know yet: %s" % expr.tostring())
