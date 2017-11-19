from pycket.prims.linklet import W_Linklet
from pycket.interpreter import check_one_val
from pycket.values import W_Symbol, W_WrappedConsProper, w_null, W_Object, Values
from pycket.values_string import W_String

DEBUG = True

def load_bootstrap_linklets(reader, pycketconfig):
    print("\n\nLoading the expander linklet... \n")
    # load the expander linklet
    expander_linkl, sys_config = W_Linklet.load_linklet("expander.rktl", reader)
    expander_instance = expander_linkl.instantiate([], config=pycketconfig)
    expander_instance.provide_all_exports_to_prim_env()
    print("\nExpander loading complete.\n")

    # Let's stick with the curent regexp-match for now
    # load the regexp linklet
    # regexp_linkl, sys_c = W_Linklet.load_linklet("regexp.rktl", reader)
    # regexp_instance = regexp_linkl.instantiate([], config=pycketconfig)
    # regexp_instance.provide_all_exports_to_prim_env()

    return sys_config

def racket_entry(require_file, load_file, require_lib, expr_str, is_repl, pycketconfig, sysconfig):

    # First run (boot) to set things like (current-module-name-resolver) (o/w it's going to stay as the
    # "core-module-name-resolver" which can't recognize modules like 'racket/base (anything other than
    # things like '#%kernel really)
    
    if DEBUG:
        print("\nEntering Boot Sequence\n")
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

    if DEBUG:
        print("...Boot Sequence Complete\n\n")

    # (namespace-require ''#%kernel) TEMPORARY
    namespace_require = get_primitive("namespace-require")
    q_kernel = W_WrappedConsProper.make(W_Symbol.make("quote"), W_WrappedConsProper.make(W_Symbol.make("#%kernel"), w_null))
    namespace_require.call_interpret([q_kernel], pycketconfig, sysconfig)

    # ns.call_interpret([W_WrappedConsProper.make(W_Symbol.make("lib"),
    #                                               W_WrappedConsProper.make(W_String.make("racket"),
    #                                                                        w_null))], pycketconfig, sysconfig)
    
    if DEBUG:
        print("\n(namespace-require ''#%kernel) ... done\n\n")

    if require_file: # -t
        # (namespace-require '(file <file-string>))
        file_form = W_WrappedConsProper.make(W_Symbol.make("file"),
                                             W_WrappedConsProper.make(W_String.make(require_file), w_null))
        namespace_require.call_interpret([file_form], pycketconfig, sysconfig)
    if require_lib: # -l
        # (namespace-require '(lib <lib-sym>))
        lib_form = W_WrappedConsProper.make(W_Symbol.make("lib"),
                                            W_WrappedConsProper.make(W_String.make(require_lib), w_null))
        namespace_require.call_interpret([lib_form], pycketconfig, sysconfig)
    if load_file: # -f
        # (load <file_name>)
        load = get_primitive("load")
        load.call_interpret([W_String.make(load_file)], pycketconfig, sysconfig)

    if expr_str: # -e
        read_eval_print_string(expr_str, pycketconfig, sysconfig)
        
    if is_repl: # -i
        dynamic_require = get_primitive("dynamic-require")
        repl = dynamic_require.call_interpret([W_Symbol.make("racket/base"), W_Symbol.make("read-eval-print-loop")], pycketconfig, sysconfig)    
        repl.call_interpret([], pycketconfig, sysconfig)

    return 0

def read_eval_print_string(expr_str, pycketconfig, sysconfig):

    # get the read, eval, print, open-input-string primitives
    ev = get_primitive("eval")
    rd = get_primitive("read")
    ex = get_primitive("expand")
    pr = get_primitive("print")
    ois = get_primitive("open-input-string")

    # Start calling
    # open-input-string
    str_port = check_one_val(ois.call_interpret([W_String.make(expr_str)], pycketconfig, sysconfig))
    # read
    sexp = check_one_val(rd.call_interpret([str_port], pycketconfig, sysconfig))
    # expand
    #expanded = check_one_val(ex.call_interpret([sexp], pycketconfig, sysconfig))
    # eval
    results = ev.call_interpret([sexp], pycketconfig, sysconfig)
    
    # print
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
    
    return 0

def get_primitive(prim_name_str):
    from pycket.prims.expose import prim_env
    from pycket.values import W_Symbol
    from pycket.error import SchemeException

    prim_sym = W_Symbol.make(prim_name_str)
    if not prim_sym in prim_env:
        raise SchemeException("Primitive not found : %s" % prim_name_str)

    return prim_env[prim_sym]
