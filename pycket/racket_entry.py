from pycket                 import values, values_string
from pycket.prims.linklet   import W_Linklet
from pycket.interpreter     import check_one_val, Done
from pycket.values          import W_Fixnum, w_null, W_Object, Values, w_false, w_true, W_Path, W_ThreadCell
from pycket.vector          import W_Vector
from pycket.expand          import JsonLoader
from pycket.util            import console_log, linklet_perf, PerfRegion
from pycket.error           import ExitException, SchemeException
from rpython.rlib.debug     import debug_start, debug_stop, debug_print
from pycket.error           import BootstrapError


sym = values.W_Symbol.make
w_str = values_string.W_String.make
lst = values.to_list


# Feature flag for loading io linklet
FFLAG_IO = "io"
FFLAG_THREAD = "thread" # no longer used, kept for debugging

class BootstrapLinklet():
    DIR = "bootstrap-linklets"
    ZO = ".linklet.zo"
    FASL = ".linklet.fasl"
    JSON = ".linklet.json"

    def __init__(self, which_str):
        self.which_str = which_str
        self.file_path = ""
        self.linkl = None
        self.instance = None
        self.load_from = None
        self.is_expander = "expander" in which_str
        self.is_loaded = False
        self.version_sexp = None # w_s-exp

    def locate(self):
        if self.file_path:
            return self.file_path

        console_log("Locating the %s linklet..." % self.which_str)
        possible_exts = [BootstrapLinklet.ZO, BootstrapLinklet.FASL, BootstrapLinklet.JSON]
        for ext in possible_exts:
            try:
                self.file_path = locate_linklet("%s/%s%s" % (BootstrapLinklet.DIR, self.which_str, ext))
                self.load_from = ext
                break
            except BootstrapError:
                continue
        if not self.file_path:
            raise BootstrapError("unable to locate %s linklet" % self.which_str)
        return self.file_path

    # Loads the linklet from the file
    def load_linklet(self):
        if self.linkl:
            return self.linkl

        console_log("Loading the %s linklet..." % self.which_str)

        if not self.file_path:
            self.locate()

        if self.load_from == BootstrapLinklet.ZO or self.load_from == BootstrapLinklet.FASL:
            self.linkl, self.version_sexp = load_linklet_from_fasl(self.file_path, set_version=self.is_expander)
        elif self.load_from == BootstrapLinklet.JSON:
            self.linkl = load_linklet_from_json(self.file_path, set_version=self.is_expander)

        if self.linkl is None:
            raise BootstrapError("loading %s linklet file : %s" % (self.which_str, self.file_path))

        return self.linkl

    # Instantiates the linklet
    def instantiate_linklet(self):
        if self.instance:
            return self.instance

        # Make sure the linklet is loaded
        if not self.linkl:
            self.load_linklet()

        self.instance = _instantiate_linklet(self.file_path, self.linkl)
        return self.instance

    # Exposes the vars to the primitive environment
    def expose(self):
        if self.is_loaded:
            return

        if not self.instance:
            self.instantiate_linklet()

        console_log("Exporting vars of %s linklet." % self.which_str)
        self.instance.expose_vars_to_prim_env()
        self.is_loaded = True

    # Loads and instantiates the linklet, exposes its vars to the primitive
    # environment
    def load(self):
        if self.is_loaded:
            console_log("%s linklet already loaded." % self.which_str)
            return 0

        with PerfRegion("%s-linklet" % self.which_str):
            # Locate the linklet file if needed
            self.locate()

            # Load the linklet from file
            self.load_linklet()

            # Instantiate the linklet
            self.instantiate_linklet()

            # Expose vars to the primitive environment
            self.expose()

            # Flag the expander as loaded if it's the expander linklet
            if self.is_expander:
                from pycket.env import w_global_config
                w_global_config.set_config_val('expander_loaded', 1)

            console_log("DONE loading the %s linklet." % self.which_str)
            return 0

    # Creates a .zo file for the linklet
    def create_zo(self):
        if not self.is_loaded:
            self.load()

        console_log("Creating .zo file for %s linklet." % self.which_str)
        from pycket.ast_vs_sexp import ast_to_sexp
        from pycket.values import W_Cons
        from pycket.prims.input_output import open_outfile

        # Make sure we have the fasl linklet loaded (for s-exp->fasl)
        FASL_LINKLET.load()

        # Make sure the linklet is loaded
        linklet = self.load_linklet()
        linklet_sexp = ast_to_sexp(linklet)

        # s-exp->fasl the linklet into a .zo
        sexp_to_fasl = get_primitive("s-exp->fasl")
        out_port = open_outfile(W_Path("%s/%s" % (BootstrapLinklet.DIR, self.which_str + BootstrapLinklet.ZO)), "w", sym("replace"))

        if self.is_expander:
            sexp_to_fasl.call_interpret([W_Cons.make(self.version_sexp, linklet_sexp), out_port])
        else:
            sexp_to_fasl.call_interpret([linklet_sexp, out_port])
        out_port.close()
        console_log("DONE creating .zo file for %s linklet." % self.which_str)

FASL_LINKLET = BootstrapLinklet('fasl')
EXPANDER_LINKLET = BootstrapLinklet('expander')
REGEXP_LINKLET = BootstrapLinklet('regexp')
IO_LINKLET = BootstrapLinklet('io')
THREAD_LINKLET = BootstrapLinklet('thread')

def locate_linklet(file_name):
    import os

    env_vars = os.environ.keys()
    if "PYTHONPATH" not in env_vars:
        console_log("For PyPy to work with Pycket, PYTHONPATH variable should be set in the environment.. See README", debug=True)
        py_paths = [""] # Try "here", wherever that is, and hope it's CWD
    else:
        python_path = os.environ["PYTHONPATH"]
        py_paths = python_path.split(os.path.pathsep)

    file_path = ""
    for p in py_paths:
        f_path = os.path.join(p, file_name)
        if os.path.exists(f_path):
            file_path = f_path
            break
        # Also try the UP (since PYTHONPATH may only point to the PyPy)
        up_file = os.path.join(p, os.path.join("..", file_name))
        if os.path.exists(up_file):
            file_path = up_file
            break
    else:
        raise BootstrapError("locating linklet : %s" % file_name)

    return file_path

def load_bootstrap_linklets(dont_load_regexp=False, feature_flag=""):

    if not dont_load_regexp:
        # Load regexp linklet
        REGEXP_LINKLET.load()

     # Load thread linklet
    # Feature Flag: thread
    if feature_flag == FFLAG_THREAD:
        THREAD_LINKLET.load()

   # Load io linklet
    # Feature Flag: io
    if feature_flag == FFLAG_IO:
        IO_LINKLET.load()

    # Load fasl linklet
    FASL_LINKLET.load()

    # Load expander linklet
    EXPANDER_LINKLET.load()

    console_log("Bootstrap linklets are ready.")
    return 0

def load_linklet_from_json(file_name, set_version=False):
    debug_start("loading-linklet")
    debug_print("Loading linklet from json -- %s" % file_name)
    linkl, sys_config = W_Linklet.load_linklet(file_name, set_version)
    debug_stop("loading-linklet")
    return linkl

def make_bootstrap_zos():
    # Regexp linklet needs to be fully loaded here. There's a slightly annoying
    # dependency between the regexp linklet and the others. E.g., primitives
    # used within the expander such as  "regexp-match?" are "simple" application
    # in RPython (no env, cont required), but not "simple" when used from the
    # regexp linklet (they're Racket closures). So they're compiled differently.
    # So if the expander is to use the regexp implementation from the regexp
    # linklet then it needs to be compiled in an environment where the
    # primitives like "regexp-match?" are not "simple" so the a-normalization
    # let-bind those. (e.g. (if (regexp-match? ...) ...)). Therefore the regexp
    # linklet needs to expose its functions before we load and compile the
    # expander.
    REGEXP_LINKLET.create_zo()
    THREAD_LINKLET.create_zo()
    FASL_LINKLET.create_zo()


def load_linklet_from_fasl(file_name, set_version=False):
    from pycket.fasl import Fasl
    from pycket.env import w_version
    from pycket.util import console_log
    from pycket.ast_vs_sexp import deserialize_loop

    debug_start("loading-linklet")
    debug_print("Loading linklet from fasl -- %s" % file_name)
    sexp = Fasl().to_sexp_from_file(file_name)
    version_sexp, linklet_sexp = w_str(""), None
    if set_version:
        version_sexp = sexp.car()
        linklet_sexp = sexp.cdr()
    else:
        linklet_sexp = sexp
    linklet = None
    if "zo" in file_name:
        linklet = deserialize_loop(linklet_sexp)
    else:
        console_log("Run pycket with --make-linklet-zos to make the compiled zo files for bootstrap linklets", 1)
        compile_linklet = get_primitive("compile-linklet")
        linklet = compile_linklet.call_interpret([linklet_sexp, sym("linkl"), w_false, w_false, w_false])

    if set_version:
        ver = version_sexp.as_str_ascii()
        console_log("Setting the version to %s" % ver)
        w_version.set_version(ver)

    debug_stop("loading-linklet")
    return linklet, version_sexp

def _instantiate_linklet(file_name_for_log, linkl):
    debug_start("instantiating-linklet")
    debug_print("Instantiating : %s" % file_name_for_log)
    instantiate_linklet = get_primitive("instantiate-linklet")
    linkl_instance = instantiate_linklet.call_interpret([linkl, w_null, w_false, w_false])
    debug_print("DONE Instantiating %s ...."  % file_name_for_log)
    debug_stop("instantiating-linklet")
    return linkl_instance

def load_inst_linklet(file_path, debug=False, set_version=False, expose_vars=False, from_fasl=False):
    from pycket.env import w_version
    linkl = None
    if from_fasl:
        linkl, _ = load_linklet_from_fasl(file_path, set_version)
    else:
        linkl = load_linklet_from_json(file_path, set_version)
    linkl_instance = _instantiate_linklet(file_path, linkl)
    if expose_vars:
        console_log("Exporting vars of %s" % file_path)
        linkl_instance.expose_vars_to_prim_env()
    console_log("DONE with the %s." % file_path)
    return linkl_instance

def load_linklets_at_startup(linklet_file_names):
    for linklet_file in linklet_file_names:
        load_inst_linklet(linklet_file, expose_vars=True)

def set_path(kind_str, path_str):
    import os
    from pycket.racket_paths import racket_sys_paths

    if not os.path.exists(path_str):
        raise Exception("File not found : %s" % path_str)

    racket_sys_paths.set_path(sym(kind_str), W_Path(path_str))

def sample_sexp():
    from pycket.expand import readfile_rpython, getkey
    from pycket import pycket_json

    data = readfile_rpython('sample-module.json')
    json = pycket_json.loads(data)
    ast = JsonLoader().to_ast(json) #module
    return ast.to_sexp()

# WARNING: this will use system's Racket, so check "which racket"
def create_linklet_json(rkt_file_name=""):
    # uses expander's extract to turn it into a linklet
    # gets the bytecodes and uses zo-expand to turn it into a json
    # and loads and instantiates it
    import os
    from pycket.util import os_get_env_var
    from rpython.rlib.rfile import create_popen_file
    PLTHOME = os_get_env_var("PLTHOME")
    EXPANDER_DIR = os.path.join(PLTHOME, os.path.join("racket", os.path.join("src", "expander")))

    # FIXME: error check
    # prep_cmd = "raco make -v %s/bootstrap-run.rkt" % EXPANDER_DIR
    # pipe1 = create_popen_file(prep_cmd, "r")
    # pipe1.read()
    # pipe1.close()
    extract_cmd = "racket -t %s/bootstrap-run.rkt -- -c compiled/cache-src/ ++knot read - -s -x -t %s -o compiled/%s.sexp" % (EXPANDER_DIR, rkt_file_name, rkt_file_name)
    pipe2 = create_popen_file(extract_cmd, "r")
    pipe2.read()
    pipe2.close()
    linklet_json_cmd = "racket linklet-extractor/linklet-sexp-to-json.rkt --output %s.linklet compiled/%s.sexp" % (rkt_file_name, rkt_file_name)
    pipe3 = create_popen_file(linklet_json_cmd, "r")
    pipe3.read()
    pipe3.close()

def dev_mode_metainterp():
    FASL_LINKLET.load()
    sexp_to_fasl = get_primitive("s-exp->fasl")
    fasl_to_sexp = get_primitive("fasl->s-exp")
    sexp = sample_sexp()
    fasl = sexp_to_fasl.call_interpret([sexp])
    sexp_out = fasl_to_sexp.call_interpret([fasl])

# 1) use run-as-linklet to generate a .linklet for a .rkt module that exports a "function : A -> B"
#    use run-as-linklet to generate a .linklet for a .rkt module that exports thunk "input : -> A"
def dev_mode_dynamic_metainterp():
    # trying to get things as close to the
    # 'dev_mode_metainterp_fasl_zo' below,
    # otherwise we could just call the 'function' on the 'input' in
    # the .rkt source and "run-as-linklet" that

    load_inst_linklet("function.rkt.linklet", expose_vars=True)
    load_inst_linklet("input.rkt.linklet", expose_vars=True)

    function = get_primitive("function")
    input_f = get_primitive("input")

    inp = input_f.call_interpret([])
    function.call_interpret([inp])

def dev_mode_metainterp_fasl_zo():
    FASL_LINKLET.load()
    from pycket.prims.input_output import open_infile, open_outfile
    from pycket.values import W_Path
    import os

    # Stuff for writing out the fasl
    if not os.path.exists("sample.fasl"):
        print("Generating sample.fasl first")
        sexp_to_fasl = get_primitive("s-exp->fasl")
        w_replace_sym = sym("replace")
        sexp = sample_sexp()
        out_port = open_outfile(W_Path("sample.fasl"), "w", w_replace_sym)
        sexp_to_fasl.call_interpret([sexp, out_port])
        out_port.close()

    fasl_to_sexp = get_primitive("fasl->s-exp")
    port = open_infile(W_Path("sample.fasl"), "r")
    r = fasl_to_sexp.call_interpret([port, w_true])

def racket_fasl_to_sexp(fasl_file):
    from pycket.prims.input_output import open_infile
    from rpython.rlib        import rtime
    FASL_LINKLET.load()
    fasl_to_sexp = get_primitive("fasl->s-exp")
    port = open_infile(W_Path(fasl_file), "r")
    start_time = rtime.time()
    sexp = fasl_to_sexp.call_interpret([port, w_true])
    console_log("racket fasl->s-exp time : %s" % (rtime.time()-start_time), debug=True)
    console_log("%s" % sexp.tostring(), 1)

def rpython_fasl_to_sexp(fasl_file):
    from rpython.rlib        import rtime
    from pycket.fasl import Fasl
    start_time = rtime.time()
    sexp = Fasl().to_sexp_from_file(fasl_file)
    console_log("rpython fasl->s-exp time : %s" % (rtime.time()-start_time), debug=True)
    console_log("%s" % sexp.tostring(), 1)

def dev_mode_entry_sexp(eval_sexp_str=None):
    from pycket.values import W_Fixnum
    from pycket.util import console_log
    from pycket.prims.linklet import W_LinkletInstance


    from pycket.prims.linklet import do_compile_linklet
    from pycket.env import ToplevelEnv
    from pycket.cont import NilCont

    linkl_sexp = racket_read_str(eval_sexp_str)
    linkl = None
    try:
        do_compile_linklet(linkl_sexp, sym("linkl"), w_false, w_false, w_false, ToplevelEnv(), NilCont())
    except Done as e:
        linkl = e.values

    instantiate_linklet = get_primitive("instantiate-linklet")
    target = W_LinkletInstance(sym("target"), {})

    res = instantiate_linklet.call_interpret([linkl, w_null, target, w_false])
    print("result : %s" % res.tostring())
    raise ExitException(linkl_sexp)

#FIXME : tidy up the arguments (e.g. pass the options dict)
def initiate_boot_sequence(command_line_arguments,
                           use_compiled=False,
                           debug=False,
                           set_run_file="",
                           set_collects_dir="",
                           set_config_dir="",
                           set_addon_dir="",
                           feature_flag="",
                           compile_any=False,
                           dont_load_regexp=False):
    from pycket.env import w_version

    load_bootstrap_linklets(dont_load_regexp=dont_load_regexp, feature_flag=feature_flag)

    with PerfRegion("set-params"):

        # These need to be set before the boot sequence
        if set_run_file:
            console_log("Setting the 'run-file path to %s" % set_run_file)
            set_path("run-file", set_run_file)

        if set_collects_dir:
            console_log("Setting the 'collects-dir path to %s" % set_collects_dir)
            set_path("collects-dir", set_collects_dir)

        if set_config_dir:
            console_log("Setting the 'config-dir path to %s" % set_config_dir)
            set_path("config-dir", set_config_dir)

        if set_addon_dir:
            console_log("Setting the 'addon-dir path to %s" % set_addon_dir)
            set_path("addon-dir", set_addon_dir)

        # Set the cmd arguments for racket
        ccla = get_primitive("current-command-line-arguments")
        ccla.call_interpret([W_Vector.fromelements(command_line_arguments)])

        # Run "boot" to set things like (current-module-name-resolver) (o/w it's going to stay as the
        # "core-module-name-resolver" which can't recognize modules like 'racket/base (anything other than
        # things like '#%kernel really)

        console_log("Entering Boot Sequence")

        console_log("(boot)")
        boot = get_primitive("boot")
        boot.call_interpret([])

        console_log("(current-library-collection-links (find-library-collection-links))")
        flcl = get_primitive("find-library-collection-links")
        lib_coll_links = flcl.call_interpret([])
        clcl = get_primitive("current-library-collection-links")
        clcl.call_interpret([lib_coll_links])

        console_log("(current-library-collection-paths (find-library-collection-paths))")
        flcp = get_primitive("find-library-collection-paths")
        lib_coll_paths = flcp.call_interpret([])
        clcp = get_primitive("current-library-collection-paths")
        clcp.call_interpret([lib_coll_paths])

        console_log("(read-accept-compiled true)")
        read_accept_compiled = get_primitive("read-accept-compiled")
        read_accept_compiled.call_interpret([w_true])

        compiled_file_path = "compiled/pycket"
        ucfp = get_primitive("use-compiled-file-paths")
        if use_compiled:
            console_log("(use-compiled-file-paths %s)" % compiled_file_path)
            ucfp.call_interpret([lst([w_str(compiled_file_path)])])
        else:
            ucfp.call_interpret([w_null])
            console_log("(use-compiled-file-paths null)")

        cctm = get_primitive("current-compile-target-machine")
        if compile_any:
            cctm.call_interpret([w_false])

        # set the current directory to the current directory
        import os
        c_dir = os.getcwd()
        console_log("(current-directory %s)" % c_dir)
        # We can't call the current-directory like a primitive
        # because it prints a report to stdout
        from pycket.values_parameter import top_level_config
        from pycket.prims.general import current_directory_param
        c = top_level_config.get(current_directory_param)
        assert isinstance(c, W_ThreadCell)
        c.set(W_Path(c_dir))

        console_log("...Boot Sequence Completed")
        from pycket.env import w_global_config as glob
        glob.boot_is_completed()

    return 0

# temporary
def namespace_require_kernel():

    namespace_require = get_primitive("namespace-require")
    kernel = lst([sym("quote"), sym("#%kernel")])

    namespace_require.call_interpret([kernel])

need_runtime_configure = [True]

def configure_runtime(m):
    dynamic_require = get_primitive("dynamic-require")
    module_declared = get_primitive("module-declared?")
    join = get_primitive("module-path-index-join")
    submod = lst([sym("submod"), w_str("."), sym("configure-runtime")])

    config_m = join.call_interpret([submod, m])

    if module_declared.call_interpret([config_m, w_true]) is w_true:
        dynamic_require.call_interpret([config_m, w_false])
    # FIXME: doesn't do the old-style language-info stuff

def namespace_require_plus(spec):
    namespace_require = get_primitive("namespace-require")
    dynamic_require = get_primitive("dynamic-require")
    module_declared = get_primitive("module-declared?")
    join = get_primitive("module-path-index-join")
    m = join.call_interpret([spec, w_false])
    submod = lst([sym("submod"), w_str("."), sym("main")])

    # FIXME: configure-runtime
    if need_runtime_configure[0]:
        configure_runtime(m)
        need_runtime_configure[0] = False
    namespace_require.call_interpret([m])
    main = join.call_interpret([submod, m])
    if module_declared.call_interpret([main, w_true]) is w_true:
        dynamic_require.call_interpret([main, w_false])

def dev_mode_entry(dev_mode, eval_sexp, run_rkt_as_linklet):
    if eval_sexp:
        dev_mode_entry_sexp(eval_sexp)
    elif run_rkt_as_linklet:
        create_linklet_json(run_rkt_as_linklet)
        load_inst_linklet("%s.linklet" % run_rkt_as_linklet)
    else:
        dev_mode_dynamic_metainterp()

def racket_entry(names, config, command_line_arguments):
    from pycket.prims.general import executable_yield_handler
    from pycket.values import W_Fixnum
    from pycket.racket_paths import report_racket_paths

    if config['make-zos']:
        make_bootstrap_zos()
        return 0

    linklet_perf.init()

    loads, startup_options, flags = get_options(names, config)

    report_racket_paths()

    init_library     = startup_options['init_library'][0]
    set_run_file     = startup_options['set_run_file'][0]
    set_collects_dir = startup_options['set_collects_dir'][0]
    set_config_dir   = startup_options['set_config_dir'][0]
    set_addon_dir    = startup_options['set_addon_dir'][0]
    eval_sexp        = startup_options['eval_sexp'][0]
    run_as_linklet   = startup_options['run_as_linklet'][0]
    load_linklets    = startup_options['load_linklets']
    load_as_linklets = startup_options['load_as_linklets']
    fasl_file        = startup_options['fasl-file'][0]
    feature_flag     = startup_options['feature-flag'][0]

    # TODO: (cderici - 04-17-2025): The entire config handling in
    # the frontend needs to be revisited. There should be local-global
    # config that should be passed around, instead of these
    # flags, startup_options, names, config...
    # For instance, the verbosity_keywords are never used here,
    # but it's passed around and accessed just for printing in get_options.

    is_repl          = flags['repl']
    no_lib           = flags['no-lib']
    just_kernel      = flags['just_kernel']
    just_init        = flags['just-init']
    use_compiled     = flags['use-compiled']
    debug            = flags['verbose']
    version          = flags['version']
    c_a              = flags['compile-machine-independent']
    dont_load_regexp = flags['no-regexp']
    dev_mode         = flags['dev-mode']
    racket_fasl      = flags['racket-fasl']
    rpython_fasl     = flags['rpython-fasl']

    if load_as_linklets:
        for rkt in load_as_linklets:
            create_linklet_json(rkt)
            load_inst_linklet("%s.linklet" % rkt)
        return 0

    if load_linklets:
        load_linklets_at_startup(load_linklets)
        return 0

    if dev_mode:
        dev_mode_entry(dev_mode, eval_sexp, run_as_linklet)
        return 0

    if racket_fasl:
        racket_fasl_to_sexp(fasl_file)
        return 0

    if rpython_fasl:
        rpython_fasl_to_sexp(fasl_file)
        return 0

    with PerfRegion("startup"):
        initiate_boot_sequence(command_line_arguments,
                               use_compiled,
                               debug,
                               set_run_file,
                               set_collects_dir,
                               set_config_dir,
                               set_addon_dir,
                               feature_flag,
                               compile_any=c_a,
                               dont_load_regexp=dont_load_regexp)

    if just_init:
        return 0

    namespace_require = get_primitive("namespace-require")
    load = get_primitive("load")

    if just_kernel:
        console_log("Running on just the #%kernel")
        namespace_require_kernel()

    if not no_lib:
        with PerfRegion("init-lib"):
            init_lib = lst([sym("lib"), w_str(init_library)])
            console_log("(namespace-require %s) ..." % init_lib.tostring())

            namespace_require_plus(init_lib)
            console_log("Init lib : %s loaded..." % (init_library))

    put_newline = False
    if loads:
        for rator_str, rand_str in loads:
            if rator_str == "load":
                # -f
                console_log("(load %s)" % (rand_str))
                load.call_interpret([w_str(rand_str)])
            elif rator_str == "file" or rator_str == "lib":
                # -t & -l
                require_spec = lst([sym(rator_str), w_str(rand_str)])
                console_log("(namespace-require '(%s %s))" % (rator_str, rand_str))
                namespace_require_plus(require_spec)
            elif rator_str == "eval":
                # -e
                console_log("(eval (read (open-input-string %s)))" % rand_str)
                read_eval_print_string(rand_str, False, debug)

    if version:
        from pycket.env import w_version
        print("Welcome to Pycket v%s" % w_version.get_version())

    if is_repl: # -i
        put_newline = True

    if THREAD_LINKLET.is_exposed:
        repl_thunk = read_eval_print_string("(lambda () ((dynamic-require 'racket/repl 'read-eval-print-loop)))", return_val=True)
        assert isinstance(repl_thunk, W_Object), "repl_thunk is %s -- %s" % (repl_thunk, type(repl_thunk))
        call_in_main_thread = get_primitive("call-in-main-thread")
        call_in_main_thread.call_interpret([repl_thunk])
    else:
        dynamic_require = get_primitive("dynamic-require")
        repl = dynamic_require.call_interpret([sym("racket/repl"),
                                               sym("read-eval-print-loop")])
        from pycket.env import w_global_config
        w_global_config.set_config_val('repl_loaded', 1)
        repl.call_interpret([])

    if put_newline:
        print


    linklet_perf.print_report()

    # we just want the global value anyway
    eyh = executable_yield_handler.call_interpret([])
    eyh.call_interpret([W_Fixnum.ZERO])

    exit = get_primitive("exit")
    exit.call_interpret([])

    return 0


def racket_read(input_port):
    read_prim = get_primitive("read")

    return check_one_val(read_prim.call_interpret([input_port]))

def racket_read_str(expr_str):
    ois = get_primitive("open-input-string")

    str_port = check_one_val(ois.call_interpret([w_str(expr_str)]))

    return racket_read(str_port)

def racket_read_file(file_name):
    oif = get_primitive("open-input-file")

    in_port = oif.call_interpret([w_str(file_name)])

    return racket_read(in_port)

def racket_eval(sexp):
    eval_prim = get_primitive("eval")
    return eval_prim.call_interpret([sexp])

def racket_read_eval(expr_str):
    sexp = racket_read_str(expr_str)
    return racket_eval(sexp)

def racket_expand(sexp):
    ex = get_primitive("expand")
    return check_one_val(ex.call_interpret([sexp]))

def racket_print(results):
    pr = get_primitive("print")

    if isinstance(results, W_Object):
        # print single
        pr.call_interpret([results])
        print
    elif isinstance(results, Values):
        # print multiple values
        for r in results.get_all_values():
            pr.call_interpret([r])
            print
    else:
        raise Exception("Unsupoorted result value : %s" % results.tostring())

def read_eval_print_string(expr_str, return_val=False, debug=False):
    # read
    try:
        sexp = racket_read_str(expr_str)
    except Exception as e:
        raise SchemeException("Error (Racket) reading : %s\n%s\n" % (expr_str, e))

    # expand
    expanded = racket_expand(sexp)

    # eval
    try:
        results = racket_eval(sexp)
    except Exception as e:
        raise SchemeException("Error (Racket) evaluating : %s\n%s\n" % (expr_str, e))

    if return_val:
        return results

    # print
    racket_print(results)

    # FIXME
    console_log("(print (eval (read (open-input-string %s))))" % (expr_str))
    return W_Fixnum.ZERO

def get_primitive(prim_name_str):
    from pycket.prims.expose import prim_env
    from pycket.error import SchemeException

    prim_sym = sym(prim_name_str)
    if not prim_sym in prim_env:
        raise SchemeException("Primitive not found : %s" % prim_name_str)

    return prim_env[prim_sym]

def get_options(names, config):

    is_repl = config.get('repl', False)
    no_lib = config.get('no-lib', False)
    just_kernel = config.get('just_kernel', False)
    just_init = config.get('just-init', False)
    use_compiled = config.get('use-compiled', False)
    debug = config.get('verbose', False)
    version = config.get('version', False)
    compile_any = config.get('compile-machine-independent', False)
    dont_load_regexp = config.get('no-regexp', False)
    dev_mode = config.get('dev-mode', False)
    racket_fasl      = config.get('racket-fasl', False)
    rpython_fasl     = config.get('rpython-fasl', False)

    load_rators = names.get('loads', [])
    load_rands = names.get('load_arguments', [])
    set_run_file = names.get('set-run-file', [""])
    set_collects_dir = names.get('set-collects-dir', [""])
    set_config_dir = names.get('set-config-dir', [""])
    set_addon_dir = names.get('set-addon-dir', [""])
    init_library = names.get('init-lib', ["racket/base"]) # racket/init
    verbosity_lvl = int(names['verbosity_level'][0]) if debug else -1
    verbosity_keywords = names.get('verbosity_keywords', [])
    feature_flag = names.get('feature_flag', [""])
    eval_sexp = names.get('eval-sexp', [""])
    run_as_linklet = names.get('run-as-linklet', [""])
    load_linklets = names.get('load-linklets', [])
    load_as_linklets = names.get('load-as-linklets', [])
    fasl_file = names.get('fasl-file', [""])

    loads_print_str = []
    loads = []
    for index, rator in enumerate(load_rators):
        rand = load_rands[index]
        loads_print_str.append("(%s %s)" % (rator, rand))
        loads.append((rator, rand))

    log_str = """Options :

loads              : %s
init_library       : %s
set-run-file       : %s
set-collects-dir   : %s
set-config-dir     : %s
set-addon-dir      : %s
eval-s-sexp        : %s
run-as-linklet     : %s
load-linklets      : %s
load-as-linklets   : %s

is_repl            : %s
no_lib             : %s
just-#%%kernel      : %s
just-init          : %s
use-compiled       : %s
verbosity-level    : %s
verbosity-keywords : %s
dev-mode           : %s
feature-flag       : %s
""" % (loads_print_str,
       init_library[0],
       set_run_file[0],
       set_collects_dir[0],
       set_config_dir[0],
       set_addon_dir[0],
       eval_sexp[0],
       run_as_linklet[0],
       load_linklets,
       load_as_linklets,

       is_repl,
       no_lib,
       just_kernel,
       just_init,
       use_compiled,
       verbosity_lvl,
       verbosity_keywords,
       dev_mode,
       feature_flag[0]
       )

    console_log(log_str, debug=debug)

    startup_options = {
        'init_library'     : init_library,
        'set_run_file'     : set_run_file,
        'set_collects_dir' : set_collects_dir,
        'set_config_dir'   : set_config_dir,
        'set_addon_dir'    : set_addon_dir,
        'eval_sexp'        : eval_sexp,
        'run_as_linklet'   : run_as_linklet,
        'load_linklets'    : load_linklets,
        'load_as_linklets' : load_as_linklets,
        'fasl-file'        : fasl_file,
        'feature-flag'     : feature_flag,
    }

    return loads, startup_options, config
