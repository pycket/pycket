import sys
from rpython.rlib             import jit, objectmodel
from pycket.small_list        import inline_small_list
from pycket.error             import SchemeException
from pycket.base              import W_Object
from pycket.callgraph         import CallGraph
from pycket.config            import get_testing_config

MIN_INT = -sys.maxint-1

class SymList(object):
    _immutable_fields_ = ["elems[*]", "prev"]

    def __init__(self, elems, prev=None):
        assert isinstance(elems, list)
        self.elems = elems
        self.prev = prev

    def check_plausibility(self, env):
        if self.elems:
            assert len(self.elems) == env.consenv_get_size()
        if self.prev:
            self.prev.check_plausibility(env.get_prev(self))

    @jit.unroll_safe
    def find_env_in_chain_speculate(self, target, env_structure, env):
        """
        find env 'target' of shape 'self' in environment chain 'env', described
        by 'env_structure'. We use the env structures to check for candidates
        of sharing. Only if the env structure that the lambda is defined in
        matches some outer env structure where it is called does it make sense
        to check if the *actual* envs match. this means that the speculation is
        essentially free:
        the env structures are known, so checking for sharing inside them is
        computed by the JIT. thus only an environment identity check that is
        very likely to succeed is executed.
        """
        jit.promote(self)
        jit.promote(env_structure)
        while env_structure is not None:
            if env_structure is self:
                if env is target:
                    return env
            env = env.get_prev(env_structure)
            env_structure = env_structure.prev
        return target

    def depth_and_size(self):
        depth = 0
        size = 0
        while self is not None:
            size += len(self.elems)
            self = self.prev
            depth += 1
        return depth, size

    def depth_of_var(self, var):
        depth = 0
        while self is not None:
            for i, x in enumerate(self.elems):
                if x is var:
                    return i, depth
            self = self.prev
            depth += 1
        return -1, -1

    @jit.unroll_safe
    def contains_sym(self, var):
        for e in self.elems:
            if e is var:
                return True
        return False

    def drop_frames(self, n):
        for i in range(n):
            self = self.prev
        return self

    def __repr__(self):
        return "SymList(%r, %r)" % (self.elems, self.prev)

class ModuleEnv(object):
    _immutable_fields_ = ["modules", "toplevel_env"]
    def __init__(self, toplevel_env):
        self.modules = {}
        self.current_module = None
        self.toplevel_env = toplevel_env

    def require(self, module_name):
        assert 0
        # load the file, evaluate it, register it in the table

    def add_module(self, name, module):
        from pycket.interpreter import Module
        # note that `name` and `module.name` are different!
        assert isinstance(module, Module)
        if name in self.modules:
            assert module is self.modules[name]
        else:
            self.modules[name] = module

    @jit.elidable
    def _find_module(self, name):
        return self.modules.get(name, None)

class GlobalConfig(object):

    attrs_ = ['config', 'callgraph', 'error_exit', 'verbose_keywords', 'environment_vars', 'pycketconfig']

    def __init__(self):
        self.config = {'verbose':MIN_INT,
                       'expander_loaded':0,
                       'repl_loaded':0,
                       'debug_active':0,
                       'boot_done':0,
                       'linklet_mode':1}
        self.callgraph = CallGraph()
        self.error_exit = None
        self.verbose_keywords = []
        self.environment_vars = {}
        self.pycketconfig = None

    # debug_active can be used to set a logical
    # point where a set_trace or a print
    # is going to be evaluated.
    # you can combine this for instance
    # with the 'expander_loaded' to say:
    # "print the asts after the expander is loaded"
    # (just activate_debug after is_expander_loaded)
    def activate_debug(self):
        self.config['debug_active'] = 1

    def deactivate_debug(self):
        self.config['debug_active'] = 0

    def is_debug_active(self):
        return self.config['debug_active']

    def is_boot_completed(self):
        return self.config['boot_done']

    def boot_is_completed(self):
        self.config['boot_done'] = 1

    def set_error_exit(self, exn):
        self.error_exit = exn

    def is_error_triggered(self):
        return self.error_exit

    def are_we_in_linklet_mode(self):
        return self.config['linklet_mode']

    def set_linklet_mode_off(self):
        self.config['linklet_mode'] = 0

    def get_verbose_keywords(self):
        return self.verbose_keywords

    def set_verbose_keywords(self, new_keywords):
        self.verbose_keywords = new_keywords

    def is_keyword_active(self, keyword):
        return keyword in self.verbose_keywords

    def activate_keyword(self, keyword):
        self.verbose_keywords.append(keyword)

    def deactivate_keyword(self, keyword):
        if keyword in self.verbose_keywords:
            self.verbose_keywords.remove(keyword)

    def get_config(self):
        return self.config

    @staticmethod
    def ask_OS_var(var_str):
        import os
        return os.environ[var_str] if var_str in os.environ.keys() else ""

    def get_env_var(self, var_str):
        # lazily cache the OS environment variable values
        if var_str not in self.environment_vars:
            self.environment_vars[var_str] = GlobalConfig.ask_OS_var(var_str)
        return self.environment_vars[var_str]

    def env_var_exists(self, var_str):
        if var_str not in self.environment_vars:
            self.environment_vars[var_str] = GlobalConfig.ask_OS_var(var_str)
        return self.environment_vars[var_str] != ""

    def get_config_val(self, name):
        return self.config[name]

    def set_config_val(self, name, val):
        self.config[name] = val

    def set_pycketconfig(self, c):
        self.pycketconfig = c

    def get_pycketconfig(self):
        return self.pycketconfig

    def is_expander_loaded(self):
        return self.config['expander_loaded'] == 1

    def is_repl_loaded(self):
        return self.config['repl_loaded'] == 1

    def lookup(self, s):
        return self.config.get(s, None)

    def load(self, ast):
        from pycket.interpreter import Module
        if self.config is not None:
            return
        assert isinstance(ast, Module)
        self.config = ast.config.copy()

    def reset_callgraph(self):
        self.callgraph = CallGraph()

w_global_config = GlobalConfig()

class Env(W_Object):
    _attrs_ = []

    def get_prev(self, env_structure):
        assert len(env_structure.elems) == 0
        return self

    @jit.unroll_safe
    def toplevel_env(self):
        while isinstance(self, ConsEnv):
            self = self._prev
        assert isinstance(self, ToplevelEnv)
        return self

    def pycketconfig(self):
        return self.toplevel_env()._pycketconfig.pycket

class Version(object):

    def __init__(self):
        self.version = ""

    def get_version(self):
        return self.version

    def set_version(self, new_version):
        self.version = new_version

w_version = Version()

class ToplevelEnv(Env):
    _attrs_ = ['bindings', 'version', 'module_env', 'commandline_arguments', 'callgraph', 'globalconfig', '_pycketconfig']
    _immutable_fields_ = ["version?", "module_env"]
    def __init__(self, pycketconfig=None):
        from rpython.config.config import Config
        self.bindings = {}
        self.version = w_version
        self.module_env = ModuleEnv(self)
        self.commandline_arguments = []
        self.callgraph = CallGraph()
        self.globalconfig = w_global_config
        if pycketconfig is None:
            assert not objectmodel.we_are_translated()
            pycketconfig = get_testing_config()
        assert isinstance(pycketconfig, Config)
        self._pycketconfig = pycketconfig

    def get_commandline_arguments(self):
        return self.commandline_arguments

    def get_current_version(self):
        return self.version

    def lookup(self, sym, env_structure):
        raise SchemeException("variable %s is unbound" % sym.variable_name())

    def get_linklet_variable(self, sym):
        jit.promote(self)
        return self._lookup(sym, jit.promote(self.version))

    def toplevel_lookup(self, sym):
        jit.promote(self)
        return self._lookup(sym, jit.promote(self.version)).get_val()

    @jit.elidable
    def _lookup(self, sym, version):
        try:
            return self.bindings[sym]
        except KeyError:
            raise SchemeException("toplevel variable %s not found" % sym.variable_name())

    def put_linklet_variable(self, sym, w_cell):
        self.bindings[sym] = w_cell

    def toplevel_set(self, sym, w_val):
        from pycket.values import W_Cell
        if sym in self.bindings:
            self.bindings[sym].set_val(w_val)
        else:
            self.bindings[sym] = W_Cell(w_val)
            self.version = Version()


@inline_small_list(immutable=True, attrname="vals", factoryname="_make", unbox_num=True, nonull=True)
class ConsEnv(Env):
    _immutable_ = True
    _immutable_fields_ = ["_prev"]
    _attrs_ = ["_prev"]
    def __init__ (self, prev):
        assert isinstance(prev, Env)
        self._prev = prev

    def consenv_get_size(self):
        return self._get_size_list()

    @staticmethod
    def make(vals, prev):
        if vals:
            return ConsEnv._make(vals, prev)
        return prev

    @staticmethod
    def make0(prev):
        return prev

    @staticmethod
    def make1(w_val, prev):
        return ConsEnv._make1(w_val, prev)

    @staticmethod
    def make2(w_val1, w_val2, prev):
        return ConsEnv._make2(w_val1, w_val2, prev)

    @staticmethod
    def make_n(n_vals, prev):
        if n_vals:
            return ConsEnv._make_n(n_vals, prev)
        return prev

    @jit.unroll_safe
    def lookup(self, sym, env_structure):
        jit.promote(env_structure)
        for i, s in enumerate(env_structure.elems):
            if s is sym:
                v = self._get_list(i)
                assert v is not None
                return v
        prev = self.get_prev(env_structure)
        return prev.lookup(sym, env_structure.prev)

    def get_prev(self, env_structure):
        jit.promote(env_structure)
        if env_structure.elems:
            return self._prev
        return self

    def __repr__(self):
        return "<%s %r %r>" % (self.__class__.__name__, [x.tostring() for  x in self._get_full_list()], self._prev)
