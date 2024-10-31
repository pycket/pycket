#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os
from pycket.error import SchemeException
from pycket import values

path_home_dir = values.W_Symbol.make("home-dir")
path_pref_dir = values.W_Symbol.make("pref-dir")
path_pref_file = values.W_Symbol.make("pref-file")
path_temp_dir = values.W_Symbol.make("temp-dir")
path_init_dir = values.W_Symbol.make("init-dir")
path_init_file = values.W_Symbol.make("init-file")
path_config_dir = values.W_Symbol.make("config-dir")
path_host_config_dir = values.W_Symbol.make("host-config-dir")
path_addon_dir = values.W_Symbol.make("addon-dir")
path_doc_dir = values.W_Symbol.make("doc-dir")
path_desk_dir = values.W_Symbol.make("desk-dir")
path_sys_dir = values.W_Symbol.make("sys-dir")
path_exec_file = values.W_Symbol.make("exec-file")
path_run_file = values.W_Symbol.make("run-file")
path_collects_dir = values.W_Symbol.make("collects-dir")
path_host_collects_dir = values.W_Symbol.make("host-collects-dir")
path_orig_dir = values.W_Symbol.make("orig-dir")

path_kinds = [path_home_dir,
              path_pref_dir,
              path_pref_file,
              path_temp_dir,
              path_init_dir,
              path_init_file,
              path_config_dir,
              path_host_config_dir,
              path_addon_dir,
              path_doc_dir,
              path_desk_dir,
              path_sys_dir,
              path_exec_file,
              path_run_file,
              path_collects_dir,
              path_host_collects_dir,
              path_orig_dir
          ]

class RacketPaths(object):

    def __init__(self):
        self.paths = {}
        self.initialized = False
        self.plthome = ""
        self.pltcollectsdir = ""
        self.pltexecfile = ""

    def get_path(self, kind):
        if not self.initialized:
            self.initialize_paths()

        if kind not in self.paths:
            raise SchemeException("Path cannot be found for : %s" % kind.tostring())

        return self.paths[kind]

    def get_path_str(self, kind):
        return self.get_path(kind).raw_str()

    def set_path(self, kind, path):
        if kind not in path_kinds:
            raise SchemeException("Possibly trying to set a wrong kind of system-path : %s" % kind.tostring())
        self.paths[kind] = path

    def initialize_paths(self):
        from pycket.util import os_get_env_var, os_check_env_var
        # FIXME : check absolute/relative paths

        # Environment Variables

        if not os_check_env_var("PLTHOME") and not os_check_env_var("PLTCOLLECTS"):
            raise SchemeException("In order to locate the Racket installation, Pycket requires a `PLTHOME` environment variable to point to Racket directory. If Racket is installed in Unix-style, then you can just set a `PLTCOLLECTS` variable to point to the Racket `collects`.")

        PLTHOME = self.plthome = os_get_env_var("PLTHOME")
        PLTCOLLECTS = self.pltcollectsdir = os_get_env_var("PLTCOLLECTS")
        PLTEXECFILE = self.pltexecfile = os_get_env_var("PLTEXECFILE")
        PLTUSERHOME = os_get_env_var("PLTUSERHOME")
        HOME = os_get_env_var("HOME")
        USER = os_get_env_var("USER")
        LOGNAME = os_get_env_var("LOGNAME")
        TMPDIR = os_get_env_var("TMPDIR")
        PLTCONFIGDIR = os_get_env_var("PLTCONFIGDIR")
        PLTADDONDIR = os_get_env_var("PLTADDONDIR")

        CURRENT_DIR = os.getcwd()

        #############
        # HOME
        #############

        W_PATH_HOME_DIR = ""
        if PLTUSERHOME:
            W_PATH_HOME_DIR = PLTUSERHOME
        elif HOME:
            W_PATH_HOME_DIR = HOME
        elif USER:
            W_PATH_HOME_DIR = USER
        elif LOGNAME:
            W_PATH_HOME_DIR = LOGNAME

        self.paths[path_home_dir] = values.W_Path(W_PATH_HOME_DIR)

        #############
        # PREF-DIR
        #############

        W_PATH_PREF_DIR = ""
        if W_PATH_HOME_DIR:
            W_PATH_PREF_DIR = os.path.join(W_PATH_HOME_DIR, ".racket")

        self.paths[path_pref_dir] = values.W_Path(W_PATH_PREF_DIR)

        #############
        # PREF-FILE
        #############

        W_PATH_PREF_FILE = ""
        if W_PATH_PREF_DIR:
            W_PATH_PREF_FILE = os.path.join(W_PATH_PREF_DIR, "racket-prefs.rktd")

        self.paths[path_pref_file] = values.W_Path(W_PATH_PREF_FILE)

        #############
        # TEMP-DIR
        #############

        W_PATH_TEMP_DIR = ""
        if TMPDIR:
            W_PATH_TEMP_DIR = TMPDIR
        elif os.path.exists("/var/tmp"):
            W_PATH_TEMP_DIR = "/var/tmp"
        elif os.path.exists("/usr/tmp"):
            W_PATH_TEMP_DIR = "/usr/tmp"
        elif os.path.exists("/tmp"):
            W_PATH_TEMP_DIR = "/tmp"

        self.paths[path_temp_dir] = values.W_Path(W_PATH_TEMP_DIR)

        #############
        # INIT-DIR
        #############

        W_PATH_INIT_DIR = W_PATH_HOME_DIR

        self.paths[path_init_dir] = values.W_Path(W_PATH_INIT_DIR)

        #############
        # INIT-FILE -- startup file
        #
        # Unix and Mac OS: ".racketrc"
        # Windows: "racketrc.rktl"
        #############

        W_PATH_INIT_FILE = ""
        if W_PATH_INIT_DIR:
            W_PATH_INIT_FILE = os.path.join(W_PATH_INIT_DIR, ".racketrc")

        self.paths[path_init_file] = values.W_Path(W_PATH_INIT_FILE)

        #############
        # CONFIG-DIR
        # defaults to an "etc" directory relative to the current executable
        # It might not exist
        #############

        W_PATH_CONFIG_DIR = ""
        if PLTCONFIGDIR:
            W_PATH_CONFIG_DIR = PLTCONFIGDIR
        else:
            if PLTHOME:
                W_PATH_CONFIG_DIR = os.path.join(PLTHOME, os.path.join("racket", "etc"))
            else:
                W_PATH_CONFIG_DIR = os.path.join(CURRENT_DIR, "etc")

        if path_config_dir not in self.paths:
            self.paths[path_config_dir] = values.W_Path(W_PATH_CONFIG_DIR)

        #############
        # HOST-CONFIG-DIR
        #############

        W_PATH_HOST_CONFIG_DIR = W_PATH_CONFIG_DIR

        self.paths[path_host_config_dir] = values.W_Path(W_PATH_HOST_CONFIG_DIR)

        #############
        # ADDON-DIR
        #############

        W_PATH_ADDON_DIR = W_PATH_PREF_DIR
        if PLTADDONDIR:
            W_PATH_ADDON_DIR = PLTADDONDIR

        if path_addon_dir not in self.paths:
            self.paths[path_addon_dir] = values.W_Path(W_PATH_ADDON_DIR)

        #############
        # DOC-DIR
        #############

        W_PATH_DOC_DIR = W_PATH_HOME_DIR

        self.paths[path_doc_dir] = values.W_Path(W_PATH_DOC_DIR)

        #############
        # SYS-DIR
        #############

        W_PATH_SYS_DIR = "/"

        self.paths[path_sys_dir] = values.W_Path(W_PATH_SYS_DIR)

        #############
        # EXEC-FILE
        #############

        # FIXME : get argv[0] from target args

        if PLTEXECFILE:
            # expect PLTEXECFILE
            W_PATH_EXEC_FILE = PLTEXECFILE
        elif PLTHOME:
            # assume the binary is at $PLTHOME/racket/bin/racket
            W_PATH_EXEC_FILE = os.path.join(PLTHOME, os.path.join("racket", os.path.join("bin", "racket")))
        else:
            # should we error?
            # set it to pycket-c for now
            W_PATH_EXEC_FILE = os.path.join(CURRENT_DIR, "pycket-c")

        self.paths[path_exec_file] = values.W_Path(W_PATH_EXEC_FILE)

        #############
        # RUN-FILE
        #############

        W_PATH_RUN_FILE = W_PATH_EXEC_FILE

        if path_run_file not in self.paths:
            self.paths[path_run_file] = values.W_Path(W_PATH_RUN_FILE)

        #############
        # COLLECTS-DIR
        #############

        if PLTCOLLECTS:
            W_PATH_COLLECTS_DIR = PLTCOLLECTS
        else:
            W_PATH_COLLECTS_DIR = os.path.join(PLTHOME, os.path.join("racket", "collects"))

        if path_collects_dir not in self.paths:
            self.paths[path_collects_dir] = values.W_Path(W_PATH_COLLECTS_DIR)

        #############
        # HOST-COLLECTS-DIR
        #############

        W_PATH_HOST_COLLECTS_DIR = W_PATH_COLLECTS_DIR

        self.paths[path_host_collects_dir] = values.W_Path(W_PATH_HOST_COLLECTS_DIR)

        #############
        # ORIG-DIR
        #############

        W_PATH_ORIG_DIR = CURRENT_DIR

        self.paths[path_orig_dir] = values.W_Path(W_PATH_ORIG_DIR)

        self.initialized = True

racket_sys_paths = RacketPaths()

def report_racket_paths():
    from pycket.util import console_log
    log_str = """

Racket Paths:

 - collects-dir     : %s
 - exec-file        : %s
 - home-dir         : %s
 - pref-dir         : %s
 - orig-dir         : %s

Env Vars:
 - PLTHOME          : %s
 - PLTCOLLECTS      : %s
 - PLTEXECFILE      : %s
""" % (racket_sys_paths.get_path_str(path_collects_dir),
       racket_sys_paths.get_path_str(path_exec_file),
       racket_sys_paths.get_path_str(path_home_dir),
       racket_sys_paths.get_path_str(path_pref_dir),
       racket_sys_paths.get_path_str(path_orig_dir),
       racket_sys_paths.plthome,
       racket_sys_paths.pltcollectsdir,
       racket_sys_paths.pltexecfile
       )

    console_log(log_str, given_verbosity_level=4)