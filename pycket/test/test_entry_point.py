#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Testing entrypoint
#
import pytest
#from pycket.entry_point import make_entry_point
from pycket.option_helper import parse_args, INIT, RETURN_OK, MISSING_ARG, JUST_EXIT, RET_JIT
from pycket.option_helper import config as init_config
#from pycket import option_helper
#from rpython.rlib import jit

#entry_point = make_entry_point()

class TestOptions(object):

    ok_jit_args = ['--jit', 'trace_limit=13000']

    def test_no_args(self):
        config, names, args, retval = parse_args(['arg0'])
        assert retval == RETURN_OK
        assert config == init_config
        assert names == {}

    def test_verbose(self):
        config, names, args, retval = parse_args(['arg0', '--verbose'])
        assert config['verbose']
        assert names['verbosity_level'] == ['0']

        config, names, args, retval = parse_args(['arg0', '--verbose', '2'])
        assert config['verbose']
        assert names['verbosity_level'] == ['2']
        assert retval == RETURN_OK

    def test_one_args(self, cool_mod):
        config, names, args, retval = parse_args(['arg0', "cool-module.rkt"])
        assert retval == RETURN_OK
        assert 'loads' in names
        assert "file" in names['loads']
        assert "cool-module.rkt" in names['load_arguments']

    def test_jitarg_fail(self, cool_mod):
        with pytest.raises(ValueError):
            parse_args(['arg0', '--jit', cool_mod])
        argv = ['arg0', "cool-module.rkt", '--jit']
        config, names, args, retval = parse_args(argv)
        assert retval == RET_JIT

    def test_jitarg_works(self, cool_mod):
        # cannot actually check jit hint.
        argv = ['arg0'] + self.ok_jit_args + [cool_mod]
        config, names, args, retval = parse_args(argv)
        assert retval == RETURN_OK

        argv = ['arg0', cool_mod] + self.ok_jit_args
        config, names, args, retval = parse_args(argv)
        assert retval == RETURN_OK

    @pytest.mark.parametrize('arg',
                             ["-h", "--help", "/?", "-?", "/h", "/help"])
    def test_help(self, arg):
        argv = ["arg0", arg]
        assert (None, None, None, RETURN_OK == parse_args(argv))

        argv = ["arg0", "foobar", arg]
        assert (None, None, None, RETURN_OK == parse_args(argv))

    def test_program_arguments_plain(self, cool_mod):
        program_args = ["foo", "bar", "baz"]
        argv = ['arg0', cool_mod] + program_args
        config, names, args, retval = parse_args(argv)
        # must use "--"
        assert retval == MISSING_ARG
        assert args == []

    def test_program_arguments_after_jit(self, cool_mod):
        program_args = ["foo", "bar", "baz"]
        argv = ['arg0', cool_mod] + self.ok_jit_args + program_args
        config, names, args, retval = parse_args(argv)
        # again, must use "--"
        assert retval == MISSING_ARG
        assert args == []

    def test_program_arguments_explicit(self, cool_mod):
        program_args = ["foo", "bar", "baz"]
        argv = ['arg0', cool_mod] + ["--"] + program_args
        config, names, args, retval = parse_args(argv)

        assert retval == RETURN_OK
        assert args == program_args

    def test_program_arguments_explicit_with_switch(self, cool_mod):
        program_args = ["--jit", "foo", "bar", "baz"]
        argv = ['arg0', cool_mod] + ["--"] + program_args
        config, names, args, retval = parse_args(argv)

        assert retval == RETURN_OK
        assert args == program_args

    def test_e(self):
        code = "(ratatta)"
        argv = ['arg0', "-e", code]
        config, names, args, retval = parse_args(argv)
        assert config == init_config
        assert retval == RETURN_OK
        assert 'loads' in names
        assert 'eval' in names['loads'] and code in names['load_arguments']

    def test_f(self, cool_mod):
        argv = ['arg0', "-f", cool_mod]
        config, names, args, retval = parse_args(argv)
        assert retval == RETURN_OK
        assert config == init_config
        assert 'loads' in names
        assert 'load' in names['loads'] and cool_mod in names['load_arguments']

    def test_r(self, cool_mod):
        argv1 = ['arg0', "-f", cool_mod, "-N", cool_mod]
        argv2 = ['arg0', "-r", cool_mod]
        assert parse_args(argv1) == parse_args(argv2)

    def test_t(self, cool_mod):
        argv = ['arg0', "-t", cool_mod]
        config, names, args, retval = parse_args(argv)
        assert retval == RETURN_OK
        assert config['no-lib']
        assert 'loads' in names
        assert 'file' in names['loads'] and cool_mod in names['load_arguments']

    def test_u(self, cool_mod):
        argv1 = ['arg0', "-t", cool_mod, "-N", cool_mod]
        argv2 = ['arg0', "-u", cool_mod]
        assert parse_args(argv1) == parse_args(argv2)

    def test_l(self, cool_mod):
        argv = ['arg0', "-l", cool_mod]
        config, names, args, retval = parse_args(argv)
        assert retval == RETURN_OK
        assert config['no-lib']
        assert 'loads' in names
        assert 'lib' in names['loads'] and cool_mod in names['load_arguments']

    # The ones below are going to fail when they're implemented
    def test_p(self, cool_mod):
        argv = ['arg0', "-p", cool_mod]
        config, names, args, retval = parse_args(argv)
        assert retval == RETURN_OK
        assert 'not-implemented' in names
        assert '-p' in names['not-implemented']

    def test_b(self, cool_mod):
        argv1 = ['arg0', "-b", cool_mod]
        config, names, args, retval = parse_args(argv1)
        assert retval == RETURN_OK
        assert 'not-implemented' in names
        assert '-b' in names['not-implemented']

    def test_m(self, cool_mod):
        argv1 = ['arg0', "-m", cool_mod]
        config, names, args, retval = parse_args(argv1)
        assert retval == RETURN_OK
        assert 'not-implemented' in names
        assert '-m' in names['not-implemented']

# class TestCommandline(object):
#     """These are quire similar to TestOptions but targeted at the higher level
#     entry_point interface. At that point, we only have the program exit code.
#     """

#     def test_no_argv(self):
#         assert entry_point(['arg0']) == 3

#     def test_one_arg(self, cool_mod):
#         assert entry_point(['arg0', cool_mod]) == 0

#     def test_jitarg_fail(self, cool_mod):
#         with pytest.raises(ValueError):
#             entry_point(['arg0', '--jit', cool_mod])
#         assert entry_point(['arg0', cool_mod, '--jit']) == 2

#     def test_jitarg_works(self, cool_mod):
#         assert entry_point(
#             ['arg0', '--jit', 'trace_limit=13000',cool_mod]) == 0
#         assert entry_point(
#             ['arg0', cool_mod, '--jit', 'trace_limit=13000']) == 0

#     def test_eval(self, capfd):
#         printval = 42
#         assert entry_point(['arg0', '-e', '(display "%s")' % printval]) == 0
#         out, err = capfd.readouterr()
#         assert out == "%s" % printval

#     def test_f(self, capfd, racket_file):
#         """(display "42")"""
#         pytest.skip("re-enable when -f works again")

#         assert entry_point(['arg0', '-f', racket_file]) == 0
#         out, err = capfd.readouterr()
#         assert out == "42"
