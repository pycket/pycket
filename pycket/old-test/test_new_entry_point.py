#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Testing entrypoint
#
import pytest
from pycket.entry_point import make_entry_point
from pycket.option_helper import parse_args#, INIT, RETURN_OK, MISSING_ARG, JUST_EXIT
from pycket import option_helper
from rpython.rlib import jit

entry_point = make_entry_point()

class TestOptions(object):

    ok_jit_args = ['--jit', 'trace_limit=13000']

    def test_no_args(self):
        config, names, args, retval = parse_args(['arg0'])
        assert retval == 3

    def test_one_args(self, empty_json):
        # Ok, this is grey-box-y
        config, names, args, retval = parse_args(['arg0', empty_json])
        assert retval == 0
        assert names['file'] == empty_json
        assert config['mode'] == option_helper._run

    def test_jitarg_fail(self, empty_json):
        with pytest.raises(ValueError):
            parse_args(['arg0', '--jit', empty_json])
        argv = ['arg0', empty_json, '--jit']
        config, names, args, retval = parse_args(argv)
        assert retval == 2

    def test_jitarg_works(self, empty_json):
        # cannot actually check jit hint.
        argv = ['arg0'] + self.ok_jit_args + [empty_json]
        config, names, args, retval = parse_args(argv)
        assert retval == 0

        argv = ['arg0', empty_json] + self.ok_jit_args
        config, names, args, retval = parse_args(argv)
        assert retval == 0

    @pytest.mark.parametrize('arg',
                             ["-h", "--help", "/?", "-?", "/h", "/help"])
    def test_help(self, arg):
        argv = ["arg0", arg]
        assert (None, None, None, 0 == parse_args(argv))

        argv = ["arg0", "foobar", arg]
        assert (None, None, None, 0 == parse_args(argv))

    def test_program_arguments_plain(self, empty_json):
        program_args = ["foo", "bar", "baz"]
        argv = ['arg0', empty_json] + program_args
        config, names, args, retval = parse_args(argv)

        assert retval == 0
        assert args == program_args

    def test_program_arguments_after_jit(self, empty_json):
        program_args = ["foo", "bar", "baz"]
        argv = ['arg0', empty_json] + self.ok_jit_args + program_args
        config, names, args, retval = parse_args(argv)

        assert retval == 0
        assert args == program_args

    def test_program_arguments_explicit(self, empty_json):
        program_args = ["foo", "bar", "baz"]
        argv = ['arg0', empty_json] + ["--"] + program_args
        config, names, args, retval = parse_args(argv)

        assert retval == 0
        assert args == program_args

    def test_program_arguments_explicit_with_switch(self, empty_json):
        program_args = ["--jit", "foo", "bar", "baz"]
        argv = ['arg0', empty_json] + ["--"] + program_args
        config, names, args, retval = parse_args(argv)

        assert retval == 0
        assert args == program_args

    def test_eval(self):
        code = "(ratatta)"
        argv = ['arg0', "-e", code]
        config, names, args, retval = parse_args(argv)
        assert retval == 0
        assert 'file' not in names
        assert config['mode'] == option_helper._eval
        assert names['exprs'] == code

    def test_f(self, empty_json):
        pytest.skip("re-enable when -f works again")
        argv = ['arg0', "-f", empty_json]
        config, names, args, retval = parse_args(argv)
        assert retval == 0
        assert names['file'] == empty_json+".f"
        assert config['mode'] == option_helper._eval
        assert names['exprs'] == '(load "%s")' % empty_json
        assert args == []

    def test_r(self, empty_json):
        pytest.skip("re-enable when -f works again")
        argv1 = ['arg0', "-f", empty_json, "--", "foo", "bar", "baz"]
        argv2 = ['arg0', "-r", empty_json, "foo", "bar", "baz"]
        assert parse_args(argv1) == parse_args(argv2)

    def test_t(self, empty_json):
        argv = ['arg0', "-t", empty_json]
        config, names, args, retval = parse_args(argv)
        assert retval == 0
        assert names['file'] == empty_json + ".t"
        assert config['mode'] == option_helper._eval
        assert names['exprs'] == '(require (file "%s"))' % empty_json
        assert args == []

    def test_u(self, empty_json):
        argv1 = ['arg0', "-t", empty_json, "--", "foo", "bar", "baz"]
        argv2 = ['arg0', "-u", empty_json, "foo", "bar", "baz"]
        assert parse_args(argv1) == parse_args(argv2)

    def test_l(self, empty_json):
        argv = ['arg0', "-l", empty_json]
        config, names, args, retval = parse_args(argv)
        assert retval == 0
        assert names['file'] == empty_json + ".l"
        assert config['mode'] == option_helper._eval
        assert names['exprs'] == '(require (lib "%s"))' % empty_json
        assert args == []

    def test_p(self, empty_json):
        argv = ['arg0', "-p", empty_json]
        config, names, args, retval = parse_args(argv)
        assert retval == 0
        assert names['file'] == empty_json + ".p"
        assert config['mode'] == option_helper._eval
        assert names['exprs'] == '(require (planet "%s"))' % empty_json
        assert args == []

    def test_b(self):
        f_name = 'dummy.rkt'
        argv1 = ['arg0', "-b", f_name]
        config1, names1, args1, retval1 = parse_args(argv1)
        assert retval1 == 0
        assert names1['byte-expand'] == f_name
        assert args1 == []

    def test_m(self):
        f_name = 'multiple-modules.json'
        argv1 = ['arg0', "-c", f_name]
        config1, names1, args1, retval1 = parse_args(argv1)
        assert retval1 == 0
        assert names1['multiple-modules'] == f_name
        assert args1 == []

# class TestCommandline(object):
#     """These are quire similar to TestOptions but targeted at the higher level
#     entry_point interface. At that point, we only have the program exit code.
#     """

#     def test_no_argv(self):
#         assert entry_point(['arg0']) == 3

#     def test_one_arg(self, empty_json):
#         assert entry_point(['arg0', empty_json]) == 0

#     def test_jitarg_fail(self, empty_json):
#         with pytest.raises(ValueError):
#             entry_point(['arg0', '--jit', empty_json])
#         assert entry_point(['arg0', empty_json, '--jit']) == 2

#     def test_jitarg_works(self, empty_json):
#         assert entry_point(
#             ['arg0', '--jit', 'trace_limit=13000',empty_json]) == 0
#         assert entry_point(
#             ['arg0', empty_json, '--jit', 'trace_limit=13000']) == 0

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
