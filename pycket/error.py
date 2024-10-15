#! /usr/bin/env python
# -*- coding: utf-8 -*-

from rpython.rlib.objectmodel import we_are_translated

class ExitException(Exception):

    def __init__(self, status_msg):
        self.exit_status = status_msg

    def is_user(self):
        return False

class SchemeException(Exception):
    _attrs_ = ["msg", "w_exn_type", "context_ast", "context_module", "extra_args"]
    _immutable_ = True

    def __init__(self, msg, w_exn_type=None, extra_args=[]):
        if not we_are_translated():
            Exception.__init__(self, msg)
        self.msg = msg
        self.context_ast = None
        self.context_module = None
        self.extra_args = []
        if w_exn_type is None:
            from pycket.prims.general import exn_fail
            self.w_exn_type = exn_fail
        else:
            self.w_exn_type = w_exn_type

    def is_user(self):
        return False

    def get_exn_type(self):
        return self.w_exn_type

    def format_error(self): # pragma: no cover
        # only error printing
        result = self.msg
        if self.context_ast:
            result += "\n  while executing: %s" % (
                self.context_ast.tostring(), )
            if self.context_ast.surrounding_lambda:
                result += "\n  in function: %s" % (
                self.context_ast.surrounding_lambda.tostring(),)
        context = self.context_module
        if context is not None:
            result += "\n  in module: %s" % context.full_module_path()
        return result

class BootstrapError(SchemeException):
    _attrs_ = ["msg"]
    _immutable_ = True

    def __init__(self, msg):
        SchemeException.__init__(self, msg)

class UserException(SchemeException):
    _attrs_ = ["msg"]
    _immutable_ = True

    def __init__(self, msg):
        from pycket.prims.general import exn_fail_user
        SchemeException.__init__(self, msg, exn_fail_user)

    def is_user(self):
        return True

class ContractException(SchemeException):
    _attrs_ = ["msg"]
    _immutable_ = True

    def __init__(self, msg):
        from pycket.prims.general import exn_fail_contract
        SchemeException.__init__(self, msg, exn_fail_contract)


class FSException(SchemeException):
    _attrs_ = ["msg"]
    _immutable_ = True

    def __init__(self, msg):
        from pycket.prims.general import exn_fail_fs
        SchemeException.__init__(self, msg, exn_fail_fs)

class ArityException(SchemeException):
    _attrs_ = ["msg"]
    _immutable_ = True

    def __init__(self, msg):
        from pycket.prims.general import exn_fail_contract_arity
        SchemeException.__init__(self, msg, exn_fail_contract_arity)
