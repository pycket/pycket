#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# A place for testing primitives
#

from pycket.test.testhelper import check_all, check_none, check_equal


class TestStructs(object):

    def test_posn(self, ast_wrap):
        """
        (struct posn (x y))
        (let* ([p  (posn 1 2)]
               [p?  (posn? p)]
               [x  (posn-x p)]
               [y  (posn-y p)])
            (and p? (= x 1) (= y 2)))
        """
