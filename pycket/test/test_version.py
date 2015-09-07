#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Test the version here.
#

import pytest
from pycket.test.testhelper import check_equal

EXPECTED_VERSION='6.2.900.15'


def test_version():
    check_equal('(version)', '"%s"' % EXPECTED_VERSION)

# EOF
