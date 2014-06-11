#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# A place for testing primitives
#

from pycket.test.testhelper import *
from pycket.values import *

class TestStructs(object):

    def test_current_inspector(self, source):
      """
      (inspector? (current-inspector))
      """
      result = run_mod_expr(source, wrap=True)
      assert result == W_Bool.make(True)

    def test_make_struct_type(self, source):
      """
      (define-values (struct:p make-p p? p-ref p-set!) (make-struct-type 'p #f 3 0 #f null 'prefab #f '(0 1 2)))
      (and 
        (struct-type? struct:p) 
        (struct-constructor-procedure? make-p) 
        (struct-predicate-procedure? p?) 
        (struct-accessor-procedure? p-ref) 
        (struct-mutator-procedure? p-set!))
      """
      result = run_mod_expr(source, wrap=True)
      assert result == W_Bool.make(True)

    def test_make_struct_field_accessor(self, source):
      """
      (define-values (struct:p make-p p? p-ref p-set!) (make-struct-type 'p #f 3 0 #f null 'prefab #f '(0 1 2)))
      (define accessor (make-struct-field-accessor p-ref 0))
      (procedure? accessor)
      """
      result = run_mod_expr(source, wrap=True)
      assert result == W_Bool.make(True)

    def test_struct_main_functions(self, source):
      """
      (struct posn (x y))

      (let* ([p  (posn 1 2)]
             [p?  (posn? p)]
             [x  (posn-x p)]
             [y  (posn-y p)])
      (and p? (= x 1) (= y 2)))
      """
      result = run_mod_expr(source, wrap=True)
      assert result == W_Bool.make(True)
