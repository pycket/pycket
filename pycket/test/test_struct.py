from pycket.test.testhelper import *
from pycket.values import *

class TestStructs(object):

    def test_current_inspector(self, source):
      """
      (inspector? (current-inspector))
      """
      result = run_mod_expr(source, wrap=True)
      assert result == w_true

    def test_make_struct_type(self, source):
      """
      (define-values (struct:p0 make-p0 p0? p0-ref p0-set!) (make-struct-type 'p0 #f 3 0 #f null 'prefab #f '(0 1 2)))
      (and 
        (struct-type? struct:p0) 
        (struct-constructor-procedure? make-p0) 
        (struct-predicate-procedure? p0?) 
        (struct-accessor-procedure? p0-ref) 
        (struct-mutator-procedure? p0-set!))
      """
      result = run_mod_expr(source, wrap=True)
      assert result == w_true

    def test_make_struct_field_accessor(self, source):
      """
      (define-values (struct:p1 make-p1 p1? p1-ref p1-set!) (make-struct-type 'p1 #f 3 0 #f null 'prefab #f '(0 1 2)))
      (define accessor (make-struct-field-accessor p1-ref 0))
      (procedure? accessor)
      """
      result = run_mod_expr(source, wrap=True)
      assert result == w_true

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
      assert result == w_true

    def test_struct_copying_and_update(self, source):
      """
      (struct posn (x y))

      (let* ([p1  (posn 1 2)]
             [p2  (struct-copy posn p1 [x 3])]
             [x1  (posn-x p1)]
             [x2  (posn-x p2)]
             [y2  (posn-y p2)])
      (and (= x1 1) (= x2 3) (= y2 2)))
      """
      result = run_mod_expr(source, wrap=True)
      assert result == w_true

    def test_struct_subtypes(self, source):
      """
      (struct posn (x y))
      (struct 3d-posn posn (z))

      (let* ([p (3d-posn 1 2 3)]
             [p?  (posn? p)]
             [x  (posn-x p)]
             [z  (3d-posn-z p)])
      (and p? (= x 1) (= z 3)))
      """
      result = run_mod_expr(source, wrap=True)
      assert result == w_true
