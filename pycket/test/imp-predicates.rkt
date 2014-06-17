#lang pycket #:stdlib

;; Some test cases for chaperone-of? and impersonator-of? operations

(define test-vector
  (make-vector 100))

(define test-proc
  (lambda (x)
    (+ x 5)))

(define chp-proc
  (chaperone-procedure
    test-proc
    (lambda (x)
      (unless (> x 0)
        (error 'chp-proc "Value less than zero"))
      (values
        x
        (lambda (res)
          (unless (> res 0)
            (error 'chp-proc "Result less than zero"))
          res)))))

(define imp-proc
  (impersonate-procedure
    test-proc
    (lambda (x)
      (unless (> x 0)
        (error 'imp-proc "Value less than zero"))
      (values
        x
        (lambda (res)
          (unless (> res 0)
            (error 'imp-proc "Result less than zero"))
          res)))))

(printf "~nchaperone-of? procedure examples~n")
(printf "test-proc ? imp-proc  => ~s = #f ~n" (chaperone-of? test-proc imp-proc))
(printf "imp-proc  ? test-proc => ~s = #f ~n" (chaperone-of? imp-proc test-proc))
(printf "chp-proc  ? test-proc => ~s = #t ~n" (chaperone-of? chp-proc test-proc))
(printf "test-proc ? chp-proc  => ~s = #f ~n" (chaperone-of? test-proc chp-proc))

(printf "~nimpersonator-of? procedure examples~n")
(printf "test-proc ? chp-proc  => ~s = #f ~n" (impersonator-of? test-proc chp-proc))
(printf "test-proc ? imp-proc  => ~s = #f ~n" (impersonator-of? test-proc imp-proc))
(printf "chp-proc  ? test-proc => ~s = #t ~n" (impersonator-of? chp-proc test-proc))
(printf "imp-proc  ? test-proc => ~s = #t ~n" (impersonator-of? imp-proc test-proc))
(printf "imp-proc  ? sub1      => ~s = #f ~n" (impersonator-of? imp-proc sub1))
(printf "sub1      ? imp-proc  => ~s = #f ~n" (impersonator-of? sub1 imp-proc))

