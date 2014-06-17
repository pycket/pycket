#lang pycket #:stdlib

;; Some test cases for chaperone-of? and impersonator-of? operations

(define test-vector
  (make-vector 1000))

(define imp-vector
  (impersonate-vector test-vector
    (lambda (vec i v) v)
    (lambda (vec i v) v)))

(define chp-vector
  (chaperone-vector test-vector
    (lambda (vec i v) v)
    (lambda (vec i v) v)))

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

(printf "~nchaperone-of? vector examples~n")
(printf "test-vector ? imp-vector  => ~s = #f ~n" (chaperone-of? test-vector imp-vector))
(printf "imp-vector  ? test-vector => ~s = #f ~n" (chaperone-of? imp-vector test-vector))
(printf "chp-vector  ? test-vector => ~s = #t ~n" (chaperone-of? chp-vector test-vector))
(printf "test-vector ? chp-vector  => ~s = #f ~n" (chaperone-of? test-vector chp-vector))

(printf "~nimpersonator-of? vector examples~n")
(printf "test-vector ? chp-vector  => ~s = #f ~n" (impersonator-of? test-vector chp-vector))
(printf "test-vector ? imp-vector  => ~s = #f ~n" (impersonator-of? test-vector imp-vector))
(printf "chp-vector  ? test-vector => ~s = #t ~n" (impersonator-of? chp-vector test-vector))
(printf "imp-vector  ? test-vector => ~s = #t ~n" (impersonator-of? imp-vector test-vector))
(printf "imp-vector  ? sub1        => ~s = #f ~n" (impersonator-of? imp-vector sub1))
(printf "sub1        ? imp-vector  => ~s = #f ~n" (impersonator-of? sub1 imp-vector))

(printf "~nequality of vectors with impersonators~n")
(printf "(equal? test-vector test-vector) => ~s~n" (equal? test-vector test-vector))
(printf "(equal? test-vector imp-vector)  => ~s~n" (equal? test-vector imp-vector))
(printf "(equal? test-vector chp-vector)  => ~s~n" (equal? test-vector chp-vector))
(printf "(equal? chp-vector test-vector)  => ~s~n" (equal? chp-vector test-vector))

;;(displayln
;;  (for ([n (in-range 100000)])
;;    (equal? imp-vector chp-vector)))

