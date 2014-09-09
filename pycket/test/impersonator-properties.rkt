#lang pycket

(define assert
  (lambda (x)
    (unless x (error 'assert "assertion failure"))))

(define vec (make-vector 100))

(define-values
  (prop:prop prop:prop? prop:get)
  (make-impersonator-property 'my-property))

(printf "~s~n" (impersonator-property? prop:prop))
(assert (impersonator-property? prop:prop))

(define imp (impersonate-vector vec
              (lambda (a b c) a)
              (lambda (a b c) a)
              prop:prop 'the-value))

(printf "~s~n" (prop:prop? imp))
(assert (prop:prop? imp))

(printf "~s~n" (prop:get imp))
(assert (eqv? (prop:get imp) 'the-value))
