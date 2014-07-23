#lang pycket #:stdlib

(define ref-ticker 0)
(define set-ticker 0)

(struct posn ([x #:mutable] [y #:mutable]) #:transparent)

(define a (posn 1 1))
(define b
  (impersonate-struct a
    posn-x (lambda (a b) (set! ref-ticker (+ ref-ticker 1)) b)
    set-posn-x! (lambda (a b) (set! set-ticker (+ set-ticker 1)) b)))
(define c
  (chaperone-struct a
    posn-y (lambda (a b) (set! ref-ticker (+ ref-ticker 1)) b)
    set-posn-y! (lambda (a b) (set! set-ticker (+ set-ticker 1)) b)))

(set-posn-x! b 2)
(unless (eqv? (posn-x b) 2)
  (error 'imp-struct "invalid value 0"))
(set-posn-y! c 3)
(unless (eqv? (posn-y c) 3)
  (error 'imp-struct "invalid value 0"))

(unless (eqv? ref-ticker 2)
  (error 'imp-struct "invalid value 1"))
(unless (eqv? set-ticker 2)
  (error 'imp-struct "invalid value 2"))

