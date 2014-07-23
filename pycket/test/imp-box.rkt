#lang pycket

(define set-ticker 0)
(define ref-ticker 0)

(define a (box 5))
(define b
  (chaperone-box a
    (lambda (b v) (set! ref-ticker (+ ref-ticker 1)) v)
    (lambda (b v) (set! set-ticker (+ set-ticker 1)) v)))
(define c
  (impersonate-box a
    (lambda (b v) (set! ref-ticker (+ ref-ticker 1)) v)
    (lambda (b v) (set! set-ticker (+ set-ticker 1)) v)))

(set-box! b 6)
(unless (eqv? (unbox b) 6)
  (error 'imp-box "wrong value 0"))

(set-box! c 7)
(unless (eqv? (unbox c) 7)
  (error 'imp-box "wrong value 1"))

(unless (eqv? (unbox a) 7)
  (error 'imp-box "wrong value 2"))

(unless (eqv? set-ticker 2)
  (error 'imp-box "wrong value 3"))
(unless (eqv? ref-ticker 2)
  (error 'imp-box "wrong value 4"))

