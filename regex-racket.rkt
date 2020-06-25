#lang racket/base

(require "regex3.rkt"
         #;"regex.rkt"
         #;"regex-anno.rkt")

(require ffi/unsafe/vm)
(define pycket:pe (vm-primitive 'pycket:pe))
(define pycket:time-apply (vm-primitive 'pycket:time-apply))

(define l (make-string 110 #\d))
(define r (make-string 110 #\p))
(define str-big (string-append l "defg" r))

(define reg "defg")

(define iter 500000)

(define residual-func
  (pycket:pe '(input-str s-pos) (string-length) (reverse)
             match-pat reg 0 str-big 0 '()))

(for ([j (in-range 10)])
  (time
   (for ([i (in-range iter)])
     (reg-match reg str-big residual-func))))
