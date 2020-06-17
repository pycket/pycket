#lang racket/base

(require "regex3.rkt"
         #;"regex.rkt"
         #;"regex-anno.rkt")

(require ffi/unsafe/vm)
(define pycket:pe (vm-primitive 'pycket:pe))
(define pycket:time-apply (vm-primitive 'pycket:time-apply))

(define l (make-string 300 #\a))
(define r (make-string 300 #\p))
(define str-big (string-append l "defg" r))

(define reg "defg")

(define iter 500000)

(for ([j (in-range 10)])
  ;(pycket:time-apply
  (time
   ;(lambda ()
   (for ([i (in-range iter)])
     (reg-match reg str-big)
     #;(let ([k (reg-match reg str-big)])
       (if k (printf "result : ~a\n" k) k))
     #;(pycket:pe 'str str-big (string-length) (reverse) reg-match reg str-big))))


