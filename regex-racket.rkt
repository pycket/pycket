#lang racket/base

(require "regex3.rkt"
         racket/port
         #;"regex.rkt"
         #;"regex-anno.rkt")

(require ffi/unsafe/vm)

(define pycket:pe (vm-primitive 'pycket:pe))
(define pycket:time-apply (vm-primitive 'pycket:time-apply))


(define l (make-string 10000 #\d))
(define r (make-string 10000 #\p))
(define str-big (string-append l "defffffffffffffffffffg" r))
(define reg "def*g")
(define iter 2000)


#|
(define in (open-input-file "shakespeare"))
(define str-big (port->string in))

(define reg "chrysolite")
(define iter 5)
|#

(define residual-func
  (pycket:pe '(input-str s-pos) (string-length) (reverse)
             match-pat reg 0 str-big 0 '()))

(for ([j (in-range 20)])
  (time
   (for ([i (in-range iter)])
     #;(regexp-match reg str-big)
     (reg-match reg str-big residual-func))))
