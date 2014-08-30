#lang racket/base
;;(require (except-in racket/tcp tcp-listen))
;;(require "./imp-vector.rkt")
;;(require (only-in "./imp-vector.rkt" check))
;;(require (rename-in "./imp-vector.rkt" [check check^]))
;;(require racket/unsafe/ops)
;;(require (prefix-in check "pycket/test/imp-vector.rkt"))

(require racket/contract/base)
(struct x (proc) #:property prop:procedure 0)
(struct y x ())

(define b (y (lambda (x) x)))
(b 10)

;;(struct wrapper (x) #:property prop:procedure 0)
;;
;;(define proc (lambda (x) x))
;;
;;(for ([i (in-range 100)])
;;  (set! proc (chaperone-struct (wrapper proc) wrapper-x (lambda (a b) b))))
;;
;;(proc 1)

