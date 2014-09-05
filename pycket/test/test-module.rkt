#lang racket
;;(require (except-in racket/tcp tcp-listen))
;;(require "./imp-vector.rkt")
;;(require (only-in "./imp-vector.rkt" check))
;;(require (rename-in "./imp-vector.rkt" [check check^]))
;;(require racket/unsafe/ops)
;;(require (prefix-in check "pycket/test/imp-vector.rkt"))

(define key (make-continuation-mark-key))

(define key-imp
  (impersonate-continuation-mark-key
    key
    (lambda (x) (printf "called getter~n") x)
    (lambda (x) (printf "called setter~n") x)))

(define result
  (with-continuation-mark key-imp "quiche"
    (with-continuation-mark key-imp "ham"
       (continuation-mark-set->list
        (current-continuation-marks)
        key-imp))))

result
;;(letrec ([x x]) x)

;;(struct wrapper (x) #:property prop:procedure 0)
;;
;;(define proc (lambda (x) x))
;;
;;(for ([i (in-range 100)])
;;  (set! proc (chaperone-struct (wrapper proc) wrapper-x (lambda (a b) b))))
;;
;;(proc 1)

