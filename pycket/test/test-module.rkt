#lang pycket #:stdlib

;;(require (except-in racket/tcp tcp-listen))
;;(require "pycket/test/imp-vector.rkt")
;;(require (only-in "pycket/test/imp-vector.rkt" check))
(require (rename-in "pycket/test/imp-vector.rkt" [check check^]))
;;(require racket/unsafe/ops)
;;(require (prefix-in check "pycket/test/imp-vector.rkt"))

(define k check^)
(displayln k)

