#lang racket/base

(require racket/contract racket/flonum racket/unsafe/ops)

(define N 10000000)

(define/contract (dot1c v1 v2)
  ((vectorof flonum?) (vectorof flonum?) . -> . flonum?)
  (for/sum ([e1 v1] [e2 v2]) (* e1 e2)))

(define v1 (for/vector #:length N #:fill 0.0 ([i (in-range N)]) (random)))
(define v2 (for/vector #:length N #:fill 0.0 ([i (in-range N)]) (random)))

(collect-garbage) (collect-garbage)
'dot1
(time (dot1c v1 v2))
