#lang racket/base

(require racket/contract racket/flonum racket/unsafe/ops)

(define N 10000000)

(define (dot3 v1 v2)
  (for/sum ([e1 (in-flvector v1)] [e2 (in-flvector v2)]) (fl* e1 e2)))

(define v1 (for/flvector #:length N #:fill 0.0 ([i (in-range N)]) (random)))
(define v2 (for/flvector #:length N #:fill 0.0 ([i (in-range N)]) (random)))

(collect-garbage) (collect-garbage)
'dot1
(time (dot3 v1 v2))
