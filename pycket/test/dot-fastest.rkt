#lang racket/base

(require racket/contract racket/flonum racket/unsafe/ops)

(define N 10000000)

(define (dot-flfastest v1 v2)
  (define len (flvector-length v1))
  (unless (= len (flvector-length v2))
    (error 'fail))
  (let loop ([n 0] [sum 0.0])
    (if (unsafe-fx= len n) sum
        (loop (unsafe-fx+ n 1) (unsafe-fl+ sum (unsafe-fl* (unsafe-flvector-ref v1 n)
                                                           (unsafe-flvector-ref v2 n)))))))
(define v1 (for/flvector #:length N #:fill 0.0 ([i (in-range N)]) (random)))
(define v2 (for/flvector #:length N #:fill 0.0 ([i (in-range N)]) (random)))

(collect-garbage) (collect-garbage)
'dot1
(time (dot-flfastest v1 v2))
