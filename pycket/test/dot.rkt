#lang racket/base

(require racket/contract racket/flonum racket/unsafe/ops)

(define N 10000000)

(define (dot1 v1 v2)
  (for/sum ([e1 v1] [e2 v2]) (* e1 e2)))

(define (dot-fast v1 v2)
  (for/fold ([sum 0.0]) ([i (in-range (vector-length v1))])
    (+ sum (* (unsafe-vector-ref v1 i) (unsafe-vector-ref v2 i)))))

(define/contract (dot1c v1 v2)
  ((vectorof flonum?) (vectorof flonum?) . -> . flonum?)
  (for/sum ([e1 v1] [e2 v2]) (* e1 e2)))

(define (dot2 v1 v2)
  (for/sum ([e1 (in-vector v1)] [e2 (in-vector v2)]) (fl* e1 e2)))

(define/contract (dot2c v1 v2)
  ((vectorof flonum?) (vectorof flonum?) . -> . flonum?)
  (for/sum ([e1 (in-vector v1)] [e2 (in-vector v2)]) (fl* e1 e2)))

(define (dot3 v1 v2)
  (for/sum ([e1 (in-flvector v1)] [e2 (in-flvector v2)]) (fl* e1 e2)))

(define/contract (dot3c v1 v2)
  (flvector? flvector? . -> . flonum?)
  (for/sum ([e1 (in-flvector v1)] [e2 (in-flvector v2)]) (fl* e1 e2)))

(define v1 (for/vector #:length N #:fill 0.0 ([i (in-range N)]) (random)))
(define v2 (for/vector #:length N #:fill 0.0 ([i (in-range N)]) (random)))

;(define fv1 (for/flvector #:length N ([i (in-range N)]) (vector-ref v1 i)))
;(define fv2 (for/flvector #:length N ([i (in-range N)]) (vector-ref v2 i)))

(collect-garbage) (collect-garbage)
'dot1
(time (dot1 v1 v2))
(collect-garbage) (collect-garbage)
'dot-fast
(time (dot-fast v1 v2))
;; (collect-garbage) (collect-garbage)
;; (time (dot2 v1 v2))
(collect-garbage) (collect-garbage)
(time (dot1c v1 v2))
;; (collect-garbage) (collect-garbage)
;; (time (dot2c v1 v2))
;; (collect-garbage) (collect-garbage)
;; (time (dot3  fv1 fv2))
;; this one involves no chaperones, so we skip it
; (time (dot3c fv1 fv2))

#|
Timing results:

[samth@huor:~/sw/pycket (master) plt] ~/sw/pycket/pycket-c ~/tmp/dot.rkt
cpu time: 38 real time: 38 gc time: 0
2498917.90318
cpu time: 35 real time: 35 gc time: 0
2498917.90318
cpu time: 510 real time: 510 gc time: 0
2498917.90318
cpu time: 470 real time: 470 gc time: 0
2498917.90318
cpu time: 14 real time: 14 gc time: 0
2498917.90318

[samth@huor:~/sw/pycket (master) plt] r ~/tmp/dot.rkt
cpu time: 517 real time: 518 gc time: 13
2502935.368641578
cpu time: 105 real time: 105 gc time: 15
2502935.368641578
cpu time: 1132 real time: 1131 gc time: 41
2502935.368641578
cpu time: 675 real time: 674 gc time: 43
2502935.368641578
cpu time: 77 real time: 77 gc time: 14
2502935.368641578


|#
