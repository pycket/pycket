#lang racket/base

(require racket/contract racket/flonum racket/unsafe/ops)

(define N 10000000)

(define (dot1 v1 v2)
  (for/sum ([e1 v1] [e2 v2]) (* e1 e2)))

(define (dot2 v1 v2)
  (for/sum ([e1 (in-vector v1)] [e2 (in-vector v2)]) (fl* e1 e2)))

(define (dot-fast v1 v2)
  (for/fold ([sum 0.0]) ([i (in-range (vector-length v1))])
    (unsafe-fl+ sum (unsafe-fl* (unsafe-vector-ref v1 i) (unsafe-vector-ref v2 i)))))

(define (dot-fastest v1 v2)
  (define len (vector-length v1))
  (unless (= len (vector-length v2))
    (error 'fail))
  (let loop ([n 0] [sum 0.0])
    (if (unsafe-fx= len n) sum
        (loop (unsafe-fx+ 1 n) (unsafe-fl+ sum (unsafe-fl* (unsafe-vector-ref v1 n)
                                                           (unsafe-vector-ref v2 n)))))))

(define (dot-flfastest v1 v2)
  (define len (flvector-length v1))
  (unless (= len (flvector-length v2))
    (error 'fail))
  (let loop ([n 0] [sum 0.0])
    (if (unsafe-fx= len n) sum
        (loop (unsafe-fx+ n 1) (unsafe-fl+ sum (unsafe-fl* (unsafe-flvector-ref v1 n)
                                                           (unsafe-flvector-ref v2 n)))))))

(define/contract (dot1c v1 v2)
  ((vectorof flonum?) (vectorof flonum?) . -> . flonum?)
  (for/sum ([e1 v1] [e2 v2]) (* e1 e2)))


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

(define fv1 (for/flvector #:length N ([i (in-range N)]) (vector-ref v1 i)))
(define fv2 (for/flvector #:length N ([i (in-range N)]) (vector-ref v2 i)))

(collect-garbage) (collect-garbage)
'dot1
(time (dot1 v1 v2))
(collect-garbage) (collect-garbage)
'dot-fast
(time (dot-fast v1 v2))
(collect-garbage) (collect-garbage)
'dot-fastest
(time (dot-fastest v1 v2))
(collect-garbage) (collect-garbage)
'dot2
(time (dot2 v1 v2))
(collect-garbage) (collect-garbage)
'dot1c
(time (dot1c v1 v2))
(collect-garbage) (collect-garbage)
'dot2c
(time (dot2c v1 v2))
(collect-garbage) (collect-garbage)
'dot3
(time (dot3  fv1 fv2))
'dot3c
(collect-garbage) (collect-garbage)
;; this one involves no chaperones, so we skip it
(time (dot3c fv1 fv2))
(collect-garbage) (collect-garbage)
'dot-flfastest
(time (dot-flfastest fv1 fv2))

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
