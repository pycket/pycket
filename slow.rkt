#lang racket/base

;; Pycket does really poorly on the second version, but fine on the first, we need
;; a way to improve this.
;; Example taken from http://rfrn.org/~shu/2013/03/20/two-reasons-functional-style-is-slow-in-spidermonkey.html

(define N 50)
(define iters 50000)

(define (arr-for-each vs f)
  (let ([n (vector-length vs)])
    (let loop ([i 0])
      (if (= i n) (void)
        (begin
          (f (vector-ref vs i))
          (loop (+ i 1)))))))

(define vec
  (build-vector N
    (λ (_)
      (build-vector N
        (λ (_) (random))))))

(define max 0.0)

(define (loop f j n)
  (if (= n 1000000000)
    j
    (loop f (f j) (+ n 1))))

(displayln 'simple)

;; XXX Making use of W_PromotableClosure causes a significant performance hit here
(time (loop (lambda (x) (add1 x)) 0 0))
(time (loop (lambda (x) (sub1 x)) 0 0))
(time (loop (lambda (x) (add1 x)) 0 0))
(time (loop (lambda (x) (sub1 x)) 0 0))

(displayln 'vectors)

(displayln 'direct)
(time
  (for ([_ (in-range iters)])
    (set! max 0.0)
    (for ([i (in-range (vector-length vec))])
      (let ([vec^ (vector-ref vec i)])
      (for ([j (in-range (vector-length vec^))])
        (let ([v (vector-ref vec^ j)])
          (when (< max v) (set! max v))))))))

(displayln 'first)
(time
  (for ([_ (in-range iters)])
    (set! max 0.0)
    (arr-for-each vec
      (λ (vec^)
        (arr-for-each vec^
          (λ (v)
            (when (< max v) (set! max v))))))))

(displayln 'second)
;; This copy runs ~5.5x slower than the one above, despite being a verbatim copy.
(time
  (for ([_ (in-range iters)])
    (set! max 0.0)
    (arr-for-each vec
      (λ (vec^)
        (arr-for-each vec^
          (λ (v)
            (when (< max v) (set! max v))))))))

