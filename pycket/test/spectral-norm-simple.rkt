#lang racket/base
;; The Computer Language Benchmarks Game
;; http://benchmarksgame.alioth.debian.org/
;; Translated from Mike Pall's Lua version.
;; Parallelized by Sam Tobin-Hochstadt

(require racket/cmdline)

(define-syntax-rule (for/par k ([i N]) b)  
  (let ([stride (quotient N k)])
    (for ([n (in-range k)])
      (for ([i (in-range (* n stride) (min N (* (+ n 1) stride)))]) b))))


;; the big let improves performance by about 20%
;(let* ()
  (define N (command-line #:args ([n "5"]) (string->number n)))
  (define (A i j)
    (let ([ij (+ i j)])
      (/ 1.0 (+ (* (* ij
                      (+ ij 1))
                   0.5) 
                 (+ i 1)))))
  (define (Av x y N)
    (for/par 1 ([i N])
             (vector-set!
              y i
              (let L ([a 0.0] [j 0])
                (if (= j N) a
                    (L (+ a (* (vector-ref x j) (A i j)))
                       (+ j 1)))))))
  (define (Atv x y N)
    (for/par 1 ([i N])
             (vector-set!
              y i
              (let L ([a 0.0] [j 0])
                (if (= j N) a
                    (L (+ a (* (vector-ref x j) (A j i)))
                       (+ j 1)))))))
  (define (AtAv x y t N) (Av x t N) (Atv t y N))
  (define u (make-vector N 1.0))
  (define v (make-vector N))
  (define t (make-vector N))
  (time (begin
    (for ([i (in-range 10)])
      (AtAv u v t N) (AtAv v u t N))
    (displayln  (sqrt 
                 (let L ([vBv 0.0] [vv 0.0] [i 0])
                   (if (= i N) (/ vBv vv)
                       (let ([ui (vector-ref u i)] [vi (vector-ref v i)])
                         (L (+ vBv (* ui vi))
                            (+ vv (* vi vi))
                            (+ i 1))))))
                )))

