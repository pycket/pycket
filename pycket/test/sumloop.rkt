#lang pycket

;; prelude

;------------------------------------------------------------------------------
(define (run-bench name count ok? run)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (apply run-maker args))
         (result (time (run-bench name count ok? run))))
    (when (not (ok? result))
      (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline)))))

(define sumloop-iters       1)

;;; SUMLOOP -- One of the Kernighan and Van Wyk benchmarks.

(define sum 0)

(define (tail-rec-aux i n)
  (if (< i n)
      (begin (set! sum (+ sum 1)) (tail-rec-aux (+ i 1) n))
      sum))

(define (tail-rec-loop n)
  (set! sum 0)
  (tail-rec-aux 0 n)
  sum)

(define (do-loop n)
  (set! sum 0)
  (do ((i 0 (+ i 1)))
      ((>= i n) sum)
    (set! sum (+ sum 1))))

(define (main . args)
  (run-benchmark
   "sumloop"
   sumloop-iters
   (lambda (result) (equal? result 100000000))
   (lambda (n) (lambda () (do-loop n)))
   100000000))

(main)
;; Local Variables:
;; mode: scheme
;; geiser-scheme-implementation: racket
;; End:
