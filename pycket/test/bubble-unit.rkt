#lang racket/base

(require racket/unit)

;; The Bubble sort benchmark from Strickland et al 2012
;; made into a rather silly unit

(define-signature constants (SIZE vec))

(define-unit constants@
  (import)
  (export constants)
  (define SIZE 10000)
  
  (define vec (make-vector SIZE)))

(define-signature bubble (bubble-sort))

(define-unit bubble@
  (import constants)
  (export bubble)
  (define (bubble-sort vec)
    (define SIZE-1 (- SIZE 1))
    (if (let loop ([swapped? #f] [i 0])
          (if (= i SIZE-1)
              swapped?
              (let ([a (vector-ref vec i)]
                    [b (vector-ref vec (+ 1 i))])
              (if (> a b)
                  (begin
                    (vector-set! vec i b)
                    (vector-set! vec (+ 1 i) a)
                    (loop #t (+ i 1)))
                  (loop swapped? (+ 1 i))))))
        (bubble-sort vec)
        #f)))

(define-unit go@
  (import constants bubble)
  (export)
  (let loop ([i 0])
    (if (< i SIZE)
        (begin
          (vector-set! vec i (- SIZE i))
          (loop (+ 1 i)))
        #f))
  
  (time (bubble-sort vec)))

(invoke-unit/infer (link bubble@ constants@ go@))
