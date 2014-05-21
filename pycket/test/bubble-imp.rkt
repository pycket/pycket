#lang s-exp pycket-lang
;; The Bubble sort benchmark from Strickland et al 2012
;;(let ()
(define SIZE 10000)

(define vec (impersonate-vector (make-vector SIZE) (lambda (x y z) z) (lambda (x y z) z)))

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
      #f))
(let loop ([i 0])
  (if (< i SIZE)
      (begin
        (vector-set! vec i (- SIZE i))
        (loop (+ 1 i)))
      #f))

(time (bubble-sort vec))
;(bubble-sort vec)
;)
