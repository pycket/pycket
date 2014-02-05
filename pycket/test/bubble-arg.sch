;; The Bubble sort benchmark from Strickland et al 2012
;;(let ()
(define SIZE 10000)

(define vec (make-vector SIZE))

(define SIZE-1 (- SIZE 1))

(define (inner-loop swapped? i vec l)
  (if (= i SIZE-1)
      swapped?
      (let ([a (vector-ref vec i)]
            [b (vector-ref vec (+ 1 i))])
        (if (> a b)
            (begin
              (vector-set! vec i b)
              (vector-set! vec (+ 1 i) a)
              (l #t (+ i 1) vec l))
            (l swapped? (+ 1 i) vec l)))))

(define (bubble-sort vec)
  (if (inner-loop #f 0 vec inner-loop)
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
