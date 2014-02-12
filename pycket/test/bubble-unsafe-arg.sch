;; The Bubble sort benchmark from Strickland et al 2012
;;(let ()
(define SIZE 10000)

(define vec (make-vector SIZE))

(define SIZE-1 (- SIZE 1))

(define (inner-loop swapped? i vec l)
  (if (unsafe-fx= i SIZE-1)
      swapped?
      (let ([a (unsafe-vector-ref vec i)]
            [b (unsafe-vector-ref vec (unsafe-fx+ 1 i))])
        (if (> a b)
            (begin
              (unsafe-vector-set! vec i b)
              (unsafe-vector-set! vec (unsafe-fx+ 1 i) a)
              (l #t (unsafe-fx+ i 1) vec l))
            (l swapped? (unsafe-fx+ 1 i) vec l)))))

(define (bubble-sort vec)
  (if (inner-loop #f 0 vec inner-loop)
      (bubble-sort vec)
      #f))
(let loop ([i 0])
  (if (< i SIZE)
      (begin
        (unsafe-vector-set! vec i (unsafe-fx- SIZE i))
        (loop (unsafe-fx+ 1 i)))
      #f))

(time (bubble-sort vec))
;(bubble-sort vec)
;)
