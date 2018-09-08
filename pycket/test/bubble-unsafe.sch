;; The Bubble sort benchmark from Strickland et al 2012
;;(let ()
(define SIZE 10000)

(define vec (make-vector SIZE))

(define (bubble-sort vec)
  (define SIZE-1 (unsafe-fx- SIZE 1))
  (if (let loop ([swapped? #f] [i 0])
        (if (= i SIZE-1)
            swapped?
            (let ([a (unsafe-vector-ref vec i)]
                  [b (unsafe-vector-ref vec (unsafe-fx+ 1 i))])
              (if (unsafe-fx> a b)
                  (begin
                    (unsafe-vector-set! vec i b)
                    (unsafe-vector-set! vec (unsafe-fx+ 1 i) a)
                    (loop #t (unsafe-fx+ i 1)))
                  (loop swapped? (unsafe-fx+ 1 i))))))
      (bubble-sort vec)
      #f))
(let loop ([i 0])
  (if (unsafe-fx< i SIZE)
      (begin
        (unsafe-vector-set! vec i (unsafe-fx- SIZE i))
        (loop (unsafe-fx+ 1 i)))
      #f))

(time (bubble-sort vec))
;(bubble-sort vec)
;)
