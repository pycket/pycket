(define (myapp a b)
  (if (null? a) 
      b
      (cons (car a) (myapp (cdr a) b))))
(define (makelist a)
  (if (zero? a)
      '()
      (cons a (makelist (- a 1)))))

(append (makelist 10000000) (makelist 1000000))
