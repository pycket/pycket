(define (for-each f l)
  (if (null? l)
      (void)
      (begin (f (car l))
             (for-each f (cdr l)))))

(define (zero? z) (= z 0))
