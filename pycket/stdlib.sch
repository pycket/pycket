(define (for-each f l)
  (if (null? l)
      (void)
      (begin (f (car l))
             (for-each f (cdr l)))))

(define (zero? z) (= z 0))
(define (not b) (if b #f #t))
(define call-with-current-continuation call/cc)

(define (newline) (write "\n"))
