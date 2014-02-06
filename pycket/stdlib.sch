(define (for-each f l)
  (if (null? l)
      (void)
      (begin (f (car l))
             (for-each f (cdr l)))))

(define (zero? z) (= z 0))
(define (not b) (if b #f #t))
(define call-with-current-continuation call/cc)

(define (newline) (write "\n"))

(define (append a b)
  (if (null? a) 
      b
      (cons (car a) (append (cdr a) b))))

(define (exists f l)
  (if (null? l) #f
      (or (f (car l))
          (exists f (cdr l)))))

(define (for-all f l)
  (if (null? l) #t
      (and (f (car l))
           (exists f (cdr l)))))

(define (map f l)
  (if (null? l)
      l
      (cons (f (car l))
            (map f (cdr l)))))
