(define (exists f l)
  (if (null? l) #f
      (or (f (car l))
          (exists f (cdr l)))))

(define (for-all f l)
  (if (null? l) #t
      (and (f (car l))
           (exists f (cdr l)))))

(define (for-each f as . bss)
  (cond [(and (null? as)
              (andmap null? bss))
         (void)]
        [(or (null? as)
             (ormap null? bss))
         (void) #;(error 'for-each "list lengths differ")]
        [else 
         (apply f (car as) (map car bss))
         (apply for-each f (cdr as) (map cdr bss))]))

(define (zero? z) (= z 0))
(define (not b) (if b #f #t))
(define call-with-current-continuation call/cc)

(define (newline) (write "\n"))

(define (append a b)
  (if (null? a) 
      b
      (cons (car a) (append (cdr a) b))))

(define ormap exists)
(define andmap for-all)

(define (map f l)
  (if (null? l)
      l
      (cons (f (car l))
            (map f (cdr l)))))

(define (member v l)
  (if (null? l) 
      #f
      (if (equal? v (car l))
          l
          (member v (cdr l)))))

(define (reverse l)
  (let loop ([acc null] [l l])
    (if (null? l)
        acc
        (loop (cons (car l) acc) (cdr l)))))
