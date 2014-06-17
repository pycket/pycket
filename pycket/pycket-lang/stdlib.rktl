
(require (only-in '#%kernel chaperone-procedure))
(require (only-in '#%kernel impersonate-procedure))

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

(define (ormap f l) (exists f l))
(define (andmap f l) (for-all f l))

(define (map f l)
  (if (null? l)
      l
      (cons (f (car l))
            (map f (cdr l)))))

(define (filter pred l)
  (if (null? l)
      '()
      (if (pred (car l))
          (cons (car l)
               (filter pred (cdr l)))
          (filter pred (cdr l)))))

(define (foldr f v l)
  (if (null? l)
      v
      (f (car l)
         (foldr f v (cdr l)))))

(define (foldl f acc l)
  (if (null? l)
      acc
      (foldl f (f acc (car l)) (cdr l))))

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

(define (displayln e)
  (display e) (newline))

(define (negative? v) (< v 0))
(define (positive? v) (> v 0))

(define (caar v) (car (car v)))
(define (cdar v) (cdr (car v)))

(define (odd? v)
  (and (integer? v) (= 1 (modulo v 2))))

(define (even? v)
  (and (integer? v) (= 0 (modulo v 2))))

(define (max u v) (if (> u v) u v))
(define (min u v) (if (< u v) u v))

(define (memq s l)
  (cond [(null? l) #f]
        [(pair? l)
         (define x (car l))
         (if (eq? x s) l (memq s (cdr l)))]
        [else (error 'memq)]))

(define (exact-nonnegative-integer? n)
  (and (integer? n) (exact? n) (>= n 0)))

(define true #t)
(define false #f)

;; Local Variables:
;; mode: scheme
;; geiser-scheme-implementation: racket
;; End:
