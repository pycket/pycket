#lang pycket

(define (n->f n)
  (cond
   [(zero? n) (λ (f) (λ (x) x))]
   [else 
    (define n-1 (n->f (- n 1)))
    (λ (f) 
       (define fn-1 (n-1 f))
       (λ (x) (f (fn-1 x))))]))

(define (f->n c)
  ((c (λ (x) (+ x 1))) 0))

(define (c:* n1)
  (λ (n2) 
     (λ (f) 
        (n1 (n2 f)))))

(define (c:zero? c)
  ((c (λ (x) #f)) #t))

;; taken from Wikipedia (but lifted out
;; the definition of 'X')
(define (c:sub1 n)
  (λ (f) 
     (define X (λ (g) (λ (h) (h (g f)))))
     (λ (x) 
        (((n X) 
          (λ (u) x)) 
         (λ (u) u)))))


(define (c:! n)
  (cond
   [(c:zero? n) (λ (f) f)]
   [else ((c:* n) (c:! (c:sub1 n)))]))

(time (display (f->n (c:! (n->f 6)))))
