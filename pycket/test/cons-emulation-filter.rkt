#lang racket/base
(struct element (x))
(let*-values (
              [(struct:cons cons pair?  cons-ref cons-set!)
               (make-struct-type 'cons #f 2 0 #f '() #f #f '(0 1) #f 'cons)]
              [(car) (make-struct-field-accessor cons-ref 0)]
              [(cdr) (make-struct-field-accessor cons-ref 1)]
              [(null) (cons (void) (void))]
              [(null?) (lambda (c) (and (eq? (car c) (void)) (eq? (cdr c) (void))))])
  (define (make-alter-list n e f)
    (define (mk-acc m l)
      (if (= 0 m) l (mk-acc (- m 1) (cons (if (odd? m) e f) l))))
    (mk-acc n null))
  (letrec
      ([e (element 1)]
       [f (element 2)]
       [head car]
       [tail cdr]
       [racket-filter (lambda (p l)
                        (cond [(null? l) null]
                              [(p (head l))
                               (cons (head l) (racket-filter p (tail l)))]
                              [else (racket-filter p (tail l))]))]
       [flt (lambda (x) (equal? x e))]
       [num 50000]
       [l (make-alter-list num e f)])
    (racket-filter flt l))
  1)
