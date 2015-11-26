#lang racket/base
(struct element (x))
(let*-values (
                [(struct:cons cons pair?  cons-ref cons-set!)
                 (make-struct-type 'cons #f 2 0 #f '() #f #f '(0 1) #f 'cons)]
                [(car) (make-struct-field-accessor cons-ref 0)]
                [(cdr) (make-struct-field-accessor cons-ref 1)]
                [(null) (cons (void) (void))]
                [(null?) (lambda (c) (and (eq? (car c) (void)) (eq? (cdr c) (void))))])
  (define (make-list n e)
    (define (mk-acc m e l)
      (if (= 0 m) l (mk-acc (- m 1) e (cons e l))))
    (mk-acc n e null))
  (letrec
      ([e (element 1)]
       [f (element 2)]
       [head car]
       [tail cdr]
       [racket-map (lambda (f l)
                   (if (null? l)
                       '()
                       (cons (f (head l)) (racket-map f (tail l)))))]
     [swap (lambda (x)
             (if (eq? x e) f e))]
     [num 50000]
     [l (make-list num e)])
  (racket-map swap l))
  1)
