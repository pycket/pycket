#lang racket/base
(define racket-null null)
(struct element ())
(let*-values (
                [(*e*) (element)]
                [(struct:cons cons pair?  cons-ref cons-set!)
                 (make-struct-type 'cons #f 2 0 #f racket-null (make-inspector) #f '(0 1) #f 'cons)]
                [(car) (make-struct-field-accessor cons-ref 0)]
                [(cdr) (make-struct-field-accessor cons-ref 1)]
                [(null) (cons (void) (void))]
                [(null?) (lambda (c) (and (eq? (car c) (void)) (eq? (cdr c) (void))))])
  (define (make-list n e)
    (define (mk-acc m e l)
      (if (= 0 m) l (mk-acc (- m 1) e (cons e l))))
    (mk-acc n e null))
  (define (map proc l) (if (null? l) l (cons (proc (car l)) (map proc (cdr l)))))
  (define (accumulate op init list) (if (null? list)
                                       init
                                       (op (car list)
                                          (accumulate op init (cdr list)))))
  (define my-list (make-list 100000 *e*))
  (define my-num-list (map (lambda (x) (if (element? x) 1 3)) my-list))
  (accumulate + 1 my-num-list))
