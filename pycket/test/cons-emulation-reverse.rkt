#lang racket/base
(define racket-null null)
(let*-values (
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
  (define (reverse l)
    (define (reverse-acc old new)
      (if (null? old) new (reverse-acc (cdr old) (cons (car old) new))))
    (reverse-acc l '()))
  (define my-list (make-list 10000 1))
  (reverse my-list)
  1)
