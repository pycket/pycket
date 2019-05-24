#lang racket/base

(provide (all-defined-out))

(define (hash-set* h . kvs)
  (let loop ([kvs kvs] [h h])
    (if (null? kvs)
        h
        (let* ([k (car kvs)]
               [v (cadr kvs)]
               [h (if v (hash-set h k v) h)])
          (loop (cddr kvs) h)))))

(define (hash* . kvs) (apply hash-set* (hash) kvs))

(define extended-reals (list +inf.0 +inf.f
                             -inf.0 -inf.f
                             +nan.0 +nan.f))

(define (convert-single-precs ext-real)
  (cond
    [(or (eqv? ext-real -nan.f) (eqv? ext-real +nan.f)) "+nan.0"]
    [(or (eqv? ext-real +inf.f) (eqv? ext-real +inf.0)) "+inf.0"]
    [(or (eqv? ext-real -inf.f) (eqv? ext-real -inf.0)) "-inf.0"]
    [else (number->string ext-real)]))

(define (improper-list? ls)
  (and (pair? ls) (not (list? ls))))
