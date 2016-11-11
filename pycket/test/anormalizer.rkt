#lang racket

(require racket/match)

(define (normalize-term M) (normalize M (λ (x) x)))
(define (value? x) (or (symbol? x) (number? x)))

(define (normalize M k)
  (match M
    [`(lambda ,params ,body)
      (k `(lambda ,params ,(normalize-term body)))]
    [`(let ([,x1 ,M1]) ,M2)
      (normalize M1 (λ (N1) `(let ([,x1 ,N1]) ,(normalize M2 k))))]
    [`(if0 ,M1 ,M2 ,M3)
      (normalize-name M1 (λ (t) (k `(if0 ,t ,(normalize-term M2) ,(normalize-term M3)))))]
    [`(,Fn . ,M*)
      (normalize-name Fn (λ (t) (normalize-name* M* (λ (t*) (k `(,t . ,t*))))))]
    [V (k V)]
    ))

(define (normalize-name M k)
  (normalize M (λ (N) (if (value? N) (k N) (let ([t (gensym)]) `(let ([,t ,N]) ,(k t)))))))

(define (normalize-name* M* k)
  (if (null? M*) (k '())
    (normalize-name (car M*) (λ (t) (normalize-name* (cdr M*) (λ (t*) (k `(,t . ,t*))))))))

(define test '(+ (f x) (g y)))
(normalize-term test)

(define test2 '(let ([x (if0 (f y) (g y) (h y))]) (+ x 1)))
(normalize-term test2)

(define test3 '(let ([x (let ([y (f g)]) y)]) (+ x y)))
(normalize-term test3)

(define test4 '(+ (if0 p e1 e2) e2))
(normalize-term test4)
