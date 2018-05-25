#lang pycket #:stdlib

(define (assp f l)
  (if (null? l)
      #f
      (if (f (car (car l)))
          (car l)
          (assp f (cdr l)))))

(define unit (lambda (s/c) (choice s/c mzero)))

(define success (lambda (s/c) (unit s/c)))

(define failure (lambda (s/c) mzero))

(define (var c) (vector c))

(define (var? x) (vector? x))

(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define mzero '())

(define (choice s/c f) (cons s/c f))

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      ((eqv? u v) s)
      (else #f))))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) (if (occurs-check x v s) #f `((,x . ,v) . ,s)))

(define (occurs-check x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (var=? v x))
      ((pair? v) (or (occurs-check x (car v) s)
                     (occurs-check x (cdr v) s)))
      (else #f))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))

(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))


(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (choice (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

(define a-and-b
  (conj 
   (call/fresh (lambda (a) (== a 7)))
   (call/fresh 
    (lambda (b) 
      (disj
       (== b 5)
       (== b 6))))))

(define fives
  (lambda (x)
    (disj
     (== x 5)      
     (lambda (a/c)
       (lambda ()
         ((fives x) a/c))))))

(newline)
(display (a-and-b '(() . 0)))
(newline)

(display 
'((((#(1) . 5) (#(0) . 7)) . 2)
  (((#(1) . 6) (#(0) . 7)) . 2)))
(newline)

(display
 ((call/fresh fives) '(() . 0)))
(newline)

(display '((((#(0) . 5)) . 1) . __procedure__))
(newline)

