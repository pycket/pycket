#lang pycket #:stdlib

(define (fun a)
  (+ (* a a) a))

(define example2
  (impersonate-vector
    (make-vector 1000)
    (lambda (a b c) c)
    (lambda (a b c) c)))

(define disp
  (lambda (x)
    (begin
      (display x)
      (display "\n")
      x)))

(define (ex f)
  (chaperone-procedure f
    (lambda (a) (values a))))

(define (ex2 f)
  (chaperone-procedure f
    (lambda (x)
      (if (even? x)
        (values x (lambda (k) (if (boolean? k) k (error 'ex2 "Not boolean output"))))
        (error 'ex2 "Number is not even")))))

(define (iota n)
  (define (loop z)
    (if (eqv? n z) '() (cons z (loop (+ z 1)))))
  (loop 0))

(define (repeat f x n)
  (if (eqv? n 0)
    (void)
    (begin (f x) (repeat f x (sub1 n)))))

;; Time unwrapped function
(time
  (for-each
    (lambda (x) (repeat fun x 1000000))
    (iota 100)))

;; One layer of impersonation
(time
  (let ([f (ex fun)])
    (for-each
      (lambda (x) (repeat f x 1000000))
      (iota 100))))

;; Two layers of impersonation
(time
  (let ([f (ex (ex fun))])
    (for-each
      (lambda (x) (repeat f x 1000000))
      (iota 100))))
