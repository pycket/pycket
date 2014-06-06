#lang pycket

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
  (impersonate-procedure f (lambda (a) (values a))))

(define (iota n)
  (define (loop z)
    (if (eqv? n z) '() (cons z (loop (+ z 1)))))
  (loop 0))

(define (repeat f x n)
  (if (eqv? n 0)
    (void)
    (begin (f x) (repeat f x (sub1 n)))))

(time
  (disp
    (for-each
      (lambda (x) (repeat (ex fun) x 1000000))
      (iota 100))))

