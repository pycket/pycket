#lang pycket #:stdlib

(define size 100)
(define times 100)

(define (fun a)
  (+ (* a a) a))

(define (ex f)
  (chaperone-procedure f
    (lambda (a) (values a))))

(define (ex2 f)
  (chaperone-procedure f
    (lambda (x)
      (if (even? x)
        (values (lambda (k) (if (boolean? k) k (error 'ex2 "Not boolean output"))) x)
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
    (lambda (x) (repeat fun x times))
    (iota size)))

;; One layer of impersonation
(time
  (let ([f (ex fun)])
    (for-each
      (lambda (x) (repeat f x times))
      (iota size))))

;; Two layers of impersonation
(time
  (let ([f (ex (ex fun))])
    (for-each
      (lambda (x) (repeat f x times))
      (iota size))))

(define cnt 0)

(define f (lambda (x) x))

(define f*
  (impersonate-procedure f
    (lambda (arg)
      (values
        (lambda (s)
          (set! cnt (+ cnt 1))
          (when (eqv? s 'secret)
            (error 'f* "Its a secret"))
          s)
        arg)
      )))

(f* 0)
(unless (eqv? cnt 1) (error 'cnt "wrong count"))
