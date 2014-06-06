#lang pycket

(define my-len 10000)
(define max-val 10000)

(define check
  (lambda (v)
    (chaperone-vector v
      (lambda (vec i val)
        (if (> val max-val)
          (error 'check-ref "Check is out of bounds in ref")
          val))
      (lambda (vec i val)
        (if (> val max-val)
          (error 'check-set "Check is out of bounds in set")
          val)))))

(define my-vec (make-vector my-len))
(define imp-vec (check (make-vector my-len)))
(define imp-imp-vec (check (check (make-vector my-len))))

(define (repeat f n)
  (if (eqv? n 0)
    (void)
    (begin (f) (repeat f (sub1 n)))))

(define (iota n)
  (define (loop z)
    (if (eqv? n z) '() (cons z (loop (+ z 1)))))
  (loop 0))

(define disp
  (lambda (x)
    (begin
      (display x)
      (display "\n")
      x)))

(time
  (for-each
    (lambda (n)
      (repeat
        (lambda () (vector-set! my-vec n (+ (vector-ref my-vec n) 1))) max-val))
    (iota my-len)))

(time
  (for-each
    (lambda (n)
      (repeat
        (lambda () (vector-set! imp-vec n (+ (vector-ref imp-vec n) 1))) max-val))
    (iota my-len)))

(time
  (for-each
    (lambda (n)
      (repeat
        (lambda () (vector-set! imp-imp-vec n (+ (vector-ref imp-imp-vec n) 1))) max-val))
    (iota my-len)))

;(for-each
  ;(lambda (n)
    ;(disp (vector-ref imp-imp-vec n)))
  ;(iota my-len))

