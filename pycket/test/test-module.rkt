#lang pycket

(define v1 (make-vector 10000 0))
(define v2 (make-vector 10000 0))

(define count 0)

(define (increment v)
  (impersonate-vector v
    (lambda (a b c) (add1 c))
    (lambda (a b c) c)))

(define v1*
  (increment
    (increment
      (increment
        (impersonate-vector v1
          (lambda (a b c) (set! count (+ count 1)) b)
          (lambda (a b c) b))))))

(define v2*
  (increment
    (increment
      (increment
        (impersonate-vector v2
          (lambda (a b c) (set! count (+ count 1)) b)
          (lambda (a b c) b))))))

(define v1^ (make-vector 10000 v1*))
(define v2^ (make-vector 10000 v2*))

(time (void (equal? v1^ v2^)))

;;;(equal? v1 v2)
;;;;(printf "Called handlers: ~sx~n" count)
;;(time (void (equal? l1 l2)))
;;;;(equal? v1^^ v2^^)


