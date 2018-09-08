#lang pycket

(define a (make-thread-cell 'a #t))
(define b (make-thread-cell 'b #f))
(define c (make-thread-cell 'c #t))

(define save (current-preserved-thread-cell-values))

(printf "~s~n" (thread-cell-ref a))
(printf "~s~n" (thread-cell-ref b))
(printf "~s~n" (thread-cell-ref c))

(thread-cell-set! a 'a2)
(thread-cell-set! b 'b2)
(thread-cell-set! c 'c2)

(unless
  (and
    (thread-cell-values? save)
    (eqv? (thread-cell-ref a) 'a2)
    (eqv? (thread-cell-ref b) 'b2)
    (eqv? (thread-cell-ref c) 'c2))
  (error 'thread-cell "Thread cell values are wrong"))

(printf "~s~n" (thread-cell-ref a))
(printf "~s~n" (thread-cell-ref b))
(printf "~s~n" (thread-cell-ref c))

(current-preserved-thread-cell-values save)

(printf "~s~n" (thread-cell-ref a))
(printf "~s~n" (thread-cell-ref b))
(printf "~s~n" (thread-cell-ref c))

(unless
  (and
    (thread-cell-values? save)
    (eqv? (thread-cell-ref a) 'a)
    (eqv? (thread-cell-ref b) 'b2)
    (eqv? (thread-cell-ref c) 'c))
  (error 'thread-cell "Thread cell values are wrong"))

