#lang racket/base

;; Use the partner file "streams.rkt" to implement the Sieve of Eratosthenes.
;; Then compute and print the 10,000th prime number.

(module streams typed/racket/base #:no-optimize
  (require typed/racket/unsafe)
  ;; Simple streams library.
  ;; For building and using infinite lists.

  (provide make-stream)
  (unsafe-provide
   stream-unfold
   stream-get
   stream-take)

  ;; ;; A stream is a cons of a value and a thunk that computes the next value when applied
  (struct: stream ([first : Natural] [rest : (-> stream)]))

  ;;--------------------------------------------------------------------------------------------------

  (: make-stream (-> Natural (-> stream) stream))
  (define (make-stream hd thunk)
    (stream hd thunk))

  ;; Destruct a stream into its first value and the new stream produced by de-thunking the tail
  (: stream-unfold (-> stream (values Natural stream)))
  (define (stream-unfold st)
    (values (stream-first st) ((stream-rest st))))

  ;; [stream-get st i] Get the [i]-th element from the stream [st]
  (: stream-get (-> stream Natural Natural))
  (define (stream-get st i)
    (define-values (hd tl) (stream-unfold st))
    (cond [(= i 0) hd]
          [else    (stream-get tl (sub1 i))]))

  ;; [stream-take st n] Collect the first [n] elements of the stream [st].
  (: stream-take (-> stream Natural (Listof Natural)))
  (define (stream-take st n)
    (cond [(= n 0) '()]
          [else (define-values (hd tl) (stream-unfold st))
                (cons hd (stream-take tl (sub1 n)))]))
  )

(require 'streams)
;;--------------------------------------------------------------------------------------------------

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define (count-from n)
  (make-stream n (lambda () (count-from (add1 n)))))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new stream.
(define (sift n st)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-stream hd (lambda () (sift n tl)))]))

;; `sieve st` Sieve of Eratosthenes
(define (sieve st)
  (define-values (hd tl) (stream-unfold st))
  (make-stream hd (lambda () (sieve (sift hd tl)))))

;; stream of prime numbers
(define primes (sieve (count-from 2)))

;; Compute the 10,000th prime number
(define N-1 999)

(define (main)
  (printf "The ~a-th prime number is: ~a\n" (add1 N-1) (stream-get primes N-1)))

(time (main))
