#lang racket/base

(define-syntax-rule (gc)
  (begin (collect-garbage) (collect-garbage) (collect-garbage)))
(define N 10000000)

;(printf "Immutable map:~n")
;(let ([M (hash)])
;  (printf "  Write: ")
;  (gc) (time (for ([i N]) (set! M (hash-set M i #f))))
;  (printf "  Read: ")
;  (gc) (time (for ([i N]) (hash-ref M i))))
;

(printf "Immutable eq-map:~n")
(let ([M (hasheq)])
  (printf "  Write: ")
  (gc) (time (for ([i N]) (set! M (hash-set M i #f))))
  (printf "  Read: ")
  (gc) (time (for ([i N]) (hash-ref M i))))

(printf "Mutable map:~n")
(let ([M (make-hash)])
  (printf "  Write: ")
  (gc) (time (for ([i N]) (hash-set! M i #f)))
  (printf "  Read: ")
  (gc) (time (for ([i N]) (hash-ref M i))))

(printf "Mutable eq-map:~n")
(let ([M (make-hasheq)])
  (printf "  Write: ")
  (gc) (time (for ([i N]) (hash-set! M i #f)))
  (printf "  Read: ")
  (gc) (time (for ([i N]) (hash-ref M i))))

; Immutable map:
;   Write: cpu time: 9737 real time: 9734 gc time: 2254
;   Read: cpu time: 1123 real time: 1123 gc time: 0
; Immutable eq-map:
;   Write: cpu time: 9365 real time: 9362 gc time: 2191
;   Read: cpu time: 1017 real time: 1016 gc time: 0
; Mutable map:
;   Write: cpu time: 4584 real time: 4583 gc time: 636
;   Read: cpu time: 2392 real time: 2390 gc time: 0
; Mutable eq-map:
;   Write: cpu time: 1166 real time: 1166 gc time: 524
;   Read: cpu time: 428 real time: 427 gc time: 0
