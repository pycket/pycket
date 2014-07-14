#lang pycket
;;(require (except-in racket/tcp tcp-listen))
;;(require "./imp-vector.rkt")
;;(require (only-in "./imp-vector.rkt" check))
;;(require (rename-in "./imp-vector.rkt" [check check^]))
;;(require racket/unsafe/ops)
;;(require (prefix-in check "pycket/test/imp-vector.rkt"))
(require "./test-module2.rkt")

;;(printf "\\")
;;(printf "two = ~s~n" two)
;;(printf "three = ~s~n" three)
(printf "~s~n" '#hash(("apple" . red) ("banana" . yellow)))
(printf "~s~n" 'red)
;;(printf "~s~n" (cons 1 (cons 2 (cons 3 4))))
;;(define k check)
;;(displayln k)

