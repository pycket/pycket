#lang racket/base
(require (prefix-in r: racket/fasl))
(define (fasl->s-exp o [i #f]) (r:fasl->s-exp o i)) 
(define (s-exp->fasl v [o #f] [k #f]) (r:s-exp->fasl v o k))
(provide (all-defined-out))
