#lang racket/base
(require (prefix-in r: racket/fasl))
(define fasl->s-exp r:fasl->s-exp)
(define s-exp->fasl r:s-exp->fasl)
(provide (all-defined-out))
