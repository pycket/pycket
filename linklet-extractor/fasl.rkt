#lang racket/base
(provide fasl->s-exp s-exp->fasl)
(require (prefix-in r: racket/fasl))
(define (fasl->s-exp i intern?) (r:fasl->s-exp i #:datum-intern? intern?))
(define (s-exp->fasl v o k) (r:s-exp->fasl v o #:keep-mutable? k))
