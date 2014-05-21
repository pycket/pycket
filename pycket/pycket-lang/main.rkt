#lang racket
(provide (except-out (all-from-out racket) #%module-begin))
(require racket/unsafe/ops (for-syntax racket/base racket/runtime-path syntax/parse))
(provide (all-from-out racket/unsafe/ops))
(provide (rename-out [modbeg #%module-begin]))

(begin-for-syntax
 (define-runtime-path stdlib.sch "./stdlib.rktl")
 (define-splicing-syntax-class stdlib
   [pattern (~seq #:stdlib)
            #:with e (datum->syntax this-syntax `(include (file ,(path->string stdlib.sch))))]
   [pattern (~seq) #:with e #'(begin)]))

(define-syntax (modbeg stx)
  (syntax-case stx ()
    [(_ lib:stdlib forms ...)
     #`(#%plain-module-begin lib.e forms ...)]))

(module reader syntax/module-reader
  pycket)
