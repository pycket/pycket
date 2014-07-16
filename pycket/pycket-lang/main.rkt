#lang racket
(provide (except-out (all-from-out racket) #%module-begin))
(require racket/unsafe/ops (for-syntax racket/base racket/runtime-path syntax/parse)
         racket/include)
(require (prefix-in r5: r5rs) (prefix-in mz: mzscheme))
(provide (all-from-out racket/unsafe/ops))
;; for now, white-listed for benchmarks.
(provide mz:call-with-output-file
         r5:lambda
         r5:define
         r5:lambda
         r5:apply
         r5:string->list
         r5:list->string
         r5:vector->list
         r5:list->vector
         r5:list
         r5:quote
         r5:quasiquote
         r5:unquote)
(provide (rename-out [modbeg #%module-begin]))

(provide include)

(begin-for-syntax
 (define-runtime-path stdlib.sch "./stdlib.rktl")
 (define-splicing-syntax-class stdlib
   [pattern (~seq (~and form #:stdlib))
            #:with e (datum->syntax #'form `(include (file ,(path->string stdlib.sch))))]
   [pattern (~seq) #:with e #'(begin)]))

(define-syntax (modbeg stx)
  (syntax-parse stx
    [(_ lib:stdlib forms ...)
     #`(#%plain-module-begin lib.e forms ...)]))

(module reader syntax/module-reader
  pycket)
