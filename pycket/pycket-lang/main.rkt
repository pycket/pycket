#lang racket/base
(provide (except-out (all-from-out racket/base) #%module-begin))
(require racket/unsafe/ops (for-syntax racket/base racket/runtime-path)
         racket/include (prefix-in k: '#%kernel) racket/contract
         (only-in '#%kernel random))
(provide random)
(provide (all-from-out racket/unsafe/ops))
(provide (rename-out [modbeg #%module-begin]))
(provide include contract)
;------------------------------------------------------------------------------

(begin-for-syntax
 (define-runtime-path stdlib.sch "./stdlib.rktl"))

;------------------------------------------------------------------------------

(define-syntax (modbeg stx)
  (syntax-case stx ()
    [(_ stdlib-kw forms ...)
     (eq? (syntax-e #'stdlib-kw) '#:stdlib)
     #`(#%plain-module-begin
        (require (only-in '#%kernel chaperone-procedure))
        (require (only-in '#%kernel impersonate-procedure))
        #,(datum->syntax #'stdlib-kw `(include (file ,(path->string stdlib.sch))))
        forms ...)]
    [(_ forms ...)
     #`(#%plain-module-begin forms ...)]))

(module reader syntax/module-reader
  pycket)
