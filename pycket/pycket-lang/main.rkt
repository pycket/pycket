#lang racket/base
(provide (except-out (all-from-out racket/base) #%module-begin))
(require racket/unsafe/ops (for-syntax racket/base racket/runtime-path)
         racket/include (prefix-in k: '#%kernel) racket/contract)
(provide (all-from-out racket/unsafe/ops))
;; for now, white-listed for benchmarks.
(provide k:call-with-output-file)
(provide (rename-out [modbeg #%module-begin]))

(provide include time if contract)

;------------------------------------------------------------------------------
;one-armed if.
(define-syntax if
  (syntax-rules ()
    ((if expr true-branch)
     (when expr true-branch))
    ((if expr true-branch false-branch)
     (cond
       (expr true-branch)
       (else false-branch)))))
;------------------------------------------------------------------------------

(begin-for-syntax
 (define-runtime-path stdlib.sch "./stdlib.rktl"))

;------------------------------------------------------------------------------
; customized timer
; in ReBench TestVMPerformance format
(define-syntax-rule (time expr1 expr ...)
  (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
    (printf "RESULT-cpu: ~a.0\nRESULT-gc: ~a.0\nRESULT-total: ~a.0\n"
            cpu gc user)
    (apply values v)))

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
