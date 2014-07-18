#lang racket/base
(require racket/unsafe/ops (for-syntax racket/base racket/runtime-path)
         racket/include
         compatibility/mlist)
(require (prefix-in r5: r5rs) (prefix-in k: '#%kernel))
;; for now, white-listed for benchmarks.
(provide k:call-with-output-file)
(provide (rename-out [modbeg #%module-begin]))

(provide include time)

;------------------------------------------------------------------------------
; customized timer
; in ReBench TestVMPerformance format
(define-syntax-rule (time expr1 expr ...)
  (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
    (printf "RESULT-cpu: ~a.0\nRESULT-gc: ~a.0\nRESULT-total: ~a.0\n"
            cpu gc user)
    (apply values (list->mlist v))))

;------------------------------------------------------------------------------

(begin-for-syntax
 (define-runtime-path stdlib.sch "./stdlib.rktl"))

(define-syntax (modbeg stx)
  (syntax-case stx ()
    [(_ stdlib-kw forms ...)
     (eq? (syntax-e #'stdlib-kw) '#:stdlib)
     #`(#%plain-module-begin
        (require r5rs)
        #,(datum->syntax #'stdlib-kw `(include (file ,(path->string stdlib.sch))))
        forms ...)]
    [(_ forms ...)
     #`(#%plain-module-begin (require (only-in r5rs)) forms ...)]))


(#%require (just-meta 0 r5rs))
(provide (except-out (all-from-out r5rs) #%module-begin))
(provide let-values time-apply null printf when error (for-syntax ... syntax-rules)
         bitwise-not bitwise-and)

(module reader syntax/module-reader
  pycket/mcons)
