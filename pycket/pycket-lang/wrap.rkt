#lang racket/base
(provide (rename-out (module-begin #%module-begin)) include time
         (except-out (all-from-out racket/base) #%module-begin))

(require racket/include
         (for-syntax racket/base racket/list))

(define-syntax-rule (time expr1 expr ...)
  (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
    (printf "RESULT-cpu: ~a.0\nRESULT-gc: ~a.0\nRESULT-total: ~a.0\n"
            cpu gc user)
    (apply values v)))

(define-syntax (module-begin stx)
  (define name (syntax-property stx 'enclosing-module-name))
  (define tokens (rest (syntax->datum stx)))
  (define r5rs? (memq 'r5rs tokens))
  (define specialize (car (or (memq 'fixflo tokens)
                              (memq 'unsafe tokens)
                              (memq 'nothing tokens)
                              '(nothing))))
  #`(#%module-begin
     #,@(if r5rs? #'((require r5rs)) #'())
     #,(datum->syntax stx '(include "../configuration/definitions-racket.rkt"))
     #,(datum->syntax stx '(include "../configuration/iterations.rkt"))
     #,(datum->syntax stx `(include ,(format "../configuration/specialize-racket-~a.rkt" specialize)))
     #,(datum->syntax stx `(include ,(format "../src/~a.scm" name)))
     #,(datum->syntax stx '(main))))
