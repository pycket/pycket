#lang racket/base
(provide (rename-out (module-begin #%module-begin)))

(require racket/include
         (for-syntax racket/base))

(define-syntax (module-begin stx)
  (define name (syntax-property stx 'enclosing-module-name))
  #`(#%module-begin
     (include #,(format "~a.scm" name))
     (main)))
