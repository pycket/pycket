#lang racket/base
(require racket/cmdline racket/pretty racket/match racket/list)
(define f (command-line #:args (file) file))

(define ns (make-base-namespace))


(define (simplify m)
  (match m
    [`(module ,n ,_ (#%module-begin ,forms ...))
     `(linklet
       () ()
       ,@(filter-map simplify-form forms))]))

(define (simplify-form f)
  (match f
    [`(module configure-runtime ,_ ...) #f]
    [`(module* main ,_ ...) #f]
    [`(module* test ,_ ...) #f]
    [`(module main ,_ ...) #f]
    [`(module test ,_ ...) #f]
    [`(module ,_  ...) (error 'simplify "not simple enough: ~s" f)] 
    [`(module*  ,_ ...) (error 'simplify "not simple enough: ~s" f)]

    [`(define-syntaxes ,_ ...) #f]
    [`(#%require ,_ ...) #f]
    [`(#%provide ,_ ...) #f]
    [`(define-values ,n ,e) `(define-values ,n ,(simplify-expr e))]
    [`(#%app call-with-values (lambda () ,e) print-values) (simplify-expr e)]
    [e (simplify-expr e)]))

(define (simplify-expr e)
  (match e
    [`(#%app ,e ...) (map simplify-expr e)]
    [`(let-values () ,e) (simplify-expr e)]
    [(list 'let-values (list [list nss rhss] ...) e)
     `(let-values ,(for/list ([ns nss]
                              [rhs rhss])
                     (list ns (simplify-expr rhs)))
        ,(simplify-expr e))]
    [`(quote ,s)
     #:when (or (boolean? s)
                (string? s)
                (number? s))
     s]
    [`(quote ,e)
     #:when (vector? e)
     `(vector ,@(for/list ([e e])
                  (simplify-expr `(quote ,e))))]
    [`(,e ...) `(,@(map simplify-expr e))]
    [e e]))



(define m (parameterize ([current-namespace ns]
                         [read-accept-reader #t]
                         [read-accept-lang #t])
            (simplify (syntax->datum (expand (read (open-input-file f)))))))



(pretty-print-abbreviate-read-macros #f)
(pretty-write m)
