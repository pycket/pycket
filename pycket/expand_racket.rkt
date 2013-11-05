#lang racket

(require json)
(define-namespace-anchor ns)
(define set-car! #f)
(define set-cdr! #f)
(current-namespace (namespace-anchor->namespace ns))
(define form (read-syntax))

(define (to-json v)
  (define (proper l)
    (match l
      [(cons a b) (cons a (proper b))]
      [_ null]))
  (match v
    [(? string? v) (hash 'string v)]
    [(? list? v) (map to-json v)]
    [(? cons?) (hash 'improper (list (map to-json (proper v))
                                     (to-json (cdr (last-pair v)))))]
    [(? symbol? v) (hash 'symbol (symbol->string v))]
    [(? vector? v) (hash 'vector (map to-json (vector->list v)))]
    [(? exact-integer? v) (hash 'integer (~a v))]
    [`(#%top . ,x) (hash 'var (symbol->string x))]
    [(? boolean?) v]
    [(? real? v) (hash 'real v)]))

(write-json (to-json (syntax->datum (expand form))))
(newline)
