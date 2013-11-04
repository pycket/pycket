#lang racket

(require json)
(current-namespace (make-base-namespace))
(define form (read-syntax))

(define (to-json v)
  (match v
    [(? string? v) (hash 'string v)]
    [(? list? v) (map to-json v)]
    [(? symbol? v) (hash 'symbol (symbol->string v))]
    [(? vector? v) (hash 'vector (map to-json (vector->list v)))]
    [(? exact-integer? v) (hash 'integer (~a v))]
    [`(#%top . ,x) (hash 'var (symbol->string x))]
    [(? real? v) (hash 'real v)]))

(write-json (to-json (syntax->datum (expand form))))
(newline)
