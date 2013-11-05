#lang racket

(require json syntax/parse)
(define-namespace-anchor ns)
(define set-car! #f)
(define set-cdr! #f)
(current-namespace (namespace-anchor->namespace ns))
(define form (read-syntax))

(define r (read))

(unless (eof-object? r)
  (error 'expand_racket "too many things on the input"))

(define (to-json v)
  (define (proper l)
    (match l
      [(cons a b) (cons a (proper b))]
      [_ null]))
  (syntax-parse v
    [v:str (hash 'string (syntax-e #'v))]
    [(_ ...) (map to-json (syntax->list v))]
    [(a . b) (hash 'improper (list (map to-json (proper (syntax-e v)))
                                   (to-json (cdr (last-pair (syntax-e v))))))]
    [i:identifier
     #:when (eq? 'lexical (identifier-binding #'i))
     (hash 'lexical (symbol->string (syntax-e v)))]
    [i:identifier
     #:when (eq? #f (identifier-binding #'i))
     (hash 'toplevel (symbol->string (syntax-e v)))]
    [i:identifier
     (hash 'module (symbol->string (syntax-e v)))]
    [#(_ ...) (hash 'vector (map to-json (vector->list (syntax-e v))))]
    [_
     #:when (exact-integer? (syntax-e v))
     (hash 'integer (~a (syntax-e v)))]
    [(#%top . x) (hash 'toplevel (symbol->string (syntax-e #'x)))]
    [_ #:when (boolean? (syntax-e v)) (syntax-e v)]
    [_ #:when (real? (syntax-e v)) (hash 'real (syntax-e v))]))

(write-json (to-json (expand form)))
(newline)
