#lang racket

(require syntax/parse racket/runtime-path racket/unsafe/ops 
         racket/fixnum racket/flonum "mycase.rkt" racket/mpair
         compatibility/mlist (prefix-in r5: r5rs) (prefix-in r: racket) (prefix-in mz: mzscheme))
(define-namespace-anchor ns)
;(define set-car! #f)
;(define set-cdr! #f)

(define-runtime-path stdlib.sch "stdlib.sch")

(define-runtime-path mpair-stdlib.sch "mpair.sch")


(define stdlib (file->list stdlib.sch))
(define mpair-stdlib (file->list mpair-stdlib.sch))

(define (do-expand stx mpair? wrap? stdlib?)
  (syntax-parse stx #:literals ()
    [((~datum module) n:id lang:expr (#%module-begin body ...))
     (define m
       (if stdlib? 
           #`(module n lang (#%module-begin (include (file #,(path->string stdlib.sch))) body ...))
           #`(module n lang (#%module-begin body ...))))
     (expand m)]    
    [_ (error 'do-expand)]))

(define (index->path i)
  (resolved-module-path-name (module-path-index-resolve i)))

(define (to-json v)
  (define (proper l)
    (match l
      [(cons a b) (cons a (proper b))]
      [_ null]))
  (syntax-parse v #:literals (#%plain-lambda #%top module #%plain-app quote)
    [v:str (hash 'string (syntax-e #'v))]
    [(module _ ...) #f] ;; ignore these
    ;; this is a simplification of the json output
    ;; disabled to avoid changing the python code for now
    #;#;
    [(#%plain-app e0 e ...)
     (hash 'operator (to-json #'e0)
           'operands (map to-json (syntax->list #'(e ...))))]
    [(quote e) (hash 'quote (to-json #'e))]
    [(_ ...) (map to-json (syntax->list v))]
    [(#%top . x) (hash 'toplevel (symbol->string (syntax-e #'x)))]
    [(a . b) (hash 'improper (list (map to-json (proper (syntax-e v)))
                                   (to-json (cdr (last-pair (syntax-e v))))))]
    [#%plain-lambda
     (hash 'module "lambda")]
    [i:identifier
     #:when (eq? 'lexical (identifier-binding #'i))
     (hash 'lexical (symbol->string (syntax-e v)))]
    [i:identifier
     #:when (eq? #f (identifier-binding #'i))
     (hash 'toplevel (symbol->string (syntax-e v)))]
    [i:identifier
     (match (identifier-binding #'i)
       [(list (app index->path src) src-id _ _ 0 0 0)
        (hash 'module (symbol->string (syntax-e v))
              'source-module (if (path? src)
                                 (path->string src)
                                 (symbol->string src))
              'source-name (symbol->string src-id))]
       [v (error 'expand_racket "phase not zero: ~a" v)])]
    [#(_ ...) (hash 'vector (map to-json (vector->list (syntax-e v))))]
    [_
     #:when (exact-integer? (syntax-e v))
     (hash 'integer (~a (syntax-e v)))]
    [_ #:when (boolean? (syntax-e v)) (syntax-e v)]
    [_ #:when (keyword? (syntax-e v)) (hash 'keyword (keyword->string (syntax-e v)))]
    [_ #:when (real? (syntax-e v)) (hash 'real (syntax-e v))]
    [_
     #:when (char? (syntax-e v))
     (hash 'char (~a (char->integer (syntax-e v))))]))

(define (convert mod)
  (syntax-parse mod #:literals (module #%plain-module-begin)
    [(module name:id lang:expr (#%plain-module-begin forms ...))
     (hash 'module-name (symbol->string (syntax-e #'name))
           'body-forms (filter-map to-json (syntax->list #'(forms ...))))]
    [_ (error 'convert)]))


(module+ main
  (require racket/cmdline json)

  (define in #f)
  (define out #f)

  (define wrap? #t)
  (define stdlib? #t)
  (define mpair? #f)

  (command-line
   #:once-any
   [("--output") file "write output to output <file>"
    (set! out (open-output-file file #:exists 'replace))]
   [("--stdout") "write output to standard out"
    (set! out (current-output-port))]
   #:once-each
   [("--stdin") "read input from standard in" (set! in (current-input-port))]
   [("--no-stdlib") "don't include stdlib.sch" (set! stdlib? #f)]
   #:args ([source #f])
   (cond [(and in source)
          (raise-user-error "can't supply --stdin with a source file")]
         [source
          (when (not (output-port? out))
            (set! out (open-output-file (string-append source ".json")
                                        #:exists 'replace)))
          (set! in (open-input-file source))]))

   (unless (input-port? in)
     (raise-user-error "no input specified"))

   (unless (output-port? out)
     (raise-user-error "no output specified"))

  (read-accept-reader #t)
  (define mod (read-syntax (object-name in) in))
  (define expanded (do-expand mod mpair? wrap? stdlib?))
  ;(pretty-print (syntax->datum expanded) (current-error-port))
  (write-json (convert expanded) out)
  (newline out))
