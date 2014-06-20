#lang racket

(require syntax/parse syntax/modresolve)

(define (do-expand stx)
  ;; error checking
  (syntax-parse stx #:literals ()
    [((~and mod-datum (~datum module)) n:id lang:expr . rest)
     (void)]
    [((~and mod-datum (~datum module)) . rest)
     (error 'do-expand "got ill-formed module: ~a\n" (syntax->datum #'rest))]
    [rest
     (error 'do-expand "got something that isn't a module: ~a\n" (syntax->datum #'rest))])
  ;; work
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-syntax-introduce stx)
    (expand stx)))

(define (index->path i)
  (define-values (v _) (module-path-index-split i))
  (and v
       (resolved-module-path-name (module-path-index-resolve i))))

(define (to-json v)
  (define (proper l)
    (match l
      [(cons a b) (cons a (proper b))]
      [_ null]))
  (syntax-parse v #:literals (#%plain-lambda #%top module* module #%plain-app quote)
    [v:str (hash 'string (syntax-e #'v))]
    [(module _ ...) #f] ;; ignore these
    [(module* _ ...) #f] ;; ignore these
    ;; this is a simplification of the json output
    ;; disabled to avoid changing the python code for now
    #;
    [(#%plain-app e0 e ...)
     (hash 'operator (to-json #'e0)
           'operands (map to-json (syntax->list #'(e ...))))]
    ;; [(quote e) (hash 'quote (to-json #'e))]

    [(_ ...) (map to-json (syntax->list v))]
    [(#%top . x) (hash 'toplevel (symbol->string (syntax-e #'x)))]
    [(a . b) (hash 'improper (list (map to-json (proper (syntax-e v)))
                                   (to-json (cdr (last-pair (syntax-e v))))))]
    [i:identifier
     (match (identifier-binding #'i)
       ['lexical (hash 'lexical  (symbol->string (syntax-e v)))]
       [#f       (hash 'toplevel (symbol->string (syntax-e v)))]
       [(list (app index->path src) src-id _ _ 0 0 0)
        (hash 'module (symbol->string (syntax-e v))
              'source-module (if (path? src)
                                 (path->string src)
                                 (and src (symbol->string src)))
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
  (define expanded (do-expand mod))
  ;(pretty-print (syntax->datum expanded) (current-error-port))
  (write-json (convert expanded) out)
  (newline out))
