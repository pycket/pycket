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

;; Extract the information from a require statement that tells us how to find
;; the desired file.
;; This ensures that all path names are normalized.
(define (require-json v)
  (define (to-path v)
    (path->string
      (simplify-path
        (resolve-module-path v #f))))
  (define (translate v)
    (case v
      [(#%kernel #%unsafe) (symbol->string v)]
      [else (to-path v)]))
  ;(eprintf ">>> ~a\n" v)
  (syntax-parse v #:literals (#%top quote)
    [v:str        (to-path (syntax-e #'v))]
    [s:identifier (translate (syntax-e #'s))]
    [(#%top . x)  (to-path (syntax-e #'x))]
    [((~datum rename) p _ ...) (require-json #'p)]
    [((~datum only) p _ ...) (require-json #'p)]
    [(_ ...)      (require-json (last (syntax->list v)))]
    ))

(define quoted? (make-parameter #f))

(define (to-json v)
  (define (proper l)
    (match l
      [(cons a b) (cons a (proper b))]
      [_ null]))
  (syntax-parse v #:literals (let-values letrec-values begin0 if #%plain-lambda #%top module* module #%plain-app quote #%require)
    [v:str (hash 'string (syntax-e #'v))]
    ;; special case when under quote to avoid the "interesting"
    ;; behavior of various forms
    [(_ ...) 
     #:when (quoted?)
     (map to-json (syntax->list v))]
    [(module _ ...) #f] ;; ignore these
    [(module* _ ...) #f] ;; ignore these
    ;; this is a simplification of the json output
    [(#%plain-app e0 e ...)
     (hash 'operator (to-json #'e0)
           'operands (map to-json (syntax->list #'(e ...))))]
    [(begin0 e0 e ...)
     (hash 'begin0 (to-json #'e0)
           'begin0-rest (map to-json (syntax->list #'(e ...))))]
    [(if e0 e1 e2)
     (hash 'test (to-json #'e0)
           'then (to-json #'e1)
           'else (to-json #'e2))]
    [(let-values ([xs es] ...) b ...)
     (hash 'let-bindings (for/list ([x (syntax->list #'(xs ...))]
                                    [e (syntax->list #'(es ...))])
                           (list (to-json x) (to-json e)))
           'let-body (map to-json (syntax->list #'(b ...))))]
    [(letrec-values ([xs es] ...) b ...)
     (hash 'letrec-bindings (for/list ([x (syntax->list #'(xs ...))]
                                       [e (syntax->list #'(es ...))])
                              (list (to-json x) (to-json e)))
           'letrec-body (map to-json (syntax->list #'(b ...))))]
    [(quote e) (hash 'quote 
                     (parameterize ([quoted? #t])
                       (to-json #'e)))]

    [(#%require . x) (hash 'require (require-json #'x))]
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
    [(~or (~datum +inf.0) (~datum -inf.0) (~datum nan.0))
     (hash 'real (number->string (syntax-e v)))]
    [_ #:when (real? (syntax-e v)) (hash 'real (syntax-e v))]
    ;;[_ #:when (regexp? (syntax-e v)) (hash 'quote (syntax-e v))]
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
  (define loop? #f)

  (command-line
   #:once-any
   [("--output") file "write output to output <file>"
    (set! out (open-output-file file #:exists 'replace))]
   [("--stdout") "write output to standard out"
    (set! out (current-output-port))]
   #:once-each
   [("--stdin") "read input from standard in" (set! in (current-input-port))]
   [("--no-stdlib") "don't include stdlib.sch" (set! stdlib? #f)]
   [("--loop") "keep process alive" (set! loop? #t)]
   
   #:args ([source #f])
   (cond [(and in source)
          (raise-user-error "can't supply --stdin with a source file")]
         [(and loop? source)
          (raise-user-error "can't loop on a file")]
         [source
          (when (not (output-port? out))
            (set! out (open-output-file (string-append source ".json")
                                        #:exists 'replace)))
          (set! in source)]))

  (define input (if (input-port? in) in (open-input-file in)))

  (unless (output-port? out)
    (raise-user-error "no output specified"))

  (unless (input-port? input)
    (raise-user-error "no input specified"))

  ;; If the given input is a file name, then chdir to its containing
  ;; directory so the expand function works properly
  (unless (input-port? in)
    (define in-dir (or (path-only in) "."))
    (current-directory in-dir))
  
  (read-accept-reader #t)
  (read-accept-lang #t)
  
  (let loop ()
    (define mod 
      ;; hack b/c I can't write EOF from Python
      (cond [loop?
             (let rd ([s null])
               (define d (read-bytes-line input))
               ;(write d (current-error-port)) (newline (current-error-port))
               (cond
                [(or (equal? d #"\0") (eof-object? d))
                 ;(eprintf "done\n")
                 (read-syntax (object-name input)
                              (open-input-bytes
                               (apply bytes-append
                                      (add-between (reverse s) #"\n"))))]
                [else
                 (rd (cons d s))]))]
            [else
             ;(eprintf "starting read-syntax\n")
             (read-syntax (object-name input) input)]))
    (when (eof-object? mod) (exit 0))
    (define expanded (do-expand mod))
    (write-json (convert expanded) out)
    (newline out) 
    (flush-output out)
    (when loop? (loop))))
