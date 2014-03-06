#lang racket

(require syntax/parse racket/runtime-path racket/unsafe/ops racket/fixnum racket/flonum "mycase.rkt")
(define-namespace-anchor ns)
;(define set-car! #f)
;(define set-cdr! #f)

(define-runtime-path stdlib.sch "stdlib.sch")

(define stdlib (file->list stdlib.sch))

(define (do-expand forms wrap? stdlib?)
  (current-namespace (namespace-anchor->namespace ns))
  (define new-form
    (if wrap?
        `(let ()
           ,@(if stdlib? stdlib null)
           (let () ,@forms))
        `(begin
           ,@forms)))
  (expand (datum->syntax #f new-form)))

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
    [_ #:when (real? (syntax-e v)) (hash 'real (syntax-e v))]
    [_
     #:when (char? (syntax-e v))
     (hash 'char (~a (char->integer (syntax-e v))))]))


(module+ main
  (require racket/cmdline json)

  (define in #f)
  (define out #f)

  (define wrap? #t)
  (define stdlib? #t)

  (command-line
   #:once-any
   [("--output") file "write output to output <file>"
    (set! out (open-output-file file #:exists 'replace))]
   [("--stdout") "write output to standard out"
    (set! out (current-output-port))]
   #:once-each
   [("--stdin") "read input from standard in" (set! in (current-input-port))]
   [("--no-wrap") "don't wrap input with a `let`" (set! wrap? #f)]
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


   (define forms (port->list read in))
   (write-json (to-json (do-expand forms wrap? stdlib?)) out)
   (write "\n" out))
