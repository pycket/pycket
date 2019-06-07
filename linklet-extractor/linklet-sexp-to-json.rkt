#lang racket

(require "utils.rkt" json racket/extflonum)
(provide sexp->json)

(define (handle-number racket-num)
  (hash* 'number (handle-number-inner racket-num)))

(define (handle-number-inner racket-num)
  (cond
    [(extflonum? racket-num)
     (let ((inex (extfl->inexact racket-num)))
       (if (memv inex extended-reals)
           (hash* 'extended-real (convert-single-precs inex))
           (hash* 'real inex)))]
    [(exact? racket-num)
     (cond
       [(integer? racket-num)
        (hash* 'integer (number->string racket-num))]
       [(rational? racket-num)
        (let
            ([num (numerator racket-num)]
             [den (denominator racket-num)])
          (hash* 'numerator (hash* 'integer (number->string num))
                 'denominator (hash* 'integer (number->string den))))]
       [(complex? racket-num)
        (let ([real (real-part racket-num)]
              [imag (imag-part racket-num)])
          (hash* 'real-part (handle-number-inner real)
                 'imag-part (handle-number-inner imag)))]
       [else (error 'handle-num (format "handle this exact num: ~a" racket-num))])]
    [else
     (cond
       [(real? racket-num)
        (if (memv racket-num extended-reals)
            (hash* 'extended-real (convert-single-precs racket-num))
            (hash* 'real (+ 0.0 (* 1.0 (inexact->exact racket-num)))))]
       [(complex? racket-num)
        (let ([real (real-part racket-num)]
              [imag (imag-part racket-num)])
          (hash* 'real-part (handle-number-inner real)
                 'imag-part (handle-number-inner imag)))]
       [else
        (error 'handle-num (format "handle this inexact num: ~a" racket-num))])]))


(define (handle-regexp racket-regexp)
  (let ([r-str (object-name racket-regexp)])
    (hash* 'operator (hash* 'source-name "regexp")
           'operands (list (hash* 'quote (hash 'string r-str))))))

(define (handle-byte-regexp racket-byte-regexp toplevels env)
  (let ([r-bytes-list (bytes->list (object-name racket-byte-regexp))])
    (hash* 'operator (hash* 'source-name "byte-regexp")
           'operands (list (hash* 'operator (hash* 'source-name "list->bytes")
                                  'operands (list (hash* 'quote (sexp->json r-bytes-list toplevels env))))))))

(define (handle-hash racket-hash toplevels env)
  (let ([keys-asts (map (lambda (k) (sexp->json k toplevels env)) (hash-keys racket-hash))]
        [vals-asts (map (lambda (v) (sexp->json v toplevels env)) (hash-values racket-hash))])
    (cond
      [(hash-eq? racket-hash)
       (hash* 'hasheq-keys keys-asts
              'hasheq-vals vals-asts)]
      [(hash-eqv? racket-hash)
       (hash* 'hasheqv-keys keys-asts
              'hasheqv-vals vals-asts)]
      [else
       (hash* 'hash-keys keys-asts
              'hash-vals vals-asts)])))

(define (handle-prefab p-srt toplevels env)
  (let ([p-key (prefab-struct-key p-srt)]
        [struct-ls (vector->list (struct->vector p-srt))])
    (hash* 'prefab-key (sexp->json p-key toplevels env)
           'struct (map (lambda (st) (sexp->json st toplevels env))
                        (cdr struct-ls)))))

(define (handle-lambda args body toplevels env)
  (match args
    [`(,fmls ... . ,rst) #:when (symbol? rst)
     (hash* 'span 0 'module (hash '%mpi (hash '%p "some-path/racket/private/kw.rkt"))
            'lambda (hash 'improper
                          (list (map (lambda (s) (hash 'lexical (symbol->string s))) fmls)
                                (hash 'lexical (symbol->string rst))))
            'body (list (sexp->json body toplevels (append fmls (list rst) env))))]
    [_ #:when (symbol? args)
       (hash* 'span 0 'module (hash '%mpi (hash '%p "some-path/racket/private/kw.rkt"))
              'lambda (hash 'lexical (symbol->string args))
              'body (list (sexp->json body toplevels (cons args env))))]
    [else
     (hash* 'span 0 'module (hash '%mpi (hash '%p "some-path/racket/private/kw.rkt"))
            'lambda (map (lambda (s) (hash 'lexical (symbol->string s))) args)
            'body (list (sexp->json body toplevels (append args env))))]))

(define quoted? (make-parameter #f))
(define val? (make-parameter #f))

(define (maybe-quote s)
  (if (quoted?) s (hash 'quote s)))

(define (sexp->json s-exp toplevels env)
  (match s-exp
    ; values
    [_ #:when (symbol? s-exp)
       (cond
         [(quoted?) (hash 'toplevel (symbol->string s-exp))]
         [(memv s-exp env) (hash 'lexical (symbol->string s-exp))]
         [(memv s-exp toplevels) (hash 'source-linklet (symbol->string s-exp))] ;toplevel
         [else (hash 'source-name (symbol->string s-exp))])] ; kernel
    [_ #:when (boolean? s-exp) (maybe-quote s-exp)]
    [_ #:when (string? s-exp) (maybe-quote (hash 'string s-exp))]
    [_ #:when (or (number? s-exp) (extflonum? s-exp)) (maybe-quote (handle-number s-exp))]
    [_ #:when (char? s-exp) (maybe-quote (hash 'char (number->string (char->integer s-exp))))]
    [_ #:when (keyword? s-exp) (maybe-quote (hash 'keyword (keyword->string s-exp)))]
    [_ #:when (path? s-exp) (maybe-quote (hash 'path (path->string (normalize-path (simplify-path s-exp #t)))))]
    [_ #:when (regexp? s-exp) (maybe-quote (hash 'regexp (object-name s-exp)))]
    [_ #:when (pregexp? s-exp) (maybe-quote (hash 'pregexp (object-name s-exp)))]
    [_ #:when (byte-regexp? s-exp) (maybe-quote (hash 'byte-regexp (bytes->list (object-name s-exp))))]
    [_ #:when (byte-pregexp? s-exp) (maybe-quote (hash 'byte-pregexp (bytes->list (object-name s-exp))))]
    [_ #:when (bytes? s-exp) (maybe-quote (hash 'bytes (bytes->list s-exp)))]
    [_ #:when (void? s-exp) (maybe-quote (hash 'void #t))]

    [_ #:when (vector? s-exp) (maybe-quote (hash 'vector (for/list ([v (in-vector s-exp)]) (sexp->json v toplevels env))))]
    [_ #:when (box? s-exp) (maybe-quote (hash 'box (sexp->json (unbox s-exp) toplevels env)))]
    [_ #:when (hash? s-exp) (parameterize ([quoted? #t]) (handle-hash s-exp toplevels env))]
    [_ #:when (prefab-struct-key s-exp) (handle-prefab s-exp toplevels env)]

    [`(quote ,e) #:when (not (quoted?)) (hash 'quote (parameterize ([quoted? #t]) (sexp->json e toplevels env)))]
    [`(,fs ...) #:when (quoted?) (map (lambda (x) (sexp->json x toplevels env)) fs)]
    [`(,fs ... . ,r) #:when (and (quoted?) (not (list? r)))
                     (hash 'improper (list (map (lambda (x) (sexp->json x toplevels env)) fs)
                                           (sexp->json r toplevels env)))]
    ; forms
    [`(begin0 ,first-expr ,rest-exprs ...)
     (hash* 'begin0 (sexp->json first-expr toplevels env)
            'begin0-rest (map (λ (expr) (sexp->json expr toplevels env)) rest-exprs))]
    [`(begin ,exprs ...) (if (= 1 (length exprs))
                            (sexp->json (car exprs) toplevels env)
                            (cons (hash* 'source-name "begin")
                                  (map (lambda (f) (sexp->json f toplevels env)) exprs)))]
    [`(define-values (,ids ...) ,rhs) (hash* 'define-values (map symbol->string ids)
                                         'define-values-names (map symbol->string ids)
                                         'define-values-body (sexp->json rhs toplevels env))]
    [`(with-continuation-mark ,key ,val ,body) (hash* 'wcm-key (sexp->json key toplevels env)
                                                      'wcm-val (sexp->json val toplevels env)
                                                      'wcm-body (sexp->json body toplevels env))]
    [`(#%variable-reference) (hash 'variable-reference #f)]
    [`(case-lambda (,fmls ,body) ...) (hash* 'case-lambda (for/list ([ids (in-list fmls)]
                                                                     [rhs (in-list body)])
                                                            (handle-lambda ids rhs toplevels env))
                                             'original true
                                             'source (hash* '%p "awesome-module.rkt")
                                             'position 311
                                             'span 522
                                             'module (hash* '%mpi (hash* '%p "happy-module.rkt")))]
    [`(lambda ,fmls ,body) (handle-lambda fmls body toplevels env)]
    [`(let-values (,rhs-clauses ...) ,body-exprs ...)
     (hash* 'let-bindings
            (map (lambda (rhs)
                   (list (map symbol->string (car rhs))
                         (sexp->json (cadr rhs) toplevels env)))
                 rhs-clauses)
            'let-body
            (map (lambda (body-form)
                   (sexp->json body-form toplevels (append (foldr append null (map car rhs-clauses)) env))) body-exprs))]
    [`(letrec-values (,rhs-clauses ...) ,body-exprs ...)
     (let ([all-ids (foldr append null (map car rhs-clauses))])
       (hash* 'letrec-bindings
              (map (lambda (rhs)
                     (list (map symbol->string (car rhs))
                           (sexp->json (cadr rhs) toplevels (append all-ids env))))
                   rhs-clauses)
              'letrec-body
              (map (lambda (body-form)
                     (sexp->json body-form toplevels (append all-ids env))) body-exprs)))]
    [`(set! ,id ,rhs) (list (hash* 'source-name "set!")
                            (sexp->json id toplevels env)
                            (sexp->json rhs toplevels env))]
    [`(if ,tst ,thn ,els) (hash* 'test (sexp->json tst toplevels env)
                                 'then (sexp->json thn toplevels env)
                                 'else (sexp->json els toplevels env))]

    [`(,rator ,rands ...) #:when (not (quoted?))
                          (hash* 'operator (sexp->json rator toplevels env)
                                 'operands (map (lambda (x) (sexp->json x toplevels env)) rands))]))

(define (sexp->linklet-json linklet-sexp)
  ;; assumes linklet-sexp is well-formed
  (define importss (cadr linklet-sexp))
  (define exports (caddr linklet-sexp))
  (define body-s-exp (cdddr linklet-sexp))

  (define toplevels (for/fold ([ls '()])
                              ([form (in-list body-s-exp)])
                      (if (and (list? form) (equal? (car form) 'define-values))
                          (append (cadr form) ls)
                          ls)))

  (define importss-hash-val '())

  (define exports-hash-val (map (lambda (exprt)
                                  (if (symbol? exprt)
                                      (hash* 'quote (hash* 'toplevel (symbol->string exprt)))
                                      (list (hash* 'quote (hash* 'toplevel (symbol->string (car exprt))))
                                            (hash* 'quote (hash* 'toplevel (symbol->string (cadr exprt)))))))
                                exports))

  (define body (map (lambda (b-form) (sexp->json b-form toplevels '())) body-s-exp))

  (define linkl-hash
    (hash* 'linklet
           (hash* 'importss importss-hash-val
                  'exports exports-hash-val
                  'config (hash* 'version (version))
                  'body body)))

  linkl-hash)

(module+ main

  (require racket/cmdline json)

  (define verbose #f)
  (define out #f)
  (define sexp-file-path #f)

  (command-line
   #:once-each
   [("-v" "--verbose") "show what you're doing" (set! verbose #t)]
   #:once-any
   [("-o" "--output") file "write output to <file>"
                      (set! out (open-output-file file #:exists 'replace))]

   #:args (file.sexp)

   (define file-contents
     (read (open-input-file file.sexp)))

   (define sexp->linklet-hash
     (sexp->linklet-json file-contents))

   (unless out
     (set! out (current-output-port)))

   (write-json sexp->linklet-hash out)
   (newline out)
   (flush-output out)))
