#! /usr/bin/env racket
#lang racket

(require json racket/cmdline)

(provide json->sexp
         json-file->sexp
         sexp->file
         json-file->composition
         simplify-json)

;; main func => simplify-json

#| 

> ./simplify-ast.rkt -a

> ./simplify-ast.rkt <file.json> +...

> ls *.ast

|#

(define (json->sexp ast*)
  
  (define val->sexp
    (λ (val)
      (let ([v* (cond
                  [(hash? val) (json->sexp val)]
                  [(list? val) (map val->sexp val)]
                  [(pair? val) `(,(val->sexp (car val))
                                 ,(val->sexp (cdr val)))]
                  [(string? val) (or (string->number val) (string->symbol val))]
                  [else val])])
        (cond
          [(and (list? v*) (= (length v*) 0)) '()]
          [(and (list? v*) (= (length v*) 1))
           (car v*)]
          [else v*]))))

  (define simple
    (lambda (v)
      (cond
        [(and (list? v) (= (length v) 1)) (car v)]
        [(and (list? v) (= (length v) 2) (null? (car v))) (cadr v)]
        [else v])))

  (cond
    [(equal? (hash-keys ast*) '(define-values-body define-values-names define-values))
     (let ((name (hash-ref ast* 'define-values-names))
           (body (hash-ref ast* 'define-values-body)))
       `(define ,(val->sexp name) ,(val->sexp body)))]
    [(equal? (hash-keys ast*) '(letrec-body letrec-bindings))
     (let ((bindings (hash-ref ast* 'letrec-bindings))
           (body (hash-ref ast* 'letrec-body)))
       `(letrec (,(val->sexp bindings)) ,(val->sexp body)))]
    [(equal? (hash-keys ast*) '(let-body let-bindings))
     (let ((bindings (hash-ref ast* 'let-bindings))
           (body (hash-ref ast* 'let-body)))
       (if (and (list? bindings) (= (length bindings) 0))
           (val->sexp body)
           `(let (,(val->sexp bindings)) ,(val->sexp body))))]
    [(or (equal? (hash-keys ast*) `(body module span source ,(string->symbol "lambda") position original))
         (equal? (hash-keys ast*) `(column body module span source ,(string->symbol "lambda") line position))
         (equal? (hash-keys ast*) `(body module span source ,(string->symbol "lambda") position)))
     (let ((body (hash-ref ast* 'body)) (args (hash-ref ast* (string->symbol "lambda"))))
       (if (empty? args)
           `(lambda () ,(val->sexp body))
           `(lambda (,(val->sexp args)) ,(val->sexp body))))]
    [(equal? (hash-keys ast*) '(operands operator))
     (let* ((rator (hash-ref ast* 'operator))
            (rands (hash-ref ast* 'operands)))
       (cond
         [(= (length rands) 0)
          `(,(simple (val->sexp rator)))]
         [(= (length rands) 1)
          `(,(simple (val->sexp rator)) ,(simple (val->sexp rands)))]
         [else
          `(,(simple (val->sexp rator)) ,@(simple (val->sexp rands)))]))]
    [(equal? (hash-keys ast*) '(else then test))
     (let ((test (hash-ref ast* 'test))
           (then (hash-ref ast* 'then))
           (els (hash-ref ast* 'else)))
       `(if ,(val->sexp test) ,(val->sexp then) ,(val->sexp els)))]
    [else (hash-map ast*
                    (λ (key val)
                      (match key
                        [(or 'position 'span 'module 'source 'source-module) `()]
                        [(or 'operator 'source-name 'operands 'integer 'number)
                         (when (symbol=? key 'operator) (displayln "warning"))
                         (simple (val->sexp val))]
                        ['lexical (val->sexp val)]
                        ['toplevel `(top ,(val->sexp val))]
                        [else
                         `(,key
                           ,(val->sexp val))])))]))

(define (trim-down-top-forms ast)
  (let* ([top-body-forms 
          (car ;; (())
           (filter (λ (a-node) (eq? (car a-node) 'body-forms)) ast))]
         [forms (cdr top-body-forms)]
         [forms (cdar forms)] ;; the first is configure-runtime
         
         ;; anything defined at the toplevel to be filtered goes here
         [filter-forms '()
                       #;'(boyer-iters browse-iters cpstak-iters ctak-iters dderiv-iters deriv-iters destruc-iters diviter-iters divrec-iters puzzle-iters tak-iters
                               takl-iters trav1-iters trav2-iters triangl-iters ack-iters array1-iters cat-iters string-iters sum1-iters sumloop-iters
                               tail-iters wc-iters fft-iters fib-iters fibfp-iters mbrot-iters nucleic-iters pnpoly-iters sum-iters sumfp-iters
                               conform-iters dynamic-iters earley-iters fibc-iters graphs-iters lattice-iters matrix-iters maze-iters mazefun-iters
                               nqueens-iters paraffins-iters peval-iters pi-iters primes-iters ray-iters scheme-iters simplex-iters slatex-iters perm9-iters
                               nboyer-iters sboyer-iters gcbench-iters parsing-iters gcold-iters quicksort-iters
                               ;; benchmark boilerplates
                               ;main run-bench run-benchmark
                               fatal-error call-with-output-file/truncate open-output-file/truncate)]
         ;; filter the filter-forms
         [forms (filter (λ (form)
                          (match form
                            [`((define-values-body ,body)
                               (define-values-names ,name1)
                               (define-values ,name2))
                             (begin
                               (when (not (equal? name1 name2))
                                 (displayln (format "WARNING: ~a not eq? ~a" name1 name2)))
                               (not (memv (if (string? name1) (string->symbol name1) name1) filter-forms)))]
                            [else true])) forms)])
    forms))


(define (get-json fname)
  (read-json (open-input-file fname)))

(define (json-file->sexp js-filename)
  (json->sexp (get-json js-filename)))

(define (sexp->file sexp outfilepath)
  (with-output-to-file outfilepath
    (λ () (pretty-write sexp))
    #:exists 'replace))

(define (simplify-json json-path-str new-path-str)
  (sexp->file (trim-down-top-forms (json-file->sexp json-path-str)) new-path-str))


(define compute-composition
  ;; counts ast nodes
  (let ([composition (make-hash)])
    (λ (ast*)
      (if (list? ast*)
          (map compute-composition ast*)
          (when (hash? ast*)
            (hash-map ast*
                      (λ (key val)
                        (begin
                          (if (hash-has-key? composition key)
                              (hash-update! composition key add1)
                              (hash-set! composition key 1))
                          (if (hash? val)
                              (compute-composition val)
                              (when (list? val)
                                (map compute-composition val))))))))
      composition)))

;; proved to be useful in some cases
(define (json-file->composition js-filename)
  (let* ([comp (compute-composition (get-json js-filename))]
         [comp-lst (hash->list comp)]
         [sorted (sort comp-lst
                       (λ (v1 v2) (string<? (symbol->string (car v1))
                                            (symbol->string (car v2)))))])
    (for ([node sorted])
      (displayln (format "~a ~a" (car node) (cdr node))))))



(module+ main

  (define all #f)
  
  (command-line
   #:once-each
   [("-a" "--all") "simplify all *.rkt.json" (set! all #t)]
   #:args files-given
   (when (and (not all) (null? files-given))
     (error 'simplify "Usage: ./simplify-ast.rkt -a OR ./simplify-ast.rkt <file.json> ..."))
   (let* ([files-to-simplify (if all
                                 (filter (lambda (p)
                                           (and (string-contains? (path->string p) ".json")
                                                (not (string-contains? (path->string p) ".json."))))
                                         (directory-list))
                                 (map string->path files-given))])
     (for ([f files-to-simplify])
       (let* ([f-str (path->string f)]
              [only-path (or (and (path-only f) (path->string (path-only f))) "")]
              [name (file-name-from-path f)])
         (simplify-json f-str (format "~a.ast" name)))))))

