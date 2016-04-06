#lang racket

(require compiler/zo-parse setup/dirs racket/undefined
         (only-in pycket/expand hash* global-config))

(provide to-ast-wrapper
         primitive-table)

(define DEBUG #f)
(define pycket-dir (path->string (current-directory))) ;; MUST BE RUN UNDER PYCKET DIR
(define collects-dir (path->string (find-collects-dir)))
(define module-name 'beSetBy-main)
(define relative-current-dir 'rel-dir-beSetBy-main)

(define TOPLEVELS '())
(define TOPSYNTAX '())

;; FROM
;; https://github.com/racket/compiler/blob/master/compiler-lib/compiler/decompile.rkt#L14
(define primitive-table
  ;; Figure out number-to-id mapping for kernel functions in `primitive'
  (let ([bindings
         (let ([ns (make-base-empty-namespace)])
           (parameterize ([current-namespace ns])
             (namespace-require ''#%kernel)
             (namespace-require ''#%unsafe)
             (namespace-require ''#%flfxnum)
             (namespace-require ''#%extfl)
             (namespace-require ''#%futures)
             (namespace-require ''#%foreign)
             (for/list ([l (namespace-mapped-symbols)])
               (cons l (with-handlers ([exn:fail? (lambda (x) #f)])
                         (compile l))))))]
        [table (make-hash)])
    (for ([b (in-list bindings)])
      (let ([v (and (cdr b)
                    (zo-parse 
                     (open-input-bytes
                      (with-output-to-bytes
                          (λ () (write (cdr b)))))))])
        (let ([n (match v
                   [(struct compilation-top (_ _ prefix (struct primval (n)))) n]
                   [else #f])])
          (hash-set! table n (car b)))))
    table))

(define primitives (hash-values primitive-table))

(define (value? form)
  (ormap (λ (f) (f form)) (list list? pair? hash? vector? number? string? symbol? char? keyword? regexp? byte-regexp? bytes?)))

(define (compile-json config language topmod body1 top-reqs-provs body-forms pycket?)
  (let ([whole-body (append top-reqs-provs body-forms)])
    (hash* 'language (list language)
           'module-name topmod
           'config config
           'body-forms (if pycket?
                           whole-body
                           (cons body1 whole-body)))))


(define (handle-def-values def-values-form localref-stack current-closure-refs)
  (let ([ids (def-values-ids def-values-form)]
        [rhs (def-values-rhs def-values-form)])
    (cond
      ((ormap (λ (def) (not (toplevel? def))) ids) ;; just a sanity check
       (error 'handle-def-values "def-values : detected a non toplevel?"))
      (else
       (let* ([poss (map toplevel-pos ids)]
              [syms (map (λ (p) (list-ref TOPLEVELS p)) poss)]
              [symstrs (map (λ (sym) (if (symbol? sym) (symbol->string sym) sym)) syms)])
         (hash* 'define-values symstrs
                'define-values-names symstrs
                'define-values-body (to-ast-single rhs (append symstrs localref-stack) current-closure-refs)))))))

(define (handle-if if-form localref-stack current-closure-refs)
  (let ([test (branch-test if-form)]
        [then (branch-then if-form)]
        [else (branch-else if-form)])
    (hash*
     'test (to-ast-single test localref-stack current-closure-refs)
     'then (to-ast-single then localref-stack current-closure-refs)
     'else (to-ast-single else localref-stack current-closure-refs))))

(define extended-reals (list +inf.0 +inf.f
                             -inf.0 -inf.f
                             +nan.0 +nan.f))

(define (handle-number racket-num)
  (hash* 'number
         (cond
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
                 (hash* 'real-part (hash* 'integer (number->string real))
                        'imag-part (hash* 'integer (number->string imag))))]
              [else (error 'handle-num (format "handle this exact num: ~a" racket-num))])]
           [else
            (cond
              [(real? racket-num)
               (if (memv racket-num extended-reals)
                   (hash* 'extended-real (number->string racket-num))
                   (hash* 'real racket-num))]
              [(complex? racket-num)
               (let ([real (real-part racket-num)]
                     [imag (imag-part racket-num)])
                 (hash* 'real-part (hash* 'real (number->string real))
                        'imag-part (hash* 'real (number->string imag))))]
              [else
               (error 'handle-num (format "handle this inexact num: ~a" racket-num))])])))

(define (handle-boolean racket-bool)
  racket-bool)

(define (handle-string racket-str)
  (hash* 'string racket-str))

(define (handle-symbol racket-sym)
  (let ([s (symbol->string racket-sym)])
    (if (memv racket-sym primitives)
        (hash* 'source-name s)
        (hash* 'toplevel s))))

(define (handle-char racket-char)
  (hash* 'char (number->string (char->integer racket-char))))

(define (handle-keyword racket-kw)
  (hash* 'keyword (keyword->string racket-kw)))

(define (handle-regexp racket-regexp)
  (hash* 'regexp (object-name racket-regexp)))

(define (handle-byte-regexp racket-byte-regexp)
  (hash* 'byte-regexp (bytes->list (object-name racket-byte-regexp))))

(define (handle-bytes racket-bytes)
  (hash* 'bytes (bytes->list racket-bytes)))

(define (handle-void racket-void)
  (hash* 'operator (hash* 'source-name "void")
         'operands (list)))

(define (handle-hash racket-hash)
  (let* ([keys (hash-keys racket-hash)]
         [vals (hash-values racket-hash)]
         [keys-asts (map to-ast-val keys)]
         [vals-asts (map to-ast-val vals)])
    (hash* 'hash-keys keys-asts
           'hash-vals vals-asts)))

(define (handle-vector racket-vector)
  (let* ([ls (vector->list racket-vector)])
    (hash* 'vector (map to-ast-val ls))))

(define (handle-list list-form)
  (map to-ast-val list-form))

(define (handle-pair pair-form)
  (let ([first-vals (takef pair-form (λ (_) #t))]
        [last-val (dropf pair-form (λ (_) #t))])
    (hash* 'improper (list (map to-ast-val first-vals)
                           (to-ast-val last-val)))))

(define (get-primval-name id)
  (symbol->string (hash-ref primitive-table id)))
  
(define (handle-primval operation)
  (let* ([id (primval-id operation)]
         [operator-name (get-primval-name id)])
    (hash* 'source-name operator-name)))


#|
if rator is a closure, create a letrec node
put closure-gen-id and closure-code in letrec-bindings

for the letrec-body
create an application node
put lexical closure-gen-id to operator
put the usual application-rands to the operands
|#
(define (handle-application app-form localref-stack current-closure-refs)
  (let* ([rator (application-rator app-form)]
         [rands (application-rands app-form)]
         [newlocalstack (append (map (λ (x) 'app-empty-slot) (range (length rands)))
                                localref-stack)]
         ;; the application pushes empty slots to run body over, so it will push the current local references further
         ;; we kinda simulate it here to make the localref pos indices point to the right identifier
         [rator-evaluated (to-ast-single rator newlocalstack current-closure-refs)]
         [operands-evaluated (map (λ (rand) (to-ast-single rand newlocalstack current-closure-refs)) rands)])
    (if (closure? rator)
        (let ([closure-ref (symbol->string (closure-gen-id rator))]
              [closure-body (closure-code rator)])
          (if (ormap (λ (cr) (string=? cr closure-ref)) current-closure-refs)
              (hash* 'operator (hash* 'lexical closure-ref)
                     'operands operands-evaluated)
              (hash* 'operator (hash* 'letrec-bindings (list (list (list closure-ref)
                                                                   (handle-closure rator newlocalstack current-closure-refs)))
                                      'letrec-body (list (hash* 'lexical closure-ref)))
                     'operands operands-evaluated)))
        (hash* 'operator rator-evaluated
               'operands operands-evaluated))))
                 

(define (handle-lambda lam-form localref-stack current-closure-refs)
  (let* ([name (lam-name lam-form)]
         ;; TODO : revisit
         [source (if (null? name) '()
                     (if (vector? name)
                         (let* ([real-name (path->string (vector-ref name 1))]
                                [splt (string-split real-name "/")])
                           (let-values ([(subs mod) (split-at splt (sub1 (length splt)))])
                             (hash* '%p (string-append "/" (string-join (append subs (list (string-append "fromBytecode_" (car mod)))) "/")))))
                         (if (not (symbol? name)) (error 'handle-lambda "we have a non symbol/vector name in a lam form")
                             (let* ([collects-dir (path->string (find-collects-dir))]
                                    [usual-prefix "/racket/private/"] ; TODO: figure out why they have a new way of naming lam's
                                    [lamname (if (and (not (string-contains? (symbol->string name) ".../more-scheme.rkt"))
                                                     (not (string-contains? (symbol->string name) "kw.rkt")))
                                                 (begin (displayln (format "writing lam name : ~a" name))
                                                        (symbol->string name))#;(error 'handle-lambda (format "lam name has an unusual form : ~a" name))
                                                 (if (string-contains? (symbol->string name) "kw.rkt")
                                                     "kw.rkt" "more-scheme.rkt"))])
                               (hash* '%p (string-append collects-dir usual-prefix lamname))))))]
         [position 321] ;; don't know what exactly are these two
         [span 123]
         ;; module seems to be the same for every lambda form,
         ;; pointing to a private module about chaperones/impersonators
         [module (hash* '%mpi (hash* '%p (string-append collects-dir "/racket/private/kw.rkt")))]                                     

         [num-args (lam-num-params lam-form)]

         [arg-types (begin (when (and (not (= num-args (length (lam-param-types lam-form))))
                                      DEBUG)
                             (error 'handle-lambda "investigate: num-args and (length param-types) are not equal"))
                           (lam-param-types lam-form))] ;; val - ref - flonum - fixnum - extflonum
         ;; formal symbols
         ;; TODO : refactor/cleanup
         [symbols-for-formals (map (λ (x)
                                     (begin
                                       (when (and (or (eq? x 'flonum) (eq? x 'fixnum) (eq? x 'extflonum))
                                                  DEBUG)
                                         (displayln (format "warning : argument to lam-name : ~a is : ~a" name x)))
                                       (let ([sym 
                                              (symbol->string
                                               (gensym
                                                (string->symbol (string-append "lam." (symbol->string x) "."))))])
                                         (if (eq? x 'ref) sym sym))))
                                   arg-types)]
         
         [rest? (lam-rest? lam-form)]
         ;; rest arg symbol
         [rest-formal (if rest? (symbol->string (gensym 'lamrest)) 'hokarz)]

         ;; vector of stack positions that are captured when evaluating the lambda form to create a closure.
         [captured-stack-positions (vector->list (lam-closure-map lam-form))] ;; vector
         [current-stack-length (length localref-stack)]
         [captured-current-stack-items (map (λ (pos) (if (>= pos current-stack-length)
                                                         (begin (displayln current-stack-length) (list-ref TOPLEVELS pos))
                                                         (list-ref localref-stack pos))) captured-stack-positions)]

         [new-localref-stack-1 (if rest?
                                   (append symbols-for-formals (list rest-formal) localref-stack)
                                   (append symbols-for-formals localref-stack))]
         
         [toplevelmap (lam-toplevel-map lam-form)] ;; either #f or a set
         [toplevel-map-size (if (not toplevelmap) 0 (set-count toplevelmap))]

         [new-localref-stack (append captured-current-stack-items new-localref-stack-1)]

         [lamBda
          (let ([args (map (λ (sym) (hash* 'lexical (if (box? sym) (unbox sym) sym))) symbols-for-formals)])
            (if rest?
                (hash* 'improper (list args ;; list of regular args list and the rest argument
                                       (hash* 'lexical rest-formal)))
                args))]

         [body (to-ast-single (lam-body lam-form)
                              new-localref-stack
                              current-closure-refs)])
    ;; pycket seems to omit source and position (and sets the span to 0) if lam-name is ()
    (if (null? source)
        (hash* 'span 0 'module module 'lambda lamBda 'body (list body))
        (hash* 'original true 'source source 'position position 'span span 'module module 'lambda lamBda 'body (list body)))))

(define (handle-inline-variant iv-form localref-stack current-closure-refs)
  (let ([direct (inline-variant-direct iv-form)]
        [inline (inline-variant-inline iv-form)])
    ;; using inlined version if possible (clearly possible if inline-variant form exists??)
    (to-ast-single direct localref-stack current-closure-refs)))

(define (handle-closure closure-form localref-stack current-closure-refs)
  (let ([code (closure-code closure-form)]
        [gen-id (symbol->string (closure-gen-id closure-form))])
    (begin
      (when DEBUG
        (displayln (format "handle-closure gen-id : ~a" gen-id))
        (display "Current-closure-refs ===>  ")
        (displayln current-closure-refs)
        (display "inside??? ====>   ")
        (displayln (if (ormap (λ (cr) (string=? gen-id cr)) current-closure-refs) true false)))
      
      (if (ormap (λ (cr) (string=? gen-id cr)) current-closure-refs)
          (hash* 'lexical gen-id)
          (if (lam? code)
              (handle-lambda code localref-stack (cons gen-id current-closure-refs))
              (error 'handle-closure "no lam inside the closure?"))))))

(define (handle-apply-values app-form localref-stack current-closure-refs)
  (let* ([proc-part (apply-values-proc app-form)]
         [args-part (apply-values-args-expr app-form)]

         [proc (to-ast-single proc-part localref-stack current-closure-refs)]

         [m (if (toplevel? proc-part)
                (let ([t (list-ref TOPLEVELS (toplevel-pos proc-part))])
                  (if (module-variable? t) t false)) false)]

         
         [mod-sym (if m (symbol->string (module-variable-sym m)) false)]
         [mod-path (if m (path->string
                          (resolved-module-path-name
                           (module-path-index-resolve
                            (module-variable-modidx m))))
                       (string-append collects-dir "/racket/private/modbeg.rkt"))]

         [new-localref-stack (if mod-sym (cons mod-sym localref-stack) localref-stack)]

         [args (to-ast-single args-part new-localref-stack current-closure-refs)]

         ;; construct the lam (lambda () args)
         [lambda-form (hash* 'source (hash* '%p (string-append relative-current-dir "fromBytecode_" module-name ".rkt")) ;; toplevel application
                             'position 321
                             'span 123
                             'module (hash* '%mpi (hash* '%p mod-path))
                             'lambda '()
                             'body (list args))])

    ;; (call-with-values lam proc)
    (hash* 'operator (hash* 'source-name "call-with-values")
           'operands (list lambda-form proc))))

;; TODO : refactor/cleanup (don't need boxes anymore)
(define (handle-localref lref-form localref-stack)
  (let* ([clear? (localref-clear? lref-form)]
         [unbox? (localref-unbox? lref-form)]
         [pos (localref-pos lref-form)]
         [stack-slot
          (let
              ([slot (with-handlers ([exn:fail? (lambda (e) (displayln (format "getting pos ~a, from ~a" pos localref-stack)) (raise e))])
                       (list-ref localref-stack pos))])
            (begin
              (when (and (not unbox?) (box? slot) DEBUG)
                (displayln
                 (format "localref warning : unbox? is false, but pos --~a-- is a box : ~a\n\n" pos slot)))
              #;(if unbox? (unbox slot) slot)
              (if (box? slot) (unbox slot) slot)))])
    (cond
      [(hash? stack-slot) (error 'handle-localref "interesting... we seem to have a hash in the stack slot : ~a" stack-slot)]
      [(box? stack-slot) (error 'handle-localref "we have unboxing issues with pos : ~a - slot : ~a" pos stack-slot)]
      [else (hash* 'lexical 
                   (let ([slot-payload (if (symbol? stack-slot) (symbol->string stack-slot) stack-slot)])
                     (if (or (box? slot-payload)
                             (and (string? stack-slot)
                                  (string-contains? stack-slot "dummy")))
                         (error 'handle-localref
                                "pos: ~a shouldn't have extracted this: ~a \n here's the stack: \n~a\n"
                                pos stack-slot localref-stack)
                         slot-payload)))])))

(define (self-mod? mpi)
    (let-values ([(mod-path base-path) (module-path-index-split mpi)])
      (and (not mod-path) (not base-path))))

(define (module-path-index->path-string mod-idx)    
  (if (self-mod? mod-idx)
      (string-append relative-current-dir module-name ".rkt")
      (let-values ([(module-path base-path) (module-path-index-split mod-idx)])
        (if (or (list? module-path) (symbol? module-path)) ;; then it is resolved
            (let ([path (resolved-module-path-name (module-path-index-resolve mod-idx))])
              (if (symbol? path)
                  (symbol->string path)
                  (path->string path)))
            (if (not (string? module-path))
                (error 'module-path-index->path-string "module-path is not a list, symbol or string : ~a, in ~a" module-path mod-idx)
                ;; resolving manually using the base-path
                (if (self-mod? base-path)
                    (string-append relative-current-dir module-path)
                    (let ([base-path-str (if (resolved-module-path? base-path)
                                             (path->string (resolved-module-path-name base-path))
                                             (module-path-index->path-string base-path))])
                      (begin ;; sanity-check : should end with .rkt
                        (when (not (string-suffix? base-path-str ".rkt"))
                          (error 'module-path-index->path-string "something's wrong with the resolved base path : ~a" base-path-str))
                        (let* ([spl (string-split base-path-str "/")]
                               [real-base (string-append "/" (string-join (take spl (sub1 (length spl))) "/") "/")])
                          (string-append real-base module-path))))))))))

(define (handle-module-variable mod-var localref-stack)
  (let* ([name (symbol->string (module-variable-sym mod-var))]
         [mod-idx (module-variable-modidx mod-var)]
         [module-path
          (module-path-index->path-string mod-idx)])
    (hash* 'source-name name
           'source-module (list module-path))))

(define (handle-varref varref-form localref-stack)
  (let ([top (varref-toplevel varref-form)]
        [dummy (varref-dummy varref-form)]
        [current-mod-path (string-append relative-current-dir "fromBytecode_" module-name ".rkt")])
    (if (boolean? top) ;; varref-toplevel is a boolean
        (error 'handle-varref (format "we got a bool at varref : ~a" varref-form))
        (let* ([topvar (list-ref TOPLEVELS (toplevel-pos top))]
               [name (cond
                       [(boolean? topvar)
                        (if topvar
                            (error 'handle-varref (format "we got a TRUE bool from TOPLEVELS : ~a - varref : ~a" topvar varref-form))
                            false)]
                       [(symbol? topvar) (symbol->string topvar)]
                       [(module-variable? topvar) (symbol->string (module-variable-sym topvar))])]
               [path-str (if (or (symbol? topvar) (boolean? topvar))
                             current-mod-path
                             ;; it's a module-variable
                             (module-path-index->path-string (module-variable-modidx topvar)))]
               ;; TODO : refactor
               [is-lifted? false]
               [name-ref-hash (if (boolean? topvar) 'dummy
                                  (if (memv (string->symbol name) primitives) ;; it's a primitive
                                      (hash* 'source-name name)
                                      (if (string-contains? name ".")
                                          (let* ([mod-split (string-split name ".")]
                                                 [original-mod (car mod-split)])
                                            (begin
                                              (set! is-lifted? true)
                                              ;; sanity check : second part of the "." should be all numbers
                                              (with-handlers ([exn:fail?
                                                               (λ (e) (error 'handle-varref (format "unusual topvar name : ~a" name)))])
                                                (string->number (cadr mod-split)))
                                              (hash* 'source-name name
                                                     'source-module path-str
                                                     'module original-mod)))
                                          (hash* 'source-name name
                                                 'source-module path-str))))]
               [source-mod (cond
                             [(or (symbol? topvar) (boolean? topvar)) path-str]
                             [(memv (string->symbol name) primitives) current-mod-path]
                             [(module-variable? topvar)
                              (if is-lifted? ;; assumption : all lifted var-refs are from kw.rkt
                                  (string-append collects-dir "/racket/private/kw.rkt")
                                  current-mod-path)])]
               [final-hash 
                (hash* 'source (hash* '%p source-mod)
                       'module (hash* '%mpi (hash* '%p source-mod))
                       'variable-reference name-ref-hash
                       'position 12
                       'span 11
                       'original true)])
          (if (boolean? topvar) ;; (hash* 'var false) produces '#hash() 
              (hash-set final-hash 'variable-reference name) ;; adding it with hash-set works
              final-hash)))))

(define (handle-let-one letform localref-stack current-closure-refs)
  (begin
    (when DEBUG
      (displayln (format "LET-ONE - UNUSED? ==> ~a" (let-one-unused? letform))))
    (let* ([unused? (let-one-unused? letform)]
           [bindingname (if unused? "letone-not-used-slot" (symbol->string (gensym 'letone)))]
           [newstack (cons bindingname localref-stack)]) ;; push uninitialized slot
      (hash* 'let-bindings (list (list (if unused? '() (list bindingname))
                                       (to-ast-single (let-one-rhs letform) newstack current-closure-refs)))
             'let-body (list (to-ast-single (let-one-body letform)
                                            newstack
                                            ;(if unused? localref-stack newstack) ;; if unused?, then rhs is not pushed to the stack
                                            current-closure-refs))))))

(define (body-name body-form)
  (cond
    ((list? body-form) "List ")
    ((hash? body-form) "Hash ")
    ((boolean? body-form) "Boolean ")
    ((number? body-form) "Number ")
    ((string? body-form) "String ")
    ((symbol? body-form) "Symbol ")
    ((char? body-form) "Char ")
    ((keyword? body-form) "Keyword ")
    ((regexp? body-form) "Regexp ")
    ((byte-regexp? body-form) "Byte Regexp ")
    ((bytes? body-form) "Byte String ")
    ((void? body-form) "Void ")
    ((with-cont-mark? body-form) "with-cont-mark ")
    ((with-immed-mark? body-form) "with-immed-mark ")
    ((boxenv? body-form) "boxenv ")
    ((let-one? body-form) "let-one ")
    ((let-void? body-form) "let-void ")
    ((let-rec? body-form) "let-rec ")
    ((case-lam? body-form) "case-lam ")
    ((install-value? body-form) "install-value ")
    ((module-variable? body-form) "module-variable ")
    ((varref? body-form) "varref ")
    ((primval? body-form) "primval ")
    ((application? body-form) "application ")
    ((def-values? body-form) "def-values ")
    ((seq? body-form) "seq ")
    ((splice? body-form) "splice ")
    ((beg0? body-form) "beg0 ")
    ((assign? body-form) "SET! ")
    ((branch? body-form) "branch ")
    ((apply-values? body-form) "apply-values ")
    ((localref? body-form) "localref ")
    ((lam? body-form) (format "lam : ~a \n" (lam-name body-form)))
    ((inline-variant? body-form) "inline-variant ")
    ((closure? body-form) "closure ")
    ((toplevel? body-form) "toplevel ")
    ((topsyntax? body-form) "topsyntax ")
    ((hash? body-form) "Already hashed Val (pushed by let-one)")
    (else "Unknown: ")))

(define (handle-toplevel form localref-stack)
  (let* ([toplevel-id (list-ref TOPLEVELS (toplevel-pos form))]
         [toplevel-id-str (if (symbol? toplevel-id) (symbol->string toplevel-id) toplevel-id)]
         [module-dir (string-append relative-current-dir module-name ".rkt")])
    (cond
      [(symbol? toplevel-id)
       (hash* 'source-name toplevel-id-str
              'source-module module-dir)]
      [(module-variable? toplevel-id)
       (handle-module-variable toplevel-id localref-stack)]
      [else (error 'handle-toplevel "not sure how to handle this kind of toplevel form")])))

(define (handle-topsyntax topsyn-form localref-stack current-closure-refs)
  (let* ([pos (topsyntax-pos topsyn-form)]
         [selected-stx (list-ref TOPSYNTAX pos)] ;; stx?
         [content (stx-content selected-stx)] ;; stx-obj?
         [datum (stx-obj-datum content)]
         [src-loc (stx-obj-srcloc content)]
         [position (if (not src-loc) 12345 (srcloc-position src-loc))]
         [span (if (not src-loc) 11 (srcloc-span src-loc))]
         [source-path (if (not src-loc) (string-append relative-current-dir module-name ".rkt") (path->string (srcloc-source src-loc)))])
    (hash* 'quote-syntax (to-ast-val datum)
           'source (hash* '%p source-path)
           'module (hash* '%mpi (hash* '%p source-path))
           'position position
           'span span)))
  
(define (handle-seq seq-expr localref-stack current-closure-refs)
  (let* ([seqs (seq-forms seq-expr)]
         [last-seq (list (last seqs))]
         [seqs-non-simple (filter (λ (form) (not (localref? form))) (take seqs (sub1 (length seqs))))]
         [exprs (map (λ (expr) (to-ast-single expr localref-stack current-closure-refs)) (append seqs-non-simple last-seq))])
    (hash* 'let-bindings (list)
           'let-body exprs)))

(define (handle-splice splice-expr localref-stack current-closure-refs)
  (let* ([splices (splice-forms splice-expr)]
         [exprs (map (λ (expr) (to-ast-single expr localref-stack current-closure-refs)) splices)])
    (hash* 'let-bindings (list)
           'let-body exprs)))

(define (handle-begin0 body-form localref-stack current-closure-refs)
  (let* ([seqs (beg0-seq body-form)]
         [first-expr (car seqs)] ;; assumes seqs is not empty
         [rest-exprs (cdr seqs)])
    (hash* 'begin0 (to-ast-single first-expr localref-stack current-closure-refs)
           'begin0-rest (map (λ (expr) (to-ast-single expr localref-stack current-closure-refs)) rest-exprs))))
         

(define (handle-wcm body-form localref-stack current-closure-refs)
  (let ([wcm-key (with-cont-mark-key body-form)]
        [wcm-val (with-cont-mark-val body-form)]
        [wcm-body (with-cont-mark-body body-form)])
    (hash* 'wcm-key (to-ast-single wcm-key localref-stack current-closure-refs)
           'wcm-val (to-ast-single wcm-val localref-stack current-closure-refs)
           'wcm-body (to-ast-single wcm-body localref-stack current-closure-refs))))

(define (handle-immed-mark body-form localref-stack current-closure-refs)
  (let* ([key (with-immed-mark-key body-form)]
         [body (with-immed-mark-body body-form)]
         [mark-formal (symbol->string (gensym))]
         [lam-form
          (hash* 'source (hash* '%p (string-append relative-current-dir "fromBytecode_" module-name ".rkt"))
                 'position 321
                 'span 123
                 'module (hash* '%mpi (hash* '%p (string-append collects-dir "/racket/private/kw.rkt")))
                 'lambda (list (hash* 'lexical mark-formal))
                 'body (list (to-ast-single body (cons mark-formal localref-stack) current-closure-refs)))])
    
    (hash* 'operator (hash* 'source-name "call-with-immediate-continuation-mark")
           'operands (list (to-ast-single key localref-stack current-closure-refs)
                           lam-form))))

;; boxenv case: lambda arg is mutated inside the body
(define (handle-boxenv body-form localref-stack current-closure-refs)
  (let* ([pos (boxenv-pos body-form)]
         [pre-pos (take localref-stack pos)]
         [post-pos (drop localref-stack pos)]
         [boxed-slot (string-append (car post-pos) "-box")])
    (begin
      #;(when DEBUG
        (displayln (format "boxenv pos : ~a | old-slot : ~a | new-slot : ~a" pos (car post-pos) boxed-slot)))
      #;(to-ast-single (boxenv-body body-form) (append pre-pos (list boxed-slot) (cdr post-pos)) current-closure-refs)
      (to-ast-single (boxenv-body body-form) localref-stack current-closure-refs))))

(define (handle-assign body-form localref-stack current-closure-refs)
  (let ([id (assign-id body-form)]
        [rhs (assign-rhs body-form)]
        [module-dir (string-append relative-current-dir module-name ".rkt")])
    (list (hash* 'source-name "set!")
          (to-ast-single id localref-stack current-closure-refs)
          (to-ast-single rhs localref-stack current-closure-refs))))

(define (handle-let-void body-form localref-stack current-closure-refs)
  ;; Pushes count uninitialized slots onto the stack and then runs body.
  ;; If boxes? is #t, then the slots are filled with boxes that contain #<undefined>.
  (let* ([count (let-void-count body-form)]
         [boxes? (let-void-boxes? body-form)]
         [body (let-void-body body-form)]
         [count-lst (range count)]
         [payload (if boxes? "let-void-box-" "let-void-")]
         [new-slots (map (λ (s) (symbol->string (gensym (string-append payload (number->string s) ".")))) count-lst)]
         [rhss (map (λ (r) (hash* 'quote (hash* 'toplevel "uninitialized"))) count-lst)]
         [newstack (begin
                     (when DEBUG
                       (displayln (format "LetVoid pushes ~a slots.. boxes? : ~a" count boxes?)))
                     (append new-slots localref-stack))]
         [rhs-ready (map (λ (slot rhs) (list (list slot) rhs)) new-slots rhss)])
    (let* ([body-ast* (to-ast-single body newstack current-closure-refs)]
           [body-ast (if (list? body-ast*) body-ast* (list body-ast*))])
      (hash* 'let-bindings rhs-ready
             'let-body body-ast))))

(define (handle-install-value body-form localref-stack current-closure-refs)
  ;; Runs rhs to obtain count results, and installs them into existing
  ;; slots on the stack in order, skipping the first pos stack positions.
  (let* ([count (install-value-count body-form)]
         [pos (install-value-pos body-form)]
         [boxes? (install-value-boxes? body-form)]
         [rhs (install-value-rhs body-form)]
         [body (install-value-body body-form)]
         [count-lst (range count)]

         [binding-list (map (λ (c) (symbol->string (gensym (string-append "inst-val" (number->string c) ".")))) count-lst)]
         
         [slot-positions (map (λ (p) (+ p pos)) count-lst)]

         [mod-region (let ([reg (map (λ (p) (list-ref localref-stack p)) slot-positions)])
                       (begin
                         (when DEBUG (displayln (format "install val boxes? : ~a --\nModified region : ~a\n" boxes? reg)))
                         reg))]

         [set-nodes (map (λ (let-void-slot inst-val-binding)
                           (list (hash* 'source-name "set!")
                                 (hash* 'lexical let-void-slot)
                                 (hash* 'lexical inst-val-binding))) mod-region binding-list)]
                  
         [rhs-ready (list (list binding-list
                                (to-ast-single rhs localref-stack current-closure-refs)))])
    ;; producing json for pycket
    (let* ([body-ast* (to-ast-single body localref-stack current-closure-refs)]
           [body-ast (if (list? body-ast*) body-ast* (list body-ast*))])
      ;; first binds new vars by evaluating the (single) rhs
      (hash* 'let-bindings rhs-ready
             ;; then sets the let-void bindings with new vars ...
             'let-body (list (hash* 'let-bindings (list)
                                    ;; ... and continue with the body
                                    'let-body (append set-nodes body-ast)))))))

(define (handle-let-rec letrec-form localref-stack current-closure-refs)
  (let* ([procs (let-rec-procs letrec-form)] ;; (listof lam?)
         [proc-names (map (lambda (proc)
                            (let ([name (lam-name proc)])
                              (if (symbol? name)
                                  (symbol->string name)
                                  (symbol->string (vector-ref name 0)))))
                          procs)]
         [slot-count (length procs)]
         [slot-positions (range slot-count)]
         [reversed-proc-names (reverse proc-names)]
         [new-localref-stack
          (begin
            ;; sanity check : validity of the pre-installed slots
            (if (andmap (λ (p) (string-contains? (list-ref localref-stack p) "let-void")) slot-positions)
                'ok
                (error 'handle-let-rec (format "posiitons : ~a ---- to be modified slots don't look good : ~a" slot-positions localref-stack)))
            (append reversed-proc-names (drop localref-stack slot-count)))]

         [proc-bodies (map (λ (proc) (to-ast-single proc new-localref-stack current-closure-refs)) procs)]
         [body (let-rec-body letrec-form)])
    
    (hash* 'letrec-bindings (map (λ (p-name p-body)
                                   (list (list p-name) p-body))
                                 proc-names proc-bodies)
           'letrec-body (list (to-ast-single body new-localref-stack current-closure-refs)))))


(define (handle-case-lambda body-form localref-stack current-closure-refs)
  (let ([clauses (case-lam-clauses body-form)])
    (hash* 'case-lambda
           (map (λ (clause-raw)
                  (if (and (not (lam? clause-raw)) (not (closure? clause-raw)))
                      (begin (displayln clause-raw (current-output-port)) (error 'handle-case-lambda "not a lam clause?"))
                      (let* ([clause (if (lam? clause-raw) clause-raw (closure-code clause-raw))] ;; assumes there's a lam in the closure

                             ;; TODO : make use of 'handle-lambda for these
                             
                             [name (lam-name clause)]
                             [num-args (lam-num-params clause)]

                             [arg-types
                              (begin
                                (when (and (not (= num-args (length (lam-param-types clause))))
                                           DEBUG)
                                  (error 'handle-lambda "investigate: num-args and (length param-types) are not equal"))
                                (lam-param-types clause))] ;; val - ref - flonum - fixnum - extflonum
                             
                             [multiple-args? (lam-rest? clause)] ;; is the rest? true

                             ;; TODO : refactor/cleanup
                             [symbols-for-formals (map (λ (x)
                                     (begin
                                       (when (and (or (eq? x 'flonum) (eq? x 'fixnum) (eq? x 'extflonum))
                                                  DEBUG)
                                         (displayln (format "warning : argument to CASE-LAM-name : ~a is : ~a" name x)))
                                       (let ([sym 
                                              (symbol->string
                                               (gensym
                                                (string->symbol (string-append "case-lam." (symbol->string x) "."))))])
                                         (if (eq? x 'ref) sym sym))))
                                   arg-types)]

                             ;[symbols-for-formals (map (λ (x) (symbol->string (gensym 'caselam-cl-arg))) (range num-args))]
                             
                             [rest-formal (if multiple-args? (symbol->string (gensym 'caselam-cl-rest)) 'hokarz)]

                             [captured-stack-positions (vector->list (lam-closure-map clause))]
                             [current-stack-length (length localref-stack)]
                             [captured-current-stack-items (map (λ (pos) (if (>= pos current-stack-length)
                                                                             (with-handlers ([exn:fail? (lambda (e)

                                                                                                          (displayln (format "pos : ~a - toplevels : ~a" pos TOPLEVELS))
                                                                                                          (raise e))])
                                                                               (list-ref TOPLEVELS pos))
                                                                             (list-ref localref-stack pos))) captured-stack-positions)]

                             [new-localref-stack-1 (if multiple-args?
                                                       (append symbols-for-formals (list rest-formal) localref-stack)
                                                       (append symbols-for-formals localref-stack))]

                             ;; toplevel stuff??
                             [new-localref-stack (append captured-current-stack-items new-localref-stack-1)]
                             
                             [arg-mapping
                              (let ([args (map (λ (sym) (hash* 'lexical (if (box? sym) (unbox sym) sym))) symbols-for-formals)])
                                (if multiple-args?
                                    (hash* 'improper (list args
                                                           (hash* 'lexical rest-formal)))
                                    args))]
                             
                             [body (to-ast-single (lam-body clause)
                                                  new-localref-stack
                                                  current-closure-refs)])
                        (hash* 'lambda arg-mapping
                               'body (list body)))))
                clauses)
           'original true
           'source (hash* '%p (string-append relative-current-dir "fromBytecode_" module-name ".rkt"))
           'position 987
           'span 456
           'module (hash* '%mpi (hash* '%p (string-append relative-current-dir "fromBytecode_" module-name ".rkt"))))))




(define (to-ast-val val-form)
  (cond
    ((list? val-form) 
     (handle-list val-form))
    ((pair? val-form)
     (handle-pair val-form))
    ((hash? val-form)
     (handle-hash val-form))
    ((vector? val-form)
     (handle-vector val-form))
    ((number? val-form)
     (handle-number val-form))
    ((string? val-form)
     (handle-string val-form))
    ((symbol? val-form)
     (handle-symbol val-form))
    ((char? val-form)
     (handle-char val-form))
    ((keyword? val-form)
     (handle-keyword val-form))
    ((regexp? val-form)
     (handle-regexp val-form))
    ((byte-regexp? val-form)
     (handle-byte-regexp val-form))
    ((bytes? val-form)
     (handle-bytes val-form))
    ((stx-obj? val-form)
     (to-ast-val (stx-obj-datum val-form)))
    ;; keep the boolean? and void? here
    ;; for they can be in lists/hashes
    ;; note that they're (not value?)
    ;; because they're handled differently as a bytecode body-form (than as a value)
    ((boolean? val-form)
     (handle-boolean val-form))
    ((void? val-form)
     (handle-void val-form))
    (else (error 'to-ast-val (format "unhandled value : ~a" val-form)))))

;; stack : (listof symbol?/prefix?/hash?)
(define (to-ast-single body-form localref-stack current-closure-refs)
  (begin
    (when DEBUG
      ;(display (format "\nTOPLEVELS : ~a" TOPLEVELS))
      (display "\n---------------------------------\n")
      (display (body-name body-form))
      (display "- ")
      (if (localref? body-form)
          (begin (displayln (format "localref-stack size : ~a" (length localref-stack)))
                 (display (format "Get pos : ~a - Unbox? : ~a - Clear? : ~a" (localref-pos body-form) (localref-unbox? body-form) (localref-clear? body-form)))
                 (display (format " - extracting : ~a" (list-ref localref-stack (localref-pos body-form)))))
          (if (primval? body-form)
              (display (get-primval-name (primval-id body-form)))
              (display "")))
      (display " - LocalRefStack size : ")
      (displayln (number->string (length localref-stack)))(newline)
      (display localref-stack)
      (display "\n---------------------------------")
      (newline)(newline))
    (cond
      ;; VALUES
      
      ((value? body-form)
       (hash* 'quote (to-ast-val body-form)))
      
      ;; specially handled vals
      ((void? body-form) 
       (handle-void body-form))
      ((boolean? body-form) ;; note it uses hash (instead of hash*)
       (hash 'quote (handle-boolean body-form)))

      ;; FORMS
      
      ;; let-void
      ((let-void? body-form)
       (handle-let-void body-form localref-stack current-closure-refs))
      ;; let-rec
      ((let-rec? body-form)
       (handle-let-rec body-form localref-stack current-closure-refs))
      ;; case-lambda
      ((case-lam? body-form)
       (handle-case-lambda body-form localref-stack current-closure-refs))
      ;; install-value
      ((install-value? body-form)
       (handle-install-value body-form localref-stack current-closure-refs))
      ;; set!
      ((assign? body-form) ;; CAUTION : returns list of hash* (instead of hash*)
       (handle-assign body-form localref-stack current-closure-refs))
      ;; toplevel
      ((toplevel? body-form)
       (handle-toplevel body-form localref-stack))
      ;; let-one (struct let-one expr (rhs body type unused?)
      ((let-one? body-form)
       (handle-let-one body-form localref-stack current-closure-refs))
      ;; seq
      ((seq? body-form)
       (handle-seq body-form localref-stack current-closure-refs))
      ;; splice
      ((splice? body-form)
       (handle-splice body-form localref-stack current-closure-refs))
      ;; beg0
      ((beg0? body-form)
       (handle-begin0 body-form localref-stack current-closure-refs))
      ;; module-variable
      ((module-variable? body-form)
       (handle-module-variable body-form localref-stack))
      ;; varref
      ((varref? body-form)
       (handle-varref body-form localref-stack))
      ;; primval : operations from run-time
      ((primval? body-form)
       (handle-primval body-form))
      ;; application
      ((application? body-form)
       (handle-application body-form localref-stack current-closure-refs))
      ;; def-values
      ((def-values? body-form)
       (handle-def-values body-form localref-stack current-closure-refs))
      ;; if
      ((branch? body-form)
       (handle-if body-form localref-stack current-closure-refs))
      ;; with-continuation-mark
      ((with-cont-mark? body-form)
       (handle-wcm body-form localref-stack current-closure-refs))
      ;; with-immed-mark
      ((with-immed-mark? body-form)
       (handle-immed-mark body-form localref-stack current-closure-refs))
      ;; boxenv
      ((boxenv? body-form)
       (handle-boxenv body-form localref-stack current-closure-refs))
      ;; apply-values
      ((apply-values? body-form)
       (handle-apply-values body-form localref-stack current-closure-refs))
      ;; localref
      ((localref? body-form)
       (handle-localref body-form localref-stack))
      ;; lambda
      ((lam? body-form)
       (handle-lambda body-form localref-stack current-closure-refs))
      ;; inline-variant (direct | inline)
      ((inline-variant? body-form)
       (handle-inline-variant body-form localref-stack current-closure-refs))
      ;; closure (procedure constant)
      ((closure? body-form)
       (handle-closure body-form localref-stack current-closure-refs))
      ((topsyntax? body-form)
       (handle-topsyntax body-form localref-stack current-closure-refs))
      (else (begin (display "-- NOT SUPPORTED YET: ")
                   (display body-form)
                   (newline)(newline)
                   "not supported yet")))))

(define (to-ast body-forms)
  (map (lambda (form) (to-ast-single form '() '())) body-forms))

(define (set-globals! debug mod-name rel-current-dir)
  (begin
    (set! DEBUG debug)
    (set! module-name mod-name)
    (set! relative-current-dir rel-current-dir)))

(define (set-toplevels! toplevels topsyntaxes)
  (set! TOPLEVELS toplevels)
  (set! TOPSYNTAX topsyntaxes))

(define (to-ast-wrapper body-forms toplevels topstxs debug mod-name relative-dir)
  (begin
    (set-globals! debug mod-name relative-dir)
    (set-toplevels! toplevels topstxs)
    (to-ast body-forms)))

(module+ main
  (require racket/cmdline json compiler/cm)

  (define debug #f)
  (define sub-dirs-str #f)
  (define out #f)


  (command-line
   #:once-each
   [("-v" "--verbose" "-d" "--debug") "show what you're doing" (set! debug #t)]
   [("--stdout") "write output to standart out" (set! out (current-output-port))]

   #:args (file.rkt)
   (let* ([subs (string-split file.rkt "/")]
          [sub-dirs (take subs (max (sub1 (length subs)) 0))]
          [is-absolute? (equal? (substring file.rkt 0 1) "/")]
          [sub-dirs-str* (if (empty? sub-dirs)
                             (path->string (current-directory))
                             (string-append (if is-absolute? "/" "") (string-join sub-dirs "/") "/"))]
          [mod-name.rkt (last subs)]
          [mod-name (substring mod-name.rkt 0 (- (string-length mod-name.rkt) 4))])
     (begin
       ;; setting the stage
       (set! sub-dirs-str sub-dirs-str*)
       (set-globals! debug mod-name sub-dirs-str)
       (managed-compile-zo file.rkt)
       ;; setting the output port
       (when (not (output-port? out))
         (set! out (open-output-file (string-append sub-dirs-str "fromBytecode_" mod-name ".rkt.json")
                                     #:exists 'replace))))))

  (define dep-file (read (open-input-file (string-append sub-dirs-str "compiled/" module-name "_rkt.dep"))))
  
  (define version (car dep-file))
  
  (define comp-top (zo-parse (open-input-file (string-append sub-dirs-str "compiled/" module-name "_rkt.zo"))))

  (define code (compilation-top-code comp-top)) ;; code is a mod

  (define regular-provides (cadr (assv 0 (mod-provides code))))
  (define syntax-phase-provides (caddr (assv 0 (mod-provides code))))

  (define all-provides '() #;(append regular-provides syntax-phase-provides))
  ;; removed handle-provides (since we don't care about the provides now)
  (define top-provides '() #;(if (not (empty? all-provides))
                           (list (cons (hash* 'source-name "#%provide")
                                       (handle-provides all-provides '())))
                           '()))
  
  (define top-reqs (mod-requires code)) ;; assoc list ((phase mods) ..)
  (define phase0 (assv 0 top-reqs))
  (define phase0-reqs (cdr phase0))
  
  (define lang (module-path-index->path-string (car phase0-reqs)))
  
  (define lang-pycket? (or (string=? lang "#%kernel")
                           (string-contains? lang "pycket-lang")))
  
  (define runtime-config
    (if lang-pycket?
        'dont-care ;; if lang-pycket?, then the runtime-config will never be added to the body forms
        (let* ([pre-submods (mod-pre-submodules code)]
               [runtime-prefix (mod-prefix (car pre-submods))]
               [runtime-mod (car (prefix-toplevels runtime-prefix))]
               ;; assert (module-variable? runtimeMod) and (eqv? module-variable-sym 'configure)
               [resolved-mod-path (resolved-module-path-name
                                 (module-path-index-resolve (module-variable-modidx runtime-mod)))]
               [runtime-config (if (or (list? resolved-mod-path)
                                      (symbol? resolved-mod-path))
                                  (error 'runtimeConfigModule "don't know how to handle a submodule here")
                                  (path->string resolved-mod-path))])
          (hash* 'language (list "#%kernel")
                 'module-name (symbol->string (mod-srcname (car pre-submods)))
                 'body-forms (list
                              (hash* 'require (list (list runtime-config)))
                              (hash* 'operator (hash* 'source-module (list runtime-config)
                                                      'source-name (symbol->string
                                                                    (module-variable-sym runtime-mod)))
                                     'operands (list (hash 'quote #f))))))))

  (define reqs (cdr phase0-reqs))

  (define top-level-req-forms
    (map (λ (req-mod) (hash* 'require (list (list (module-path-index->path-string req-mod))))) reqs))

  ;; code-body : listof def-values
  (define (prepare-toplevels code-body)
    (let* ([defvals (filter def-values? code-body)]
           [new-toplevels (collect-toplevels defvals)]) ;; <-- ((pos-num top-sym) ...)
      (if (null? new-toplevels)
          toplevels ;; don't bother
          (let*
              ([toplen (length toplevels)]
               ;; aligning new toplevels like ((30 x) (32 a) (35 b)) ==> ((30 x) (31 dummy) (32 a) (33 dummy) (34 dummy) (35 b))
               ;; we know that there's no reference in the code to the missing toplevels, but the position matters (for list-ref)
               ;; TODO : revisit : use a hashmap for toplevels
               [aligned-new-toplevels (foldr (λ (x rest)
                                               (if (or (null? rest)
                                                       (= 1 (- (caar rest) (car x))))
                                                   (cons x rest)
                                                   (let ([diff (- (caar rest) (car x) 1)])
                                                     (cons x (append (build-list diff (λ (n) (list (+ n x 1) 'dummytop))) rest))))) null new-toplevels)]

               [padded-new-toplevels (let* ([diff (- (caar aligned-new-toplevels) toplen)]
                                            [pad (build-list diff (λ (n) (list (+ n toplen) 'dummytop)))])
                                       (append pad aligned-new-toplevels))])
                                       
            ;; at this point, we know everything's in place,
            ;; so we can safely append prepared new-toplevels to the current global toplevels
            (let ([top-syms (map cadr padded-new-toplevels)])
              (append toplevels top-syms))))))
  
  ;; collect-toplevels : (listof def-values) -> (listof pos-num top-var-sym)
  (define (collect-toplevels code-body)
    (sort
     (filter
      (compose not null?)
      (map (λ (defval)
             (let* ([def-ids (def-values-ids defval)]
                    [def-rhs (def-values-rhs defval)]
                    [poss (map toplevel-pos def-ids)]
                    [toplen (length toplevels)]) ; <- this is the real prefix-toplevels from comp-top
               (if (= (length poss) 1)
                   (if (< (car poss) toplen)
                       '()
                       (let ([sym (let ([name (cond [(lam? def-rhs) (lam-name def-rhs)]
                                                    [(inline-variant? def-rhs) (lam-name (inline-variant-direct def-rhs))]
                                                    [else (error 'collect-toplevels "couldn't get the name from ~a" defval)])])
                                    (if (symbol? name) name (gensym (vector-ref name 0))))])
                         (list (car poss) sym)))
                   ;; we have multiple toplevels at the defval
                   (if (ormap (λ (pos) (>= pos toplen)) poss)
                       (error 'prepare-toplevels "one of the ids have >toplevel pos : ~a in defval : " poss defval)
                       ;; then all the top-posses are < toplen, thus we ignore
                       '()))))
           code-body))
     (λ (l1 l2) (< (car l1) (car l2)))))

  ;; toplevels : #f | global-bucket | module-variable
  (define toplevels (prefix-toplevels (mod-prefix code)))
  (define complete-toplevels (prepare-toplevels (mod-body code)))

  (define topsyntaxes (prefix-stxs (mod-prefix code))) ;; (listof stx?)
  
  (set-toplevels! complete-toplevels topsyntaxes)

  (define final-json-hash (compile-json global-config
                                        (if lang-pycket? "#%kernel" lang)
                                        (string-append sub-dirs-str "fromBytecode_" module-name)
                                        runtime-config
                                        (append top-level-req-forms top-provides)
                                        (to-ast (mod-body code))
                                        lang-pycket?))
  
  (begin
    (write-json final-json-hash out)
    (newline out)
    (flush-output out)))
