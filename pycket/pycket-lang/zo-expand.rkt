#lang racket

(require compiler/zo-parse setup/dirs
         (only-in pycket/expand hash* global-config))

(provide to-ast-wrapper primitive-table)

(define DEBUG #f)
(define pycket-dir (path->string (current-directory))) ;; MUST BE RUN UNDER PYCKET DIR
(define collects-dir (path->string (find-collects-dir)))
(define module-name 'beSetBy-main)

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

(define (compile-json config language topmod body1 top-require-forms body-forms pycket?)
  (let ([whole-body (append top-require-forms body-forms)])
    (hash* 'language (list language)
           'module-name topmod
           'config config
           'body-forms (if pycket?
                           whole-body
                           (cons body1 whole-body)))))
  
(define (handle-def-values def-values-form toplevels  localref-stack)
  (let ((ids (def-values-ids def-values-form))
        (rhs (def-values-rhs def-values-form)))
    (cond
      ((or (not (= 1 (length ids))) (not (toplevel? (car ids))))
       (error 'handle-def-values "look into multiple toplevels"))
      (else
       (let* [(toplevel-form (car ids))
              (pos (toplevel-pos toplevel-form))
              (sym (list-ref toplevels pos))
              (symstr (if (symbol? sym) (symbol->string sym) sym))]
         (hash* 'define-values (list symstr)
                'define-values-names (list symstr)
                'define-values-body (to-ast-single rhs toplevels
                                                   (cons symstr localref-stack))))))))

(define (handle-if if-form toplevels localref-stack)
  (let ((test (branch-test if-form))
        (then (branch-then if-form))
        (else (branch-else if-form)))
    (hash*
     'test (to-ast-single test toplevels localref-stack)
     'then (to-ast-single then toplevels localref-stack)
     'else (to-ast-single else toplevels localref-stack))))

;; TODO: we'll probably need a more precise one
(define (handle-number racket-num)
  (cond
    [(integer? racket-num)
     (hash* 'integer (number->string racket-num))]
    [(real? racket-num)
     (if (inexact? racket-num)
         (hash* 'real racket-num)
         (let ;; assumes it's an exact rational
             ([num (numerator racket-num)]
              [den (denominator racket-num)])
           (hash* 'numerator (handle-number num)
                  'denominator (handle-number den))))]
    [else ; this part assumes (for the moment) it's a complex num
     (let ([real (real-part racket-num)]
           [imag (imag-part racket-num)])
       (hash* 'real-part (handle-number real)
              'imag-part (handle-number imag)))]))

(define (handle-boolean racket-bool)
  (hash* 'quote racket-bool))

(define (handle-string racket-str)
  (hash* 'quote (hash* 'string racket-str)))

(define (handle-symbol racket-sym)
  (hash* 'quote (hash* 'toplevel (symbol->string racket-sym))))

(define (get-primval-name id)
  (symbol->string (hash-ref primitive-table id)))
  
(define (handle-primval operation toplevels)
  (let* ((id (primval-id operation))
         (operator-name (get-primval-name id)))
    (hash* 'source-name operator-name)))

(define (handle-application app-form toplevels localref-stack)
  (let* [(rator (application-rator app-form))
         (rands (application-rands app-form))
         (newlocalstack (append (map (lambda (x) 'app-empty-slot) (range (length rands)))
                                localref-stack))
         ;; the application pushes empty slots to run body over, so it will push the current local references further
         ;; we kinda simulate it here to make the localref pos indices point to the right identifier
         (operands-evaluated (map (λ (rand) (to-ast-single rand toplevels newlocalstack)) rands))]
    (hash* 'operator (to-ast-single rator toplevels localref-stack)
           'operands operands-evaluated)))

(define (handle-lambda lam-form toplevels localref-stack is-inlined)
  (let* [(name (lam-name lam-form))
         (source (if (null? name) '()
                     (if (vector? name)
                         (hash* '%p (path->string (vector-ref name 1)))
                         (if (not (symbol? name)) (error 'handle-lambda "we have a non symbol/vector name in a lam form")
                             (let* ((collects-dir (path->string (find-collects-dir)))
                                    (usual-prefix "/racket/private/") ; TODO: figure out why they have a new way of naming lam's
                                    (lamname (if (not (string-contains? (symbol->string name) ".../more-scheme.rkt"))
                                                 (error 'handle-lambda "lam name has an unusual form")
                                                 "more-scheme.rkt")))
                               (hash* '%p (string-append collects-dir usual-prefix lamname)))))))
         (position 321) ;; don't know what exactly are these two
         (span 123) 
         ;; module seems to be the same for every lambda form,
         ;; pointing to a private module about chaperones/impersonators
         (module (hash* '%mpi (hash* '%p (string-append collects-dir "/racket/private/kw.rkt"))))

         (num-args (lam-num-params lam-form))
         (symbols-for-formals (map (lambda (x) (symbol->string (gensym))) (range num-args)))
         (body (to-ast-single (lam-body lam-form) toplevels (if is-inlined
                                                                (cons 'lambda-dummy
                                                                      (append symbols-for-formals localref-stack))
                                                                (append symbols-for-formals localref-stack))))
         (lamBda (map (lambda (sym) (hash* 'lexical sym)) symbols-for-formals))]
    ;; pycket seems to omit source and position (and sets the span to 0) if lam-name is ()
    (if (null? source)
        (hash* 'span 0 'module module 'lambda lamBda 'body (list body))
        (hash* 'source source 'position position 'span span 'module module 'lambda lamBda 'body (list body)))))

(define (handle-inline-variant iv-form toplevels localref-stack)
  (let ((direct (inline-variant-direct iv-form))
        (inline (inline-variant-inline iv-form)))
    ;; don't know what to do with the inline yet
    (to-ast-single direct toplevels localref-stack)))

(define (handle-closure closure-form toplevels localref-stack)
  (let ((code (closure-code closure-form)))
    (if (lam? code)
        (handle-lambda code toplevels localref-stack false)
        (error 'handle-closure "no lam inside the closure?"))))

(define (handle-apply-values app-form toplevels localref-stack)
  (let* ([proc-part (apply-values-proc app-form)]
         [args-part (apply-values-args-expr app-form)]

         [mod-var (if (not (toplevel? proc-part))
                      (error 'handle-apply-values "proc is NOT a toplevel")
                      (list-ref toplevels (toplevel-pos proc-part)))]

         [proc (if (not (module-variable? mod-var))
                   (error 'handle-apply-values "toplevel is NOT a module-variable")
                   (to-ast-single mod-var toplevels localref-stack))]

         [args (to-ast-single args-part toplevels localref-stack)]

         ;; construct the lam (lambda () args)
         [lambda-form (hash* 'source (hash* '%p (string-append pycket-dir "fromBytecode_" module-name ".rkt")) ;; toplevel application
                             'position 321
                             'span 123
                             'module (hash* '%mpi (hash* '%p (path->string
                                                              (resolved-module-path-name
                                                               (module-path-index-resolve
                                                                (module-variable-modidx mod-var))))))
                             'lambda '()
                             'body (list args))])

  ;; (call-with-values lam proc)
    (hash* 'operator (hash* 'source-name "call-with-values")
           'operands (list lambda-form proc))
    ))

(define (handle-localref lref-form toplevels localref-stack)
  (let* [(pos (localref-pos lref-form))
         (stack-slot-raw (list-ref localref-stack pos))
         (stack-slot (if (box? stack-slot-raw) (unbox stack-slot-raw) stack-slot-raw))]
    (cond
      [(hash? stack-slot) stack-slot]
      [else (hash* 'lexical stack-slot)])))

;;(define (handle-toplevel toplevel-form toplevels)
  ;;(let ((pos (toplevel-pos toplevel-form)))

(define (handle-module-variable mod-var toplevels localref-stack)
  (let ((name (symbol->string (module-variable-sym mod-var)))
        (module-path (path->string
                         (resolved-module-path-name
                          (module-path-index-resolve
                           (module-variable-modidx mod-var))))))
  (hash* 'source-name name
         'source-module (list module-path))))

(define (handle-let-one letform toplevels localref-stack)
  (let* [(newstack-prev (cons 'let-one-uninitialized-slot localref-stack)) ;; push uninitialized slot
         (rhs (to-ast-single (let-one-rhs letform) toplevels newstack-prev)) ;; evaluate rhs
         (newstack (cons rhs (cdr newstack-prev)))] ;; put the rhs to the slot
    (hash* 'let-bindings '()
           'let-body (list (to-ast-single (let-one-body letform)
                                          toplevels
                                          newstack)))))

(define (body-name body-form)
  (cond
    ((list? body-form) "ATTENTION : we have a LIST")
    ((boolean? body-form) "Boolean ")
    ((number? body-form) "Number ")
    ((string? body-form) "String ")
    ((symbol? body-form) "Symbol ")
    ((let-one? body-form) "let-one ")
    ((let-void? body-form) "let-void ")
    ((case-lam? body-form) "case-lam ")
    ((install-value? body-form) "install-value ")
    ((module-variable? body-form) "module-variable ")
    ((primval? body-form) "primval ")
    ((application? body-form) "application ")
    ((def-values? body-form) "def-values ")
    ((seq? body-form) "seq ")
    ((assign? body-form) "SET! ")
    ((branch? body-form) "branch ")
    ((apply-values? body-form) "apply-values ")
    ((localref? body-form) "localref ")
    ((lam? body-form) "lam ")
    ((inline-variant? body-form) "inline-variant ")
    ((closure? body-form) "closure ")
    ((toplevel? body-form) "toplevel ")
    ((hash? body-form) "Already hashed Val (pushed by let-one)")
    (else "Unknown: ")))

(define (handle-toplevel form toplevels localref-stack)
  (let* ((toplevel-id (list-ref toplevels (toplevel-pos form)))
         (toplevel-id-str (if (symbol? toplevel-id) (symbol->string toplevel-id) toplevel-id))
         (module-dir (string-append pycket-dir module-name ".rkt")))
    (cond
      [(symbol? toplevel-id)
       (hash* 'source-name toplevel-id-str
              'source-module module-dir)]
      [(module-variable? toplevel-id)
       (handle-module-variable toplevel-id toplevels localref-stack)]
      [else (error 'handle-toplevel "not sure how to handle this kind of toplevel form")])))
  #|
  (let* ((toplevel-id (list-ref toplevels (toplevel-pos form)))
         (toplevel-id-str (if (symbol? toplevel-id) (symbol->string toplevel-id) toplevel-id)))
  (to-ast-single toplevel-id-str toplevels localref-stack)))
|#
  
(define (handle-seq seq-expr toplevels localref-stack)
  (let* ((seqs (seq-forms seq-expr))
         (vals (map (λ (expr) (to-ast-single expr toplevels localref-stack)) seqs)))
    vals)) ;;(list-ref vals (sub1 (length vals)))))
    
(define (handle-assign body-form toplevels localref-stack)
  (let ((id (assign-id body-form))
        (rhs (assign-rhs body-form))
        (module-dir (string-append pycket-dir module-name ".rkt")))
    (list (hash* 'source-name "set!")
          (to-ast-single id toplevels localref-stack)
          (to-ast-single rhs toplevels localref-stack))))

(define (handle-let-void body-form toplevels localref-stack)
  ;; Pushes count uninitialized slots onto the stack and then runs body.
  ;; If boxes? is #t, then the slots are filled with boxes that contain #<undefined>.
  (let* ((count (let-void-count body-form))
         (boxes? (let-void-boxes? body-form))
         (body (let-void-body body-form))
         (boxls (build-list count (lambda (x) (box 'uninitialized-slot))))
         (newstack (append boxls localref-stack)))
    (to-ast-single body toplevels newstack)))

(define (handle-case-lambda body-form toplevels localref-stack)
  (let ((clauses (case-lam-clauses body-form)))
    (hash* 'case-lambda (map (lambda (clause)
                               (if (not (lam? clause))
                                   (error 'handle-case-lambda "not a lam clause?")
                                   (let* ([name (lam-name clause)]
                                          [num-args (lam-num-params clause)]
                                          [multiple-args? (lam-rest? clause)] ;; is the rest? true
                                          
                                          [symbols-for-formals
                                           (if (not multiple-args?)
                                               (map (lambda (x) (symbol->string (gensym))) (range num-args))
                                               (list (symbol->string (gensym))))]
                                          
                                          [arg-mapping
                                           (if (not multiple-args?)
                                               (map (lambda (arg) (hash* 'lexical arg)) symbols-for-formals)
                                               (hash* 'lexical (car symbols-for-formals)))] ;; this is ugly
                                          [body (to-ast-single (lam-body clause)
                                                               toplevels
                                                               (append symbols-for-formals localref-stack))])
                                     (hash* 'lambda arg-mapping
                                            'body body))))
                             clauses))))
    

(define (handle-install-value body-form toplevels localref-stack)
  ;; Runs rhs to obtain count results, and installs them into existing
  ;; slots on the stack in order, skipping the first pos stack positions.
  (let* [(count (install-value-count body-form))
         (pos (install-value-pos body-form))
         (boxes? (install-value-boxes? body-form))
         (rhs (install-value-rhs body-form))
         (body (install-value-body body-form))
         (count-lst (range count))

         (binding-list (map (λ (c) (string-append "inst-val" (number->string c))) count-lst))
         
         (box-positions (map (λ (p) (+ p pos)) count-lst))]
    (begin
      ;; setting the boxes that let-void has put in the stack
      (for ([i box-positions]) (set-box! (list-ref localref-stack i) (list-ref binding-list (- i pos))))
      ;; producing json for pycket
      (hash*
       ;; let-bindings <- [count] {eval rhs}
       'let-bindings (list (list binding-list
                                 (to-ast-single rhs toplevels localref-stack)))
       ;; let-body <- body
       'let-body (to-ast-single body toplevels localref-stack)))))

(define (handle-list list-form toplevels localref-stack)
  ;; currently don't know how we can have a list as a body form,
  ;; but experiments show that we could have some '()s as arguments to an application
  ;; pycket seems to reflect this as source-name : "null"
  (if (not (null? list-form))
      (error 'handle-list "INVESTIGATE... we seem to have a (not null) list as a single body form in the byte-code")
      (hash* 'source-name "null")))
  ;; (map (lambda (form) (to-ast-single form toplevels localref-stack)) list-form))

;; stack : (listof symbol?/prefix?/hash?)
(define (to-ast-single body-form toplevels localref-stack)
  (begin
    (when DEBUG
      (begin
        (display (body-name body-form))
        (display "- ")
        (if (localref? body-form)
            (begin (display (number->string (localref-pos body-form)))
                   (display " - extracting : ")
                   (display (list-ref localref-stack (localref-pos body-form))))
            (if (primval? body-form)
                (display (get-primval-name (primval-id body-form)))
                (display "")))
        (display " - LocalRefStack : ")
        (display (number->string (length localref-stack)))
        (display localref-stack)
        (newline)(newline)))
    (cond
      ;;;;;;;
      ;
      ; for-loop
      ;; localref fixnum?
      ;;;;;;;
      ((list? body-form) 
       (handle-list body-form toplevels localref-stack))
      ((boolean? body-form)
       (handle-boolean body-form))
      ((number? body-form)
       (hash* 'quote (hash* 'number (handle-number body-form))))
      ((string? body-form)
       (handle-string body-form))
      ((symbol? body-form)
       (handle-symbol body-form))

      ((hash? body-form) body-form) ;; return already hashed body-form
      ;; let-void
      ((let-void? body-form)
       (handle-let-void body-form toplevels localref-stack))
      ;; case-lambda
      ((case-lam? body-form)
       (handle-case-lambda body-form toplevels localref-stack))
      ;; install-value
      ((install-value? body-form)
       (handle-install-value body-form toplevels localref-stack))
      ;; set!
      ((assign? body-form) ;; CAUTION : returns list of hash* (instead of hash*)
       (handle-assign body-form toplevels localref-stack))
      ;; toplevel
      ((toplevel? body-form)
       (handle-toplevel body-form toplevels localref-stack))
      ;; let-one (struct let-one expr (rhs body type unused?)
      ((let-one? body-form)
       (handle-let-one body-form toplevels localref-stack))
      ;; seq
      ((seq? body-form)
       (handle-seq body-form toplevels localref-stack))
      ;; module-variable
      ((module-variable? body-form)
       (handle-module-variable body-form toplevels localref-stack))
      ;; primval : operations from run-time
      ((primval? body-form)
       (handle-primval body-form toplevels))
      ;; application
      ((application? body-form)
       (handle-application body-form toplevels localref-stack))
      ;; def-values
      ((def-values? body-form)
       (handle-def-values body-form toplevels localref-stack))
      ;; if
      ((branch? body-form)
       (handle-if body-form toplevels localref-stack))
      ;; apply-values
      ((apply-values? body-form)
       (handle-apply-values body-form toplevels localref-stack))
      ;; localref
      ((localref? body-form)
       (handle-localref body-form toplevels localref-stack))
      ;; lambda
      ((lam? body-form)
       (handle-lambda body-form toplevels localref-stack true))
      ;; inline-variant (direct | inline)
      ((inline-variant? body-form)
       (handle-inline-variant body-form toplevels localref-stack))
      ;; closure (procedure constant)
      ((closure? body-form)
       (handle-closure body-form toplevels localref-stack))
      (else (begin (display "-- NOT SUPPORTED YET: ")
                   (display (prefab-struct-key body-form))
                   (newline)(newline)
                   "not supported yet")))))

(define (to-ast body-forms toplevels)
  (map (lambda (form) (to-ast-single form toplevels '())) body-forms))

(define (set-globals! debug mod-name)
  (begin
    (set! DEBUG debug)
    (set! module-name mod-name)))

(define (to-ast-wrapper body-forms toplevels debug mod-name)
  (begin
    (set-globals! debug mod-name)
    (to-ast body-forms toplevels)))

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
          [sub-dirs-str* (if (empty? sub-dirs) "" (string-append "/" (string-join sub-dirs "/") "/"))]
          [mod-name.rkt (last subs)]
          [mod-name (substring mod-name.rkt 0 (- (string-length mod-name.rkt) 4))])
     (begin
       ;; setting the stage
       (set! sub-dirs-str sub-dirs-str*)
       (set-globals! debug mod-name)
       (managed-compile-zo file.rkt)
       ;; setting the output port
       (when (not (output-port? out))
         (set! out (open-output-file (string-append sub-dirs-str "fromBytecode_" mod-name ".rkt.json")
                                     #:exists 'replace))))))
          


  (define dep-file (read (open-input-file (string-append sub-dirs-str "compiled/" module-name "_rkt.dep"))))
  
  (define version (car dep-file))


  ;; config
  ;; global-config
  
  ;; language          (language . ("/home/caner/programs/racket/collects/racket/main.rkt"))
  ;; langDep -> '(collects #"racket" #"main.rkt")
  
  (define comp-top (zo-parse (open-input-file (string-append sub-dirs-str "compiled/" module-name "_rkt.zo"))))

  (define code (compilation-top-code comp-top)) ;; code is a mod
  
  ;; toplevels : #f | global-bucket | module-variable
  (define toplevels (prefix-toplevels (mod-prefix code)))
  
  ;; language          (language . ("/home/caner/programs/racket/collects/racket/main.rkt"))
  ;; langDep -> '(collects #"racket" #"main.rkt")
  
  (define top-reqs (mod-requires code)) ;; assoc list ((phase mods) ..)
  (define phase0 (assv 0 top-reqs))
  (define phase0-reqs (cdr phase0))
  (define lang-mod-path (resolved-module-path-name
                       (module-path-index-resolve (car phase0-reqs))))
  (define lang (if (or (list? lang-mod-path) (symbol? lang-mod-path))
                   (error 'lang-config "don't know how to handle a submodule here")
                   (path->string lang-mod-path)))
  
  (define lang-pycket? (string-contains? lang "pycket-lang"))
  
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
  (define top-level-req-forms (map (lambda (req)
                                      (let ((resolved-req-path (resolved-module-path-name
                                                              (module-path-index-resolve req))))
                                        (if (or (list? resolved-req-path) (symbol? resolved-req-path))
                                            (error 'req-forms "don't know how to handle a submodule here")
                                            (hash* 'require (list (list (path->string resolved-req-path)))))))
                                    reqs))

  ;; body-forms is a (listof hash hash)  

  (define final-json-hash (compile-json global-config
                                        (if lang-pycket? "#%kernel" lang)
                                        (string-append sub-dirs-str "fromBytecode_" module-name)
                                        runtime-config
                                        top-level-req-forms
                                        (to-ast (mod-body code) toplevels)
                                        lang-pycket?))
  
  (begin
    (write-json final-json-hash out)
    (newline out)
    (flush-output out)))

