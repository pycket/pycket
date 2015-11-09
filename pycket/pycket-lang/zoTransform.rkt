#lang racket

(require compiler/zo-parse setup/dirs
         (only-in pycket/expand hash* global-config))

(define DEBUG #f)
(define pycketDir (path->string (current-directory))) ;; MUST BE RUN UNDER PYCKET DIR
(define collectsDir (path->string (find-collects-dir)))
(define moduleName 'gonnaBeSetBy-main)

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
                   [(struct compilation-top (_ prefix (struct primval (n)))) n]
                   [else #f])])
          (hash-set! table n (car b)))))
    table))

(define (compile-json config language topmod body1 body-forms)
  (hash* 'language (list language)
         'module-name topmod
         'config config
         'body-forms (cons body1
                           body-forms)))

(define (handle-def-values def-values-form toplevels  localref-stack)
  (let ((ids (def-values-ids def-values-form))
        (rhs (def-values-rhs def-values-form)))
    (cond
      ((or (not (= 1 (length ids))) (not (toplevel? (car ids))))
       (error 'handle-def-values "look into multiple toplevels"))
      (else
       (let* ((toplevel-form (car ids))
              (pos (toplevel-pos toplevel-form))
              (sym (list-ref toplevels pos))
              (symstr (if (symbol? sym) (symbol->string sym) sym))
              )
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
     
(define (handle-number racket-num)
  (hash* 'quote (hash* 'number (hash* 'integer (number->string racket-num)))))

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
  (let* ((rator (application-rator app-form))
         (rands (application-rands app-form))
         (newlocalstack (append (map (lambda (x) 'app-empty-slot) (range (length rands)))
                                localref-stack))
         ;; because we know that the application will push empty slots when running, so it will push the local references further
         ;; we kinda simulate it here to make the localref pos indices point to the right identifier
         (operands-evaluated (map (λ (rand) (to-ast-single rand toplevels newlocalstack)) rands))
         )
    (hash* 'operator (to-ast-single rator toplevels localref-stack)
           'operands operands-evaluated)))

(define (handle-lambda lam-form toplevels localref-stack is-inlined)
  (let* ((source (hash* '%p (path->string (vector-ref (lam-name lam-form) 1))))
         (position 321) ;; don't know what exactly are these two
         (span 123) 
         ;; module seems to be the same for every lambda form,
         ;; pointing to a private module about chaperones/impersonators
         (module (hash* '%mpi (hash* '%p (string-append collectsDir "/racket/private/kw.rkt"))))

         (num-args (lam-num-params lam-form))
         (symbols-for-formals (map (lambda (x) (symbol->string (gensym))) (range num-args)))
         (body (to-ast-single (lam-body lam-form) toplevels (if is-inlined
                                                                (cons 'lambda-dummy
                                                                      (append symbols-for-formals localref-stack))
                                                                (append symbols-for-formals localref-stack))))
         (lamBda (map (lambda (sym) (hash* 'lexical sym)) symbols-for-formals))
         )
  (hash* 'source source
         'position position
         'span span
         'module module
         'lambda lamBda
         'body (list body))))

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
         [lambda-form (hash* 'source (hash* '%p (string-append pycketDir "frombytecode_" moduleName ".rkt")) ;; toplevel application
                             'position 321
                             'span 123
                             'module (hash* '%mpi (hash* '%p (path->string
                                                              (resolved-module-path-name
                                                               (module-path-index-resolve
                                                                (module-variable-modidx mod-var))))))
                             'lambda '()
                             'body (list args))]
         )

  ;; (call-with-values lam proc)
    (hash* 'operator (hash* 'source-name "call-with-values")
           'operands (list lambda-form proc))
    ))

(define (handle-localref lref-form toplevels localref-stack)
  (let* (
         (pos (localref-pos lref-form))
         (stack-slot-raw (list-ref localref-stack pos))
         (stack-slot (if (box? stack-slot-raw) (unbox stack-slot-raw) stack-slot-raw))
         )
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
  (let* (
         (newstack-prev (cons 'let-one-uninitialized-slot localref-stack)) ;; push uninitialized slot
         (rhs (to-ast-single (let-one-rhs letform) toplevels newstack-prev)) ;; evaluate rhs
         (newstack (cons rhs (cdr newstack-prev))) ;; put the rhs to the slot
        )
    (hash* 'let-bindings '()
           'let-body (list (to-ast-single (let-one-body letform)
                                          toplevels
                                          newstack)))))

(define (body-name body-form)
  (cond
    ((number? body-form) "Number ")
    ((string? body-form) "String ")
    ((symbol? body-form) "Symbol ")
    ((let-one? body-form) "let-one ")
    ((module-variable? body-form) "module-variable")
    ((primval? body-form) "primval")
    ((application? body-form) "application")
    ((def-values? body-form) "def-values")
    ((assign? body-form) "SET!")
    ((branch? body-form) "branch")
    ((apply-values? body-form) "apply-values")
    ((localref? body-form) "localref")
    ((lam? body-form) "lam")
    ((inline-variant? body-form) "inline-variant")
    ((closure? body-form) "closure")
    ((toplevel? body-form) "toplevel")
    ((hash? body-form) "Already hashed Val (pushed by let-one)")
    (else "Unknown: ")))

(define (handle-toplevel form toplevels localref-stack)
  (let* ((toplevel-id (list-ref toplevels (toplevel-pos form)))
         (toplevel-id-str (if (symbol? toplevel-id) (symbol->string toplevel-id) toplevel-id))
         (module-dir (string-append pycketDir moduleName ".rkt")))
    (if (symbol? toplevel-id)
        (hash* 'source-name toplevel-id-str
               'source-module module-dir)
        (hash* 'source-name toplevel-id))))
  #|
  (let* ((toplevel-id (list-ref toplevels (toplevel-pos form)))
         (toplevel-id-str (if (symbol? toplevel-id) (symbol->string toplevel-id) toplevel-id)))
  (to-ast-single toplevel-id-str toplevels localref-stack)))
|#
  
(define (handle-seq seq-expr toplevels localref-stack)
  (let* ((seqs (seq-forms seq-expr))
         (vals (map (λ (expr) (to-ast-single expr toplevels localref-stack)) seqs)))
    (list-ref vals (sub1 (length vals)))))
    
(define (handle-assign body-form toplevels localref-stack)
  (let ((id (assign-id body-form))
        (rhs (assign-rhs body-form))
        (module-dir (string-append pycketDir moduleName ".rkt")))
    (list (hash* 'source-name "set!")
          (to-ast-single id toplevels localref-stack)
          (to-ast-single rhs toplevels localref-stack))))

(define (handle-let-void body-form toplevels localref-stack)
  ;; Pushes count uninitialized slots onto the stack and then runs body.
  ;; If boxes? is #t, then the slots are filled with boxes that contain #<undefined>.
  (let* ((count (let-void-count body-form))
         (boxes? (let-void-boxes? body-form))
         (body (let-void-body body-form))
         (boxls (build-list count (lambda (x) (box 'dummy))))
         (newstack (append boxls localref-stack)))
    (to-ast-single body toplevels newstack)))

(define (handle-install-value body-form toplevels localref-stack)
  ;; Runs rhs to obtain count results, and installs them into existing
  ;; slots on the stack in order, skipping the first pos stack positions.
  (let* ((count (install-value-count body-form))
         (pos (install-value-pos body-form))
         (boxes? (install-value-boxes? body-form))
         (rhs (install-value-rhs body-form))
         (body (install-value-body body-form)))
    ;; obtainng boxes
    (let* ((box-positions (map (λ (p) (+ p pos)) (range count))))
      (begin
        (for ([i box-positions])
          (set-box! (list-ref localref-stack i) 'installed-val))
        (to-ast-single body toplevels localref-stack)))))

;; stack : (listof symbol?/prefix?/hash?)
(define (to-ast-single body-form toplevels localref-stack)
  (begin
    (if DEBUG
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
          (newline)(newline))
        1)
    (cond
      ;;;;;;;
      ;
      ; for-loop
      ;; localref fixnum?
      ;;;;;;;
      ((number? body-form)
       (handle-number body-form))
      ((string? body-form)
       (handle-string body-form))
      ((symbol? body-form)
       (handle-symbol body-form))
      ;; already hashed? then return it (will be a problem when implementing pycket hashes)
      ((hash? body-form) body-form)
      ;; let-void
      ((let-void? body-form)
       (handle-let-void body-form toplevels localref-stack))
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

(define (setGlobals! debug modName)
  (begin
    (set! DEBUG debug)
    (set! moduleName modName)))

(module+ main
  (require racket/cmdline json
           compiler/cm
           
           
           )

  (define debug #f)
  
  (define file.rkt
    (command-line
     #:once-each
     [("-v" "--verbose" "-d" "--debug") "show what you're doing" (set! debug #t)]

     #:args (file.rkt)

     file.rkt))

  (managed-compile-zo file.rkt) ;; compile to bytecode (checks if the source has changed)

  (define modName (substring file.rkt 0 (- (string-length file.rkt) 4))) ;; stripping the extension
  (setGlobals! debug modName)
  
  (define depFile (read (open-input-file (string-append "compiled/" moduleName "_rkt.dep"))))
  (define version (car depFile))


  ;; config
  ;; global-config
  
  ;; language          (language . ("/home/caner/programs/racket/collects/racket/main.rkt"))
  ;; langDep -> '(collects #"racket" #"main.rkt")

  (define langDep (cadddr depFile))
  (define lang (string-append collectsDir "/racket" "/main.rkt"))
  
  ;; module-name          (module-name . "canerStr")
  ;; body-forms

  (define comp-top (zo-parse (open-input-file (string-append "compiled/" moduleName "_rkt.zo"))))

  ;; (struct compilation-top zo (max-let-depth prefix code)
  ;; (struct prefix zo (num-lifts toplevels stxs)

  ;; toplevels : #f | global-bucket | module-variable

  (define code (compilation-top-code comp-top)) ;; code is a mod

  ;; (struct mod form (name
  ;;  	 	     srcname
  ;;  	 	     self-modidx
  ;;  	 	     prefix
  ;;  	 	     provides ; each phase maps to two lists: exported variables, exported syntax
  ;;  	 	     requires ; each phase maps to a list of imported module paths
  ;;  	 	     body    <<-- run-time (phase 0) code -> listof form?
  ;;  	 	     syntax-bodies
  ;;  	 	     unexported
  ;;  	 	     max-let-depth
  ;;  	 	     dummy
  ;;  	 	     lang-info
  ;;  	 	     internal-context
  ;;  	 	     flags
  ;;  	 	     pre-submodules
  ;;  	 	     post-submodules)
  ;;
  ;;     #:extra-constructor-name make-mod
  ;;     #:prefab)

  (define toplevels (prefix-toplevels (mod-prefix code)))

  (define preConfig (mod-pre-submodules code))

  (define runtimeDep (caddr depFile))
  ;; '(collects #"racket" #"runtime-config.rkt")
  (define runtimeConfig (string-append collectsDir
                                       "/" (bytes->string/utf-8 (cadr runtimeDep)) ;"/racket"
                                       "/" (bytes->string/utf-8 (caddr runtimeDep)) ;"runtime-config.rkt"))
                                       ))

  ;; body-forms is a (listof hash hash)

  (define config global-config)
  (define language lang)

  (define body1 (hash* 'language (list "#%kernel")
                       'module-name (symbol->string (mod-srcname (car preConfig)))
                       'body-forms (list
                                    (hash* 'require (list (list runtimeConfig)))
                                    (hash* 'operator (hash* 'source-module (list runtimeConfig)
                                                            'source-name "configure" ;;; <-- ????
                                                            )
                                           'operands (list (hash 'quote #f))))))
  

  (define final-json-hash (compile-json global-config
                                        lang
                                        (string-append "frombytecode_" moduleName)
                                        body1
                                        (to-ast (mod-body code) toplevels)))
  
  (define out (open-output-file (string-append "frombytecode_" moduleName ".rkt.json")
                                #:exists 'replace))
  (begin
    (display (string-append "WRITTEN: frombytecode_" moduleName ".rkt.json\n\n"))
    (write-json final-json-hash out)
    (newline out)
    (flush-output out))

  )

