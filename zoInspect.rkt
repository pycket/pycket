#lang racket

(require compiler/zo-parse
         setup/dirs
         ;racket/cmdline
         json)

(require "pycket/pycket-lang/expand.rkt") ;; global-config - hash*


(define moduleName "fact")

(define depFile (read (open-input-file (string-append "compiled/" moduleName "_rkt.dep"))))
(define version (car depFile))

(define pycketDir (path->string (current-directory))) ;; MUST BE RUN UNDER PYCKET DIR
;; config
;; global-config
  
;; language          (language . ("/home/caner/programs/racket/collects/racket/main.rkt"))
;; langDep -> '(collects #"racket" #"main.rkt")
(define collectsDir (path->string (find-collects-dir)))
(define langDep (cadddr depFile))
(define lang (string-append collectsDir "/racket" "/main.rkt"))
  
;; module-name          (module-name . "canerStr")



;; body-forms

(define comp-top (zo-parse (open-input-file (string-append "compiled/" moduleName "_rkt.zo"))))

;; (struct compilation-top zo (max-let-depth prefix code)
;; (struct prefix zo (num-lifts toplevels stxs)

;; toplevels : #f | global-bucket | module-variable

(define code (compilation-top-code comp-top))

;; (struct mod form (name
;;  	 	     srcname
;;  	 	     self-modidx
;;  	 	     prefix
;;  	 	     provides
;;  	 	     requires
;;  	 	     body
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

;(mod-name code)
;(mod-srcname code)
;(mod-self-modidx code)
;(mod-prefix code)
;(mod-provides code) ; each phase maps to two lists: exported variables, exported syntax
;(mod-requires code) ; each phase maps to a list of imported module paths
;(mod-body code) ; run-time (phase 0) code -> list of form?
;(mod-syntax-bodies code)

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
(define topmodule moduleName)

(define body1 (hash* 'language (list "#%kernel")
                     'module-name (symbol->string (mod-srcname (car preConfig)))
                     'body-forms (list
                                  (hash* 'require (list (list runtimeConfig)))
                                  (hash* 'operator (hash* 'source-module (list runtimeConfig)
                                                          'source-name "configure" ;;; <-- ????
                                                          )
                                         'operands (list (hash 'quote #f))))))

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
                                                   (cons sym localref-stack))))))))

(define (handle-if if-form toplevels localref-stack)
  (let ((test (branch-test if-form))
        (then (branch-then if-form))
        (else (branch-else if-form)))
    (hash*
     'test (to-ast-single test toplevels localref-stack)
     ;'test (to-ast-single test toplevels (cons 'dirty-hack-slot localref-stack))
     ;'then (to-ast-single then toplevels (cons 'branch-test-slot_then localref-stack))
     ;'else (to-ast-single else toplevels (cons 'branch-test-slot_else localref-stack)))))
     'then (to-ast-single then toplevels localref-stack)
     'else (to-ast-single else toplevels localref-stack))))

     
(define (handle-number racket-num)
  (hash* 'quote (hash* 'number (hash* 'integer (number->string racket-num)))))

(define (handle-string racket-str)
  (hash* 'quote (hash* 'string racket-str)))

(define (handle-symbol racket-sym)
  (hash* 'quote (hash* 'toplevel (symbol->string racket-sym))))

(define (get-primval-name id)
  (cond
    ((= id 248) "sub1")
    ((= id 249) "+")
    ((= id 251) "*")
    ((= id 261) "<=")
    ((= id 257) "modulo")
    (else (error 'get-primval-name (format "dont know yet : ~a" id)))))

(define (handle-primval operation toplevels)
  (let* ((id (primval-id operation))
         (operator-name (get-primval-name id)))
    (hash* 'source-name operator-name)))

(define (handle-application app-form toplevels localref-stack)
  (let* ((rator (application-rator app-form))
         (rands (application-rands app-form))
         (newlocalstack (append (map (lambda (x) 'empty-slot) (range (length rands)))
                                localref-stack))
         ;; because we know that the application will push empty slots when running, so it will push the local references further
         ;; we kinda simulate it here to make the localref pos indices point to the right identifier
         )
    (hash* 'operator (to-ast-single rator toplevels localref-stack)
           'operands (map (λ (rand) (to-ast-single rand toplevels newlocalstack)) rands))))

(define (handle-lambda lam-form toplevels localref-stack)
  (let* ((source (hash* '%p (path->string (vector-ref (lam-name lam-form) 1))))
         (position 321) ;; don't know what exactly are these two
         (span 123) 
         ;; module seems to be the same for every lambda form,
         ;; pointing to a private module about chaperones/impersonators
         (module (hash* '%mpi (hash* '%p (string-append collectsDir "/racket/private/kw.rkt"))))

         (num-args (lam-num-params lam-form))
         (symbols-for-formals (map (lambda (x) (gensym)) (range num-args)))
         (body (to-ast-single (lam-body lam-form) toplevels (append symbols-for-formals localref-stack)))
         (lamBda (map (lambda (sym) (hash* 'lexical sym)) symbols-for-formals))
         )
  (hash* 'source source
         'position position
         'span span
         'module module
         'lambda lamBda
         'body body)))

(define (handle-inline-variant iv-form toplevels localref-stack)
  (let ((direct (inline-variant-direct iv-form))
        (inline (inline-variant-inline iv-form)))
    ;; don't know what to do with the inline yet
    (to-ast-single direct toplevels localref-stack)))

(define (handle-closure closure-form toplevels localref-stack)
  (to-ast-single (closure-code closure-form) toplevels localref-stack))

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
         [lambda-form (hash* 'source (hash* '%p (string-append pycketDir "custom_" topmodule ".rkt")) ;; toplevel application
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
  (let ((pos (localref-pos lref-form)))
    (hash* 'lexical (list-ref localref-stack pos))))

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
  (let* ((newstack (cons 'let-one-hack-slot ;; <-- this is not good
                               localref-stack))
         (rhs (to-ast-single (let-one-rhs letform) toplevels newstack))
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
    ((branch? body-form) "branch")
    ((apply-values? body-form) "apply-values")
    ((localref? body-form) "localref")
    ((lam? body-form) "lam")
    ((inline-variant? body-form) "inline-variant")
    ((closure? body-form) "closure")
    (else "Unknown: ")))

(define (handle-toplevel form toplevels localref-stack)
  (to-ast-single (list-ref toplevels (toplevel-pos form)) toplevels localref-stack))

;; stack : (listof symbol?/prefix?)
(define (to-ast-single body-form toplevels localref-stack)
  (begin
    (display (body-name body-form))
    (display "- ")
    (if (localref? body-form)
        (display (number->string (localref-pos body-form)))
        (display ""))
    (display " - LocalRefStack : ")
    (display localref-stack)
    (newline)
    (cond
      ((number? body-form)
       (handle-number body-form))
      ((string? body-form)
       (handle-string body-form))
      ((symbol? body-form)
       (handle-symbol body-form))
      ;; toplevel
      ((toplevel? body-form)
      (handle-toplevel body-form toplevels localref-stack))
      
      ;; let-one (struct let-one expr (rhs body type unused?)
      ((let-one? body-form)
       (handle-let-one body-form toplevels localref-stack))
      
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
       (handle-lambda body-form toplevel localref-stack))
      ;; inline-variant (direct | inline)
      ((inline-variant? body-form)
       (handle-inline-variant body-form toplevels localref-stack))
      ;; closure (procedure constant)
      ((closure? body-form)
       (handle-closure body-form toplevels localref-stack))
      (else "not supported yet"))))

(define (to-ast body-forms toplevels)
  (map (lambda (form) (to-ast-single form toplevels '())) body-forms))


(define final-json-hash (compile-json global-config
                                      lang
                                      (string-append "custom_" moduleName)
                                      body1
                                      (to-ast (mod-body code) toplevels)))
#|
(define out (open-output-file (string-append "custom_" topmodule ".rkt.json")
                              #:exists 'replace))

(write-json final-json-hash out)
(newline out)
(flush-output out)

|#



         
         