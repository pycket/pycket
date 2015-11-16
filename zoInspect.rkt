#lang racket

(require compiler/zo-parse
         setup/dirs
         ;racket/cmdline
         json)

(require "pycket/pycket-lang/zoTransform.rkt")
(require "pycket/pycket-lang/expand.rkt") ;; global-config - hash*


(define args (current-command-line-arguments))

(define modName "dotSimple")
(define DEBUG true)


(define KNOWN (list '(number? . ("number"))
                    '(string? . ("string"))
                    '(symbol? . ("symbol"))
                    '(let-one? . ("let-one" let-one-rhs let-one-body))
                    '(let-void? . ("let-void" let-void-count let-void-boxes? let-void-body))
                    '(install-value? . ("install-value" install-value-count install-value-pos install-value-boxes? install-value-rhs install-value-body))
                    '(module-variable? . ("module-variable" module-variable-sym module-variable-modidx))
                    '(primval? . ("primval"))
                    '(application? . ("application" application-rator application-rands))
                    '(def-values? . ("def-values" def-values-ids def-values-rhs))
                    '(seq? . ("seq" seq-forms))
                    '(assign? . ("assign" assign-id assign-rhs))
                    '(branch? . ("branch" branch-test branch-then branch-else))
                    '(apply-values? . ("apply-values" apply-values-proc apply-values-args-expr))
                    '(localref? . ("localref" localref-pos))
                    '(lam? . ("lam" lam-name lam-num-params lam-body))
                    '(inline-variant? . ("inline-variant" inline-variant-direct inline-variant-inline))
                    '(closure? . ("closure" closure-code))
                    '(toplevel? . ("toplevel" toplevel-pos))
                    ))

#|
;; see-if-we-have-an-unknown-form
(define (siwhauf code)
  (let* [(which-form (foldr (x y) (lambda (if y y
                                              (if ((car x) code) (cdr x) #f)))) #f KNOWN)
         (form-name (car which-form))
         (form-accessors (cdr which-form))]
    (cond
      ((or (number? code) (string? code) (symbol? code)) code)
      ((eqv? form-name "let-one") (and (siwhauf (let-one-body code)) (siwhauf (let-one-body code))))
      ((eqv? form-name "let-void") (siwhauf (let-void-body code)))
      ((eqv? form-name "install-value") (and (siwhauf (install-value-rhs code))
                                             (siwhauf (install-value-body code))))
      ((eqv? form-name "module-variable") (module-variable-sym . module-variable-modidx))
      ((eqv? form-name "primval") code)
      ((eqv? form-name "application") (and (siwhauf (application-rator code)) (andmap siwhauf (application-rands code))))
      ((eqv? form-name "def-values") (siwhauf (def-values-rhs code)))
      ((eqv? form-name "seq") (andmap siwhauf (seq-forms code)))
      ((eqv? form-name "assign") (siwhauf (assign-rhs code)))
      ((eqv? form-name "branch") (and (siwhauf (branch-test code)) (siwhauf (branch-then code)) (siwhauf (branch-else code))))
      ((eqv? form-name "apply-values") (and (siwhauf (apply-values-proc code)) (andmap siwhauf (apply-values-args-expr code))))
      ((eqv? form-name "localref
|#
(define depFile (read (open-input-file (string-append "compiled/" modName "_rkt.dep"))))
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

(define comp-top (zo-parse (open-input-file (string-append "compiled/" modName "_rkt.zo"))))

;; (struct compilation-top zo (max-let-depth prefix code)
;; (struct prefix zo (num-lifts toplevels stxs)
;; (struct	module-variable zo (modidx sym pos phase constantness)

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

(define (compile-json2 config language topmod body1 body-forms)
  (hash* 'language (list language)
         'module-name topmod
         'config config
         'body-forms (cons body1
                           body-forms)))


(define config global-config)
(define language lang)
(define topmodule modName)

(define body1 (hash* 'language (list "#%kernel")
                     'module-name (symbol->string (mod-srcname (car preConfig)))
                     'body-forms (list
                                  (hash* 'require (list (list runtimeConfig)))
                                  (hash* 'operator (hash* 'source-module (list runtimeConfig)
                                                          'source-name "configure" ;;; <-- ????
                                                          )
                                         'operands (list (hash 'quote #f))))))

(define final-json-hash (compile-json2 global-config
                                       lang
                                       (string-append "frombytecode_" modName)
                                       body1
                                       (to-ast-wrapper (mod-body code) toplevels DEBUG modName)))
#|
(define out (open-output-file (string-append "custom_" topmodule ".rkt.json")
                              #:exists 'replace))
(begin
  (display (string-append "WRITTEN: custom_" topmodule ".rkt.json\n\n"))
  (write-json final-json-hash out)
  (newline out)
  (flush-output out))
|#
