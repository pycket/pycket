#lang racket

(require racket/linklet compiler/zo-parse json
         #;(only-in '#%linklet compiled-position->primitive)
         #;"expand.rkt"
         "zo-expand.rkt"
         "normalizer.rkt")

(provide (all-defined-out))

;; pkgs/compiler-lib/compiler/decompile.rkt
#;(define primitive-table
  (for/hash ([i (in-naturals)]
             #:break (not (compiled-position->primitive i)))
    (define v (compiled-position->primitive i))
(values i (or (object-name v) v))))

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
               (cons l (with-handlers ([exn:fail? (λ (x) #f)])
                         (compile l))))))]
        [table (make-hash)])
    (for ([b (in-list bindings)])
      (let* ([v (and (cdr b)
                     (zo-parse
                      (open-input-bytes
                       (with-output-to-bytes
                         (lambda () (write (cdr b)))))))]
             [l (and v
                     (hash-ref (linkl-bundle-table
                                (hash-ref (linkl-directory-table v)
                                          '()))
                               0))]
             [n (match (match l
                         [(struct linkl (name importss im-shapess exports
                                              internals lifts source-names
                                              body max-let-depth _)) body]
                         [else #f])
                  [`(,(struct primval (n))) n]
                  [else #f])])
        (hash-set! table n (car b))))
    table))

(define (prim num)
  (hash-ref primitive-table num))

#|------Examples ----|#

#;(define l '(linklet
  ;((f x)) ; imports
  ()
  () ; exports
  (display 5)
  (newline)))

#;(define l2 '(linklet
             ()
             (export f)
             (define-values (f) (lambda () (printf "Hello")))
             (display 5)
             (newline)
             ))

#;(define compiled (compile-linklet l))

;; either
;; -------> LINKL-BUNDLE (if module has no submodules)
#|
 A linklet bundle maps an INTEGER to a linklet representing forms
 to evaluate at the integer-indicated phase.
 SYMBOLS are mapped to metadata, such as a module’s name as compiled or
 a linklet implementing literal syntax objects.

pre ::: listof symbol? (pre-submodules)
name ::: name
side-effects ::: listof number? [(0) display] (??????????????)
decl ::: ????????????????


;; -------> LINKL-DIRECTORY

 --- A linklet directory normally maps '() to the main 
 linklet bundle for a module or a single top-level form;
 --- For a linklet directory that corresponds to a sequence of top-level forms,
 there is no “main” linklet bundle, and symbol forms of integers are used to order the linkets.
 (For a module with submodules, the linklet directory maps submodule paths (as lists of symbols)
 to linklet bundles for the corresponding submodules.

 --- LINKL ---

name           :
importss       : (top) (listof (listof symbol?))
((symbolic-names-of-export-from-source1) (symbolic-names-of-export-from-source2) ...)

import-shapess : [parallel to importss, optimization assumptions checked by bytecode validator when linklet is initialized]
exports        : (top) (listof symbol?) [exported defined names]
internals      : (top) [defined names that are not accesible from outside] [#f can appear -- unreferenced removed]
lifts          : (top) [extension of internals, lifted procedures by the compiler]
source-names   : [maps symbols in exports, internals, and lifts to other symbols,
                  potentially not distinct, that correspond to original source names for the definition.
                  Used only for debugging.]
body           : (listof (or form? any?)) [executable content]
                 [the value of the last element may be returned when the linklet is initialized, depending on the way it's instantiated]
max-let-depth  : [max size of the stack that will be created by any body]


********* importss exports internals lifts ***** are mutually exclusive.
toplevels = (flatten-and-append importss exports internals lifts)
********* [initial slot is reserved for a variable-like reference that strongly retains a connection to an instance of its enclosing linklet]

|#

;(define (to-ast-single body-form localref-stack current-closure-refs)
;
;(define (to-ast body-forms)
;  (map (lambda (form) (to-ast-single form '() '())) body-forms))

(define toplevels 'toBeSetByHandleLinkl)

(define (handle-linkl linklet localref-stack debug module-name mod-ext rel-current-dir)
  (let* ([name (linkl-name linklet)]
         ;; (listof (listof symbol?))
         [importss (linkl-importss linklet)]
         [import-shapess (linkl-import-shapess linklet)]
         ;; (listof symbol?)
         [exports (linkl-exports linklet)]
         [internals (linkl-internals linklet)]
         [lifts (linkl-lifts linklet)]
         [source-names (linkl-source-names linklet)]
         ;; (listof (or form? any?))
         [body (linkl-body linklet)]
         ;; number
         [max-let-depth (linkl-max-let-depth linklet)]
         [toplevels_ (cons 'initial-reserved-slot
                           (append
                            (flatten importss)
                            exports
                            internals
                            lifts))])
    (begin
      (set-globals! debug module-name mod-ext rel-current-dir #f)
      (set-toplevels! toplevels_ toplevels_ '())
      (set! toplevels toplevels_)
      (displayln (format "IMPORTS : ~a" importss))
      (displayln (format "EXPORTS : ~a" exports))
      (hash* 'linklet
             (hash*
              'exports (to-ast exports true importss)
              'body (to-ast body true importss)
              'config (hash* 'version (version))
              #;(hash-set* global-config
                           'collects-dir
                           "/home/cderici/racketland/racket/racket/collects"))))))

(define byte-codes 'toBeSetByMain)
(define json-hash 'toBeSetByMain)

(define (set-out main-linklet final-json)
  (set! byte-codes main-linklet)
  (set! json-hash final-json))

(module+ main
  (require racket/cmdline json compiler/cm)

  (define debug #f)
  (define sub-dirs-str #f)
  (define out #f)
  (define module-name #f)
  (define expander #f)

  (define byte-file
    (open-input-file
     (command-line
      #:once-each
      [("-v" "--verbose" "-d" "--debug") "show what you're doing" (set! debug #t)]
      [("-e" "--expander") "generate the expander linklet json" (set! expander #t)]
      #:once-any
      [("--output") file "write output to output <file>"
                    (set! out (open-output-file file #:exists 'replace))]
      [("--stdout") "write output to standart out" (set! out (current-output-port))]

      #:args (file.zo) ; accept only bytecode
      (begin
        (when expander
          (when (not out)
            (set! out (open-output-file "expander.rktl.linklet" #:exists 'truncate/replace)))
          (set! module-name "expander"))

        ;; setting the stage
        (when (not module-name)
          (set! module-name (car (string-split file.zo "."))))
        (when (not sub-dirs-str)
          (set! sub-dirs-str (path->string (current-directory))))
        (when (not out)
          (set! out (open-output-file (build-path sub-dirs-str (format "~a.json" module-name))
                                      #:exists 'replace)))
        file.zo))))

  (define top (zo-parse byte-file))

  (unless (or (linkl-directory? top) (linkl-bundle? top))
    (error (format "what sort of compile top is this : ~a" top)))

  (when debug
    (printf " \n top is a : ~a\n" (if (linkl-directory? top) "linkl-directory" "linkl-bundle")))

  (define main-table
    (if (linkl-directory? top) (hash-ref (linkl-directory-table top) '()) (linkl-bundle-table top)))

  (when debug
    (printf "\n main-table \n\n ~a\n\n" main-table))

  (define main-linklet (if (linkl-bundle? top)
                           (hash-ref main-table 0)
                           (hash-ref (linkl-bundle-table main-table) 0)))

  (unless (linkl? main-linklet)
    (error (format "what sort of linklet is this : ~a" main-linklet)))

  (when debug
    (printf "\n main-linklet \n\n ~a\n\n" main-linklet))

  (define final-json-hash
    (handle-linkl main-linklet '() debug module-name "rkt" sub-dirs-str))

  #;(define final-json-hash
    (normalize-linklet _final-json-hash))

  (unless (jsexpr? final-json-hash)
    (error "\n something wrong with the json :\n\n ~a\n\n" final-json-hash))

  (when debug
    (printf "\n final-json-hash \n\n")
    final-json-hash)

  (when debug
    (printf "\nwriting json into : ~a ...." out))

  (set-out main-linklet final-json-hash)
  
  (write-json final-json-hash out)
  (newline out)
  (flush-output out)

  (when debug (printf "done\n"))
)
