#lang racket

(require racket/linklet compiler/zo-parse json)

(require (only-in '#%linklet compiled-position->primitive))

(require "zo-expand.rkt")

(provide handle-linkl)

;; for manual usage
(define module-name "map-bytes")

#|--- UTILS ---|#

(define (hash-set* h . kvs)
  (let loop ([kvs kvs] [h h])
    (if (null? kvs)
        h
        (let* ([k (car kvs)]
               [v (cadr kvs)]
               [h (if v (hash-set h k v) h)])
          (loop (cddr kvs) h)))))

(define (hash* . kvs) (apply hash-set* (hash) kvs))

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

(define l '(linklet 
  ;((f x)) ; imports
  ()
  () ; exports
  (display 5)
  (newline)))

(define l2 '(linklet
             ()
             (export f)
             (define-values (f) (lambda () (printf "Hello")))
             (display 5)
             (newline)
             ))

(define compiled (compile-linklet l))

(define comp-dir "compiled/")

#;(define zo-file (format "~a~a_rkt.zo" comp-dir module-name))

(define zo-file "map-bytes")

#;(define top (zo-parse (open-input-file zo-file)))
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

|#

#;(define main-table (linkl-bundle-table top))

#;main-table

;; -------> LINKL-DIRECTORY
#|
 --- A linklet directory normally maps '() to the main 
 linklet bundle for a module or a single top-level form;
 --- For a linklet directory that corresponds to a sequence of top-level forms,
 there is no “main” linklet bundle, and symbol forms of integers are used to order the linkets.
 (For a module with submodules, the linklet directory maps submodule paths (as lists of symbols)
 to linklet bundles for the corresponding submodules.

|#

#;(define main-table (linkl-directory-table top)) ;; hash?
;main-table
#;(define main-linklet (hash-ref (linkl-bundle-table
                                (hash-ref main-table '())) 0))
#;(define main-linklet (hash-ref (linkl-bundle-table top) 0))

#;(define toplevels (append
                   '(initial-reserved-slot)
                   (flatten (linkl-importss main-linklet))
                   (linkl-exports main-linklet)
                   (linkl-internals main-linklet)
                   (linkl-lifts main-linklet)))


#;(define main-bundle (linkl-bundle-table main-module)) ;; hash?

#;(define main (hash-ref main-bundle 0))


#| --- LINKL ---

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

(define (handle-linkl linklet localref-stack)
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
         [toplevels (cons 'initial-reserved-slot
                          (append
                           (flatten importss)
                           exports
                           internals
                           lifts))])
    (begin
      (set-globals! false
                    module-name "rkt" "/home/caner/research/pycket-stuff/linklets/" #f)
      (set-toplevels! toplevels toplevels '())
      (displayln (format "IMPORTS : ~a" importss))
      (displayln (format "EXPORTS : ~a" exports))
      (hash* 'linklet
             (hash*
              'exports (to-ast exports true importss)
              'body (to-ast body true importss))))))


;; for manual use

#;(define out (open-output-file (string-append module-name ".linklet") #:exists 'replace))

#;(define final-json-hash
  (handle-linkl main-linklet '()))

#;(begin
  (write-json final-json-hash out)
  (newline out)
  (flush-output out))















































