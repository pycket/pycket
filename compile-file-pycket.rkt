#lang racket/base
(require racket/path racket/list
         racket/file
         racket/cmdline)
(provide compile-file compile-lib-path)

(define compile-file
  (case-lambda
    [(src)
     (define cdir (build-path (path-only src) "pycket-compiled"))
     (make-directory* cdir)
     (compile-file src (build-path cdir (path-add-suffix (file-name-from-path src) #".zo")))]
    [(src dest)
     (compile-file src dest values)]
    [(src dest filter)
     (define in (open-input-file src))
     (dynamic-wind
      void
      (lambda ()
        (define ok? #f)
        ; This must be based on the path to dest. Renaming typically cannot be done
        ; atomically across file systems, so the temporary directory is not an option
        ; because it is often a ram disk. src (or dir below) couldn't be used because
        ; it may be on a different filesystem. Since dest must be a file path, this
        ; guarantees that the temp file is in the same directory. It would take a weird
        ; filesystem configuration to break that.
        (define temp-filename (make-temporary-file "tmp~a" #f (path-only dest)))
        (port-count-lines! in)
        (dynamic-wind
         void
         (lambda ()
           ; XXX: This seems like it should be a library function named 'relative-path-only'
           (define dir
             (let-values ([(base name dir?) (split-path src)])
               (if (eq? base 'relative)
                   (current-directory)
                   (path->complete-path base (current-directory)))))
           (define out (open-output-file temp-filename #:exists 'truncate/replace))
           (parameterize ([current-load-relative-directory dir]
                          [current-write-relative-directory dir]
                          [read-accept-reader #t])
             ; Rather than installing a continuation barrier, we detect reinvocation.
             ; The only thing that can cause reinvocation is if the filter captures the
             ; continuation and communicates it externally.
             (define count 0)
             (dynamic-wind
              (lambda ()
                (if (zero? count)
                    (set! count 1)
                    (error 'compile-file "filter function should not be re-entrant")))
              (lambda ()
                #;(printf "read-accept-lang : ~a -- read-accept-reader : ~a\n" (read-accept-lang) (read-accept-reader))
                (for ([r (in-port (lambda (in) (read-syntax src in)) in)])
                  (write (compile-syntax (filter (namespace-syntax-introduce r))) out))
                (set! ok? #t))
              (lambda ()
                (close-output-port out)))))
         (lambda ()
           (if ok?
               (rename-file-or-directory temp-filename dest #t)
               (with-handlers ([exn:fail:filesystem? void])
                 (delete-file temp-filename))))))
      (lambda () (close-input-port in)))

     (build-path dest)]))

(define racket-modules
  '("racket/private/stx"
    "racket/private/qq-and-or"
    "racket/private/gen-temp"
    "racket/private/cond"
    "racket/private/member"
    "racket/private/define-et-al"
    "racket/private/ellipses"
    "racket/private/sc"
    "racket/private/stxcase"
    "racket/private/template"
    "racket/private/stxloc"
    "racket/private/with-stx"
    "racket/private/letstx-scheme"
    "racket/private/stxcase-scheme"
    "racket/private/qqstx"
    "racket/private/norm-define"
    "racket/private/define"
    "racket/private/sort"
    "racket/private/case"
    "racket/private/logger"
    "racket/private/more-scheme"
    "racket/private/path"
    "racket/private/path-list"
    "racket/private/reading-param"
    "racket/repl"
    "racket/private/misc"
    "racket/private/kw-prop-key"
    "racket/private/name"
    "racket/private/procedure-alias"
    "racket/private/kw"
    "racket/private/stxparamkey"
    "racket/private/stxparam"
    "racket/stxparam-exptime"
    "racket/private/generic-methods"
    "racket/private/struct-info"
    "racket/private/define-struct"
    "racket/require-transform"
    "racket/provide-transform"
    "racket/private/reqprov"
    "racket/private/modbeg"
    "racket/private/submodule"
    "racket/private/reverse"
    "racket/private/for"
    "racket/private/map"
    "racket/private/kernstruct"
    "racket/private/top-int"
    "racket/private/pre-base"
    "racket/runtime-config"
    "racket/private/hash"
    "racket/private/list"
    "racket/private/string"
    "racket/private/kw-file"
    "racket/private/namespace"
    "racket/private/struct"
    "racket/private/base"
    "racket/base"
    "racket/private/generic-interfaces"
    "racket/private/kw-syntax-binding"
    "racket/private/executable-path"
    "racket/private/old-path"
    "racket/private/immediate-default"
    "racket/private/collect"
    "racket/private/cert"
    "racket/private/list-predicates"
    "racket/list"
    "racket/private/arity"
    "racket/private/norm-arity"
    "racket/promise"
    "racket/private/promise"
    "racket/private/config"
    "syntax/readerr"
    "syntax/module-reader"
    "syntax/wrap-modbeg"
    "setup/cross-system"
    "setup/dirs"
    "setup/private/dirs"
    "compiler/private/mach-o"
    "compiler/private/winutf16"
    "planet/private/define-config"
    "racket/cmdline"
    "setup/path-relativize"
    "pkg/path"
    "racket/private/vector-wraps"
    "racket/unsafe/ops"
    "racket/flonum"
    "racket/fixnum"
    "syntax/private/boundmap"
    "syntax/srcloc"
    "syntax/location"
    "racket/path"
    ;; "racket/file"
    ))




;; TODO list

;; racket/contract stuff

;; blame.rkt
;; rand.rkt
;; guts.rkt
;; prop.rkt
;; arrow-common.rkt
;; arity-checking.rkt
;; arr-util.rkt
;; opt.rkt
;; opt-guts.rkt
;; generate.rkt
;; exists.rkt
;; arrow-val-first.rkt
;; arrow-higher-order.rkt
;; orc.rkt
;; generate-base.rkt

(define (split s)
  (define l (regexp-split "/" s))
  (cons (last l) (drop-right l 1)))

(define (compile-lib-path p)
  (let* ([p-list (split p)]
         [mod-name (format "~a.rkt" (car p-list))]
         [dirs (cdr p-list)]
         [p (apply collection-file-path mod-name dirs)]
         [compiled-file-name (format "~a_rkt.zo" (car p-list))]
         [zo-path (build-path (path-only p) "pycket-compiled" compiled-file-name)])
    (if (or force-recompile (not (file-exists? zo-path)))
        (begin
          (printf "PYCKET COMPILE FILE -- compiling : ~a\n" p)
          (compile-file p))
        (printf "PYCKET COMPILE FILE -- PASS -- already exists : ~a\n" p))))


(define (compile-path p)
  (let* ([path-to-file.rkt (string->path p)]
         [p (path-only path-to-file.rkt)])
    (printf "PYCKET COMPILE FILE -- path-to-file : ~a -- p : ~a\n" path-to-file.rkt p)
    (if (not p)
        (compile-file (build-path (current-directory) path-to-file.rkt))
        (compile-file (build-path path-to-file.rkt)))))

(define (clean-file p)
  (let* ([p-list (split p)]
         [mod-name (format "~a.rkt" (car p-list))]
         [dirs (cdr p-list)]
         [p (apply collection-file-path (cons mod-name dirs))])
    (let* ((d (path-only p))
           (zo-name (format "~a_rkt.zo" (car p-list)))
           (zo-path (build-path d "pycket-compiled" zo-name)))
      (printf "REMOVING : ~a\n" zo-path)
      (when (file-exists? zo-path)
        (delete-file zo-path)))))

;; fake-parameterize
(define old-ns (current-namespace))

(define batch #f)
(define clean #f)
(define force-recompile #f)
(define lib-path? #f)
(command-line
 #:once-each
 [("-b" "--batch") "compile the Racket modules statically listed here"
                   (set! batch #t)]
 [("--clean") "remove all the generated pycket .zo files for racket modules"
              (set! clean #t)]
 [("-f" "--force") "force re-generation of existing zo files (disabled by default)"
                   (set! force-recompile #t)]
 [("-l") "interpret paths as library paths"
         (set! lib-path? #t)]
 #:args paths
 ;; do this with mutation because parameterization doesn't currently work
 (current-namespace (make-base-namespace))
 ;; to do multiple
 (when batch
   (for ([p (in-list racket-modules)])
     (compile-lib-path p)))
 
 ;; to compile individual paths
 (when (not (null? paths))
   (for ([p (in-list paths)])
     (if lib-path?
         (compile-lib-path p)
         (compile-path p))))
 
 (when clean
   (for ([p (in-list racket-modules)])
     (clean-file p)))
 (printf "DONE.\n"))

(current-namespace old-ns)
