#lang racket/base
(require racket/path
         racket/file
         racket/cmdline)
(provide compile-file)

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
                (printf "read-accept-lang : ~a -- read-accept-reader : ~a\n" (read-accept-lang) (read-accept-reader))
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
  (list (list "stx" "racket" "private")
        (list "qq-and-or" "racket" "private")
        (list "gen-temp" "racket" "private")
        (list "cond" "racket" "private")
        (list "member" "racket" "private")
        (list "define-et-al" "racket" "private")
        (list "ellipses" "racket" "private")
        (list "sc" "racket" "private")
        (list "stxcase" "racket" "private")
        (list "template" "racket" "private")
        (list "stxloc" "racket" "private")
        (list "with-stx" "racket" "private")
        (list "letstx-scheme" "racket" "private")
        (list "stxcase-scheme" "racket" "private")
        (list "qqstx" "racket" "private")
        (list "norm-define" "racket" "private")
        (list "define" "racket" "private")
        (list "sort" "racket" "private")
        (list "case" "racket" "private")
        (list "logger" "racket" "private")
        (list "more-scheme" "racket" "private")
        (list "path" "racket" "private")
        (list "path-list" "racket" "private")
        (list "reading-param" "racket" "private")
        (list "repl" "racket")
        (list "misc" "racket" "private")
        (list "kw-prop-key" "racket" "private")
        (list "name" "racket" "private")
        (list "procedure-alias" "racket" "private")
        (list "kw" "racket" "private")
        (list "stxparamkey" "racket" "private")
        (list "stxparam" "racket" "private")
        (list "stxparam-exptime" "racket")
        (list "generic-methods" "racket" "private")
        (list "struct-info" "racket" "private")
        (list "define-struct" "racket" "private")
        (list "require-transform" "racket")
        (list "provide-transform" "racket")
        (list "reqprov" "racket" "private")
        (list "modbeg" "racket" "private")
        (list "submodules" "racket" "private")
        (list "reverse" "racket" "private")
        (list "for" "racket" "private")
        (list "map" "racket" "private")
        (list "kernstruct" "racket" "private")
        (list "top-int" "racket" "private")
        (list "pre-base" "racket" "private")
        (list "runtime-config" "racket")
        (list "hash" "racket" "private")
        (list "list" "racket" "private")
        (list "string" "racket" "private")
        (list "kw-file" "racket" "private")))

#;(collection-file-path "namespace.rkt" "racket" "private")
#;(collection-file-path "struct.rkt" "racket" "private")
#;(collection-file-path "base.rkt" "racket" "private")
#;(collection-file-path "generic-interfaces.rkt" "racket" "private")
#;(collection-file-path "kw-syntax-local.rkt" "racket" "private")

;; TODO list

;; promise.rkt
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


(define batch #f)
(define clean #f)

(command-line
 #:once-each
 [("-b" "--batch") "compile the Racket modules statically listed here" (set! batch #t)]
 [("--clean") "remove all the generated pycket .zo files for racket modules" (set! clean #t)]
 #:args paths
 (parameterize ([current-namespace (make-base-namespace)])
   (when batch
     (for ([p-list (in-list racket-modules)])
       (let* ([mod-name (format "~a.rkt" (car p-list))]
              [dirs (cdr p-list)]
              [p (apply collection-file-path (cons mod-name dirs))])
         (printf "PYCKET COMPILE FILE -- compiling : ~a\n" p)
         (compile-file p))))
   (when (not (null? paths))
     (for ([p (in-list paths)])
       (let* ([path-to-file.rkt (string->path p)]
              [p (path-only path-to-file.rkt)])
         (printf "PYCKET COMPILE FILE -- path-to-file : ~a -- p : ~a\n" path-to-file.rkt p)
         (if (not p)
             (compile-file (build-path (current-directory) path-to-file.rkt))
             (compile-file (build-path path-to-file.rkt))))))

   (when clean
     (for ([p-list (in-list racket-modules)])
       (let* ([mod-name (format "~a.rkt" (car p-list))]
              [dirs (cdr p-list)]
              [p (apply collection-file-path (cons mod-name dirs))])
         (let* ((d (path-only p))
                (zo-name (format "~a_rkt.zo" (car p-list)))
                (zo-path (build-path d "pycket-compiled" zo-name)))
           (printf "REMOVING : ~a\n" zo-path)
           (when (file-exists? zo-path)
             (delete-file zo-path))))))))
