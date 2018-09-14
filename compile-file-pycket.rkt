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
                          [current-write-relative-directory dir])
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
  (list (collection-file-path "stx.rkt" "racket" "private")
        (collection-file-path "qq-and-or.rkt" "racket" "private")
        (collection-file-path "gen-temp.rkt" "racket" "private")
        (collection-file-path "cond.rkt" "racket" "private")
        (collection-file-path "member.rkt" "racket" "private")
        (collection-file-path "define-et-al.rkt" "racket" "private")
        (collection-file-path "ellipses.rkt" "racket" "private")
        (collection-file-path "sc.rkt" "racket" "private")
        (collection-file-path "stxcase.rkt" "racket" "private")
        (collection-file-path "template.rkt" "racket" "private")
        (collection-file-path "stxloc.rkt" "racket" "private")
        (collection-file-path "with-stx.rkt" "racket" "private")
        (collection-file-path "letstx-scheme.rkt" "racket" "private")
        (collection-file-path "stxcase-scheme.rkt" "racket" "private")
        (collection-file-path "qqstx.rkt" "racket" "private")
        (collection-file-path "norm-define.rkt" "racket" "private")
        (collection-file-path "define.rkt" "racket" "private")
        (collection-file-path "sort.rkt" "racket" "private")
        (collection-file-path "case.rkt" "racket" "private")
        (collection-file-path "logger.rkt" "racket" "private")
        (collection-file-path "more-scheme.rkt" "racket" "private")
        (collection-file-path "path.rkt" "racket" "private")
        (collection-file-path "path-list.rkt" "racket" "private")
        (collection-file-path "reading-params.rkt" "racket" "private")
        (collection-file-path "repl.rkt" "racket")
        (collection-file-path "misc.rkt" "racket" "private")
        (collection-file-path "kw-prop-key.rkt" "racket" "private")
        (collection-file-path "name.rkt" "racket" "private")
        (collection-file-path "procedure-alias.rkt" "racket" "private")
        (collection-file-path "kw.rkt" "racket" "private")
        (collection-file-path "stxparamkey.rkt" "racket" "private")
        (collection-file-path "stxparam.rkt" "racket" "private")
        (collection-file-path "stxparam-exptime.rkt" "racket")
        (collection-file-path "generic-methods.rkt" "racket" "private")
        (collection-file-path "struct-info.rkt" "racket" "private")
        (collection-file-path "define-struct.rkt" "racket" "private")
        (collection-file-path "require-transform.rkt" "racket")
        (collection-file-path "provide-transform.rkt" "racket")
        (collection-file-path "reqprov.rkt" "racket" "private")
        (collection-file-path "modbeg.rkt" "racket" "private")
        (collection-file-path "reverse.rkt" "racket" "private")
        (collection-file-path "for.rkt" "racket" "private")
        (collection-file-path "map.rkt" "racket" "private")
        (collection-file-path "kernstruct.rkt" "racket" "private")
        (collection-file-path "top-int.rkt" "racket" "private")
        #;(collection-file-path "pre-base.rkt" "racket" "private")
        #;(collection-file-path "runtime-config.rkt" "racket")
        #;(collection-file-path "hash.rkt" "racket")
        #;(collection-file-path "list.rkt" "racket")
        #;(collection-file-path "string.rkt" "racket")
        #;(collection-file-path "kw-file.rkt" "racket" "private")
        #;(collection-file-path "namespace.rkt" "racket" "private")
        #;(collection-file-path "struct.rkt" "racket" "private")
        #;(collection-file-path "base.rkt" "racket" "private")
        #;(collection-file-path "generic-interfaces.rkt" "racket" "private")
        #;(collection-file-path "kw-syntax-local.rkt" "racket" "private")))

(define batch #f)

(command-line
 #:once-each
 [("-b" "--batch") "compile the Racket modules statically listed here" (set! batch #t)]
 #:args paths
 (parameterize ([current-namespace (make-base-namespace)])
   (when batch
     (for ([p (in-list racket-modules)])
       (printf "PYCKET COMPILE FILE -- compiling : ~a\n" p)
       (compile-file p)))
   (when (not (null? paths))
     (for ([p (in-list paths)])
       (let* ([path-to-file.rkt (string->path p)]
              [p (path-only path-to-file.rkt)])
         (printf "PYCKET COMPILE FILE -- path-to-file : ~a -- p : ~a\n" path-to-file.rkt p)
         (if (not p)
             (compile-file (build-path (current-directory) path-to-file.rkt))
             (compile-file (build-path path-to-file.rkt))))))))
