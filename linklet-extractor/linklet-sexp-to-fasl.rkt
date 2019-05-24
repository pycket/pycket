#lang racket/base

(module+ main

  (require racket/cmdline racket/fasl)

  (define out #f)
  (define ver #f)
  (define sexp-file-path #f)

  (command-line
   #:once-each
   [("--version") "put the version in front of the sexp (e.g. (7.3 . <sexp>))"
                  (set! ver #t)]
   #:once-any
   [("-o" "--output") file "write output to <file>"
                      (set! out (open-output-file file #:exists 'replace))]

   #:args (file.sexp)

   (define contents
     (let ([file-contents (read (open-input-file file.sexp))])
       (if ver
           (cons (version) file-contents)
           file-contents)))

   (s-exp->fasl contents out)))
