#lang racket/base

(require racket/fasl racket/cmdline racket/pretty)

(define f (command-line #:args (file) file))

(define p (open-input-file f))
(read-bytes 2 p)
(define sz (string->number (string (read-char p))))
(read-bytes sz p)
(pretty-print (fasl->s-exp p))
