#lang racket/base

(require racket/cmdline)

(provide (all-defined-out))

(define (generate-branchy-test-file file-name gen-function input-size [outer-iteration 15][inner-iteration 1000])
  (with-output-to-file file-name
    (lambda ()
      (printf "#lang racket/base

(require \"simpler-branchy2.rkt\")

(define input '~a)

(printf \"---- LOOP BEGINS ---- result : ~~a\\n\" (function input))
(for ([i (in-range ~a)])
  (time (for ([j (in-range ~a)])
          (function input))))
(printf \"---- LOOP ENDS ---- \\n\")" (build-list input-size gen-function) outer-iteration inner-iteration))
#:exists 'replace))

(define (make-all-random input-size [outer-iteration 15][inner-iteration 1000])
  (generate-branchy-test-file (format "random38-~a-~a-~a.rkt" input-size outer-iteration inner-iteration)
                 (lambda (x) (random 38)) ;; specific to function in simpler-branchy2
                 input-size outer-iteration inner-iteration))

(define (make-all-same what? input-size [outer-iteration 15][inner-iteration 1000])
  (generate-branchy-test-file (format "all-~as-~a-~a-~a.rkt" what? input-size outer-iteration inner-iteration)
                 (lambda (x) what?)
                 input-size outer-iteration inner-iteration))

(define (make-all-zeros input-size [outer-iteration 15][inner-iteration 1000])
  (make-all-same 0 input-size outer-iteration inner-iteration))

(define (make-all-ones input-size [outer-iteration 15][inner-iteration 1000])
  (make-all-same 1 input-size outer-iteration inner-iteration))
