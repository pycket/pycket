#lang racket/base

(require racket/cmdline)

(provide (all-defined-out))

(define (generate-file file-name gen-function input-size [outer-iteration 15][inner-iteration 1000])
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
  (generate-file (format "random38-~a-~a-~a.rkt" input-size outer-iteration inner-iteration)
                 (lambda (x) (random 38)) ;; specific to function in simpler-branchy2
                 input-size outer-iteration inner-iteration))

(define (make-all-same what? input-size [outer-iteration 15][inner-iteration 1000])
  (generate-file (format "all-~as-~a-~a-~a.rkt" what? input-size outer-iteration inner-iteration)
                 (lambda (x) what?)
                 input-size outer-iteration inner-iteration))

(define (make-all-zeros input-size [outer-iteration 15][inner-iteration 1000])
  (make-all-same 0 input-size outer-iteration inner-iteration))

(define (make-all-ones input-size [outer-iteration 15][inner-iteration 1000])
  (make-all-same 1 input-size outer-iteration inner-iteration))

(module+ main

  (define what? #f)
  (define input-size 0)
  (define outer-iteration 15)
  (define inner-iteration 2000)

  (command-line
   #:once-any
   ["--make-same" -what? how-many? "" (set! what? -what?) (set! input-size how-many?)]
   ["--make-ones" how-many? "" (set! what? 1) (set! input-size how-many?)]
   ["--make-zeros" how-many? "" (set! what? 0) (set! input-size how-many?)]
   ["--make-random" how-many? "" (set! what? 'random) (set! input-size how-many?)]
   #:once-each
   ["--outer" outer-count "" (set! outer-iteration outer-count)]
   ["--inner" inner-count "" (set! inner-iteration inner-count)]
   #:args ()

   (when (string? what?)
     (set! what? (string->number what?)))

   (unless (number? input-size)
     (set! input-size (string->number input-size)))

   (unless (number? outer-iteration)
     (set! outer-iteration (string->number outer-iteration)))

   (unless (number? inner-iteration)
     (set! inner-iteration (string->number inner-iteration)))

   (cond
     [(number? what?)
      (begin
        (printf "Generating : all-~as-~a-~a-~a.rkt\n" 
                what? input-size outer-iteration inner-iteration)
        (make-all-same what? input-size outer-iteration inner-iteration))]
     [(equal? what? 'random)
      (begin
        (printf "Generating : random38-~a-~a-~a.rkt\n" 
                input-size outer-iteration inner-iteration)
        (make-all-random input-size outer-iteration inner-iteration))]
     [else
      (printf 
"Usage: racket branchy-generator.rkt <make> N:0 [--outer:15] [--inner:2000]

        <make> : --make-same K | --make-ones | --make-zeros | --make-random\n")])))
