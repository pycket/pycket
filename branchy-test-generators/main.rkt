#lang racket/base

(require "fasl-generator.rkt"
         "branchy-generator.rkt"
         racket/cmdline)


(module+ main

  (define what? #f)
  (define what-kind? #f) ; for fasl
  (define input-size 0)
  (define outer-iteration 15)
  (define inner-iteration 2000)
  #;(define wk? 'random)

  (define usage (format
"Usage: racket main.rkt <make> N:0 [--outer:15] [--inner:2000]

        <make> : --make-same K | --make-ones | --make-zeros | --make-random | --make-fasl <kind>

        <kind> : random | integer | flonum | rational | complex | symbol | string | char | boolean | path | regexp | box \n"))

  (command-line
   #:once-any
   ["--make-same" -what? how-many? "" (set! what? -what?) (set! input-size how-many?)]
   ["--make-ones" how-many? "" (set! what? 1) (set! input-size how-many?)]
   ["--make-zeros" how-many? "" (set! what? 0) (set! input-size how-many?)]
   ["--make-random" how-many? "" (set! what? 'random) (set! input-size how-many?)]
   ["--make-fasl" wk? how-many? "" (set! what? 'fasl) (set! what-kind? (and (not (equal? wk? "random")) wk?)) (set! input-size how-many?)]
   #:once-each
   ["--outer" outer-count "" (set! outer-iteration outer-count)]
   ["--inner" inner-count "" (set! inner-iteration inner-count)]
   #:args ()

   ;; Handling/Type correcting the inputs
   (when (string? what?)
     ;; if it's a string than we got a number from cmd line
     (set! what? (string->number what?)))
   (when (string? what-kind?)
     (set! what-kind? (string->symbol what-kind?)))
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
     [(equal? what? 'fasl)
      (let ([fname (format "use-fasl-~a-~a-~a-~a.rkt"
                           input-size (or what-kind? 'random)  outer-iteration inner-iteration)])
        (printf "Generating : ~a\n" fname)
        (generate-fasl-test-file input-size fname what-kind? outer-iteration inner-iteration))]
     [else (printf usage)])))
