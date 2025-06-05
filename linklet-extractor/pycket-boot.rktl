;; This is a hand-rolled utility code for Pycket that's wrapped
;; into a linklet to make it convenient to load these functions
;; at racket_entry at boot
(linklet
  ()
  (
   (2/namespace-require-plus-nt pycket:no-thread:namespace-require-plus)
   (2/configure-runtime-nt pycket:no-thread:configure-runtime)
   (2/load-repl-nt pycket:no-thread:load-repl)
   (2/namespace-require-plus pycket:namespace-require-plus)
   (2/configure-runtime pycket:configure-runtime)
   (2/load-repl pycket:load-repl)
   (2/just-kernel pycket:just-kernel)
   )
  (define-values
    (2/configure-runtime)
    (lambda (m)
      (let-values (((subm) (list 'submod "." 'configure-runtime)))
        (let-values (((config_m) (module-path-index-join subm m)))
          (if (module-declared? config_m #t)
            (dynamic-require config_m #f)
            (void))))))
  (define-values
    (2/namespace-require-plus)
    (lambda (spec config?)
      (call-in-main-thread
        (lambda ()
          (let-values (((subm) (list 'submod "." 'main)))
            (let-values (((m) (module-path-index-join spec #f)))
              (if config?
                (2/configure-runtime m)
                (void))
              (namespace-require m)
              (let-values (((main) (module-path-index-join subm m)))
                (if (module-declared? main #t)
                  (dynamic-require main #f)
                  (void)))))))))
  (define-values
    (2/load-repl)
    (lambda ()
      (call-in-main-thread
        (lambda ()
          ((dynamic-require 'racket/repl 'read-eval-print-loop))))))
  (define-values
    (2/just-kernel)
    (lambda ()
      (call-in-main-thread
        (lambda ()
          (namespace-require ''#%kernel)))))
  ;; no thread versions below
  (define-values
    (2/configure-runtime-nt)
    (lambda (m)
      (let-values (((subm) (list 'submod "." 'configure-runtime)))
        (let-values (((config_m) (module-path-index-join subm m)))
          (if (module-declared? config_m #t)
            (dynamic-require config_m #f)
            (void))))))
  (define-values
    (2/namespace-require-plus-nt)
    (lambda (spec)
      (let-values (((subm) (list 'submod "." 'main)))
        (let-values (((m) (module-path-index-join spec #f)))
          (2/configure-runtime m)
          (let-values (((main) (module-path-index-join subm m)))
            (if (module-declared? main #t)
              (dynamic-require main #f)
              (void)))))))
  (define-values
    (2/load-repl-nt)
    (lambda ()
      ((dynamic-require 'racket/repl 'read-eval-print-loop))))
  )
