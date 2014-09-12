#lang racket/base
(require racket/contract (for-syntax racket/base))

(provide run-church)

(define-syntax-rule (run-church the-mode)
  (...
   (begin
     ;; Adjust the last id in the following form to select the mode,
     ;; picking one of the following:
     ;;  plain wrap chaperone chaperone/a impersonate contract
     (define-syntax-rule (define-begin-current-mode)
       (define-begin-mode the-mode))

     (define-syntax (define-begin-mode stx)
       (syntax-case stx ()
         [(_ mode)
          (with-syntax ([mode (datum->syntax #'here (syntax-e #'mode))]
                        [begin-mode (syntax-local-introduce #'begin-mode)])
            #'(...
               (define-syntax begin-mode
                 (syntax-rules (plain wrap chaperone chaperone/a impersonate contract wrap/proxy)
                   [(_ mode e ...) (begin e ...)]
                   [(_ wrap/proxy e ...) 
                    (begin (begin-mode wrap e ...)
                           (begin-mode impersonate e ...)
                           (begin-mode chaperone e ...)
                           (begin-mode chaperone/a e ...))]
                   [(_ other e ...) (begin)]))))]))

     (define-begin-current-mode)

     ;; --------------------------------------------------

     (begin-mode
      plain
      (define-syntax-rule (define/contract f c b)
        (define f b)))

     (begin-mode 
      wrap/proxy
      (define (exact-nonnegative-integer/c n)
        (and (exact-nonnegative-integer? n)
             n))
      (define (boolean/c n)
        (and (boolean? n)
             n))
      (define any/c (lambda (x) x))
      (define any (lambda (x) x))
      (define-syntax-rule (define/contract (f x) c b)
        (define f (c (lambda (x) b)))))

     (begin-mode 
      wrap
      (define (-> a b) (lambda (f)
                         (and (procedure? f)
                              (lambda (x)
                                (b (f (a x))))))))

     (begin-mode 
      chaperone
      (define (-> a b) (lambda (f)
                         (chaperone-procedure 
                          f
                          (lambda (v)
                            (values b (a v)))))))

     (begin-mode 
      chaperone/a
      (define (-> a b) (lambda (f)
                         (if (and (eq? a any/c) (eq? b any))
                             (and (procedure? f) f)
                             (chaperone-procedure 
                              f
                              (if (eq? b any)
                                  a
                                  (lambda (v)
                                    (values b (a v)))))))))

     (begin-mode 
      impersonate
      (define (-> a b) (lambda (f)
                         (impersonate-procedure 
                          f
                          (lambda (v)
                            (values b (a v)))))))

     (begin-mode
      contract
      (define exact-nonnegative-integer/c
        exact-nonnegative-integer?)
      (define boolean/c
        boolean?))

     ;; --------------------------------------------------

     (define church/c (-> (-> any/c any) (-> any/c any)))

     (define/contract (n->f n)
       (-> exact-nonnegative-integer/c church/c)
       (cond
        [(zero? n) (λ (f) (λ (x) x))]
        [else 
         (define n-1 (n->f (- n 1)))
         (λ (f) 
            (define fn-1 (n-1 f))
            (λ (x) (f (fn-1 x))))]))

     (define/contract (f->n c)
       (-> church/c exact-nonnegative-integer/c)
       ((c (λ (x) (+ x 1))) 0))

     (define/contract (c:* n1)
       (-> church/c (-> church/c church/c))
       (λ (n2) 
          (λ (f) 
             (n1 (n2 f)))))

     (define/contract (c:zero? c)
       (-> church/c boolean/c)
       ((c (λ (x) #f)) #t))

     ;; taken from Wikipedia (but lifted out
     ;; the definition of 'X')
     (define/contract (c:sub1 n)
       (-> church/c church/c)
       (λ (f) 
          (define X (λ (g) (λ (h) (h (g f)))))
          (λ (x) 
             (((n X) 
               (λ (u) x)) 
              (λ (u) u)))))

     (define/contract (c:! n)
       (-> church/c church/c)
       (cond
        [(c:zero? n) (λ (f) f)]
        [else ((c:* n) (c:! (c:sub1 n)))]))

     #|
     (define (! n)
     (if (zero? n) 1 (* n (! (- n 1)))))

     (check-equal? (f->n (n->f 0)) 0)
     (check-equal? (f->n (n->f 3)) 3)
     (check-equal? (f->n ((c:* (n->f 2)) (n->f 3))) 6)
     (check-equal? (f->n ((c:* (n->f 3)) (n->f 2))) 6)
     (check-equal? (f->n ((c:* (n->f 4)) (n->f 3))) 12)
     (check-equal? (f->n ((c:* (n->f 3)) (n->f 4))) 12)
     (check-equal? (f->n (c:sub1 (n->f 3))) 2)
     (check-equal? (f->n (c:sub1 (n->f 0))) 0)
     (check-equal? (f->n (c:! (n->f 3))) (! 3))
     |#

     (time (void (f->n (c:! (n->f 8)))))

     #|
     (require (only-in ffi/unsafe get-ffi-obj _int))
     (list (get-ffi-obj 'makes #f _int)
     (get-ffi-obj 'apps #f _int)
     (get-ffi-obj 'struct_apps #f _int))
     |#
     )))
