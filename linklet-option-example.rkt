(module linklet-exp racket/base
  ;; #lang wouldn't work, use (module ...
  (require racket/linklet)

  (define l-0
    (compile-linklet
     (datum->correlated
      '(linklet
        ()
        (x)
        (define-values (x) 3)
        ))))

  ;; use (list) for empty list
  (define inst-0 (instantiate-linklet l-0 (list)))

  (define l-1
    (compile-linklet
     (datum->correlated
      '(linklet
        ((x))
        (sam)
        (define-values (sam) (lambda (p) x))
        #;(lambda (x y) t)))))

  (define inst-1 (instantiate-linklet l-1 (list inst-0)))

  (define l-2
    (compile-linklet
     (datum->correlated
      '(linklet
        ((x) (sam))
        ()
        #;(define-values (a) 103)
        (+ x (sam -1))
        ))))

  (define target-linkl
    (compile-linklet
     (datum->correlated
      '(linklet
        ()
        ()
        #;(define-values (k) 1000)
        #;(define-values (a) 10)))))

  (define target (instantiate-linklet target-linkl (list)))

  (instantiate-linklet l-2 (list inst-0 inst-1) target)
)
