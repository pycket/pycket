#lang racket
(define-syntax time
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr1 expr ...)
       (syntax/loc
           stx
         (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
           (printf "0:RESULT-cpu:ms: ~a.0\n0:RESULT-total:ms: ~a.0\n0:RESULT-gc:ms: ~a.0\n"
                   cpu user gc)
           (apply values v)))])))
(letrec
    ([E    'E]
     [head car]
     [tail cdr]
     [racket-reverse (lambda (l)
                       (letrec ((aux (lambda (list acc)
                                       (if (null? list)
                                           acc
                                           (aux (tail list) (cons (head list) acc))))))
                         (aux l '())))]
     [make-list (lambda (n)
                  (letrec ((aux (lambda (m acc)
                                  (if (= 0 m)
                                      acc
                                      (aux (- m 1 )(cons E acc))))))
                    (aux n '())))]
     [listnum (lambda (l)
                (let*
                    ([pairish (pair? l)]
                     [numberish (if pairish (string->number (car l)) pairish)])
                  (if numberish numberish 20000000)))]
     [num (listnum (vector->list (current-command-line-arguments)))]
     [l (make-list num)]
     )
  (time (racket-reverse l)))
