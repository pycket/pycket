#lang racket

(provide (all-defined-out))

;; MAJOR TODO : unit tests for these normalizations

(define (hash-set* h . kvs)
  (let loop ([kvs kvs] [h h])
    (if (null? kvs)
        h
        (let* ([k (car kvs)]
               [v (cadr kvs)]
               [h (if v (hash-set h k v) h)])
          (loop (cddr kvs) h)))))

(define (hash* . kvs) (apply hash-set* (hash) kvs))

(define ((make-pred . keys) term-ht)
  (and (hash? term-ht) (equal? (list->set (hash-keys term-ht)) (list->set keys))))

(define app? (make-pred 'operator 'operands))
(define let? (make-pred 'let-bindings 'let-body))
(define letrec? (make-pred 'letrec-bindings 'letrec-body))
(define lexical-id? (make-pred 'lexical))
(define if? (make-pred 'test 'then 'else))
(define begin? (lambda (term-ls)
                 (and (list? term-ls)
                      (not (empty? term-ls))
                      (hash? (car term-ls))
                      (equal? "begin" (hash-ref (car term-ls) 'source-name #f)))))

;; Just some meta info
(define let-app-count 0)
(define if-anorm-count 0)
(define begin-single-count 0)
(define let-rhs-count 0)
(define begin-let-count 0)

(define (normalize-linklet h)
  (printf "\nNormalizer:\n")
  (define linklet (hash-ref h 'linklet))
  (define config (hash-ref linklet 'config))
  (define exports (hash-ref linklet 'exports))
  (define body (hash-ref linklet 'body))

  ; re-application of normalize is not ideal but solves cascading
  ; effects for now (i.e. a normalization can allow/block another in a
  ; single run)
  (define normalized-body (normalize (normalize body)))

  (printf "~a let-apps normalized\n" let-app-count)
  (printf "~a if-anorms normalized\n" if-anorm-count)
  (printf "~a begin-singles normalized\n" begin-single-count)
  (printf "~a let-rhss normalized\n" let-rhs-count)
  (printf "~a begin-lets normalized\n" begin-let-count)
  (hash* 'linklet
         (hash* 'config config
                'exports exports
                'body normalized-body)))

#| LET-APP

((let ((func (lambda (x y z) ...))) func) 1 2 3)

            ||
            ||  move the app inside the let body
            vv

(let ((func (lambda (x y z) ...))) (func 1 2 3)) 

|#

(define (extract-bindings bindingss is-let?)
  (caar bindingss))

(define (let-app? term)
  (and (or (let? term) (letrec? term))
       (let*
           ([body-sym (if (let? term) 'let-body 'letrec-body)]
            [body (hash-ref term body-sym)])
         (and (= 1 (length body))
              (lexical-id? (car body))))))

(define (apply-let-app app-form)
  (let* ([rator (hash-ref app-form 'operator)]
         [rands (hash-ref app-form 'operands)]
         [rhs-sym (if (let? rator) 'let-bindings 'letrec-bindings)]
         [body-sym (if (let? rator) 'let-body 'letrec-body)]
         [rhs (hash-ref rator rhs-sym)]
         ; we know the body is just a lexical ID
         [body (car (hash-ref rator body-sym))])
    (set! let-app-count (add1 let-app-count))
    ; reconstructing let/letrec
    (normalize
     (hash* rhs-sym rhs
            body-sym (list (hash* 'operator body
                                  'operands rands))))))

#| IF-ANORM

(if (expr expr ...) 1 2)  ===> (let ((if_gen (expr expr ...))) (if if_gen 1 2)) 

|#

(define (check-if-anorm if-form)
  (not (lexical-id? (hash-ref if-form 'test))))

(define (apply-if-anorm if-form)
  (let ([tst (hash-ref if-form 'test)]
        [thn (hash-ref if-form 'then)]
        [els (hash-ref if-form 'else)])
    (let ([norm-tst (normalize tst)]
          [norm-thn (normalize thn)]
          [norm-els (normalize els)])
      ; create a let
      (let ([new-id-for-if (symbol->string (gensym 'if))])
        (set! if-anorm-count (add1 if-anorm-count))

        (hash* 'let-bindings (list (list (list new-id-for-if)
                                         norm-tst))
               'let-body (list (hash* 'test (hash* 'lexical new-id-for-if)
                                      'then norm-thn
                                      'else norm-els)))))))

#| LET-RHS : move the let in the rhs of a wrapping let, outside

(let ([a-sym (let ([b-sym b-rhs]) body-b)]) body-a) ==> (let ([b-sym b-rhs])
                                                          (let ([a-sym body-b]) body-a))

DISABLED FOR NOW - changes the meaning in the current form, there's an
implicit assumption I've yet to find |#

(define (check-let-rhs let-form)
  (let ([bindings (hash-ref let-form 'let-bindings)])
    (and (= (length bindings) 1)
         (let? (cadr (car bindings))))))

(define (apply-let-rhs let-form)
  (let* ([bindings-a (car (hash-ref let-form 'let-bindings))]
         [a-sym (car bindings-a)]
         [b-let (cadr bindings-a)]
         [bindings-b (car (hash-ref b-let 'let-bindings))]
         [b-sym (car bindings-b)]
         [b-rhs (cadr bindings-b)]

         [body-a (hash-ref let-form 'let-body)]
         [body-b (hash-ref b-let 'let-body)])
    (set! let-rhs-count (add1 let-rhs-count))
    (normalize
     (hash* 'let-bindings (list (list b-sym
                                      b-rhs))
            'let-body (list (hash* 'let-bindings (list (list a-sym
                                                             (car body-b)))
                                   'let-body body-a))))))

#| BEGIN-LET

Convert (begin (let ([...]) letbody) rest ...) =>
(let ([...]) letbody ... rest ...) 

|#

(define (check-begin-let begin-form)
  (let? (cadr begin-form)))

(define (apply-begin-let begin-form)
  (set! begin-let-count (add1 begin-let-count))
  (let* ([let-1 (cadr begin-form)]
         [let-1-bindings (hash-ref let-1 'let-bindings)]
         [let-1-body (hash-ref let-1 'let-body)]

         [rest-begin (cddr begin-form)])
    (normalize
     (hash* 'let-bindings let-1-bindings
            'let-body (append let-1-body rest-begin)))))

#| BEGIN-SINGLE : get rid of begins with single expressions

(begin expr) => expr 

|#

(define (apply-begin-single begin-form)
  (begin (set! begin-single-count (add1 begin-single-count))
         (normalize (cadr begin-form))))



(define (normalize body)
  (cond
    [(or (number? body) (boolean? body) (string? body)) body]

    [(and (begin? body) (= 1 (length body))) (apply-begin-single body)]
    [(and (begin? body) (check-begin-let body)) (apply-begin-let body)]

    ;; [(and (let? body) (check-let-rhs body))
    ;;  (apply-let-rhs body)]
    [(and (if? body) (check-if-anorm body)) (apply-if-anorm body)]
    [(and (app? body) (let-app? (hash-ref body 'operator))) (apply-let-app body)]

    [(hash? body) (for/hash ([(key value) (in-hash body)])
                    #;(printf "getting inside key : ~a\n" key)
                    (values key (normalize value)))]

    [(list? body) (map normalize body)]
    [else (begin (printf "didn't go in there : ~a\n" body) body)]))
