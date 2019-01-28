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
(define kernel-prim-id? (make-pred 'source-name))

;; Just some meta info
(define app-let-count 0)
(define if-anorm-count 0)
(define let-rhs-count 0)
(define begin-let-count 0)
(define app-rand-count 0)
(define merge-let-count 0)

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

  (printf "~a app-lets normalized\n" app-let-count)
  (printf "~a app-rands normalized\n" app-rand-count)
  (printf "~a let-rhss normalized\n" let-rhs-count)
  (printf "~a merge-lets normalized\n" merge-let-count)
  (printf "~a if-anorms normalized\n" if-anorm-count)
  (printf "~a begin-lets normalized\n" begin-let-count)

  (hash* 'linklet
         (hash* 'config config
                'exports exports
                'body normalized-body)))

#| MERGE-LET

(let ([v1 e1]) (let ([v2 e2]) e3) rest-body) => (let ([v1 e1] [v2 e2]) e3 rest-body)

|#

(define (extract-bindings rhs)
  (foldr append null (map car rhs)))

(define (free-vars rhs) ; vars is a set
  (cond
    [(lexical-id? rhs) (list (hash-ref rhs 'lexical))]
    [(list? rhs) (foldr append null (map free-vars rhs))]
    [(hash? rhs) (foldr append null
                        (for/list ([(key value) (in-hash rhs)])
                          (free-vars value)))]
    [else '()]))

(define (check-merge-let let-form)
  (let ([out-body (hash-ref let-form 'let-body)]
        [out-rhs (hash-ref let-form 'let-bindings)])
    (and (let? (car out-body))
         (let* ([inner-rhs (hash-ref (car out-body) 'let-bindings)]
                [outer-bindings (extract-bindings out-rhs)]
                [inner-frees (free-vars inner-rhs)])
           (not (ormap (lambda (b) (member b inner-frees)) outer-bindings))))))

(define (apply-merge-let let-form)
  (set! merge-let-count (add1 merge-let-count))
  (let* ([out-rhs (hash-ref let-form 'let-bindings)]
         [out-body (hash-ref let-form 'let-body)]
         [inner-let (car out-body)]
         [inner-let-rhs (hash-ref inner-let 'let-bindings)]
         [inner-let-body (hash-ref inner-let 'let-body)])
    (normalize
     (hash* 'let-bindings (append out-rhs inner-let-rhs)
            'let-body (append inner-let-body (cdr out-body))))))

#| APP-LET

((let ((func (lambda (x y z) ...))) func) 1 2 3)

            ||
            ||  move the app inside the let body
            vv

(let ((func (lambda (x y z) ...))) (func 1 2 3))

|#

(define (app-let? term)
  (and (or (let? term) (letrec? term))
       (let*
           ([body-sym (if (let? term) 'let-body 'letrec-body)]
            [body (hash-ref term body-sym)])
         (and (= 1 (length body))
              (lexical-id? (car body))))))

(define (apply-app-let app-form)
  (let* ([rator (hash-ref app-form 'operator)]
         [rands (hash-ref app-form 'operands)]
         [rhs-sym (if (let? rator) 'let-bindings 'letrec-bindings)]
         [body-sym (if (let? rator) 'let-body 'letrec-body)]
         [rhs (hash-ref rator rhs-sym)]
         ; we know the body is just a lexical ID
         [body (car (hash-ref rator body-sym))])
    (set! app-let-count (add1 app-let-count))
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

(let ([a-sym (let ([b-sym b-rhs] ...) body-b ...)]) body-a) ==> (let ([b-sym b-rhs]....)
                                                          (let ([a-sym body-b]) body-a))

More general:

(let ([c-sym c-body]
      [a-sym (let ([b-sym b-rhs] ...) body-b ...)]
      [d-sym d-body] ...) body-a ...)
===>
(let ([c-sym c-body][b-sym b-rhs] ...)
  (let ([a-sym (begin body-b ...)]
        [d-sym d-body] ...)
    body-a ...))

|#

(define (check-let-rhs let-form)
  (let ([bindings (hash-ref let-form 'let-bindings)])
    (ormap (lambda (b) (let? (cadr b))) bindings)))

(define (split-the-rhss bindings before)
  (cond
    [(null? bindings) (error 'split-the-rhss "something's wrong")]
    [(let? (cadr (car bindings)))
     (values (reverse before) (car bindings) (cdr bindings))]
    [else
     (split-the-rhss (cdr bindings) (cons (car bindings) before))]))

(define (apply-let-rhs let-form)
  (let ([bindings (hash-ref let-form 'let-bindings)])
    ;; before-rhs : list of [c-sym c-body] before the let-rhs
    ;; let-rhs : [a-sym (let ([b-sym b-rhs] ...) body-b ...)]
    ;; after-rhs : list of [d-sym d-body] after the let-rhs
    (let-values ([(before-rhs a-let-rhs after-rhs)
                  (split-the-rhss bindings null)])
      (let* ([a-sym (car a-let-rhs)]
             [b-let (cadr a-let-rhs)] ; (let ([b-sym b-rhs] ...) body-b ...)
             [rhs-bindings (hash-ref b-let 'let-bindings)] ; ([b-sym b-rhs] ...)
             [bindings-b (car rhs-bindings)] ; [b-sym b-rhs]
             [body-a (hash-ref let-form 'let-body)]
             [body-b* (hash-ref b-let 'let-body)]
             [body-b (if (= (length body-b*) 1)
                         (car body-b*)
                         (cons (hash* 'source-name "begin") body-b*))])
        (set! let-rhs-count (add1 let-rhs-count))
        (normalize
         (hash* 'let-bindings (append before-rhs rhs-bindings)
                'let-body (list (hash* 'let-bindings (cons (list a-sym body-b) after-rhs)
                                       'let-body body-a))))))))

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

#;(define (apply-begin-single begin-form)
  (begin (set! begin-single-count (add1 begin-single-count))
         (normalize (cadr begin-form))))


#| APP-RAND

(rator rand1 rand2 (....) rand3 ...) ===>
(let ([AppRand0 (....)]) (rator rand1 rand2 AppRand0 rand3))

(rator rand1 rand2 (let ([sym rhs]) body) rand3 ...) ===>
(let ([sym rhs])
  (let ([AppRand0 body])
    (rator rand1 rand2 AppRand0 rand3 ...)))

(rator rand1 (let ([sym0 rhs0]) body0) (let ([sym rhs]) body) rand3 ...) ===>
(let ([sym0 rhs0])
  (let ([AppRand0 body0])
    (rator rand1 AppRand0 (let ([sym rhs]) body) rand3 ...))) ==>


-- reminder : this is a one-step normalization
|#

(define (one-of-these? name)
  (member name '("hash-set" "hash-set!" "eq?" "bytes->path" "string->bytes/locale" "regexp-replace*")))

(define (check-app-rand app-form)
  (let* ([rator (hash-ref app-form 'operator)]
         [rands (hash-ref app-form 'operands)])
    (and (list? rands)
         #;(>= (length rands) 1)
         (or (ormap app? rands) (ormap let? rands))
         (kernel-prim-id? rator)
         #;(printf "rator id : ~a\n" (hash-ref rator 'source-name #f))
         (one-of-these? (hash-ref rator 'source-name #f)))))

(define gensym-counter 0)

(define (apply-app-rand app-form)
  (let* ([rator (hash-ref app-form 'operator)]
         [rands (hash-ref app-form 'operands)])
    (set! app-rand-count (add1 app-rand-count))
    (define found false)
    (define let-rhss false)
    (define wrapper-rhss false)
    (define new-rands
      (for/list ([r (in-list rands)])
        (cond
          [found r]
          [(app? r) (let ((new-sym-str (format "AppRand~a" gensym-counter)))
                      (begin
                        (set! gensym-counter (add1 gensym-counter))
                        (set! found true)
                        (set! let-rhss (list (list new-sym-str) r))
                        (hash* 'lexical new-sym-str)))]
          [(let? r) (let ((new-sym-str (format "AppRand~a" gensym-counter))
                          (wrapping-let-rhss (hash-ref r 'let-bindings))
                          (wrapping-let-body (hash-ref r 'let-body)))
                      (when (> (length wrapping-let-body) 1)
                        (error "rands have let with more than one body expression : ~a" app-form))
                      (begin
                        (set! gensym-counter (add1 gensym-counter))
                        (set! found true)
                        (set! wrapper-rhss wrapping-let-rhss)
                        (set! let-rhss (list (list new-sym-str) (car wrapping-let-body)))
                        (hash* 'lexical new-sym-str)))]
          [else r])))
    (normalize
     (if wrapper-rhss
         ;; there's a wrapping let
         (hash* 'let-bindings (normalize wrapper-rhss)
                'let-body (list (hash* 'let-bindings (list (normalize let-rhss))
                                       'let-body (list (hash* 'operator rator
                                                              'operands new-rands)))))
         (hash* 'let-bindings (list (normalize let-rhss))
                'let-body (list (hash* 'operator rator
                                       'operands new-rands)))))))

(define (normalize body)
  (cond
    [(or (number? body) (boolean? body) (string? body)) body]

    [(and (begin? body) (check-begin-let body)) (apply-begin-let body)]

    [(and (let? body) (check-let-rhs body)) (apply-let-rhs body)]
    [(and (let? body) (check-merge-let body)) (apply-merge-let body)]

    [(and (if? body) (check-if-anorm body)) (apply-if-anorm body)]
    [(and (app? body) (app-let? (hash-ref body 'operator))) (apply-app-let body)]
    [(and (app? body) (check-app-rand body)) (apply-app-rand body)]

    [(hash? body) (for/hash ([(key value) (in-hash body)])
                    #;(printf "getting inside key : ~a\n" key)
                    (values key (normalize value)))]

    [(list? body) (map normalize body)]
    [else (begin (printf "didn't go in there : ~a\n" body) body)]))
