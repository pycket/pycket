#lang racket/base
(require racket/shared racket/match (for-syntax racket/base))
(define cur-section '())(define errs '())

(define-syntax defvar
  (syntax-rules ()
    [(_ name val)
     (define name val)
     #;
     (namespace-variable-value 'name #f
       (lambda () (namespace-set-variable-value! 'name val)))]))

(defvar building-flat-tests? #f)
(defvar in-drscheme?         #f)

;; used when quiet testing (through "quiet.rktl") to really show something
(defvar real-output-port #f)
(defvar real-error-port #f)

(define Section-prefix
  (namespace-variable-value 'Section-prefix #f (lambda () "")))

(define (Section . args)
  (let ([p (or real-output-port (current-output-port))])
    (fprintf p "~aSection~s\n" Section-prefix args)
    (flush-output p))
  (set! cur-section args)
  #t)

(define (record-error e)
  (set! errs (cons (list cur-section e) errs)))

(print-struct #t)

(define number-of-tests 0)
(define number-of-error-tests 0)
(define number-of-exn-tests 0)


(define test
  (let ()
    (define (test* expect fun args kws kvs)
      (define form
        `(,fun ,@args ,@(apply append (if kws (map list kws kvs) '()))))
      (set! number-of-tests (add1 number-of-tests))
      (printf "~s ==> " form)
      (flush-output)
      (let ([res (if (procedure? fun)
                   (if kws (keyword-apply fun kws kvs args) (apply fun args))
                   (car args))])
        (printf "~s\n" res)
        (let ([ok? (equal? expect res)])
          (unless ok?
            (record-error (list res expect form))
            (printf "  BUT EXPECTED ~s\n" expect))
          ok?)))
    (define (test/kw kws kvs expect fun . args) (test* expect fun args kws kvs))
    (define (test    expect fun         . args) (test* expect fun args #f #f))
    (make-keyword-procedure test/kw test)))

(define (nonneg-exact? x)
  (and (exact? x)
       (integer? x)
       (x . >= . 0)))

(define (pos-exact? x)
  (and (exact? x)
       (integer? x)
       (positive? x)))

(define exn-table
  (list (cons exn? (cons exn-message string?))
	(cons exn? (cons exn-continuation-marks continuation-mark-set?))
	(cons exn:fail:contract:variable? (cons exn:fail:contract:variable-id symbol?))
	(cons exn:fail:syntax? (cons exn:fail:syntax-exprs (lambda (x) (and (list? x) (andmap syntax? x)))))

	(cons exn:fail:read? (cons exn:fail:read-srclocs (lambda (x) (and (list? x) (andmap srcloc? x)))))))

(define exn:application:mismatch? exn:fail:contract?)
(define exn:application:type? exn:fail:contract?)
(define exn:application:arity? exn:fail:contract:arity?)

(define mz-test-syntax-errors-allowed? #t)

(define thunk-error-test
  (case-lambda
   [(th expr) (thunk-error-test th expr exn:application:type?)]
   [(th expr exn-type?)
    (set! expr (syntax->datum expr))
    (set! number-of-error-tests (add1 number-of-error-tests))
    (printf "~s  =e=> " expr)
    (flush-output)
    (call/ec (lambda (escape)
	       (let* ([old-esc-handler (error-escape-handler)]
		      [orig-err-port (current-error-port)]
		      [test-exn-handler
		       (lambda (e)
			 (when (and exn-type? (not (exn-type? e)))
			       (printf " WRONG EXN TYPE: ~s " e)
			       (record-error (list e 'exn-type expr)))
			 (when (and (exn:fail:syntax? e)
				    (not mz-test-syntax-errors-allowed?))
			       (printf " LATE SYNTAX EXN: ~s " e)
			       (record-error (list e 'exn-late expr)))

			 (for-each
			  (lambda (row)
			    (let ([pred? (car row)])
			      (when (pred? e)
				    (set! number-of-exn-tests
					  (add1 number-of-exn-tests))
				    (let ([sel (cadr row)]
					  [pred? (cddr row)])
				      (unless (pred? (sel e))
					      (printf " WRONG EXN ELEM ~s: ~s " sel e)
					      (record-error (list e (cons 'exn-elem sel) expr)))))))
			  exn-table)
                         
                         (printf "~s~n" (if (exn? e) (exn-message e) e))
                         #; ;g;
                         ((error-display-handler)
                          (if (exn? e)
                              (exn-message e)
                              (format "misc. exn: ~s" e))
                          e)

                         (escape #t))])
		 (dynamic-wind
		  (lambda ()
		    (current-error-port (current-output-port)))
		  (lambda ()
                    (call-with-continuation-prompt
                     (lambda ()
                       (call-with-exception-handler
                        test-exn-handler
                        (lambda ()
                          (let ([v (call-with-values th list)])
                            (write (cons 'values v))
                            (display " BUT EXPECTED ERROR")
                            (record-error (list v 'Error expr))
                            (newline)
                            #f))))))
		  (lambda ()
		    (current-error-port orig-err-port)
		    (error-escape-handler old-esc-handler))))))]))

(defvar error-test
  (case-lambda
    [(expr) (error-test expr exn:application:type?)]
    [(expr exn-type?) (thunk-error-test (lambda () (eval expr)) expr exn-type?)]))

(require (only-in racket [lambda err:mz:lambda])) ; so err/rt-test works with beginner.rktl
(define-syntax err/rt-test
  (lambda (stx)
    (syntax-case stx ()
      [(_ e exn?)
       (syntax
	(thunk-error-test (err:mz:lambda () e) (quote-syntax e) exn?))]
      [(_ e)
       (syntax
	(err/rt-test e exn:application:type?))])))

(define no-extra-if-tests? #f)

(define (syntax-test expr [rx #f])
  (error-test expr exn:fail:syntax?)
  (unless no-extra-if-tests?
    (error-test (datum->syntax expr `(if #f ,expr (void)) expr)
                (lambda (x)
                  (and (exn:fail:syntax? x)
                       (or (not rx)
                           (regexp-match? rx (exn-message x))))))))

(define arity-test
  (case-lambda
   [(f min max except)
    (letrec ([aok?
	      (lambda (a)
		(cond
		 [(integer? a) (= a min max)]
		 [(arity-at-least? a) (and (negative? max)
					   (= (arity-at-least-value a) min))]
		 [(and (list? a) (andmap integer? a))
		  (and (= min (car a)) (= max
					  (let loop ([l a])
					    (if (null? (cdr l))
						(car l)
						(loop (cdr l))))))]
		 [(list? a)
		  ;; Just check that all are consistent for now.
		  ;; This should be improved.
		  (andmap
		   (lambda (a)
		     (if (number? a)
			 (<= min a (if (negative? max) a max))
			 (>= (arity-at-least-value a) min)))
		   a)]
		 [else #f]))]
	     [make-ok?
	      (lambda (v)
		(lambda (e)
		  (exn:application:arity? e)))]
	     [do-test
	      (lambda (f args check?)
		(set! number-of-error-tests (add1 number-of-error-tests))
		(printf "(apply ~s '~s)  =e=> " f args)
		(let/ec done
		  (let ([v (with-handlers ([void
					    (lambda (exn)
					      (if (check? exn)
						  (printf " ~a\n" (if (exn? exn)
                                                                      (exn-message exn)
                                                                      (format "uncaught ~x" exn)))
						  (let ([ok-type? (exn:application:arity? exn)])
						    (printf " WRONG EXN ~a: ~s\n"
							    (if ok-type?
								"FIELD"
								"TYPE")
							    exn)
						    (record-error (list exn
									(if ok-type?
									    'exn-field
									    'exn-type)
									(cons f args)))))
					      (done (void)))])
			     (apply f args))])
		    (printf "~s\n BUT EXPECTED ERROR\n" v)
		    (record-error (list v 'Error (cons f args))))))])
      (test #t aok? (procedure-arity f))
      (let loop ([n 0][l '()])
	(unless (>= n min)
	  (unless (memq n except)
	    (do-test f l (make-ok? n)))
	  (loop (add1 n) (cons 1 l))))
      (let loop ([n min])
	(unless (memq n except)
	  (test #t procedure-arity-includes? f n))
	(unless (>= n max)
	  (loop (add1 n))))
      (if (>= max 0)
	  (do-test f (let loop ([n 0][l '(1)])
		       (if (= n max)
			   l
			   (loop (add1 n) (cons 1 l))))
		   (make-ok? (add1 max)))
	  (test #t procedure-arity-includes? f (arithmetic-shift 1 100))))]
   [(f min max) (arity-test f min max null)]))

(define (test-values l thunk)
  (test l call-with-values thunk list))

(define (report-errs . final?)
  (let* ([final? (and (pair? final?) (car final?))]
         [ok?    (null? errs)])
    (parameterize ([current-output-port
                    (cond [(not ok?) (or real-error-port (current-error-port))]
                          [final? (or real-output-port (current-output-port))]
                          [else (current-output-port)])])
      (printf "\n~aPerformed ~a expression tests (~a ~a, ~a ~a)\n"
              Section-prefix
              (+ number-of-tests number-of-error-tests)
              number-of-tests "value expressions"
              number-of-error-tests "exn expressions")
      (printf "~aand ~a exception field tests.\n\n"
              Section-prefix
              number-of-exn-tests)
      (if ok?
        (printf "~aPassed all tests.\n" Section-prefix)
        (begin (printf "~aErrors were:\n~a(Section (got expected (call)))\n"
                       Section-prefix Section-prefix)
               (for-each (lambda (l) (printf "~a~s\n" Section-prefix l))
                         (reverse errs))
               (when final? (exit 1))))
      (flush-output)
      (when final? (exit (if ok? 0 1)))
      (printf "(Other messages report successful tests of~a.)\n"
              " error-handling behavior")
      (flush-output))))

(define type? exn:application:type?)
(define arity? exn:application:arity?)
(define syntaxe? exn:fail:syntax?)

(define non-z void)

(define (find-depth go)
  ; Find depth that triggers a stack overflow (assuming no other
  ; threads are running and overflowing)
  (let ([v0 (make-vector 6)]
	[v1 (make-vector 6)])
    (let find-loop ([d 100])
      (vector-set-performance-stats! v0)
      (go d)
      (vector-set-performance-stats! v1)
      (if (> (vector-ref v1 5)
	     (vector-ref v0 5))
	  d
	  (find-loop (* 2 d))))))

(Section 'struct)

; previour tests in struct-test0.rkt

;; ------------------------------------------------------------
;; Property accessor errors

(let-values ([(prop:p p? p-ref) (make-struct-type-property 'prop1 #f '() #t)])
  (test 42 p-ref 5 42)
  (test 17 p-ref 5 (lambda () (* 1 17)))
  (err/rt-test (p-ref 5) exn:fail:contract?))

;; ------------------------------------------------------------
;; Property type supers

(require (only-in mzscheme [prop:procedure mz:prop:procedure])) ; more primitive - no keywords

(let ([try
       (lambda (base prop:procedure)
         (err/rt-test (make-struct-type '? base 1 0 #f (list (cons prop:procedure 0) 
                                                             (cons prop:procedure 1))
                                        #f #f '(0)))
         ;; Ok to re-set to same value:
         (test #t list? (call-with-values
                            (lambda () (make-struct-type '? base 1 0 #f (list (cons prop:procedure 0) 
                                                                              (cons prop:procedure 0))
                                                         #f #f '(0)))
                          list))
         (err/rt-test (make-struct-type '? base 1 0 #f (list (cons prop:procedure 0)) #f 1))
         (test #t list? (call-with-values
                            (lambda () (make-struct-type '? base 1 0 #f (list (cons prop:procedure 0)) #f 0))
                          list))

         (let-values ([(prop:s s? s-get)
                       (make-struct-type-property 's #f (list (cons prop:procedure (lambda (v) (add1 v)))))])
           (define-struct a (x y) #:super base #:property prop:s 0)
           (test 0 s-get struct:a)
           (test #t procedure-struct-type? struct:a)
           (test 5 (make-a 1 (lambda (v) (+ 2 v))) 3)

           (err/rt-test (make-struct-type-property 't #f 10))
           (err/rt-test (make-struct-type-property 't #f (list (cons prop:s 10))))

           ;; Allow multiple inheritances of a property at this stage, because we can't in general
           ;;  tell whether the results will be eq?
           (test #t list?
                 (call-with-values
                     (lambda ()
                       (make-struct-type-property 't #f (list (cons prop:s void) (cons prop:s values))))
                   list))
           (test #t list?
                 (call-with-values
                     (lambda ()
                       (make-struct-type-property 't #f (list (cons prop:s void) (cons prop:procedure values))))
                   list))

           (let-values ([(prop:t t? t-get)
                         (make-struct-type-property 't #f (list (cons prop:s (lambda (v) (add1 v)))))]
                        [(prop:u u? u-get)
                         (make-struct-type-property 'u)])
             (define-struct b (x y z) #:super base #:property prop:u '? #:property prop:t 0)
             (test 8 (make-b 1 2 (lambda (v) (- v 4))) 12)
             (test 0 t-get struct:b)
             (test 1 s-get struct:b)
             (test '? u-get struct:b)

             (let-values ([(prop:w w? w-get)
                           (make-struct-type-property 'w (lambda (v s) (sub1 v)) (list (cons prop:u values)))]
                          [(prop:z z? z-get)
                           (make-struct-type-property 'z #f (list (cons prop:u values)))])
               (define-struct c () #:super base #:property prop:w 10)
               (test 9 w-get struct:c)
               (test 9 u-get struct:c) ; i.e., after guard

               (err/rt-test (make-struct-type '? base 0 0 #f (list (cons prop:w 3) (cons prop:z 3))))
               (err/rt-test (make-struct-type '? base 3 0 #f (list (cons prop:s 0) (cons prop:t 0)) #f #f '(0 1 2)))
               (err/rt-test (make-struct-type '? base 3 0 #f (list (cons prop:s 0) (cons prop:procedure 0)) #f #f '(0 1 2)))
               ))))])

  (try #f mz:prop:procedure)
  (try #f prop:procedure)
  (let ([props (map (lambda (n)
                      (let-values ([(prop ? -get) (make-struct-type-property n)])
                        prop))
                    '(a b c d e f g h j i))])
    (let-values ([(s: make-s s? s-ref s-set!)
                  (make-struct-type 'base #f 0 0 #f (map (lambda (p) (cons p 5)) props))])
      (try s: mz:prop:procedure)
      (try s: prop:procedure))))

(let ()
  (define-struct a (x y) #:property prop:procedure (lambda (s v #:kw [kw #f]) (list (a-x s) v kw)))
  (test '(1 3 #f) (make-a 1 2) 3)
  (test '(1 3 8) 'kw ((make-a 1 2) 3 #:kw 8))
  (test-values '(() (#:kw)) (lambda () (procedure-keywords (make-a 1 2)))))

;; ------------------------------------------------------------
;; Check that struct definition sequences work:

(let ()
  (define-struct a (x y))
  (define-struct (b a) (z))
  (define-struct (c b) (w))

  (test 1 a-x (make-a 1 2))
  (test 10 a-x (make-b 10 20 30))
  (test 100 a-x (make-c 100 200 300 400)))

;; ------------------------------------------------------------
;; Prefab

(let ([v1 #s(v one)]
      [v2 #s(v one two)]
      [v2-prime #s((v 2) one two)]
      [vw3 #s((v w 2) one two three)]
      [vw3-prime #s((v 1 w 2) one two three)])
  (test #f equal? v1 v2)
  (test #t equal? v2 v2-prime)
  (test #t equal? vw3 vw3-prime)
  (let ()
    (define-struct v (a) #:prefab)
    (test #t v? v1)
    (test #f v? v2)
    (test #f v? vw3)
    (test 'one v-a v1))
  (let ()
    (define-struct v (a b) #:prefab)
    (test #f v? v1)
    (test #t v? v2)
    (test #f v? vw3)
    (test 'one v-a v2)
    (test 'two v-b v2))
  (let ()
    (define-struct w (a b) #:prefab)
    (define-struct (v w) (c) #:prefab)
    (test #f v? v1)
    (test #f v? v2)
    (test #t v? vw3)
    (test #t w? vw3)
    (test 'one w-a vw3)
    (test 'two w-b vw3)
    (test 'three v-c vw3)))

(err/rt-test (make-struct-type 'bad struct:date 2 0 #f null 'prefab))

;; ------------------------------------------------------------
;; Misc. built-in structures

(test #f srcloc? 10)
(test #t srcloc? (make-srcloc 'ok 1 2 3 4))
(test 'ok srcloc-source (make-srcloc 'ok 1 2 3 4))
(test 1 srcloc-line (make-srcloc 'ok 1 2 3 4))
(test #f srcloc-line (make-srcloc 'ok #f 2 3 4))
(test 2 srcloc-column (make-srcloc 'ok 1 2 3 4))
(test 0 srcloc-column (make-srcloc 'ok 1 0 3 4))
(test #f srcloc-column (make-srcloc 'ok 1 #f 3 4))
(test 3 srcloc-position (make-srcloc 'ok 1 2 3 4))
(test #f srcloc-position (make-srcloc 'ok 1 2 #f 4))
(test 4 srcloc-span (make-srcloc 'ok 1 2 3 4))
(test 0 srcloc-span (make-srcloc 'ok 1 2 3 0))
(test #f srcloc-span (make-srcloc 'ok 1 2 3 #f))

(err/rt-test (make-srcloc 'ok 'no 2 3 4))
(err/rt-test (make-srcloc 'ok 0 2 3 4))
(err/rt-test (make-srcloc 'ok 1 'no 3 4))
(err/rt-test (make-srcloc 'ok 1 -1 3 4))
(err/rt-test (make-srcloc 'ok 1 2 'no 4))
(err/rt-test (make-srcloc 'ok 1 2 0 4))
(err/rt-test (make-srcloc 'ok 1 2 3 'no))
(err/rt-test (make-srcloc 'ok 1 2 3 -1))

; ;; ------------------------------------------------------------
; ;; Custom write

; (define (custom-write-check normal?)
;   (define (tuple-print tuple port write?)
;     (when write? (write-string "<" port))
;     (let ([l (tuple-ref tuple 0)])
;       (unless (null? l)
; 	((if write? write display) (car l) port)
; 	(for-each (lambda (e)
; 		    (write-string ", " port)
; 		    (if normal?
; 			;; Test normal recusrive write:
; 			((if write? write display) e port)
; 			;; Test writing recursively to a string:
; 			(let ([p (open-output-string)])
; 			  (port-write-handler p (port-write-handler port))
; 			  (port-display-handler p (port-display-handler port))
; 			  (port-print-handler p (port-print-handler port))
; 			  ((if write? write display) e p)
; 			  (write-string (get-output-string p) port))))
; 		  (cdr l))))
;     (when write? (write-string ">" port)))

;   (define-values (s:tuple make-tuple tuple? tuple-ref tuple-set!)
;     (make-struct-type 'tuple #f 1 0 #f
; 		      (list (cons prop:custom-write tuple-print))))
  
;   (define (with-output-string thunk)
;     (let ([p (open-output-string)])
;       (parameterize ([current-output-port p])
; 	(thunk))
;       (get-output-string p)))

;   (test "<1, 2, \"a\">" with-output-string 
; 	(lambda ()
; 	  (tuple-print (make-tuple '(1 2 "a")) (current-output-port) #t)))

;   (test "1, 2, a" with-output-string 
; 	(lambda ()
; 	  (display (make-tuple '(1 2 "a")))))
;   (test "#0=<#&#0#, 2, \"a\">" with-output-string 
; 	(lambda ()
; 	  (let ([t (make-tuple (list (box 1) 2 "a"))])
; 	    (set-box! (car (tuple-ref t 0)) t)
; 	    (write t))))
;   (test "ack: here: <10, 2, \"a\">" with-output-string 
; 	(lambda ()
; 	  (with-handlers ([exn:fail? (lambda (exn)
; 				       (printf "~a" (exn-message exn)))])
; 	    (error 'ack "here: ~e" (make-tuple (list 10 2 "a"))))))
  
;   (test "ack: here: <100000..." with-output-string 
; 	(lambda ()
; 	  (parameterize ([error-print-width 10])  
; 	    (with-handlers ([exn:fail? (lambda (exn)
; 					 (printf "~a" (exn-message exn)))])
; 	      (error 'ack "here: ~e" (make-tuple (list 10000000000000000000000000000000000 2 "a"))))))))

; (custom-write-check #t)
; (custom-write-check #f)

;; ----------------------------------------

(let ()
  (define-struct t1 (a b) #:transparent)
  (define-struct t2 (c d) #:transparent #:mutable)
  ; (define-struct o (x y z)
  ;   #:property prop:equal+hash (list
  ;                               (lambda (a b equal?)
  ;                                 (and (equal? (o-x a) (o-x b))
  ;                                      (equal? (o-z a) (o-z b))))
  ;                               (lambda (a hash)
  ;                                 (+ (hash (o-x a)) (* 9 (hash (o-z a)))))
  ;                               (lambda (a hash)
  ;                                 (+ (hash (o-x a)) (hash (o-z a)))))
  ;   #:mutable)

  (test #f equal? (make-t1 0 1) (make-t2 0 1))
  (test #t equal? (make-t1 0 1) (make-t1 0 1))
  (test #t equal? (make-t2 0 1) (make-t2 0 1))
  ; (test #t equal? 
  ;       (shared ([t (make-t2 0 t)]) t) 
  ;       (shared ([t (make-t2 0 t)]) t))
  ; (test #f equal?
  ;       (shared ([t (make-t2 0 t)]) t) 
  ;       (shared ([t (make-t2 1 t)]) t))
  ; (test #t = 
  ;       (equal-hash-code (make-t1 0 1))
  ;       (equal-hash-code (make-t1 0 1)))
  ; (test #t =
  ;       (equal-hash-code (shared ([t (make-t2 0 t)]) t))
  ;       (equal-hash-code (shared ([t (make-t2 0 t)]) t)))
  ; (test #t = 
  ;       (equal-secondary-hash-code (make-t1 0 1))
  ;       (equal-secondary-hash-code (make-t1 0 1)))
  ; (test #t =
  ;       (equal-secondary-hash-code (shared ([t (make-t2 0 t)]) t))
  ;       (equal-secondary-hash-code (shared ([t (make-t2 0 t)]) t)))
  
  ; (test #t equal? (make-o 1 2 3) (make-o 1 20 3))
  ; (test #f equal? (make-o 10 2 3) (make-o 1 2 3))
  ; (test #f equal? (make-o 1 2 3) (make-o 1 2 30))
  ; (test #t equal? 
  ;       (shared ([t (make-o t 0 t)]) t) 
  ;       (shared ([t (make-o t 0 t)]) t))
  ; (test #t equal?
  ;       (shared ([t (make-o t 0 t)]) t) 
  ;       (shared ([t (make-o t 1 t)]) t))
  ; (test #f equal?
  ;       (shared ([t (make-o t 0 0)]) t) 
  ;       (shared ([t (make-o t 0 1)]) t))

  ; (test #t = 
  ;       (equal-hash-code (make-o 1 2 3))
  ;       (equal-hash-code (make-o 1 20 3)))
  ; (test #t =
  ;       (equal-hash-code (shared ([t (make-o t 0 t)]) t))
  ;       (equal-hash-code (shared ([t (make-o t 0 t)]) t)))
  ; (test #t =
  ;       (equal-hash-code (shared ([t (make-o t 1 t)]) t))
  ;       (equal-hash-code (shared ([t (make-o t 1 t)]) t)))
  ; (test #t =
  ;       (equal-secondary-hash-code (shared ([t (make-o t 0 t)]) t))
  ;       (equal-secondary-hash-code (shared ([t (make-o t 0 t)]) t)))
  ; (test #t =
  ;       (equal-secondary-hash-code (shared ([t (make-o t 1 t)]) t))
  ;       (equal-secondary-hash-code (shared ([t (make-o t 1 t)]) t)))

  (void))

;; ----------------------------------------

(let ()
  (define-struct foo (a [b #:mutable]) #:transparent)
  (define-struct (bar foo) (f g)
    #:transparent
    #:property
    prop:procedure
    (struct-field-index f))
  (test '(1) (make-bar 1 2 list 4) 1)
  (test '(foo 2 0 (0)) call-with-values 
        (lambda () (struct-type-info struct:foo))
        (lambda (name cnt auto-cnt acc mut imm super skipped?)
          (list name cnt auto-cnt imm)))
  (test '(bar 2 0 (0 1)) call-with-values 
        (lambda () (struct-type-info struct:bar))
        (lambda (name cnt auto-cnt acc mut imm super skipped?)
          (list name cnt auto-cnt imm))))

(let ()
  (define-struct foo (a [b #:mutable] [z #:auto]) #:transparent)
  (define-struct (bar foo) (f g)
    #:transparent
    #:property
    prop:procedure
    (struct-field-index f))
  (test '#&1 (make-bar 1 2 box 4) 1)
  (test '(foo 2 1 (0)) call-with-values 
        (lambda () (struct-type-info struct:foo))
        (lambda (name cnt auto-cnt acc mut imm super skipped?)
          (list name cnt auto-cnt imm)))
  (test '(bar 2 0 (0 1)) call-with-values 
        (lambda () (struct-type-info struct:bar))
        (lambda (name cnt auto-cnt acc mut imm super skipped?)
          (list name cnt auto-cnt imm))))

(let ()
  (define-struct foo (a [b #:mutable] [z #:auto]) #:transparent)
  (define (try v)
    (define-struct (bar foo) ([f #:mutable] g [q #:auto])
      #:property
      prop:procedure
      v)
    10)
  (err/rt-test (try 0))
  (err/rt-test (try 2))
  (err/rt-test (try -1))
  (err/rt-test (try 'x))
  (test 10 try 1))

;; ----------------------------------------

(require (for-syntax scheme/struct-info))

(let ()
  (define-struct a (x y))
  (define-syntax foo (make-struct-info
                      (lambda ()
                        (list #'struct:a #'make-a #'a?
                              (list #'a-y #'a-x)
                              (list #f #f)
                              #f))))
  (define-syntax foo2 (let ()
                        (define-struct si (pred)
                          #:property 
                          prop:struct-info
                          (lambda (v)
                            (list #'struct:a #'make-a (si-pred v)
                                  (list #'a-y #'a-x)
                                  (list #f #f)
                                  #f)))
                        (make-si #'a?)))
  (test (list 1 2) 'match (match (make-a 1 2)
                            [(struct foo (x y)) (list x y)]))
  (test (list 1 2) 'match (match (make-a 1 2)
                            [(struct foo2 (x y)) (list x y)])))
                              

;; ----------------------------------------

(let ()
  (struct s (a b))
  (struct t s (c))
  (struct u t (d))
  (test 11
        'struct-copy1
        (t-c (struct-copy t (t 1 2 3) [c 11])))
  (test 11
        'struct-copy2
        (s-a (struct-copy t (t 1 2 3) [a #:parent s 11])))
  (test 11
        'struct-copy2
        (s-a (struct-copy u (u 1 2 3 4) [a #:parent s 11])))
  
  (syntax-test #'(struct-copy t (t 1 2 3) [a #:parent p 11])))

(let ()
  (struct s (a b) #:transparent)
  (struct t s (c) #:transparent)
  (struct u t (d) #:transparent)
  (test (t 1 2 11)
        'struct-copy1
        (struct-copy t (t 1 2 3) [c 11]))
  (test (t 11 2 3)
        'struct-copy2
        (struct-copy t (t 1 2 3) [a #:parent s 11]))
  (test (s 11 2)
        'struct-copy2
        (struct-copy s (t 1 2 3) [a 11]))
  (test (u 11 2 3 4)
        'struct-copy2
        (struct-copy u (u 1 2 3 4) [a #:parent s 11]))
  
  (syntax-test #'(struct-copy t (t 1 2 3) [a #:parent p 11])))

(let ()
  (struct s (a b) #:prefab)
  (struct t s (c) #:prefab)
  (struct u t (d) #:prefab)
  (test (t 1 2 11)
        'struct-copy1
        (struct-copy t (t 1 2 3) [c 11]))
  (test (t 11 2 3)
        'struct-copy2
        (struct-copy t (t 1 2 3) [a #:parent s 11]))
  (test (s 11 2)
        'struct-copy2
        (struct-copy s (t 1 2 3) [a 11]))
  (test (u 11 2 3 4)
        'struct-copy2
        (struct-copy u (u 1 2 3 4) [a #:parent s 11]))
  
  (syntax-test #'(struct-copy t (t 1 2 3) [a #:parent p 11])))

(test #t prefab-key? 'apple)
(test #f prefab-key? '#(apple))
(test #t prefab-key? '(apple 4))

;; ----------------------------------------
;; We can make a bogus mutator, but we can't apply it:

(let ()
  ;; Test based on code from dmarshall:
  (define-values (struct:thing make-thing thing? thing-ref thing-set!)
    (make-struct-type
     'thing #f 1 0  
     #f               ; auto val
     (list)           ; property list
     #f               ; inspector
     #f               ; proc-spec
     (list 0)))       ; immutables
  
  (define thing.id  (make-struct-field-accessor thing-ref 0))
  (define thing.id! (make-struct-field-mutator thing-set! 0))

  (test #t struct-mutator-procedure? thing.id!)
  (err/rt-test (thing.id!  'new-val))
  
  (let ([f #f])
    ;; defeat inlining to ensure that thunk is JITted:
    (set! f (lambda () (thing.id! (make-thing 1) 'new-val)))
    (err/rt-test (f))))

;; ----------------------------------------
;; Check interaction of `struct-type-info` and GC:

(struct-type-info struct:arity-at-least)
(collect-garbage)
(let-values ([(name init-cnt auto-cnt acc mut immut super skipped?)
              (struct-type-info struct:arity-at-least)])
  (test #t procedure? acc)
  (test #t procedure? mut))

;; ----------------------------------------

(report-errs)
