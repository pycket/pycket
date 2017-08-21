#lang racket

(require compiler/zo-parse setup/dirs racket/undefined racket/path racket/extflonum
         (only-in pycket/expand hash* global-config)
         (only-in '#%linklet compiled-position->primitive))

#;(provide to-ast-wrapper to-ast-val
         primitive-table)

(provide (all-defined-out))

#;(define global-config
  (let ()
    (define sysconfig
      (for/hash ([k '(collects-dir temp-dir init-dir pref-dir home-dir
                                   pref-file init-file config-dir addon-dir
                                   exec-file run-file sys-dir doc-dir orig-dir)])
        (values k (full-path-string (find-system-path k)))))
    (hash-set* sysconfig
               'version (version)
               'bytecode-expand "false")))

(define DEBUG #t)
(define DEBUG-STACK #t)
(define pycket-dir (path->string (current-directory))) ;; MUST BE RUN UNDER PYCKET DIR
(define collects-dir (path->string (find-collects-dir)))
(define file-name 'beSetBy-main)
(define module-name 'beSetBy-main)
(define module-extension 'beSetBy-main)
(define relative-current-dir 'rel-dir-beSetBy-main)
(define TEST #f)

(define initial-toplevels 'beSetBy-main)
(define LIFTEDS '())
(define TOPLEVELS '())
(define TOPSYNTAX '())

;; FROM
;; https://github.com/racket/compiler/blob/master/compiler-lib/compiler/decompile.rkt#L14

#;(define (hash-set* h . kvs)
  (let loop ([kvs kvs] [h h])
    (if (null? kvs)
        h
        (let* ([k (car kvs)]
               [v (cadr kvs)]
               [h (if v (hash-set h k v) h)])
          (loop (cddr kvs) h)))))

#;(define (hash* . kvs) (apply hash-set* (hash) kvs))

;; pkgs/compiler-lib/compiler/decompile.rkt
(define primitive-table2
  (for/hash ([i (in-naturals)]
             #:break (not (compiled-position->primitive i)))
    (define v (compiled-position->primitive i))
    (values i (or (object-name v) v))))

;;;;;; 1291

;;;;;; 1396

;;;;;;;;;;; 1445

(define primitive-table
  ;; Figure out number-to-id mapping for kernel functions in `primitive'
  (let ([bindings
         (let ([ns (make-base-empty-namespace)])
           (parameterize ([current-namespace ns])
             (namespace-require ''#%kernel)
             (namespace-require ''#%unsafe)
             (namespace-require ''#%flfxnum)
             (namespace-require ''#%extfl)
             (namespace-require ''#%futures)
             (namespace-require ''#%foreign)
             (for/list ([l (namespace-mapped-symbols)])
               (cons l (with-handlers ([exn:fail? (λ (x) #f)])
                         (compile l))))))]
        [table (make-hash)])
    (for ([b (in-list bindings)])
      (let* ([v (and (cdr b)
                     (zo-parse
                      (open-input-bytes
                       (with-output-to-bytes
                         (lambda () (write (cdr b)))))))]
             [l (and v
                     (hash-ref (linkl-bundle-table
                                (hash-ref (linkl-directory-table v)
                                          '()))
                               0))]
             [n (match (match l
                         [(struct linkl (name importss im-shapess exports
                                              internals lifts source-names
                                              body max-let-depth _)) body]
                         [else #f])
                  [`(,(struct primval (n))) n]
                  [else #f])])
        (hash-set! table n (car b))))
    table))

(define primitives (set-union (hash-values primitive-table)
                              (hash-values primitive-table2)))

(define (value? form)
  (ormap (λ (f) (f form)) (list path? list? box? pair? hash? vector? number? string? symbol? char? keyword? regexp? byte-regexp? bytes? extflonum?)))

(define (compile-json config language topmod body1 top-reqs-provs body-forms pycket?)
  (let ([whole-body (append top-reqs-provs body-forms)])
    (hash* 'language (list language)
           'module-name topmod
           'config config
           'body-forms (if pycket?
                           whole-body
                           (cons body1 whole-body)))))


(define (handle-def-values def-values-form localref-stack current-closure-refs [linklet? #f] [importss '()])
  (let ([ids (def-values-ids def-values-form)]
        [rhs (def-values-rhs def-values-form)])
    (cond
      ((ormap (λ (def) (not (toplevel? def))) ids) ;; just a sanity check
       (error 'handle-def-values "def-values : detected a non toplevel?"))
      (else
       (let* ([poss (map toplevel-pos ids)]
              [syms (map (λ (p) (list-ref TOPLEVELS p)) poss)]
              [symstrs (map (λ (sym) (if (symbol? sym) (symbol->string sym) sym)) syms)])
         (hash* 'define-values symstrs
                'define-values-names symstrs
                'define-values-body (to-ast-single rhs (append symstrs localref-stack) current-closure-refs linklet? importss)))))))

(define (handle-if if-form localref-stack current-closure-refs [linklet? #f])
  (let ([test (branch-test if-form)]
        [then (branch-then if-form)]
        [else (branch-else if-form)])
    (hash*
     'test (to-ast-single test localref-stack current-closure-refs linklet?)
     'then (to-ast-single then localref-stack current-closure-refs linklet?)
     'else (to-ast-single else localref-stack current-closure-refs linklet?))))

(define extended-reals (list +inf.0 +inf.f
                             -inf.0 -inf.f
                             +nan.0 +nan.f))

(define (convert-single-precs ext-real)
  (cond
    [(or (eqv? ext-real -nan.f) (eqv? ext-real +nan.f)) "+nan.0"]
    [(or (eqv? ext-real +inf.f) (eqv? ext-real +inf.0)) "+inf.0"]
    [(or (eqv? ext-real -inf.f) (eqv? ext-real -inf.0)) "-inf.0"]
    [else (number->string ext-real)]))

(define (handle-number racket-num)
  (hash* 'number (handle-number-inner racket-num)))

(define (handle-number-inner racket-num)
  (cond
    [(extflonum? racket-num)
     (let ((inex (extfl->inexact racket-num)))
       (if (memv inex extended-reals)
           (hash* 'extended-real (convert-single-precs inex))
           (hash* 'real inex)))]
    [(exact? racket-num)
     (cond
       [(integer? racket-num)
        (hash* 'integer (number->string racket-num))]
       [(rational? racket-num)
        (let
            ([num (numerator racket-num)]
             [den (denominator racket-num)])
          (hash* 'numerator (hash* 'integer (number->string num))
                 'denominator (hash* 'integer (number->string den))))]
       [(complex? racket-num)
        (let ([real (real-part racket-num)]
              [imag (imag-part racket-num)])
          (hash* 'real-part (handle-number-inner real)
                 'imag-part (handle-number-inner imag)))]
       [else (error 'handle-num (format "handle this exact num: ~a" racket-num))])]
    [else
     (cond
       [(real? racket-num)
        (if (memv racket-num extended-reals)
            (hash* 'extended-real (convert-single-precs racket-num))
            (hash* 'real (+ 0.0 (* 1.0 (inexact->exact racket-num)))))]
       [(complex? racket-num)
        (let ([real (real-part racket-num)]
              [imag (imag-part racket-num)])
          (hash* 'real-part (handle-number-inner real)
                 'imag-part (handle-number-inner imag)))]
       [else
        (error 'handle-num (format "handle this inexact num: ~a" racket-num))])]))

(define (handle-boolean racket-bool)
  racket-bool)

(define (handle-string racket-str)
  (hash* 'string racket-str))

(define (handle-symbol racket-sym [are-we-in-a-linklet? #f])
  (let ([s (symbol->string racket-sym)])
    (begin
      (displayln (format "handle-symbol : ~a : are-we-in-a-linklet? : ~a" racket-sym are-we-in-a-linklet?))
      (if (and (not are-we-in-a-linklet?) (memv racket-sym primitives))
          (hash* 'source-name s)
          (hash* 'toplevel s)))))

(define (handle-prefab p-srt [linklet? #f])
  (let ([p-key (prefab-struct-key p-srt)]
        [struct-ls (vector->list (struct->vector p-srt))])
    (hash* 'prefab-key (to-ast-val p-key linklet?)
           'struct (map (lambda (st) (to-ast-val st linklet?))
                        (cdr struct-ls)))))

(define (handle-char racket-char)
  (hash* 'char (number->string (char->integer racket-char))))

(define (handle-keyword racket-kw)
  (hash* 'keyword (keyword->string racket-kw)))

(define (handle-regexp racket-regexp)
  (hash* 'regexp (object-name racket-regexp)))

(define (handle-byte-regexp racket-byte-regexp)
  (hash* 'byte-regexp (bytes->list (object-name racket-byte-regexp))))

(define (handle-bytes racket-bytes)
  (hash* 'bytes (bytes->list racket-bytes)))

(define (handle-void racket-void)
  (hash* 'operator (hash* 'source-name "void")
         'operands (list)))

(define (handle-hash racket-hash [linklet? #f])
  (let* ([keys (hash-keys racket-hash)]
         [vals (hash-values racket-hash)]
         [keys-asts (map (lambda (k) (to-ast-val k linklet?)) keys)]
         [vals-asts (map (lambda (v) (to-ast-val v linklet?)) vals)])
    (hash* 'hash-keys keys-asts
           'hash-vals vals-asts)))

(define (handle-vector racket-vector [linklet? #f])
  (let* ([ls (vector->list racket-vector)])
    (hash* 'vector (map (lambda (v) (to-ast-val v linklet?)) ls))))

(define (handle-list list-form [linklet? #f] [importss '()])
  (map (λ (x) (to-ast-val x linklet? importss)) list-form))

(define (handle-path path-form)
  (hash* 'path (path->string path-form)))

(define (handle-box box-form [linklet? #f])
  (hash* 'box (to-ast-val (unbox box-form) linklet?)))

(define (handle-pair pair-form [linklet? #f])
  (let ([first-vals (takef pair-form (λ (_) #t))]
        [last-val (dropf pair-form (λ (_) #t))])
    (hash* 'improper (list (map (lambda (v) (to-ast-val v linklet?)) first-vals)
                           (to-ast-val last-val linklet?)))))

(define (get-primval-name id)
  (symbol->string (prim id)))

(define (prim num)
  (or (hash-ref primitive-table num (lambda () #f))
      (hash-ref primitive-table2 num
                (lambda () (error 'primitive "couldn't find primitive for : ~a" num)))))

(define (handle-primval operation [linklet? #f])
  (let* ([id (primval-id operation)]
         [operator-name (get-primval-name id)])
    (if (string-contains? operator-name "unsafe") ;; cheat
        (hash* 'source-name operator-name
               'source-module '("#%unsafe"))
        (hash* 'source-name operator-name))))


#|
if rator is a closure, create a letrec node
put closure-gen-id and closure-code in letrec-bindings

for the letrec-body
create an application node
put lexical closure-gen-id to operator
put the usual application-rands to the operands
|#
(define (handle-application app-form localref-stack current-closure-refs [linklet? #f] [importss '()])
  (let* ([rator (application-rator app-form)]
         [rands (application-rands app-form)]
         [newlocalstack (append (map (λ (x) 'app-empty-slot) (range (length rands)))
                                localref-stack)]
         ;; the application pushes empty slots to run the args over, so it will push the existing local refs further in the stack
         ;; we simulate it here to make the localref pos indices point to the right identifier
         [rator-evaluated (to-ast-single rator newlocalstack current-closure-refs linklet? importss)]
         [operands-evaluated (map (λ (rand) (to-ast-single rand newlocalstack current-closure-refs linklet? importss)) rands)])
    (if (closure? rator)
        (let ([closure-ref (symbol->string (closure-gen-id rator))]
              [closure-body (closure-code rator)])
          (if (ormap (λ (cr) (string=? cr closure-ref)) current-closure-refs)
              (hash* 'operator (hash* 'lexical closure-ref)
                     'operands operands-evaluated)
              (hash* 'operator (hash* 'letrec-bindings (list (list (list closure-ref)
                                                                   (handle-closure rator newlocalstack current-closure-refs linklet? importss)))
                                      'letrec-body (list (hash* 'lexical closure-ref)))
                     'operands operands-evaluated)))
        (hash* 'operator rator-evaluated
               'operands operands-evaluated))))


(define (handle-lambda lam-form localref-stack current-closure-refs [linklet? #f] [importss '()])
  (let* ([name (lam-name lam-form)]
         ;; TODO : revisit : there's no canonical way to figure out the source info from lam struct,
         ;; so we try to infer it from the name field (which is there for debugging purposes)
         [source (if (null? name) '()
                     (if (vector? name)
                         (let* ([fpath (vector-ref name 1)] ;; get the name, and put it in the relative-dir
                                ;; b/c lam-name depends on where raco make is run (as opposed to where pycket is run)
                                [fname (file-name-from-path fpath)])
                           (hash* '%p (path->string (if (eq? relative-current-dir "") fname
                                                        (build-path (string->path relative-current-dir) fname)))))
                         (if (not (symbol? name)) (error 'handle-lambda "we have a non symbol/vector name in a lam form")
                             (let* ([collects-dir (path->string (find-collects-dir))]
                                    [name-str (symbol->string name)]
                                    [usual-prefix "/racket/private/"]
                                    [path (cond
                                            [(string-contains? name-str ".../more-scheme.rkt")
                                             (string-append collects-dir usual-prefix "more-scheme.rkt")]
                                            [(string-contains? name-str "kw.rkt")
                                             (string-append collects-dir usual-prefix "kw.rkt")]
                                            [(string-contains? name-str "...rivate/parse.rkt")
                                             (string-append collects-dir "/syntax/parse/private/parse.rkt")]
                                            [else
                                             (begin (when DEBUG (displayln (format "writing lam name : ~a" name)))
                                                    (symbol->string name))])])
                               (hash* '%p path)))))]
         [position 321] ;; TODO: span&pos info are inside the lam-name
         [span 123]
         ;; module seems to be the same for every lambda form,
         ;; pointing to a private module about chaperones/impersonators
         [module (hash* '%mpi (hash* '%p (string-append collects-dir "/racket/private/kw.rkt")))]                                     

         [num-args (lam-num-params lam-form)]

         [arg-types (begin (when (and (not (= num-args (length (lam-param-types lam-form))))
                                      DEBUG)
                             (error 'handle-lambda "investigate: num-args and (length param-types) are not equal"))
                           (lam-param-types lam-form))] ;; val - ref - flonum - fixnum - extflonum
         ;; formal symbols
         ;; TODO : refactor/cleanup
         [symbols-for-formals (map (λ (x)
                                     (begin
                                       (when (and (or (eq? x 'flonum) (eq? x 'fixnum) (eq? x 'extflonum))
                                                  DEBUG)
                                         (displayln (format "warning : argument to lam-name : ~a is : ~a" name x)))
                                       (let ([sym 
                                              (symbol->string
                                               (gensym
                                                (string->symbol (string-append "lam." (symbol->string x) "."))))])
                                         (if (eq? x 'ref) sym sym))))
                                   arg-types)]
         
         [rest? (lam-rest? lam-form)]
         ;; rest arg symbol
         [rest-formal (if rest? (symbol->string (gensym 'lamrest)) 'hokarz)]

         [new-localref-stack-1 (if rest?
                                   (append symbols-for-formals (list rest-formal) localref-stack)
                                   (append symbols-for-formals localref-stack))]

         ;; capture-prefix : a vector of stack positions that are captured when
         ;;                  evaluating the lambda form to create a closure.
         [capture-prefix (vector->list (lam-closure-map lam-form))] ;; vector->list
         
         ;; top-map : indicates which top-level and lifted variables are
         ;;           actually used by the closure
         ;;           and whether any syntax objects are used (<- it's not clear how it does this).
         [top-map (let ([top-map* (lam-toplevel-map lam-form)])
                    (if (not top-map*)
                        (if (empty? capture-prefix)
                            '()
                            #f)
                        (set->list top-map*)))] ;; list
         ;; A #f value indicates either that no prefix is captured or
         ;; all variables and syntax objects in the prefix should be considered used.
         
         ;; In the prefix:
         ;; -- Variables are numbered consecutively by position starting from 0
         ;; -- the number equal to the number of non-lifted variables corresponds to syntax objects
         ;; -- Lifted variables are numbered immediately afterward

         ;; ----- non-lifteds ---- syntax-objects ---- lifteds ----
         
         ;; Let's regenerate the conditions when the prefix was captured (reverse engineer the then stack)

         ;; make-symbol : any -> symbol/boolean
         [make-symbol (lambda (v)
                        (cond
                          [(or (boolean? v) (symbol? v)) v]
                          ;[(module-variable? v) (module-variable-sym v)]
                          [(string? v) (string->symbol v)]
                          [else (error 'lambda:make-symbol "add a case for ~a in TOPLEVELS" v)]))]

         [toplevels-symbol (map make-symbol initial-toplevels)]
         [num-real-toplevels (length toplevels-symbol)]
         
         [lifted-variables LIFTEDS] ;; andmap symbol?

         [toplevel-and-lifteds (map make-symbol TOPLEVELS)]

         
         [non-lifteds (filter (lambda (s)
                                (let* ([s (make-symbol s)]
                                       [s-sym (if (boolean? s) s (symbol->string s))])
                                  (or (member s toplevels-symbol)
                                      (string-contains? s-sym "lam.val")
                                      (string-contains? s-sym "lam.ref")
                                      (string-contains? s-sym "letone")
                                      (string-contains? s-sym "let-void")
                                      (string-contains? s-sym "-letrec")
                                      (string-contains? s-sym "app-empty-slot"))))
                              localref-stack)]

         [num-of-non-lifteds (length non-lifteds)]

         [stack-captured-items non-lifteds]

         ;; Future problem -> we don't know how many syntax objects we have in the prefix!!
         
         [top-map-toplevels (and top-map
                                 (map (lambda (top) (list-ref toplevels-symbol top))
                                      (sort (filter (lambda (top) (< top num-real-toplevels)) top-map) <)))]
         ;; ASSUMPTION : toplevels stay in the prefix sorted by their positions
         ;; reason : "test/control.rktl" - function has a toplevel-map like (set 44 43 14) was only capturing 14 with closure-map #(0)
         [top-map-lifteds (and top-map
                               (map (lambda (top)
                                      (with-handlers
                                        ([exn:fail?
                                          (lambda (e)
                                            (error 'lambda:top-map-lifteds
                                                   "capture prefix : ~a \n --- non-lifteds : ~a\n --- top-map : ~a\n --- toplevels : ~a\n --- topsyntaxes : ~a\n ~a\n --- lifteds : ~a"
                                                   capture-prefix non-lifteds top-map toplevels-symbol (length TOPSYNTAX) e LIFTEDS))])
                                        (list-ref toplevel-and-lifteds top)))
                                    (filter (lambda (top) (>= top num-real-toplevels)) (take (sort top-map <) (min (length top-map)
                                                                                                              (length capture-prefix))))))]

         [toplevel-captured-items (if (not top-map)
                                      toplevel-and-lifteds
                                      (append top-map-toplevels top-map-lifteds))] ;; <- this order is important

         ;; actual-prefix when the closure was captured
         [actual-prefix (let ([actual-prefix* (append stack-captured-items #;syntax-objects??? toplevel-captured-items)])
                          (begin
                            ;; just a sanity check
                            (when (not (>= (length actual-prefix*)
                                           (length capture-prefix)))
                              (error 'lambda:capture "capture-prefix : ~a ---- actual-prefix : ~a ---- #-of-non-lifteds : ~a"
                                     capture-prefix actual-prefix* num-of-non-lifteds))
                            (when DEBUG
                              (displayln (format "****** CAPTURE : prefix : ~a \n---- actual-prefix : ~a \n---- top-map : ~a \n---- #-of-non-lifteds : ~a"
                                                 capture-prefix actual-prefix* top-map num-of-non-lifteds)))
                            actual-prefix*))]

         [captured-items (map (lambda (pos) (with-handlers
                                         ([exn:fail? (lambda (e)
                                                       (error 'capture "capture-prefix : ~a ---- actual-prefix : ~a ---- #-of-non-lifteds : ~a"
                                                              capture-prefix actual-prefix num-of-non-lifteds))])
                                         (list-ref actual-prefix pos))) capture-prefix)]

         
         [new-localref-stack (append captured-items new-localref-stack-1)]

         [lamBda
          (let ([args (map (λ (sym) (hash* 'lexical (if (box? sym) (unbox sym) sym))) symbols-for-formals)])
            (if rest?
                (hash* 'improper (list args ;; list of regular args list and the rest argument
                                       (hash* 'lexical rest-formal)))
                args))]

         [body (to-ast-single (lam-body lam-form)
                              new-localref-stack
                              current-closure-refs linklet? importss)])
    ;; pycket seems to omit source and position (and sets the span to 0) if lam-name is ()
    (if (null? source)
        (hash* 'span 0 'module module 'lambda lamBda 'body (list body))
        (hash* 'original true 'source source 'position position 'span span 'module module 'lambda lamBda 'body (list body)))))

(define (handle-inline-variant iv-form localref-stack current-closure-refs [linklet? #f])
  (let ([direct (inline-variant-direct iv-form)]
        [inline (inline-variant-inline iv-form)])
    ;; using inlined version if possible (clearly possible if inline-variant form exists??)
    (to-ast-single direct localref-stack current-closure-refs linklet?)))

(define (handle-closure closure-form localref-stack current-closure-refs [linklet? #f] [importss '()])
  (let ([code (closure-code closure-form)]
        [gen-id (symbol->string (closure-gen-id closure-form))])
    (begin
      (when DEBUG
        (displayln (format "handle-closure gen-id : ~a" gen-id))
        (display "Current-closure-refs ===>  ")
        (displayln current-closure-refs)
        (display "inside??? ====>   ")
        (displayln (if (ormap (λ (cr) (string=? gen-id cr)) current-closure-refs) true false)))
      
      (if (ormap (λ (cr) (string=? gen-id cr)) current-closure-refs)
          (hash* 'lexical gen-id)
          (if (lam? code)
              (handle-lambda code localref-stack (cons gen-id current-closure-refs) linklet? importss)
              (error 'handle-closure "no lam inside the closure?"))))))

(define (handle-apply-values app-form localref-stack current-closure-refs [linklet? #f])
  (let* ([proc-part (apply-values-proc app-form)]
         [args-part (apply-values-args-expr app-form)]

         [proc (to-ast-single proc-part localref-stack current-closure-refs linklet?)]

         [m (if (toplevel? proc-part)
                (let ([t (list-ref TOPLEVELS (toplevel-pos proc-part))])
                  false #;(if (module-variable? t) t false)) false)]

         
         [mod-sym false #;(if m (symbol->string (module-variable-sym m)) false)]
         [mod-path (string-append collects-dir "/racket/private/modbeg.rkt")
                   #;(if m (path->string
                          (resolved-module-path-name
                           (module-path-index-resolve
                            (module-variable-modidx m))))
                       (string-append collects-dir "/racket/private/modbeg.rkt"))
                   ]

         [new-localref-stack (if mod-sym (cons mod-sym localref-stack) localref-stack)]

         [args (to-ast-single args-part new-localref-stack current-closure-refs linklet?)]

         ;; construct the lam (lambda () args)
         [lambda-form (hash* 'source (hash* '%p (string-append relative-current-dir module-name ".rkt"))
                             'position 321
                             'span 123
                             'module (hash* '%mpi (hash* '%p mod-path))
                             'lambda '()
                             'body (list args))])

    ;; (call-with-values lam proc)
    (hash* 'operator (hash* 'source-name "call-with-values")
           'operands (list lambda-form proc))))

;; TODO : refactor/cleanup (don't need boxes anymore)
(define (handle-localref lref-form localref-stack)
  (let* ([clear? (localref-clear? lref-form)]
         [unbox? (localref-unbox? lref-form)]
         [pos (localref-pos lref-form)]
         [stack-slot
          (let
              ([slot (with-handlers ([exn:fail? (lambda (e) (displayln (format "getting pos ~a, from ~a" pos localref-stack)) (raise e))])
                       (list-ref localref-stack pos))])
            (begin
              (when (and (not unbox?) (box? slot) DEBUG)
                (displayln
                 (format "localref warning : unbox? is false, but pos --~a-- is a box : ~a\n\n" pos slot)))
              #;(if unbox? (unbox slot) slot)
              (if (box? slot) (unbox slot) slot)))])
    (cond
      [(hash? stack-slot) (error 'handle-localref "interesting... we seem to have a hash in the stack slot : ~a" stack-slot)]
      [(box? stack-slot) (error 'handle-localref "we have unboxing issues with pos : ~a - slot : ~a" pos stack-slot)]
      [else (hash* 'lexical 
                   (let ([slot-payload (if (symbol? stack-slot) (symbol->string stack-slot) stack-slot)])
                     (if (or (box? slot-payload)
                             (and (string? stack-slot)
                                  (string-contains? stack-slot "dummy")))
                         (error 'handle-localref
                                "pos: ~a shouldn't have extracted this: ~a \n here's the stack: \n~a\n"
                                pos stack-slot localref-stack)
                         slot-payload)))])))

(define (self-mod? mpi)
  (let-values ([(mod-path base-path) (module-path-index-split mpi)])
    (and (not mod-path) (not base-path))))

(define (module-path-index->path-string mod-idx)

  (define (put-relative req-mod)
    (if (string-contains? req-mod ".rkt")
        (string-append relative-current-dir req-mod) req-mod))

  (define (resolved-to-string name)
    (cond
      [(path? name) (path->string name)]
      [(symbol? name) (symbol->string name)]
      [(list? name) (map resolved-to-string name)]
      [(string? name) name]
      [else (error 'module-path-index->path-string "we have ~a as the resolved name in mod-idx : ~a" name mod-idx)]))
  
  (if (self-mod? mod-idx)
      (string-append relative-current-dir module-name ".rkt")
      (let-values ([(module-path base-path) (module-path-index-split mod-idx)])
        (if (list? module-path) ;; it may be resolved ;; TODO : refactor/cleanup
            (let* ([resolved
                    (if
                     (and (eq? 'submod (car module-path))
                          (string? (cadr module-path))
                          (self-mod? base-path)
                          (or (string=? "." (cadr module-path))
                              (string=? ".." (cadr module-path)))) ;; regular submod
                     (cons (cadr module-path) (map resolved-to-string (cddr module-path)))
                     (with-handlers
                       ([exn:fail?
                         (λ (e)
                           (cond
                             [(or (not (eq? 'submod (car module-path)))
                                  (not (string? (cadr module-path)))
                                  (not (self-mod? base-path)))
                              (error 'module-path-index->path-string "check mod-idx : ~a" mod-idx)]
                             [else
                              (let ([relative-module (string-append relative-current-dir (cadr module-path))])
                                (cons relative-module (map resolved-to-string (cddr module-path))))]))])
                       (module-path-index-resolve mod-idx)))]
                   [resolved-name (if (resolved-module-path? resolved) (resolved-module-path-name resolved) resolved)])
              (resolved-to-string resolved-name))
            (if (symbol? module-path) ;; then it is resolved
                (let ([path (resolved-module-path-name (module-path-index-resolve mod-idx))])
                  (if (symbol? path)
                      (symbol->string path)
                      (path->string path)))
                (if (not (string? module-path))
                    (error 'module-path-index->path-string "module-path is not a list, symbol or string : ~a, in ~a" module-path mod-idx)
                    ;; resolving manually using the base-path
                    (if (self-mod? base-path)
                        (string-append relative-current-dir module-path)
                        #;(if TEST
                              module-path
                              (string-append relative-current-dir module-path))
                        (let ([base-path-str (if (resolved-module-path? base-path)
                                                 (path->string (resolved-module-path-name base-path))
                                                 (module-path-index->path-string base-path))])
                          (begin ;; sanity-check : should end with .rkt
                            (when (not (string-suffix? base-path-str ".rkt"))
                              (error 'module-path-index->path-string "something's wrong with the resolved base path : ~a" base-path-str))
                            (let* ([spl (string-split base-path-str "/")]
                                   [real-base (string-append "/" (string-join (take spl (sub1 (length spl))) "/") "/")])
                              (string-append real-base module-path)))))))))))

#;(define (handle-module-variable mod-var localref-stack)
  (let* ([name (symbol->string (module-variable-sym mod-var))]
         [mod-idx (module-variable-modidx mod-var)]
         [module-path
          (module-path-index->path-string mod-idx)])
    (hash* 'source-name name
           'source-module (if (and TEST (string? module-path) ;; TODO : refactor
                                   (string-contains? module-path (string-append "/" module-name "." module-extension)))
                              (list ".")
                              (if (list? module-path) module-path (list module-path))))))

(define (handle-varref varref-form localref-stack)
  (let ([top (varref-toplevel varref-form)]
        [dummy (varref-dummy varref-form)]
        [current-mod-path (string-append relative-current-dir module-name ".rkt")])
    (let* ([topvar (if (boolean? top) top (list-ref TOPLEVELS (toplevel-pos top)))]
           [name (cond
                   [(boolean? topvar)
                    topvar #;(if topvar
                                 (error 'handle-varref (format "we got a TRUE bool from TOPLEVELS : ~a - varref : ~a" topvar varref-form))
                                 false)]
                   [(symbol? topvar) (symbol->string topvar)]
                   #;[(module-variable? topvar) (symbol->string (module-variable-sym topvar))])]
           [path-str current-mod-path #;(if (or (symbol? topvar) (boolean? topvar))
                         current-mod-path
                         ;; it's a module-variable
                         (module-path-index->path-string (module-variable-modidx topvar)))]
           ;; TODO : refactor
           [is-lifted? false]
           [name-ref-hash (if (boolean? topvar) topvar
                              (if (memv (string->symbol name) primitives) ;; it's a primitive
                                  (hash* 'source-name name)
                                  (if (string-contains? name ".")
                                      (let* ([mod-split (string-split name ".")]
                                             [original-mod (car mod-split)])
                                        (begin
                                          (set! is-lifted? true)
                                          ;; sanity check : second part of the "." should be all numbers
                                          (with-handlers ([exn:fail?
                                                           (λ (e) (error 'handle-varref (format "unusual topvar name : ~a" name)))])
                                            (string->number (cadr mod-split)))
                                          (hash* 'source-name name
                                                 'source-module (if (and TEST (string-contains? path-str (string-append "/" module-name "." module-extension)))
                                                                    (list ".") (list path-str))
                                                 'module original-mod)))
                                      (hash* 'source-name name
                                             'source-module (if (and TEST (string-contains? path-str (string-append "/" module-name "." module-extension)))
                                                                (list ".") (list path-str))))))]
           [source-mod (cond
                         [(or (symbol? topvar) (boolean? topvar)) path-str]
                         [(memv (string->symbol name) primitives) current-mod-path]
                         #;[(module-variable? topvar)
                          (if is-lifted? ;; assumption : all lifted var-refs are from kw.rkt
                              (string-append collects-dir "/racket/private/kw.rkt")
                              current-mod-path)])]
           [final-hash 
            (hash* 'source (hash* '%p source-mod)
                   'module (hash* '%mpi (hash* '%p source-mod))
                   'variable-reference name-ref-hash
                   'position 12
                   'span 11
                   'original true)])
      (if (boolean? topvar) ;; (hash* 'var false) produces '#hash() 
          (hash-set final-hash 'variable-reference name) ;; adding it with hash-set works
          final-hash))))

(define (handle-let-one letform localref-stack current-closure-refs [linklet? #f])
  (begin
    (when DEBUG
      (displayln (format "LET-ONE - UNUSED? ==> ~a" (let-one-unused? letform))))
    (let* ([unused? (let-one-unused? letform)]
           [bindingname (if unused? "letone-not-used-slot" (symbol->string (gensym 'letone)))]
           [newstack (cons bindingname localref-stack)]) ;; push uninitialized slot
      (hash* 'let-bindings (list (list (list bindingname)
                                       (to-ast-single (let-one-rhs letform) newstack current-closure-refs linklet?)))
             'let-body (list (to-ast-single (let-one-body letform)
                                            newstack
                                            ;(if unused? localref-stack newstack) ;; if unused?, then rhs is not pushed to the stack
                                            current-closure-refs linklet?))))))

(define (body-name body-form)
  (cond
    ((list? body-form) "List ")
    ((hash? body-form) "Hash ")
    ((boolean? body-form) "Boolean ")
    ((number? body-form) "Number ")
    ((string? body-form) "String ")
    ((symbol? body-form) "Symbol ")
    ((char? body-form) "Char ")
    ((keyword? body-form) "Keyword ")
    ((regexp? body-form) "Regexp ")
    ((byte-regexp? body-form) "Byte Regexp ")
    ((bytes? body-form) "Byte String ")
    ((void? body-form) "Void ")
    ((with-cont-mark? body-form) "with-cont-mark ")
    ((with-immed-mark? body-form) "with-immed-mark ")
    ((boxenv? body-form) "boxenv ")
    ((let-one? body-form) "let-one ")
    ((let-void? body-form) "let-void ")
    ((let-rec? body-form) "let-rec ")
    ((case-lam? body-form) "case-lam ")
    ((install-value? body-form) "install-value ")
    ;((module-variable? body-form) "module-variable ")
    ((varref? body-form) "varref ")
    ((primval? body-form) "primval ")
    ((application? body-form) "application ")
    ((def-values? body-form) "def-values ")
    ((seq? body-form) "seq ")
    ;((splice? body-form) "splice ")
    ((beg0? body-form) "beg0 ")
    ((assign? body-form) "SET! ")
    ((branch? body-form) "branch ")
    ((apply-values? body-form) "apply-values ")
    ((localref? body-form) "localref ")
    ((lam? body-form) (format "lam : ~a \n" (lam-name body-form)))
    ((inline-variant? body-form) "inline-variant ")
    ((closure? body-form) "closure ")
    ((toplevel? body-form) "toplevel ")
    ;((topsyntax? body-form) "topsyntax ")
    ((hash? body-form) "Already hashed Val (pushed by let-one)")
    ((prefab-struct-key body-form) "Prefab struct ")
    (else "Unknown: ")))

;; (find-which-import 'e '((a b) (f) (d e c)) 0) -> 3
;; (find-which-import 'x '((a b) (f) (d e c)) 0) -> 0
(define (find-import-instance-of element ls count)
    (cond
      [(null? ls) -1]
      [(memq element (car ls)) count]
      [else (find-import-instance-of element (cdr ls) (add1 count))]))
;; import from -1 means "." (i.e. current linklet instance)

(define (handle-toplevel form localref-stack [linklet? #f] [importss '()])
  (let* ([toplevel-id (list-ref TOPLEVELS (toplevel-pos form))]
         [toplevel-id-str (if (symbol? toplevel-id) (symbol->string toplevel-id) toplevel-id)]
         [module-dir (string-append relative-current-dir module-name "." module-extension)])
    (cond
      [(symbol? toplevel-id)
       (begin
         (displayln (format "TOPLEVEL id : ~a --- linklet? : ~a --- importss : ~a" toplevel-id linklet? importss))
       (if linklet?
           (hash* 'source-name toplevel-id-str
                  'source-linklet
                  (let ([import-instance-number (find-import-instance-of toplevel-id importss 0)])
                    (if (< import-instance-number 0)
                        (hash* 'quote (hash* 'toplevel "self"))
                        (hash* 'quote (handle-number import-instance-number)))))
           (hash* 'source-name toplevel-id-str
                  'source-module (if (and TEST (string-contains? module-dir (string-append "/" module-name "." module-extension)))
                                     (list ".") (list module-dir)))))]
      #;[(module-variable? toplevel-id)
       (handle-module-variable toplevel-id localref-stack)]
      [else (error 'handle-toplevel "not sure how to handle this kind of toplevel form")])))

#;(define (handle-topsyntax topsyn-form localref-stack current-closure-refs)
  (let* ([pos (topsyntax-pos topsyn-form)]
         [selected-stx (list-ref TOPSYNTAX pos)] ;; stx?
         [content (stx-content selected-stx)] ;; stx-obj?
         [datum (stx-obj-datum content)]
         [src-loc (stx-obj-srcloc content)]
         [position (if (not src-loc) 12345 (srcloc-position src-loc))]
         [span (if (not src-loc) 11 (srcloc-span src-loc))]
         [source-path (if (not src-loc)
                          (string-append relative-current-dir module-name ".rkt")
                          (let ([s-path (srcloc-source src-loc)])
                            (if (string? s-path) s-path (path->string s-path))))]
         [q-syntax (to-ast-val datum)]
         [topsyntax-node (hash* 'source (hash* '%p source-path)
                                'module (hash* '%mpi (hash* '%p source-path))
                                'position position
                                'span span)])
    (hash-set topsyntax-node 'quote-syntax q-syntax)))

(define (handle-seq seq-expr localref-stack current-closure-refs [linklet? #f])
  (let* ([seqs (seq-forms seq-expr)]
         [last-seq (list (last seqs))]
         [seqs-non-simple (filter (λ (form) (not (localref? form))) (take seqs (sub1 (length seqs))))]
         [exprs (map (λ (expr) (to-ast-single expr localref-stack current-closure-refs linklet?)) (append seqs-non-simple last-seq))])
    (hash* 'let-bindings (list)
           'let-body exprs)))

#;(define (handle-splice splice-expr localref-stack current-closure-refs)
  (let* ([splices (splice-forms splice-expr)]
         [exprs (map (λ (expr) (to-ast-single expr localref-stack current-closure-refs)) splices)])
    (hash* 'let-bindings (list)
           'let-body exprs)))

(define (handle-begin0 body-form localref-stack current-closure-refs [linklet? #f])
  (let* ([seqs (beg0-seq body-form)]
         [first-expr (car seqs)] ;; assumes seqs is not empty
         [rest-exprs (cdr seqs)])
    (hash* 'begin0 (to-ast-single first-expr localref-stack current-closure-refs linklet?)
           'begin0-rest (map (λ (expr) (to-ast-single expr localref-stack current-closure-refs linklet?)) rest-exprs))))


(define (handle-wcm body-form localref-stack current-closure-refs [linklet? #f])
  (let ([wcm-key (with-cont-mark-key body-form)]
        [wcm-val (with-cont-mark-val body-form)]
        [wcm-body (with-cont-mark-body body-form)])
    (hash* 'wcm-key (to-ast-single wcm-key localref-stack current-closure-refs linklet?)
           'wcm-val (to-ast-single wcm-val localref-stack current-closure-refs linklet?)
           'wcm-body (to-ast-single wcm-body localref-stack current-closure-refs linklet?))))

(define (handle-immed-mark body-form localref-stack current-closure-refs [linklet? #f])
  (let* ([key (with-immed-mark-key body-form)]
         [body (with-immed-mark-body body-form)]
         [mark-formal (symbol->string (gensym))]
         [lam-form
          (hash* 'source (hash* '%p (string-append relative-current-dir module-name ".rkt"))
                 'position 321
                 'span 123
                 'module (hash* '%mpi (hash* '%p (string-append collects-dir "/racket/private/kw.rkt")))
                 'lambda (list (hash* 'lexical mark-formal))
                 'body (list (to-ast-single body (cons mark-formal localref-stack) current-closure-refs linklet?)))])
    
    (hash* 'operator (hash* 'source-name "call-with-immediate-continuation-mark")
           'operands (list (to-ast-single key localref-stack current-closure-refs linklet?)
                           lam-form))))

;; boxenv case: lambda arg is mutated inside the body
(define (handle-boxenv body-form localref-stack current-closure-refs [linklet? #f])
  (let* ([pos (boxenv-pos body-form)]
         [pre-pos (take localref-stack pos)]
         [post-pos (drop localref-stack pos)]
         [boxed-slot (string-append (car post-pos) "-box")])
    (begin
      #;(when DEBUG
          (displayln (format "boxenv pos : ~a | old-slot : ~a | new-slot : ~a" pos (car post-pos) boxed-slot)))
      #;(to-ast-single (boxenv-body body-form) (append pre-pos (list boxed-slot) (cdr post-pos)) current-closure-refs)
      (to-ast-single (boxenv-body body-form) localref-stack current-closure-refs linklet?))))

(define (handle-assign body-form localref-stack current-closure-refs [linklet? #f])
  (let ([id (assign-id body-form)]
        [rhs (assign-rhs body-form)]
        [module-dir (string-append relative-current-dir module-name ".rkt")])
    (list (hash* 'source-name "set!")
          (to-ast-single id localref-stack current-closure-refs linklet?)
          (to-ast-single rhs localref-stack current-closure-refs linklet?))))

(define (handle-let-void body-form localref-stack current-closure-refs [linklet? #f])
  ;; Pushes count uninitialized slots onto the stack and then runs body.
  ;; If boxes? is #t, then the slots are filled with boxes that contain #<undefined>.
  (let* ([count (let-void-count body-form)]
         [boxes? (let-void-boxes? body-form)]
         [body (let-void-body body-form)]
         [count-lst (range count)]
         [payload (if boxes? "let-void-box-" "let-void-")]
         [new-slots (map (λ (s) (symbol->string (gensym (string-append payload (number->string s) ".")))) count-lst)]
         [rhss (map (λ (r) (hash* 'quote (hash* 'toplevel "uninitialized"))) count-lst)]
         [newstack (begin
                     (when DEBUG
                       (displayln (format "LetVoid pushes ~a slots.. boxes? : ~a" count boxes?)))
                     (append new-slots localref-stack))]
         [rhs-ready (map (λ (slot rhs) (list (list slot) rhs)) new-slots rhss)])
    (let* ([body-ast* (to-ast-single body newstack current-closure-refs linklet?)]
           [body-ast (if (list? body-ast*) body-ast* (list body-ast*))])
      (hash* 'let-bindings rhs-ready
             'let-body body-ast))))

(define (handle-install-value body-form localref-stack current-closure-refs [linklet? #f])
  ;; Runs rhs to obtain count results, and installs them into existing
  ;; slots on the stack in order, skipping the first pos stack positions.
  (let* ([count (install-value-count body-form)]
         [pos (install-value-pos body-form)]
         [boxes? (install-value-boxes? body-form)]
         [rhs (install-value-rhs body-form)]
         [body (install-value-body body-form)]
         [count-lst (range count)]

         [binding-list (map (λ (c) (symbol->string (gensym (string-append "inst-val" (number->string c) ".")))) count-lst)]
         
         [slot-positions (map (λ (p) (+ p pos)) count-lst)]

         [mod-region (let ([reg (map (λ (p) (list-ref localref-stack p)) slot-positions)])
                       (begin
                         (when DEBUG (displayln (format "install val boxes? : ~a --\nModified region : ~a\n" boxes? reg)))
                         reg))]

         [set-nodes (map (λ (let-void-slot inst-val-binding)
                           (list (hash* 'source-name "set!")
                                 (hash* 'lexical let-void-slot)
                                 (hash* 'lexical inst-val-binding))) mod-region binding-list)]
         
         [rhs-ready (list (list binding-list
                                (to-ast-single rhs localref-stack current-closure-refs linklet?)))])
    ;; producing json for pycket
    (let* ([body-ast* (to-ast-single body localref-stack current-closure-refs linklet?)]
           [body-ast (if (list? body-ast*) body-ast* (list body-ast*))])
      ;; first binds new vars by evaluating the (single) rhs
      (hash* 'let-bindings rhs-ready
             ;; then sets the let-void bindings with new vars ...
             'let-body (append set-nodes body-ast)
             #;(list (hash* 'let-bindings (list)
                            ;; ... and continue with the body
                            'let-body (append set-nodes body-ast)))))))

(define (handle-let-rec letrec-form localref-stack current-closure-refs [linklet? #f])
  (let* ([procs (let-rec-procs letrec-form)] ;; (listof lam?)
         [proc-names (map (lambda (proc)
                            (let ([name (lam-name proc)])
                              (if (symbol? name)
                                  (format "~a-letrec" (symbol->string (gensym name)))
                                  (format "~a-letrec" (symbol->string (gensym (vector-ref name 0)))))))
                          procs)]
         [slot-count (length procs)]
         [slot-positions (range slot-count)]
         [reversed-proc-names (reverse proc-names)]
         [new-localref-stack
          (begin
            ;; sanity check : validity of the pre-installed slots
            (if (andmap (λ (p) (string-contains? (list-ref localref-stack p) "let-void")) slot-positions)
                'ok
                (error 'handle-let-rec (format "positions : ~a ---- to be modified slots don't look good : ~a" slot-positions localref-stack)))
            (append reversed-proc-names #;localref-stack
                    (drop localref-stack slot-count)
                    ))]

         [proc-bodies (map (λ (proc) (to-ast-single proc new-localref-stack current-closure-refs linklet?)) procs)]
         [body (let-rec-body letrec-form)])
    
    (hash* 'letrec-bindings (map (λ (p-name p-body)
                                   (list (list p-name) p-body))
                                 proc-names proc-bodies)
           'letrec-body (list (to-ast-single body new-localref-stack current-closure-refs linklet?)))))


(define (handle-case-lambda body-form localref-stack current-closure-refs [linklet? #f])
  (let ([clauses (case-lam-clauses body-form)])
    (hash* 'case-lambda
           (map (λ (clause-raw)
                  (if (and (not (lam? clause-raw)) (not (closure? clause-raw)))
                      (begin (when DEBUG (displayln clause-raw))
                             (error 'handle-case-lambda "not a lam clause?"))
                      (let* ([clause (if (lam? clause-raw) clause-raw (closure-code clause-raw))] ;; assumes there's a lam in the closure

                             ;; TODO : make use of 'handle-lambda for these
                             
                             [name (lam-name clause)]
                             [num-args (lam-num-params clause)]

                             [arg-types
                              (begin
                                (when (and (not (= num-args (length (lam-param-types clause))))
                                           DEBUG)
                                  (error 'handle-lambda "investigate: num-args and (length param-types) are not equal"))
                                (lam-param-types clause))] ;; val - ref - flonum - fixnum - extflonum
                             
                             [multiple-args? (lam-rest? clause)] ;; is the rest? true

                             ;; TODO : refactor/cleanup
                             [symbols-for-formals (map (λ (x)
                                                         (begin
                                                           (when (and (or (eq? x 'flonum) (eq? x 'fixnum) (eq? x 'extflonum))
                                                                      DEBUG)
                                                             (displayln (format "warning : argument to CASE-LAM-name : ~a is : ~a" name x)))
                                                           (let ([sym 
                                                                  (symbol->string
                                                                   (gensym
                                                                    (string->symbol (string-append "case-lam." (symbol->string x) "."))))])
                                                             (if (eq? x 'ref) sym sym))))
                                                       arg-types)]

                             ;[symbols-for-formals (map (λ (x) (symbol->string (gensym 'caselam-cl-arg))) (range num-args))]
                             
                             [rest-formal (if multiple-args? (symbol->string (gensym 'caselam-cl-rest)) 'hokarz)]

                             [captured-stack-positions (vector->list (lam-closure-map clause))]
                             [current-stack-length (length localref-stack)]
                             [captured-current-stack-items (map (λ (pos) (if (>= pos current-stack-length)
                                                                             (with-handlers ([exn:fail? (lambda (e)

                                                                                                          (displayln (format "pos : ~a - toplevels : ~a" pos TOPLEVELS))
                                                                                                          (raise e))])
                                                                               (list-ref TOPLEVELS pos))
                                                                             (list-ref localref-stack pos))) captured-stack-positions)]

                             [new-localref-stack-1 (if multiple-args?
                                                       (append symbols-for-formals (list rest-formal) localref-stack)
                                                       (append symbols-for-formals localref-stack))]

                             ;; toplevel stuff??
                             [new-localref-stack (append captured-current-stack-items new-localref-stack-1)]
                             
                             [arg-mapping
                              (let ([args (map (λ (sym) (hash* 'lexical (if (box? sym) (unbox sym) sym))) symbols-for-formals)])
                                (if multiple-args?
                                    (hash* 'improper (list args
                                                           (hash* 'lexical rest-formal)))
                                    args))]
                             
                             [body (to-ast-single (lam-body clause)
                                                  new-localref-stack
                                                  current-closure-refs
                                                  linklet?)])
                        (hash* 'lambda arg-mapping
                               'body (list body)))))
                clauses)
           'original true
           'source (hash* '%p (string-append relative-current-dir module-name ".rkt"))
           'position 987
           'span 456
           'module (hash* '%mpi (hash* '%p (string-append relative-current-dir module-name ".rkt"))))))




(define (to-ast-val val-form [linklet? #f] [importss '()])
  (cond
    ((list? val-form) 
     (handle-list val-form linklet? importss))
    ((path? val-form)
     (handle-path val-form))
    ((box? val-form)
     (handle-box val-form))
    ((pair? val-form)
     (handle-pair val-form linklet?))
    ((hash? val-form)
     (handle-hash val-form linklet?))
    ((vector? val-form)
     (handle-vector val-form linklet?))
    ((or (number? val-form)
         (extflonum? val-form))
     (handle-number val-form))
    ((string? val-form)
     (handle-string val-form))
    ((symbol? val-form)
     (handle-symbol val-form linklet?))
    ((char? val-form)
     (handle-char val-form))
    ((keyword? val-form)
     (handle-keyword val-form))
    ((regexp? val-form)
     (handle-regexp val-form))
    ((byte-regexp? val-form)
     (handle-byte-regexp val-form))
    ((bytes? val-form)
     (handle-bytes val-form))
    #;((stx-obj? val-form)
     (to-ast-val (stx-obj-datum val-form)))
    ;; keep the boolean? and void? here
    ;; for they can be in lists/hashes
    ;; note that they're (not value?)
    ;; because they're handled differently as a bytecode body-form (than as a value)
    ((boolean? val-form)
     (handle-boolean val-form))
    ((void? val-form)
     (handle-void val-form))
    ((prefab-struct-key val-form)
     (handle-prefab val-form linklet?))
    (else (error 'to-ast-val (format "unhandled value : ~a" val-form)))))

;; stack : (listof symbol?/prefix?/hash?)
(define (to-ast-single body-form localref-stack current-closure-refs [linklet? #f] [importss '()])
  (begin
    ;(set! DEBUG-STACK #t)
    (when DEBUG
      ;(display (format "\nTOPLEVELS : ~a" TOPLEVELS))
      (display (format "\n---------------------linklet? : ~a ------------\n" linklet?))
      (display (body-name body-form))
      (display "- ")
      (if (localref? body-form)
          (begin (displayln (format "localref-stack size : ~a" (length localref-stack)))
                 (display (format "Get pos : ~a - Unbox? : ~a - Clear? : ~a" (localref-pos body-form) (localref-unbox? body-form) (localref-clear? body-form)))
                 (display (format " - extracting : ~a" (list-ref localref-stack (localref-pos body-form)))))
          (if (primval? body-form)
              (display (get-primval-name (primval-id body-form)))
              (display "")))
      #;(when (topsyntax? body-form)
        (display (format " pos : ~a " (topsyntax-pos body-form))))
      (display " - LocalRefStack size : ")
      (displayln (number->string (length localref-stack)))(newline)
      (when DEBUG-STACK (display localref-stack))
      (display "\n---------------------------------")
      (newline)(newline))
    (cond
      ;; VALUES
      
      ((value? body-form)
       (hash* 'quote (to-ast-val body-form linklet? importss)))
      
      ;; specially handled vals
      ((void? body-form) 
       (handle-void body-form))
      ((boolean? body-form) ;; note it uses hash (instead of hash*)
       (hash 'quote (handle-boolean body-form)))

      ;; FORMS
      
      ;; let-void
      ((let-void? body-form)
       (handle-let-void body-form localref-stack current-closure-refs linklet?))
      ;; let-rec
      ((let-rec? body-form)
       (handle-let-rec body-form localref-stack current-closure-refs linklet?))
      ;; case-lambda
      ((case-lam? body-form)
       (handle-case-lambda body-form localref-stack current-closure-refs linklet?))
      ;; install-value
      ((install-value? body-form)
       (handle-install-value body-form localref-stack current-closure-refs linklet?))
      ;; set!
      ((assign? body-form) ;; CAUTION : returns list of hash* (instead of hash*)
       (handle-assign body-form localref-stack current-closure-refs linklet?))
      ;; toplevel
      ((toplevel? body-form)
       (handle-toplevel body-form localref-stack linklet? importss))
      ;; let-one (struct let-one expr (rhs body type unused?)
      ((let-one? body-form)
       (handle-let-one body-form localref-stack current-closure-refs linklet?))
      ;; seq
      ((seq? body-form)
       (handle-seq body-form localref-stack current-closure-refs linklet?))
      ;; splice
      #;((splice? body-form)
       (handle-splice body-form localref-stack current-closure-refs))
      ;; beg0
      ((beg0? body-form)
       (handle-begin0 body-form localref-stack current-closure-refs linklet?))
      ;; module-variable
      #;((module-variable? body-form)
       (handle-module-variable body-form localref-stack))
      ;; varref
      ((varref? body-form)
       (handle-varref body-form localref-stack))
      ;; primval : operations from run-time
      ((primval? body-form)
       (handle-primval body-form linklet?))
      ;; application
      ((application? body-form)
       (handle-application body-form localref-stack current-closure-refs linklet? importss))
      ;; def-values
      ((def-values? body-form)
       (handle-def-values body-form localref-stack current-closure-refs linklet? importss))
      ;; if
      ((branch? body-form)
       (handle-if body-form localref-stack current-closure-refs linklet?))
      ;; with-continuation-mark
      ((with-cont-mark? body-form)
       (handle-wcm body-form localref-stack current-closure-refs linklet?))
      ;; with-immed-mark
      ((with-immed-mark? body-form)
       (handle-immed-mark body-form localref-stack current-closure-refs linklet?))
      ;; boxenv
      ((boxenv? body-form)
       (handle-boxenv body-form localref-stack current-closure-refs linklet?))
      ;; apply-values
      ((apply-values? body-form)
       (handle-apply-values body-form localref-stack current-closure-refs linklet?))
      ;; localref
      ((localref? body-form)
       (handle-localref body-form localref-stack))
      ;; lambda
      ((lam? body-form)
       (handle-lambda body-form localref-stack current-closure-refs linklet? importss))
      ;; inline-variant (direct | inline)
      ((inline-variant? body-form)
       (handle-inline-variant body-form localref-stack current-closure-refs linklet?))
      ;; closure (procedure constant)
      ((closure? body-form)
       (handle-closure body-form localref-stack current-closure-refs linklet? importss))
      #;((topsyntax? body-form)
       (handle-topsyntax body-form localref-stack current-closure-refs))
      ;; prefab handling needs to stay here (b/c all the above are prefabs)
      ((prefab-struct-key body-form)
       (hash* 'quote (handle-prefab body-form linklet?)))
      (else (begin (display "-- NOT SUPPORTED YET: ")
                   (display body-form)
                   (newline)(newline)
                   "not supported yet")))))

(define (to-ast body-forms [linklet? #f] [importss '()])
  (map (lambda (form) (to-ast-single form '() '() linklet? importss)) body-forms))

(define (set-globals! debug mod-name mod-ext rel-current-dir test)
  (begin
    (set! DEBUG debug)
    (set! module-name mod-name)
    (set! module-extension mod-ext)
    (set! relative-current-dir rel-current-dir)
    (set! TEST test)
    (set! file-name mod-name)))

(define (set-toplevels! real-toplevels toplevels topsyntaxes)
  (set! initial-toplevels real-toplevels)
  (set! TOPLEVELS toplevels)
  (set! TOPSYNTAX topsyntaxes))

#;(define (to-ast-wrapper code toplevels topstxs debug mod-name relative-dir)
  (begin
    (set-globals! debug mod-name "rkt" relative-dir false)
    #;(set-toplevels! toplevels topstxs)
    ((expand-mod false) code)#;(to-ast body-forms)))

;; code-body : listof def-values
(define (prepare-toplevels code-body toplevels)
  (let* ([defvals (filter def-values? code-body)]
         [new-toplevels (collect-toplevels defvals toplevels)]) ;; <-- ((pos-num top-sym) ...)
    (if (null? new-toplevels)
        toplevels ;; don't bother
        (let*
            ([toplen (length toplevels)]
             ;; aligning new toplevels like ((30 x) (32 a) (35 b)) ==> ((30 x) (31 dummy) (32 a) (33 dummy) (34 dummy) (35 b))
             ;; we know that there's no reference in the code to the missing toplevels, but the position matters (for list-ref)
             ;; TODO : revisit : use a hashmap for toplevels
             [aligned-new-toplevels (foldr (λ (x rest)
                                             (if (or (null? rest)
                                                     (= 1 (- (caar rest) (car x))))
                                                 (cons x rest)
                                                 (let ([diff (- (caar rest) (car x) 1)])
                                                   (cons x (append (build-list diff (λ (n) (list (+ n x 1) 'dummytop))) rest))))) null new-toplevels)]

             [padded-new-toplevels (let* ([diff (- (caar aligned-new-toplevels) toplen)]
                                          [pad (build-list diff (λ (n) (list (+ n toplen) 'dummytop)))])
                                     (append pad aligned-new-toplevels))])
          
          ;; at this point, we know everything's in place,
          ;; so we can safely append prepared new-toplevels to the current global toplevels
          (let ([top-syms (map cadr padded-new-toplevels)])
            (append toplevels top-syms))))))

;; collect-toplevels : (listof def-values) -> (listof pos-num top-var-sym)
(define (collect-toplevels code-body toplevels)
  (sort
   (filter
    (compose not null?)
    (map (λ (defval)
           (let* ([def-ids (def-values-ids defval)]
                  [def-rhs (def-values-rhs defval)]
                  [poss (map toplevel-pos def-ids)]
                  [toplen (length toplevels)]) ; <- this is the real prefix-toplevels from comp-top
             (if (= (length poss) 1)
                 (if (< (car poss) toplen)
                     '()
                     (let* ([sym (let ([name (cond [(lam? def-rhs) (lam-name def-rhs)]
                                                   [(inline-variant? def-rhs) (lam-name (inline-variant-direct def-rhs))]
                                                   [else (error 'collect-toplevels "couldn't get the name from ~a" defval)])])
                                   (if (symbol? name) (gensym name) (gensym (vector-ref name 0))))]
                            [new-sym (string->symbol (string-append (symbol->string sym) "-lifted"))])
                       (begin
                         (set! LIFTEDS (cons new-sym LIFTEDS))
                         (list (car poss) new-sym))))
                 ;; we have multiple toplevels at the defval
                 (if (ormap (λ (pos) (>= pos toplen)) poss)
                     (error 'prepare-toplevels "one of the ids have >toplevel pos : ~a in defval : " poss defval)
                     ;; then all the top-posses are < toplen, thus we ignore
                     '()))))
         code-body))
   (λ (l1 l2) (< (car l1) (car l2)))))

#;(define (expand-mod submodule?)
  (lambda (code)  ;; code is a mod
    

    ;; 1) Dive in to the requires to gather info
    
    ;; requires
    (define top-reqs (mod-requires code)) ;; assoc list ((phase mods) ..)
    (define phase0 (assv 0 top-reqs))
    (define phase0-reqs (cdr phase0))

    (define module-name (let ([module-name (mod-name code)])
                          (cond
                            [(symbol? module-name) (symbol->string module-name)]
                            [(list? module-name) (symbol->string (last module-name))]
                            [else (error 'expand-mod "module-name cannot be determined : ~a" module-name)])))
    
    (define language (let ([lang* (module-path-index->path-string (car phase0-reqs))])
                       (cond
                         [(or (and (string? lang*)
                                   (string-contains? lang* "pycket/pycket-lang/main.rkt")) ;; cheat
                              (equal? lang* '("#%kernel"))) '("#%kernel")]
                         [(string? lang*) (list lang*)]
                         [else false])))
    ;(error 'expand-mod "language cannot be determined from this : ~a in module : ~a" lang* module-name)])))

    ;; 2) Now let's look at what we have in the required modules
    (define reqs (cdr phase0-reqs))

    (define top-level-req-forms
      (if (empty? reqs) reqs
          (list (hash* 'require 
                       (map (λ (req-mod)
                              (let ([mod-path (module-path-index->path-string req-mod)])
                                (if (list? mod-path) mod-path (list mod-path)))) reqs)))))


    ;; 3) Go with the provides (pycket doesn't care about it for now - mostly)
    
    ;; (define phase0-provides (assv 0 (mod-provides code)))
    ;; (define regular-provides (cadr phase0-provides))
    ;; (define syntax-phase-provides (caddr phase0-provides))
    
    (define top-provides* (apply append (cdr (assv 0 (mod-provides code)))))
    (define top-provide-names (map (λ (prov) (list (provided-name prov)
                                                   (provided-src-name prov))) top-provides*))
    (define (handle-provides provs out)
      (cond
        ((null? provs) out)
        (else (let*
                  ([pr (car provs)]
                   [out-name (car pr)]
                   [orig-name (cadr pr)])
                (handle-provides
                 (cdr provs)
                 (cons
                  (if (eqv? out-name orig-name)
                      ;; no rename-out
                      (hash* 'source-name (symbol->string orig-name)
                             'source-module (list
                                             (string-append relative-current-dir file-name ".rkt")
                                             module-name))
                      ;; rename-out
                      (list (hash* 'toplevel "rename")
                            (hash* 'source-name (symbol->string orig-name)
                                   'source-module
                                   (list (string-append relative-current-dir file-name ".rkt")))
                            (hash* 'toplevel (symbol->string out-name)))) out))))))
    
    #;(define all-provides '() #;(append regular-provides syntax-phase-provides))
    ;; removed handle-provides (since we don't care about the provides now)
    #;(define top-provides '() #;(if (not (empty? all-provides))
                                     (list (cons (hash* 'source-name "#%provide")
                                                 (handle-provides all-provides '())))
                                     '()))

    (define top-provides #;(handle-provides top-provide-names '())
      (let ([prepared (handle-provides top-provide-names '())])
        (if (empty? prepared) prepared 
            (list (cons (hash* 'source-name "#%provide")
                        prepared)))))

    (define top-reqs-provs (append top-level-req-forms top-provides))
    
    ;; 4) Then let's build our:
    (define pre-submods (map (expand-mod true) (mod-pre-submodules code))) ;; module-declared submods
    (define post-submods (map (expand-mod true) (mod-post-submodules code))) ;; module*-declared submods

    
    ;; 5) Set! the toplevel reference bucket
    (define toplevels (prefix-toplevels (mod-prefix code)))
    (define complete-toplevels (prepare-toplevels (mod-body code) toplevels))
    (define topsyntaxes (prefix-stxs (mod-prefix code)))

    ;; setting the global toplevels for the expansion of the body
    (set-toplevels! toplevels complete-toplevels topsyntaxes)    
    
    ;; 6) Now we can build the body
    (define body-forms (to-ast (mod-body code)))
    
    ;; 7) Stitch everything together
    (define whole-body (append top-reqs-provs pre-submods post-submods body-forms))

    (let* ([final-hash (hash* 'module-name module-name
                              'body-forms whole-body)]
           [final-hash (if language
                           (hash-set final-hash 'language language)
                           final-hash)]
           [final-hash (if (not submodule?)
                           (hash-set final-hash 'config (hash-set global-config 'bytecode-expand "true"))
                           final-hash)])
      final-hash)))


#;(module+ main
  (require racket/cmdline json compiler/cm)

  (define debug #f)
  (define sub-dirs-str #f)
  (define out #f)
  (define test #f)

  (define-syntax (get-extension stx)
    (syntax-case stx ()
      [(_ p)
       (if (identifier-binding #'path-get-extension)
           #'(path-get-extension p)
           (if (identifier-binding #'filename-extension)
               #'(filename-extension p)
               (error 'use-correct-extension "couldn't find a good extension getter")))]))

  (command-line
   #:once-each
   [("-v" "--verbose" "-d" "--debug") "show what you're doing" (set! debug #t)]
   [("--test") "running from test" (set! test #t)] ;; put ["."] to all source-module paths
   #:once-any
   [("--output") file "write output to output <file>"
    (set! out (open-output-file file #:exists 'replace))]
   [("--stdout") "write output to standart out" (set! out (current-output-port))]
   
   #:args (file.rkt)
   (let* ([p (string->path file.rkt)]
          [only-path-str (or (and (path-only p) (path->string (path-only p))) "")]
          [mod-name (let* ([fn-path (file-name-from-path p)]
                           [fn-str (if fn-path (path->string fn-path) (error 'zo-expand "check the filename ~a" file.rkt))])
                      (car (string-split fn-str ".")))]
          [mod-extension (let ([ext (bytes->string/utf-8 (get-extension p))])
                           (if (string-contains? ext ".") (string-replace ext "." "") ext))])
     (begin
       ;; setting the stage
       (set! sub-dirs-str only-path-str)
       (set-globals! debug mod-name mod-extension sub-dirs-str test)
       (system (format "raco make \"~a\"" file.rkt))
       #;(managed-compile-zo file.rkt)
       ;; setting the output port
       (when (not out)
         (set! out (open-output-file (format "~a~a.~a.json" sub-dirs-str mod-name mod-extension)
                                     #:exists 'replace))))))

  (define dep-file (read (open-input-file (format "~acompiled/~a_~a.dep" sub-dirs-str module-name module-extension))))
  
  (define version (car dep-file))
  
  (define comp-top (zo-parse (open-input-file (format "~acompiled/~a_~a.zo" sub-dirs-str module-name module-extension))))

  (define code (compilation-top-code comp-top)) ;; code is a mod


  (define final-json-hash ((expand-mod false) code))
  
  (begin
    (write-json final-json-hash out)
    (newline out)
    (flush-output out)))
