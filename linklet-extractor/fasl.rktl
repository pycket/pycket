;; This is not the original source code. Instead, this is the code after
;; fully expanding and flattening into a single linklet.
(linklet
 ()
 ((fasl->s-exp fasl->s-exp) (s-exp->fasl s-exp->fasl))
 (define-values (not-there) (gensym))
 (define-values
  (do-hash-update)
  (lambda (who_0 mut?_0 set_0 ht_0 key_0 xform_0 default_0)
    (begin
      (begin
        (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (let-values ()
            (begin
              (if (if (hash? ht_0) (if mut?_0 (not (immutable? ht_0)) (immutable? ht_0)) #f)
                (void)
                (let-values ()
                  (raise-argument-error
                   who_0
                   (if mut?_0 "(and/c hash? (not/c immutable?))" "(and/c hash? immutable?)")
                   ht_0)))
              (if (if (procedure? xform_0) (procedure-arity-includes? xform_0 1) #f)
                (void)
                (let-values () (raise-argument-error who_0 "(any/c . -> . any/c)" xform_0))))))
        (let-values (((v_0) (hash-ref ht_0 key_0 default_0)))
          (if (eq? v_0 not-there)
            (raise-mismatch-error who_0 "no value found for key: " key_0)
            (set_0 ht_0 key_0 (xform_0 v_0))))))))
 (define-values
  (hash-update!)
  (case-lambda
   ((ht_1 key_1 xform_1 default_1) (begin (do-hash-update 'hash-update! #t hash-set! ht_1 key_1 xform_1 default_1)))
   ((ht_2 key_2 xform_2) (hash-update! ht_2 key_2 xform_2 not-there))))
 (define-values
  (path-string?)
  (lambda (s_0)
    (begin
      (let-values (((or-part_0) (path? s_0)))
        (if or-part_0
          or-part_0
          (if (string? s_0)
            (let-values (((or-part_1) (relative-path? s_0))) (if or-part_1 or-part_1 (absolute-path? s_0)))
            #f))))))
 (define-values
  (prop:procedure-accessor procedure-accessor? procedure-accessor-ref)
  (make-struct-type-property
   'procedure
   (lambda (v_1 info-l_0) (if (exact-integer? v_1) (make-struct-field-accessor (list-ref info-l_0 3) v_1) #f))))
 (define-values
  (new-prop:procedure new-procedure? new-procedure-ref)
  (make-struct-type-property
   'procedure
   #f
   (list (cons prop:procedure values) (cons prop:procedure-accessor values))
   #t))
 (define-values
  (1/reverse)
  (lambda (l_0)
    (begin
      'reverse
      (begin
        (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (if (list? l_0) (void) (raise-argument-error 'reverse "list?" l_0)))
        (letrec-values (((loop_0)
                         (lambda (a_0 l_1) (begin 'loop (if (null? l_1) a_0 (loop_0 (cons (car l_1) a_0) (cdr l_1)))))))
          (loop_0 null l_0))))))
 (define-values
  (1/raise-argument-error)
  (lambda (who_1 . args_0) (begin 'raise-argument-error (apply raise-argument-error* who_1 'racket/primitive args_0))))
 (define-values
  (1/raise-arguments-error)
  (lambda (who_2 . args_1)
    (begin 'raise-arguments-error (apply raise-arguments-error* who_2 'racket/primitive args_1))))
 (define-values
  (1/raise-range-error)
  (lambda (who_3 . args_2) (begin 'raise-range-error (apply raise-range-error* who_3 'racket/primitive args_2))))
 (define-values
  (prop:stream stream-via-prop? stream-ref)
  (make-struct-type-property
   'stream
   (lambda (v_2 si_0)
     (begin
       (if (if (vector? v_2)
             (if (= 3 (vector-length v_2))
               (if (procedure? (vector-ref v_2 0))
                 (if (procedure-arity-includes? (vector-ref v_2 0) 1)
                   (if (procedure? (vector-ref v_2 1))
                     (if (procedure-arity-includes? (vector-ref v_2 1) 1)
                       (if (procedure? (vector-ref v_2 2)) (procedure-arity-includes? (vector-ref v_2 2) 1) #f)
                       #f)
                     #f)
                   #f)
                 #f)
               #f)
             #f)
         (void)
         (let-values ()
           (1/raise-argument-error
            'guard-for-prop:stream
            (string-append
             "(vector/c (procedure-arity-includes/c 1)\n"
             "          (procedure-arity-includes/c 1)\n"
             "          (procedure-arity-includes/c 1))")
            v_2)))
       (vector->immutable-vector v_2)))
   '()
   #t))
 (define-values
  (prop:gen-sequence sequence-via-prop? sequence-ref)
  (make-struct-type-property
   'sequence
   (lambda (v_3 si_1)
     (begin
       (if (if (procedure? v_3) (procedure-arity-includes? v_3 1) #f)
         (void)
         (let-values () (1/raise-argument-error 'guard-for-prop:sequence "(procedure-arity-includes/c 1)" v_3)))
       v_3))))
 (define-values
  (struct:range make-range range? range-ref range-set!)
  (make-struct-type
   'stream
   #f
   3
   0
   #f
   (list
    (cons
     prop:stream
     (vector
      (lambda (v_4) (let-values (((cont?_0) (range-ref v_4 2))) (if cont?_0 (not (cont?_0 (range-ref v_4 0))) #f)))
      (lambda (v_5) (range-ref v_5 0))
      (lambda (v_6) (make-range ((range-ref v_6 1) (range-ref v_6 0)) (range-ref v_6 1) (range-ref v_6 2)))))
    (cons
     prop:gen-sequence
     (lambda (v_7) (values values #f (range-ref v_7 1) (range-ref v_7 0) (range-ref v_7 2) #f #f))))))
 (define-values (check-range) (lambda (a_1 b_0 step_0) (begin (check-range-generic 'in-range a_1 b_0 step_0))))
 (define-values
  (check-range-generic)
  (lambda (who_4 a_2 b_1 step_1)
    (begin
      (begin
        (if (real? a_2) (void) (let-values () (1/raise-argument-error who_4 "real?" a_2)))
        (if (real? b_1) (void) (let-values () (1/raise-argument-error who_4 "real?" b_1)))
        (if (real? step_1) (void) (let-values () (1/raise-argument-error who_4 "real?" step_1)))))))
 (define-values
  (check-naturals)
  (lambda (n_0)
    (begin
      (if (if (integer? n_0) (if (exact? n_0) (>= n_0 0) #f) #f)
        (void)
        (let-values () (1/raise-argument-error 'in-naturals "exact-nonnegative-integer?" n_0))))))
 (define-values
  (struct:list-stream make-list-stream list-stream? list-stream-ref list-stream-set!)
  (make-struct-type
   'stream
   #f
   1
   0
   #f
   (list
    (cons
     prop:stream
     (vector
      (lambda (v_8) (not (pair? (list-stream-ref v_8 0))))
      (lambda (v_9) (car (list-stream-ref v_9 0)))
      (lambda (v_10) (make-list-stream (cdr (list-stream-ref v_10 0))))))
    (cons prop:gen-sequence (lambda (v_11) (values car cdr values (list-stream-ref v_11 0) pair? #f #f))))))
 (define-values
  (check-list)
  (lambda (l_2) (begin (if (list? l_2) (void) (let-values () (1/raise-argument-error 'in-list "list?" l_2))))))
 (define-values
  (check-ranges)
  (lambda (who_5 type-name_0 vec_0 start_0 stop_0 step_2 len_0)
    (begin
      (begin
        (if (exact-nonnegative-integer? start_0)
          (void)
          (let-values () (1/raise-argument-error who_5 "exact-nonnegative-integer?" start_0)))
        (if (let-values (((or-part_2) (< start_0 len_0))) (if or-part_2 or-part_2 (= len_0 start_0 stop_0)))
          (void)
          (let-values () (1/raise-range-error who_5 type-name_0 "starting " start_0 vec_0 0 (sub1 len_0))))
        (if (exact-integer? stop_0) (void) (let-values () (1/raise-argument-error who_5 "exact-integer?" stop_0)))
        (if (if (<= -1 stop_0) (<= stop_0 len_0) #f)
          (void)
          (let-values () (1/raise-range-error who_5 type-name_0 "stopping " stop_0 vec_0 -1 len_0)))
        (if (if (exact-integer? step_2) (not (zero? step_2)) #f)
          (void)
          (let-values () (1/raise-argument-error who_5 "(and/c exact-integer? (not/c zero?))" step_2)))
        (if (if (< start_0 stop_0) (< step_2 0) #f)
          (let-values ()
            (1/raise-arguments-error
             who_5
             "starting index less than stopping index, but given a negative step"
             "starting index"
             start_0
             "stopping index"
             stop_0
             "step"
             step_2))
          (void))
        (if (if (< stop_0 start_0) (> step_2 0) #f)
          (let-values ()
            (1/raise-arguments-error
             who_5
             "starting index more than stopping index, but given a positive step"
             "starting index"
             start_0
             "stopping index"
             stop_0
             "step"
             step_2))
          (void))))))
 (define-values
  (normalise-inputs)
  (lambda (who_6 type-name_1 vector?_0 unsafe-vector-length_0 vec_1 start_1 stop_1 step_3)
    (begin
      (begin
        (if (vector?_0 vec_1)
          (void)
          (let-values () (1/raise-argument-error who_6 (string-append type-name_1 "?") vec_1)))
        (let-values (((len_1) (unsafe-vector-length_0 vec_1)))
          (let-values (((stop*_0) (if stop_1 stop_1 len_1)))
            (begin
              (check-ranges who_6 type-name_1 vec_1 start_1 stop*_0 step_3 len_1)
              (values vec_1 start_1 stop*_0 step_3))))))))
 (define-values
  (unsafe-normalise-inputs)
  (lambda (unsafe-vector-length_1 vec_2 start_2 stop_2 step_4)
    (begin
      (values
       vec_2
       start_2
       (let-values (((or-part_3) stop_2)) (if or-part_3 or-part_3 (unsafe-vector-length_1 vec_2)))
       step_4))))
 (define-values
  (check-vector)
  (lambda (v_12)
    (begin
      (if (vector? v_12)
        (void)
        (let-values () (1/raise-argument-error 'in-vector (string-append "vector" "?") v_12))))))
 (define-values
  (struct:do-stream make-do-stream do-stream? do-stream-ref do-stream-set!)
  (make-struct-type
   'stream
   #f
   3
   0
   #f
   (list
    (cons
     prop:stream
     (vector
      (lambda (v_13) ((do-stream-ref v_13 0)))
      (lambda (v_14) ((do-stream-ref v_14 1)))
      (lambda (v_15) ((do-stream-ref v_15 2))))))))
 (define-values (empty-stream) (make-do-stream (lambda () #t) void void))
 (define-values
  (print-value-columns)
  (make-parameter
   +inf.0
   (lambda (c_0)
     (if (let-values (((or-part_0) (eqv? c_0 +inf.0))) (if or-part_0 or-part_0 (if (exact-integer? c_0) (> c_0 5) #f)))
       c_0
       (raise-argument-error 'print-value-columns "(or/c +inf.0 (and/c exact-integer? (>/c 5)))" c_0)))
   'print-value-columns))
 (define-values (kernel) (primitive-table '#%kernel))
 (define-values (1/syntax?) (hash-ref kernel 'syntax?))
 (define-values (1/syntax-e) (hash-ref kernel 'syntax-e))
 (define-values (1/datum->syntax) (hash-ref kernel 'datum->syntax))
 (define-values (1/syntax->datum) (hash-ref kernel 'syntax->datum))
 (define-values (1/syntax-property) (hash-ref kernel 'syntax-property))
 (define-values (1/syntax-property-symbol-keys) (hash-ref kernel 'syntax-property-symbol-keys))
 (define-values (1/syntax-source) (hash-ref kernel 'syntax-source))
 (define-values (1/syntax-line) (hash-ref kernel 'syntax-line))
 (define-values (1/syntax-column) (hash-ref kernel 'syntax-column))
 (define-values (1/syntax-position) (hash-ref kernel 'syntax-position))
 (define-values (1/syntax-span) (hash-ref kernel 'syntax-span))
 (define-values (correlated?) (lambda (e_0) (begin (1/syntax? e_0))))
 (define-values
  (datum->correlated)
  (let-values (((datum->correlated_0)
                (lambda (d3_0 srcloc1_0 props2_0)
                  (begin
                    'datum->correlated
                    (let-values (((d_0) d3_0))
                      (let-values (((srcloc_0) srcloc1_0))
                        (let-values (((props_0) props2_0))
                          (let-values () (1/datum->syntax #f d_0 srcloc_0 props_0)))))))))
    (case-lambda
     ((d_1) (begin (datum->correlated_0 d_1 #f #f)))
     ((d_2 srcloc_1 props2_1) (datum->correlated_0 d_2 srcloc_1 props2_1))
     ((d_3 srcloc1_1) (datum->correlated_0 d_3 srcloc1_1 #f)))))
 (define-values (correlated-e) (lambda (e_1) (begin (1/syntax-e e_1))))
 (define-values
  (correlated-property)
  (case-lambda ((e_2 k_0) (begin (1/syntax-property e_2 k_0))) ((e_3 k_1 v_16) (1/syntax-property e_3 k_1 v_16))))
 (define-values (correlated-property-symbol-keys) (lambda (e_4) (begin (1/syntax-property-symbol-keys e_4))))
 (define-values (correlated-source) (lambda (s_1) (begin (1/syntax-source s_1))))
 (define-values (correlated-line) (lambda (s_2) (begin (1/syntax-line s_2))))
 (define-values (correlated-column) (lambda (s_3) (begin (1/syntax-column s_3))))
 (define-values (correlated-position) (lambda (s_4) (begin (1/syntax-position s_4))))
 (define-values (correlated-span) (lambda (s_5) (begin (1/syntax-span s_5))))
 (define-values
  (check-fxvector)
  (lambda (v_17)
    (begin
      (if (fxvector? v_17)
        (void)
        (let-values () (1/raise-argument-error 'in-fxvector* (string-append "fxvector" "?") v_17))))))
 (define-values (not-an-fX.1$1) (lambda (who_7 v_18) (begin 'not-an-fX (raise-argument-error who_7 "fixnum?" v_18))))
 (define-values
  (check-flvector)
  (lambda (v_17)
    (begin
      (if (flvector? v_17)
        (void)
        (let-values () (1/raise-argument-error 'in-flvector* (string-append "flvector" "?") v_17))))))
 (define-values (not-an-fX.1) (lambda (who_7 v_18) (begin 'not-an-fX (raise-argument-error who_7 "flonum?" v_18))))
 (define-values
  (truncate-path)
  (lambda (p_0)
    (begin
      (let-values (((base1_0 name1_0 dir?_0) (split-path p_0)))
        (if (path-for-some-system? base1_0)
          (let-values ()
            (let-values (((base2_0 name2_0 dir?_1) (split-path base1_0)))
              (if (not base2_0)
                (let-values () (path-for-some-system->string p_0))
                (if (symbol? name2_0)
                  (let-values () (string-append ".../" (path-elem->string name1_0)))
                  (let-values ()
                    (string-append ".../" (path-for-some-system->string name2_0) "/" (path-elem->string name1_0)))))))
          (if (eq? base1_0 'relative)
            (let-values () (path-elem->string name1_0))
            (let-values () (path-for-some-system->string p_0))))))))
 (define-values
  (path-elem->string)
  (lambda (p_1)
    (begin
      (if (eq? p_1 'same)
        (let-values () ".")
        (if (eq? p_1 'up) (let-values () "..") (let-values () (path-for-some-system->string p_1)))))))
 (define-values
  (path-for-some-system->string)
  (lambda (p_2)
    (begin
      (if (path? p_2)
        (let-values () (path->string p_2))
        (let-values () (bytes->string/utf-8 (path->bytes p_2) '#\�))))))
 (define-values
  (make-path->relative-path-elements.1)
  (lambda (who1_0 wr-dir3_0)
    (begin
      'make-path->relative-path-elements
      (let-values (((wr-dir_0) (if (eq? wr-dir3_0 unsafe-undefined) (current-write-relative-directory) wr-dir3_0)))
        (let-values (((who_8) who1_0))
          (let-values ()
            (begin
              (if who_8
                (let-values ()
                  (if (let-values (((or-part_4) (not wr-dir_0)))
                        (if or-part_4
                          or-part_4
                          (let-values (((or-part_5) (if (path-string? wr-dir_0) (complete-path? wr-dir_0) #f)))
                            (if or-part_5
                              or-part_5
                              (if (pair? wr-dir_0)
                                (if (path-string? (car wr-dir_0))
                                  (if (complete-path? (car wr-dir_0))
                                    (if (path-string? (cdr wr-dir_0)) (complete-path? (cdr wr-dir_0)) #f)
                                    #f)
                                  #f)
                                #f)))))
                    (void)
                    (let-values ()
                      (raise-argument-error
                       who_8
                       (string-append
                        "(or/c (and/c path-string? complete-path?)\n"
                        "      (cons/c (and/c path-string? complete-path?)\n"
                        "              (and/c path-string? complete-path?))\n"
                        "      #f)")
                       wr-dir_0))))
                (void))
              (if (not wr-dir_0)
                (let-values () (lambda (v_19) #f))
                (let-values ()
                  (let-values (((exploded-base-dir_0) 'not-ready))
                    (let-values (((exploded-wrt-rel-dir_0) 'not-ready))
                      (lambda (v_20)
                        (begin
                          (if (if (eq? exploded-base-dir_0 'not-ready) (path? v_20) #f)
                            (let-values ()
                              (let-values (((wrt-dir_0) (if wr-dir_0 (if (pair? wr-dir_0) (car wr-dir_0) wr-dir_0) #f)))
                                (let-values (((exploded-wrt-dir_0) (explode-path wrt-dir_0)))
                                  (let-values (((base-dir_0)
                                                (if wr-dir_0 (if (pair? wr-dir_0) (cdr wr-dir_0) wr-dir_0) #f)))
                                    (begin
                                      (set! exploded-base-dir_0 (if base-dir_0 (explode-path base-dir_0) #f))
                                      (set! exploded-wrt-rel-dir_0
                                        (if (eq? base-dir_0 wrt-dir_0)
                                          (let-values () '())
                                          (let-values ()
                                            (let-values (((exploded-wrt-dir_1) (explode-path wrt-dir_0)))
                                              (let-values (((base-len_0) (length exploded-base-dir_0)))
                                                (begin
                                                  (if who_8
                                                    (let-values ()
                                                      (if (if (>= (length exploded-wrt-dir_1) base-len_0)
                                                            (let-values (((result_0)
                                                                          (let-values (((result_1) #t)) result_1)))
                                                              (let-values (((lst_0) exploded-wrt-dir_1)
                                                                           ((lst_1) exploded-base-dir_0))
                                                                (begin
                                                                  (if (variable-reference-from-unsafe?
                                                                       (#%variable-reference))
                                                                    (void)
                                                                    (let-values () (check-list lst_0)))
                                                                  (if (variable-reference-from-unsafe?
                                                                       (#%variable-reference))
                                                                    (void)
                                                                    (let-values () (check-list lst_1)))
                                                                  ((letrec-values (((for-loop_0)
                                                                                    (lambda (result_2 lst_2 lst_3)
                                                                                      (begin
                                                                                        'for-loop
                                                                                        (let-values ()
                                                                                          (if (if (pair? lst_2)
                                                                                                (pair? lst_3)
                                                                                                #f)
                                                                                            (let-values (((a_3)
                                                                                                          (unsafe-car
                                                                                                           lst_2))
                                                                                                         ((rest_0)
                                                                                                          (unsafe-cdr
                                                                                                           lst_2))
                                                                                                         ((b_2)
                                                                                                          (unsafe-car
                                                                                                           lst_3))
                                                                                                         ((rest_1)
                                                                                                          (unsafe-cdr
                                                                                                           lst_3)))
                                                                                              (let-values (((result_3)
                                                                                                            (let-values (((result_4)
                                                                                                                          (let-values ()
                                                                                                                            (let-values ()
                                                                                                                              (equal?
                                                                                                                               a_3
                                                                                                                               b_2)))))
                                                                                                              (values
                                                                                                               result_4))))
                                                                                                (if (if (not
                                                                                                         ((lambda x_0
                                                                                                            (not
                                                                                                             result_3))
                                                                                                          a_3))
                                                                                                      (if (not
                                                                                                           ((lambda x_1
                                                                                                              (not
                                                                                                               result_3))
                                                                                                            b_2))
                                                                                                        (not #f)
                                                                                                        #f)
                                                                                                      #f)
                                                                                                  (for-loop_0
                                                                                                   result_3
                                                                                                   rest_0
                                                                                                   rest_1)
                                                                                                  result_3)))
                                                                                            result_2))))))
                                                                     for-loop_0)
                                                                   result_0
                                                                   lst_0
                                                                   lst_1))))
                                                            #f)
                                                        (void)
                                                        (let-values ()
                                                          (raise-arguments-error
                                                           who_8
                                                           "relative-directory pair's first path does not extend second path"
                                                           "first path"
                                                           wrt-dir_0
                                                           "second path"
                                                           base-dir_0))))
                                                    (void))
                                                  (list-tail exploded-wrt-dir_1 base-len_0))))))))))))
                            (void))
                          (if exploded-base-dir_0
                            (if (path? v_20)
                              (let-values (((exploded_0) (explode-path v_20)))
                                (if (let-values (((result_5) (let-values (((result_6) #t)) result_6)))
                                      (let-values (((lst_4) exploded-base-dir_0) ((lst_5) exploded_0))
                                        (begin
                                          (if (variable-reference-from-unsafe? (#%variable-reference))
                                            (void)
                                            (let-values () (check-list lst_4)))
                                          (if (variable-reference-from-unsafe? (#%variable-reference))
                                            (void)
                                            (let-values () (check-list lst_5)))
                                          ((letrec-values (((for-loop_1)
                                                            (lambda (result_7 lst_6 lst_7)
                                                              (begin
                                                                'for-loop
                                                                (let-values ()
                                                                  (if (if (pair? lst_6) (pair? lst_7) #f)
                                                                    (let-values (((base-p_0) (unsafe-car lst_6))
                                                                                 ((rest_2) (unsafe-cdr lst_6))
                                                                                 ((p_3) (unsafe-car lst_7))
                                                                                 ((rest_3) (unsafe-cdr lst_7)))
                                                                      (let-values (((result_8)
                                                                                    (let-values (((result_9)
                                                                                                  (let-values ()
                                                                                                    (let-values ()
                                                                                                      (equal?
                                                                                                       base-p_0
                                                                                                       p_3)))))
                                                                                      (values result_9))))
                                                                        (if (if (not
                                                                                 ((lambda x_2 (not result_8)) base-p_0))
                                                                              (if (not
                                                                                   ((lambda x_3 (not result_8)) p_3))
                                                                                (not #f)
                                                                                #f)
                                                                              #f)
                                                                          (for-loop_1 result_8 rest_2 rest_3)
                                                                          result_8)))
                                                                    result_7))))))
                                             for-loop_1)
                                           result_5
                                           lst_4
                                           lst_5))))
                                  (if (>= (length exploded_0) (length exploded-base-dir_0))
                                    ((letrec-values (((loop_1)
                                                      (lambda (exploded-wrt-rel-dir_1 rel_0)
                                                        (begin
                                                          'loop
                                                          (if (null? exploded-wrt-rel-dir_1)
                                                            (let-values ()
                                                              (1/reverse
                                                               (let-values (((fold-var_0)
                                                                             (let-values (((fold-var_1) null))
                                                                               fold-var_1)))
                                                                 (let-values (((lst_8) rel_0))
                                                                   (begin
                                                                     (if (variable-reference-from-unsafe?
                                                                          (#%variable-reference))
                                                                       (void)
                                                                       (let-values () (check-list lst_8)))
                                                                     ((letrec-values (((for-loop_2)
                                                                                       (lambda (fold-var_2 lst_9)
                                                                                         (begin
                                                                                           'for-loop
                                                                                           (let-values ()
                                                                                             (if (pair? lst_9)
                                                                                               (let-values (((p_4)
                                                                                                             (unsafe-car
                                                                                                              lst_9))
                                                                                                            ((rest_4)
                                                                                                             (unsafe-cdr
                                                                                                              lst_9)))
                                                                                                 (let-values (((fold-var_3)
                                                                                                               (let-values (((fold-var_4)
                                                                                                                             (let-values ()
                                                                                                                               (cons
                                                                                                                                (let-values ()
                                                                                                                                  (if (path?
                                                                                                                                       p_4)
                                                                                                                                    (path-element->bytes
                                                                                                                                     p_4)
                                                                                                                                    p_4))
                                                                                                                                fold-var_2))))
                                                                                                                 (values
                                                                                                                  fold-var_4))))
                                                                                                   (if (not #f)
                                                                                                     (for-loop_2
                                                                                                      fold-var_3
                                                                                                      rest_4)
                                                                                                     fold-var_3)))
                                                                                               fold-var_2))))))
                                                                        for-loop_2)
                                                                      fold-var_0
                                                                      lst_8))))))
                                                            (if (if (pair? rel_0)
                                                                  (equal? (car rel_0) (car exploded-wrt-rel-dir_1))
                                                                  #f)
                                                              (let-values ()
                                                                (loop_1 (cdr exploded-wrt-rel-dir_1) (cdr rel_0)))
                                                              (let-values ()
                                                                (append
                                                                 (1/reverse
                                                                  (let-values (((fold-var_5)
                                                                                (let-values (((fold-var_6) null))
                                                                                  fold-var_6)))
                                                                    (let-values (((lst_10) exploded-wrt-rel-dir_1))
                                                                      (begin
                                                                        (if (variable-reference-from-unsafe?
                                                                             (#%variable-reference))
                                                                          (void)
                                                                          (let-values () (check-list lst_10)))
                                                                        ((letrec-values (((for-loop_3)
                                                                                          (lambda (fold-var_7 lst_11)
                                                                                            (begin
                                                                                              'for-loop
                                                                                              (let-values ()
                                                                                                (if (pair? lst_11)
                                                                                                  (let-values (((p_5)
                                                                                                                (unsafe-car
                                                                                                                 lst_11))
                                                                                                               ((rest_5)
                                                                                                                (unsafe-cdr
                                                                                                                 lst_11)))
                                                                                                    (let-values (((fold-var_8)
                                                                                                                  (let-values (((fold-var_9)
                                                                                                                                (let-values ()
                                                                                                                                  (cons
                                                                                                                                   (let-values ()
                                                                                                                                     'up)
                                                                                                                                   fold-var_7))))
                                                                                                                    (values
                                                                                                                     fold-var_9))))
                                                                                                      (if (not #f)
                                                                                                        (for-loop_3
                                                                                                         fold-var_8
                                                                                                         rest_5)
                                                                                                        fold-var_8)))
                                                                                                  fold-var_7))))))
                                                                           for-loop_3)
                                                                         fold-var_5
                                                                         lst_10)))))
                                                                 (1/reverse
                                                                  (let-values (((fold-var_10)
                                                                                (let-values (((fold-var_11) null))
                                                                                  fold-var_11)))
                                                                    (let-values (((lst_12) rel_0))
                                                                      (begin
                                                                        (if (variable-reference-from-unsafe?
                                                                             (#%variable-reference))
                                                                          (void)
                                                                          (let-values () (check-list lst_12)))
                                                                        ((letrec-values (((for-loop_4)
                                                                                          (lambda (fold-var_12 lst_13)
                                                                                            (begin
                                                                                              'for-loop
                                                                                              (let-values ()
                                                                                                (if (pair? lst_13)
                                                                                                  (let-values (((p_6)
                                                                                                                (unsafe-car
                                                                                                                 lst_13))
                                                                                                               ((rest_6)
                                                                                                                (unsafe-cdr
                                                                                                                 lst_13)))
                                                                                                    (let-values (((fold-var_13)
                                                                                                                  (let-values (((fold-var_14)
                                                                                                                                (let-values ()
                                                                                                                                  (cons
                                                                                                                                   (let-values ()
                                                                                                                                     (if (path?
                                                                                                                                          p_6)
                                                                                                                                       (path-element->bytes
                                                                                                                                        p_6)
                                                                                                                                       p_6))
                                                                                                                                   fold-var_12))))
                                                                                                                    (values
                                                                                                                     fold-var_14))))
                                                                                                      (if (not #f)
                                                                                                        (for-loop_4
                                                                                                         fold-var_13
                                                                                                         rest_6)
                                                                                                        fold-var_13)))
                                                                                                  fold-var_12))))))
                                                                           for-loop_4)
                                                                         fold-var_10
                                                                         lst_12)))))))))))))
                                       loop_1)
                                     exploded-wrt-rel-dir_0
                                     (list-tail exploded_0 (length exploded-base-dir_0)))
                                    #f)
                                  #f))
                              #f)
                            #f))))))))))))))
 (define-values (1/write-byte) (lambda (byte_0 out_0) (begin 'write-byte (write-byte byte_0 out_0))))
 (define-values
  (1/write-bytes)
  (let-values (((write-bytes_0)
                (lambda (bstr3_0 out4_0 start-pos1_0 end-pos2_0)
                  (begin
                    'write-bytes
                    (let-values (((bstr_0) bstr3_0))
                      (let-values (((out_1) out4_0))
                        (let-values (((start-pos_0) start-pos1_0))
                          (let-values (((end-pos_0)
                                        (if (eq? end-pos2_0 unsafe-undefined) (bytes-length bstr_0) end-pos2_0)))
                            (let-values () (write-bytes bstr_0 out_1 start-pos_0 end-pos_0))))))))))
    (case-lambda
     ((bstr_1 out_2) (begin 'write-bytes (write-bytes_0 bstr_1 out_2 0 unsafe-undefined)))
     ((bstr_2 out_3 start-pos_1 end-pos2_1) (write-bytes_0 bstr_2 out_3 start-pos_1 end-pos2_1))
     ((bstr_3 out_4 start-pos1_1) (write-bytes_0 bstr_3 out_4 start-pos1_1 unsafe-undefined)))))
 (define-values (fasl-graph-def-type) 1)
 (define-values (fasl-graph-ref-type) 2)
 (define-values (fasl-false-type) 3)
 (define-values (fasl-true-type) 4)
 (define-values (fasl-null-type) 5)
 (define-values (fasl-void-type) 6)
 (define-values (fasl-eof-type) 7)
 (define-values (fasl-integer-type) 8)
 (define-values (fasl-flonum-type) 9)
 (define-values (fasl-single-flonum-type) 10)
 (define-values (fasl-rational-type) 11)
 (define-values (fasl-complex-type) 12)
 (define-values (fasl-char-type) 13)
 (define-values (fasl-symbol-type) 14)
 (define-values (fasl-unreadable-symbol-type) 15)
 (define-values (fasl-uninterned-symbol-type) 16)
 (define-values (fasl-keyword-type) 17)
 (define-values (fasl-string-type) 18)
 (define-values (fasl-immutable-string-type) 19)
 (define-values (fasl-bytes-type) 20)
 (define-values (fasl-immutable-bytes-type) 21)
 (define-values (fasl-path-type) 22)
 (define-values (fasl-relative-path-type) 23)
 (define-values (fasl-pregexp-type) 24)
 (define-values (fasl-regexp-type) 25)
 (define-values (fasl-byte-pregexp-type) 26)
 (define-values (fasl-byte-regexp-type) 27)
 (define-values (fasl-list-type) 28)
 (define-values (fasl-list*-type) 29)
 (define-values (fasl-pair-type) 30)
 (define-values (fasl-vector-type) 31)
 (define-values (fasl-immutable-vector-type) 32)
 (define-values (fasl-box-type) 33)
 (define-values (fasl-immutable-box-type) 34)
 (define-values (fasl-prefab-type) 35)
 (define-values (fasl-hash-type) 36)
 (define-values (fasl-immutable-hash-type) 37)
 (define-values (fasl-srcloc-type) 38)
 (define-values (fasl-extflonum-type) 39)
 (define-values (fasl-correlated-type) 40)
 (define-values (fasl-undefined-type) 41)
 (define-values (fasl-prefab-type-type) 42)
 (define-values (fasl-fxvector-type) 43)
 (define-values (fasl-flvector-type) 44)
 (define-values (fasl-small-integer-start) 100)
 (define-values (fasl-lowest-small-integer) -10)
 (define-values (fasl-highest-small-integer) (- 255 (- fasl-small-integer-start fasl-lowest-small-integer) 1))
 (define-values (fasl-prefix) #"racket/fasl:")
 (define-values (fasl-prefix-length) (bytes-length fasl-prefix))
 (define-values (fasl-hash-eq-variant) 0)
 (define-values (fasl-hash-equal-variant) 1)
 (define-values (fasl-hash-eqv-variant) 2)
 (define-values (fasl-hash-equal-always-variant) 3)
 (define-values
  (s-exp->fasl.1)
  (lambda (external-lift?7_0 handle-fail6_0 keep-mutable?5_0 skip-prefix?8_0 v14_0 orig-o13_0)
    (begin
      's-exp->fasl
      (let-values (((v_21) v14_0))
        (let-values (((orig-o_0) orig-o13_0))
          (let-values (((keep-mutable?_0) keep-mutable?5_0))
            (let-values (((handle-fail_0) handle-fail6_0))
              (let-values (((external-lift?_0) external-lift?7_0))
                (let-values (((skip-prefix?_0) skip-prefix?8_0))
                  (let-values ()
                    (let-values ((()
                                  (begin
                                    (if orig-o_0
                                      (let-values ()
                                        (if (output-port? orig-o_0)
                                          (void)
                                          (let-values ()
                                            (raise-argument-error 's-exp->fasl "(or/c output-port? #f)" orig-o_0))))
                                      (void))
                                    (values))))
                      (let-values ((()
                                    (begin
                                      (if handle-fail_0
                                        (let-values ()
                                          (if (if (procedure? handle-fail_0)
                                                (procedure-arity-includes? handle-fail_0 1)
                                                #f)
                                            (void)
                                            (let-values ()
                                              (raise-argument-error
                                               's-exp->fasl
                                               "(or/c (procedure-arity-includes/c 1) #f)"
                                               handle-fail_0))))
                                        (void))
                                      (values))))
                        (let-values ((()
                                      (begin
                                        (if external-lift?_0
                                          (let-values ()
                                            (if (if (procedure? external-lift?_0)
                                                  (procedure-arity-includes? external-lift?_0 1)
                                                  #f)
                                              (void)
                                              (let-values ()
                                                (raise-argument-error
                                                 's-exp->fasl
                                                 "(or/c (procedure-arity-includes/c 1) #f)"
                                                 external-lift?_0))))
                                          (void))
                                        (values))))
                          (let-values (((o_0)
                                        (let-values (((or-part_6) orig-o_0))
                                          (if or-part_6 or-part_6 (open-output-bytes)))))
                            (let-values (((shared_0) (make-hasheq)))
                              (let-values (((external-lift_0) (if external-lift?_0 (make-hasheq) #f)))
                                (let-values (((shared-counter_0) 0))
                                  (let-values ((()
                                                (begin
                                                  ((letrec-values (((loop_2)
                                                                    (lambda (v_22)
                                                                      (begin
                                                                        'loop
                                                                        (if (if external-lift_0
                                                                              (hash-ref external-lift_0 v_22 #f)
                                                                              #f)
                                                                          (let-values () (void))
                                                                          (if (if external-lift?_0
                                                                                (external-lift?_0 v_22)
                                                                                #f)
                                                                            (let-values ()
                                                                              (begin
                                                                                (hash-set! external-lift_0 v_22 #t)
                                                                                (set! shared-counter_0
                                                                                  (add1 shared-counter_0))
                                                                                (hash-set!
                                                                                 shared_0
                                                                                 v_22
                                                                                 (- shared-counter_0))))
                                                                            (if (let-values (((or-part_7)
                                                                                              (symbol? v_22)))
                                                                                  (if or-part_7
                                                                                    or-part_7
                                                                                    (let-values (((or-part_8)
                                                                                                  (keyword? v_22)))
                                                                                      (if or-part_8
                                                                                        or-part_8
                                                                                        (let-values (((or-part_9)
                                                                                                      (string? v_22)))
                                                                                          (if or-part_9
                                                                                            or-part_9
                                                                                            (let-values (((or-part_10)
                                                                                                          (bytes?
                                                                                                           v_22)))
                                                                                              (if or-part_10
                                                                                                or-part_10
                                                                                                (let-values (((or-part_11)
                                                                                                              (fxvector?
                                                                                                               v_22)))
                                                                                                  (if or-part_11
                                                                                                    or-part_11
                                                                                                    (let-values (((or-part_12)
                                                                                                                  (flvector?
                                                                                                                   v_22)))
                                                                                                      (if or-part_12
                                                                                                        or-part_12
                                                                                                        (path?
                                                                                                         v_22)))))))))))))
                                                                              (let-values ()
                                                                                (hash-update! shared_0 v_22 add1 0))
                                                                              (if (pair? v_22)
                                                                                (let-values ()
                                                                                  (begin
                                                                                    (loop_2 (car v_22))
                                                                                    (loop_2 (cdr v_22))))
                                                                                (if (vector? v_22)
                                                                                  (let-values ()
                                                                                    (begin
                                                                                      (let-values ()
                                                                                        (let-values (((vec_3 len_2)
                                                                                                      (let-values (((vec_4)
                                                                                                                    v_22))
                                                                                                        (begin
                                                                                                          (if (variable-reference-from-unsafe?
                                                                                                               (#%variable-reference))
                                                                                                            (void)
                                                                                                            (let-values ()
                                                                                                              (check-vector
                                                                                                               vec_4)))
                                                                                                          (values
                                                                                                           vec_4
                                                                                                           (unsafe-vector-length
                                                                                                            vec_4))))))
                                                                                          ((letrec-values (((for-loop_5)
                                                                                                            (lambda (pos_0)
                                                                                                              (begin
                                                                                                                'for-loop
                                                                                                                (if (unsafe-fx<
                                                                                                                     pos_0
                                                                                                                     len_2)
                                                                                                                  (let-values (((e_5)
                                                                                                                                (unsafe-vector-ref
                                                                                                                                 vec_3
                                                                                                                                 pos_0)))
                                                                                                                    (let-values ((()
                                                                                                                                  (let-values ((()
                                                                                                                                                (let-values ()
                                                                                                                                                  (begin
                                                                                                                                                    (let-values ()
                                                                                                                                                      (loop_2
                                                                                                                                                       e_5))
                                                                                                                                                    (values)))))
                                                                                                                                    (values))))
                                                                                                                      (if (not
                                                                                                                           #f)
                                                                                                                        (for-loop_5
                                                                                                                         (unsafe-fx+
                                                                                                                          1
                                                                                                                          pos_0))
                                                                                                                        (values))))
                                                                                                                  (values))))))
                                                                                             for-loop_5)
                                                                                           0)))
                                                                                      (void)))
                                                                                  (if (hash? v_22)
                                                                                    (let-values ()
                                                                                      (hash-for-each
                                                                                       v_22
                                                                                       (lambda (k_2 v_23)
                                                                                         (begin
                                                                                           (loop_2 k_2)
                                                                                           (loop_2 v_23)))
                                                                                       #t))
                                                                                    (if (box? v_22)
                                                                                      (let-values ()
                                                                                        (loop_2 (unbox v_22)))
                                                                                      (let-values (((c2_0)
                                                                                                    (prefab-struct-key
                                                                                                     v_22)))
                                                                                        (if c2_0
                                                                                          ((lambda (k_3)
                                                                                             (begin
                                                                                               (loop_2 k_3)
                                                                                               (let-values ()
                                                                                                 (let-values (((v*_0
                                                                                                                start*_0
                                                                                                                stop*_1
                                                                                                                step*_0)
                                                                                                               (if (variable-reference-from-unsafe?
                                                                                                                    (#%variable-reference))
                                                                                                                 (unsafe-normalise-inputs
                                                                                                                  unsafe-vector-length
                                                                                                                  (struct->vector
                                                                                                                   v_22)
                                                                                                                  1
                                                                                                                  #f
                                                                                                                  1)
                                                                                                                 (normalise-inputs
                                                                                                                  'in-vector
                                                                                                                  "vector"
                                                                                                                  (lambda (x_4)
                                                                                                                    (vector?
                                                                                                                     x_4))
                                                                                                                  (lambda (x_5)
                                                                                                                    (unsafe-vector-length
                                                                                                                     x_5))
                                                                                                                  (struct->vector
                                                                                                                   v_22)
                                                                                                                  1
                                                                                                                  #f
                                                                                                                  1))))
                                                                                                   ((letrec-values (((for-loop_6)
                                                                                                                     (lambda (idx_0)
                                                                                                                       (begin
                                                                                                                         'for-loop
                                                                                                                         (if (unsafe-fx<
                                                                                                                              idx_0
                                                                                                                              stop*_1)
                                                                                                                           (let-values (((e_6)
                                                                                                                                         (unsafe-vector-ref
                                                                                                                                          v*_0
                                                                                                                                          idx_0)))
                                                                                                                             (let-values ((()
                                                                                                                                           (let-values ((()
                                                                                                                                                         (let-values ()
                                                                                                                                                           (begin
                                                                                                                                                             (let-values ()
                                                                                                                                                               (loop_2
                                                                                                                                                                e_6))
                                                                                                                                                             (values)))))
                                                                                                                                             (values))))
                                                                                                                               (if (not
                                                                                                                                    #f)
                                                                                                                                 (for-loop_6
                                                                                                                                  (unsafe-fx+
                                                                                                                                   idx_0
                                                                                                                                   1))
                                                                                                                                 (values))))
                                                                                                                           (values))))))
                                                                                                      for-loop_6)
                                                                                                    start*_0)))
                                                                                               (void)))
                                                                                           c2_0)
                                                                                          (if (srcloc? v_22)
                                                                                            (let-values ()
                                                                                              (loop_2
                                                                                               (srcloc-source v_22)))
                                                                                            (if (correlated? v_22)
                                                                                              (let-values ()
                                                                                                (begin
                                                                                                  (loop_2
                                                                                                   (correlated-e v_22))
                                                                                                  (loop_2
                                                                                                   (correlated-source
                                                                                                    v_22))
                                                                                                  (let-values ()
                                                                                                    (let-values (((lst_14)
                                                                                                                  (correlated-property-symbol-keys
                                                                                                                   v_22)))
                                                                                                      (begin
                                                                                                        (if (variable-reference-from-unsafe?
                                                                                                             (#%variable-reference))
                                                                                                          (void)
                                                                                                          (let-values ()
                                                                                                            (check-list
                                                                                                             lst_14)))
                                                                                                        ((letrec-values (((for-loop_7)
                                                                                                                          (lambda (lst_15)
                                                                                                                            (begin
                                                                                                                              'for-loop
                                                                                                                              (if (pair?
                                                                                                                                   lst_15)
                                                                                                                                (let-values (((k_4)
                                                                                                                                              (unsafe-car
                                                                                                                                               lst_15))
                                                                                                                                             ((rest_7)
                                                                                                                                              (unsafe-cdr
                                                                                                                                               lst_15)))
                                                                                                                                  (let-values ((()
                                                                                                                                                (let-values ((()
                                                                                                                                                              (let-values ()
                                                                                                                                                                (begin
                                                                                                                                                                  (let-values ()
                                                                                                                                                                    (begin
                                                                                                                                                                      (loop_2
                                                                                                                                                                       k_4)
                                                                                                                                                                      (loop_2
                                                                                                                                                                       (correlated-property
                                                                                                                                                                        v_22
                                                                                                                                                                        k_4))))
                                                                                                                                                                  (values)))))
                                                                                                                                                  (values))))
                                                                                                                                    (if (not
                                                                                                                                         #f)
                                                                                                                                      (for-loop_7
                                                                                                                                       rest_7)
                                                                                                                                      (values))))
                                                                                                                                (values))))))
                                                                                                           for-loop_7)
                                                                                                         lst_14))))
                                                                                                  (void)))
                                                                                              (let-values (((c1_0)
                                                                                                            (if (struct-type?
                                                                                                                 v_22)
                                                                                                              (prefab-struct-type-key+field-count
                                                                                                               v_22)
                                                                                                              #f)))
                                                                                                (if c1_0
                                                                                                  ((lambda (k+c_0)
                                                                                                     (begin
                                                                                                       (loop_2
                                                                                                        (car k+c_0))
                                                                                                       (loop_2
                                                                                                        (cdr k+c_0))))
                                                                                                   c1_0)
                                                                                                  (let-values ()
                                                                                                    (void)))))))))))))))))))
                                                     loop_2)
                                                   v_21)
                                                  (values))))
                                    (let-values (((treat-immutable?_0)
                                                  (lambda (v_24)
                                                    (begin
                                                      'treat-immutable?
                                                      (let-values (((or-part_13) (not keep-mutable?_0)))
                                                        (if or-part_13 or-part_13 (immutable? v_24)))))))
                                      (let-values (((path->relative-path-elements_0)
                                                    (let-values ()
                                                      (make-path->relative-path-elements.1 #f unsafe-undefined))))
                                        (let-values ((()
                                                      (begin
                                                        (if skip-prefix?_0
                                                          (void)
                                                          (let-values () (1/write-bytes fasl-prefix o_0)))
                                                        (values))))
                                          (let-values (((bstr_4)
                                                        (let-values (((o_1) (open-output-bytes)))
                                                          (begin
                                                            ((letrec-values (((loop_3)
                                                                              (lambda (v_25)
                                                                                (begin
                                                                                  'loop
                                                                                  (if (not
                                                                                       (eq?
                                                                                        (hash-ref shared_0 v_25 1)
                                                                                        1))
                                                                                    (let-values ()
                                                                                      (let-values (((c_1)
                                                                                                    (hash-ref
                                                                                                     shared_0
                                                                                                     v_25)))
                                                                                        (if (negative? c_1)
                                                                                          (let-values ()
                                                                                            (begin
                                                                                              (1/write-byte
                                                                                               fasl-graph-ref-type
                                                                                               o_1)
                                                                                              (write-fasl-integer
                                                                                               (sub1 (- c_1))
                                                                                               o_1)))
                                                                                          (let-values ()
                                                                                            (let-values (((pos_1)
                                                                                                          shared-counter_0))
                                                                                              (begin
                                                                                                (set! shared-counter_0
                                                                                                  (add1
                                                                                                   shared-counter_0))
                                                                                                (1/write-byte
                                                                                                 fasl-graph-def-type
                                                                                                 o_1)
                                                                                                (write-fasl-integer
                                                                                                 pos_1
                                                                                                 o_1)
                                                                                                (hash-remove!
                                                                                                 shared_0
                                                                                                 v_25)
                                                                                                (loop_3 v_25)
                                                                                                (hash-set!
                                                                                                 shared_0
                                                                                                 v_25
                                                                                                 (- (add1 pos_1)))))))))
                                                                                    (if (not v_25)
                                                                                      (let-values ()
                                                                                        (1/write-byte
                                                                                         fasl-false-type
                                                                                         o_1))
                                                                                      (if (eq? v_25 #t)
                                                                                        (let-values ()
                                                                                          (1/write-byte
                                                                                           fasl-true-type
                                                                                           o_1))
                                                                                        (if (null? v_25)
                                                                                          (let-values ()
                                                                                            (1/write-byte
                                                                                             fasl-null-type
                                                                                             o_1))
                                                                                          (if (void? v_25)
                                                                                            (let-values ()
                                                                                              (1/write-byte
                                                                                               fasl-void-type
                                                                                               o_1))
                                                                                            (if (eof-object? v_25)
                                                                                              (let-values ()
                                                                                                (1/write-byte
                                                                                                 fasl-eof-type
                                                                                                 o_1))
                                                                                              (if (exact-integer? v_25)
                                                                                                (let-values ()
                                                                                                  (if (<=
                                                                                                       fasl-lowest-small-integer
                                                                                                       v_25
                                                                                                       fasl-highest-small-integer)
                                                                                                    (let-values ()
                                                                                                      (1/write-byte
                                                                                                       (+
                                                                                                        fasl-small-integer-start
                                                                                                        (-
                                                                                                         v_25
                                                                                                         fasl-lowest-small-integer))
                                                                                                       o_1))
                                                                                                    (let-values ()
                                                                                                      (begin
                                                                                                        (1/write-byte
                                                                                                         fasl-integer-type
                                                                                                         o_1)
                                                                                                        (write-fasl-integer
                                                                                                         v_25
                                                                                                         o_1)))))
                                                                                                (if (flonum? v_25)
                                                                                                  (let-values ()
                                                                                                    (begin
                                                                                                      (1/write-byte
                                                                                                       fasl-flonum-type
                                                                                                       o_1)
                                                                                                      (write-fasl-flonum
                                                                                                       v_25
                                                                                                       o_1)))
                                                                                                  (if (single-flonum?
                                                                                                       v_25)
                                                                                                    (let-values ()
                                                                                                      (begin
                                                                                                        (1/write-byte
                                                                                                         fasl-single-flonum-type
                                                                                                         o_1)
                                                                                                        (1/write-bytes
                                                                                                         (if (eqv?
                                                                                                              v_25
                                                                                                              (real->single-flonum
                                                                                                               +nan.0))
                                                                                                           #"\0\0\300\177"
                                                                                                           (real->floating-point-bytes
                                                                                                            v_25
                                                                                                            4
                                                                                                            #f))
                                                                                                         o_1)))
                                                                                                    (if (extflonum?
                                                                                                         v_25)
                                                                                                      (let-values ()
                                                                                                        (let-values ((()
                                                                                                                      (begin
                                                                                                                        (1/write-byte
                                                                                                                         fasl-extflonum-type
                                                                                                                         o_1)
                                                                                                                        (values))))
                                                                                                          (let-values (((bstr_5)
                                                                                                                        (string->bytes/utf-8
                                                                                                                         (format
                                                                                                                          "~a"
                                                                                                                          v_25))))
                                                                                                            (begin
                                                                                                              (write-fasl-integer
                                                                                                               (bytes-length
                                                                                                                bstr_5)
                                                                                                               o_1)
                                                                                                              (1/write-bytes
                                                                                                               bstr_5
                                                                                                               o_1)))))
                                                                                                      (if (rational?
                                                                                                           v_25)
                                                                                                        (let-values ()
                                                                                                          (begin
                                                                                                            (1/write-byte
                                                                                                             fasl-rational-type
                                                                                                             o_1)
                                                                                                            (loop_3
                                                                                                             (numerator
                                                                                                              v_25))
                                                                                                            (loop_3
                                                                                                             (denominator
                                                                                                              v_25))))
                                                                                                        (if (complex?
                                                                                                             v_25)
                                                                                                          (let-values ()
                                                                                                            (begin
                                                                                                              (1/write-byte
                                                                                                               fasl-complex-type
                                                                                                               o_1)
                                                                                                              (loop_3
                                                                                                               (real-part
                                                                                                                v_25))
                                                                                                              (loop_3
                                                                                                               (imag-part
                                                                                                                v_25))))
                                                                                                          (if (char?
                                                                                                               v_25)
                                                                                                            (let-values ()
                                                                                                              (begin
                                                                                                                (1/write-byte
                                                                                                                 fasl-char-type
                                                                                                                 o_1)
                                                                                                                (write-fasl-integer
                                                                                                                 (char->integer
                                                                                                                  v_25)
                                                                                                                 o_1)))
                                                                                                            (if (symbol?
                                                                                                                 v_25)
                                                                                                              (let-values ()
                                                                                                                (let-values ((()
                                                                                                                              (begin
                                                                                                                                (if (symbol-interned?
                                                                                                                                     v_25)
                                                                                                                                  (let-values ()
                                                                                                                                    (1/write-byte
                                                                                                                                     fasl-symbol-type
                                                                                                                                     o_1))
                                                                                                                                  (if (symbol-unreadable?
                                                                                                                                       v_25)
                                                                                                                                    (let-values ()
                                                                                                                                      (1/write-byte
                                                                                                                                       fasl-unreadable-symbol-type
                                                                                                                                       o_1))
                                                                                                                                    (let-values ()
                                                                                                                                      (1/write-byte
                                                                                                                                       fasl-uninterned-symbol-type
                                                                                                                                       o_1))))
                                                                                                                                (values))))
                                                                                                                  (let-values (((bstr_6)
                                                                                                                                (string->bytes/utf-8
                                                                                                                                 (symbol->string
                                                                                                                                  v_25))))
                                                                                                                    (begin
                                                                                                                      (write-fasl-integer
                                                                                                                       (bytes-length
                                                                                                                        bstr_6)
                                                                                                                       o_1)
                                                                                                                      (1/write-bytes
                                                                                                                       bstr_6
                                                                                                                       o_1)))))
                                                                                                              (if (keyword?
                                                                                                                   v_25)
                                                                                                                (let-values ()
                                                                                                                  (let-values ((()
                                                                                                                                (begin
                                                                                                                                  (1/write-byte
                                                                                                                                   fasl-keyword-type
                                                                                                                                   o_1)
                                                                                                                                  (values))))
                                                                                                                    (let-values (((bstr_7)
                                                                                                                                  (string->bytes/utf-8
                                                                                                                                   (keyword->string
                                                                                                                                    v_25))))
                                                                                                                      (begin
                                                                                                                        (write-fasl-integer
                                                                                                                         (bytes-length
                                                                                                                          bstr_7)
                                                                                                                         o_1)
                                                                                                                        (1/write-bytes
                                                                                                                         bstr_7
                                                                                                                         o_1)))))
                                                                                                                (if (string?
                                                                                                                     v_25)
                                                                                                                  (let-values ()
                                                                                                                    (begin
                                                                                                                      (write-fasl-integer
                                                                                                                       (if (treat-immutable?_0
                                                                                                                            v_25)
                                                                                                                         fasl-immutable-string-type
                                                                                                                         fasl-string-type)
                                                                                                                       o_1)
                                                                                                                      (write-fasl-string
                                                                                                                       v_25
                                                                                                                       o_1)))
                                                                                                                  (if (bytes?
                                                                                                                       v_25)
                                                                                                                    (let-values ()
                                                                                                                      (begin
                                                                                                                        (write-fasl-integer
                                                                                                                         (if (treat-immutable?_0
                                                                                                                              v_25)
                                                                                                                           fasl-immutable-bytes-type
                                                                                                                           fasl-bytes-type)
                                                                                                                         o_1)
                                                                                                                        (write-fasl-bytes
                                                                                                                         v_25
                                                                                                                         o_1)))
                                                                                                                    (if (path-for-some-system?
                                                                                                                         v_25)
                                                                                                                      (let-values ()
                                                                                                                        (let-values (((rel-elems_0)
                                                                                                                                      (path->relative-path-elements_0
                                                                                                                                       v_25)))
                                                                                                                          (if rel-elems_0
                                                                                                                            (let-values ()
                                                                                                                              (begin
                                                                                                                                (1/write-byte
                                                                                                                                 fasl-relative-path-type
                                                                                                                                 o_1)
                                                                                                                                (loop_3
                                                                                                                                 rel-elems_0)))
                                                                                                                            (let-values ()
                                                                                                                              (begin
                                                                                                                                (1/write-byte
                                                                                                                                 fasl-path-type
                                                                                                                                 o_1)
                                                                                                                                (write-fasl-bytes
                                                                                                                                 (path->bytes
                                                                                                                                  v_25)
                                                                                                                                 o_1)
                                                                                                                                (loop_3
                                                                                                                                 (path-convention-type
                                                                                                                                  v_25)))))))
                                                                                                                      (if (if (srcloc?
                                                                                                                               v_25)
                                                                                                                            (let-values (((src_0)
                                                                                                                                          (srcloc-source
                                                                                                                                           v_25)))
                                                                                                                              (let-values (((or-part_14)
                                                                                                                                            (not
                                                                                                                                             src_0)))
                                                                                                                                (if or-part_14
                                                                                                                                  or-part_14
                                                                                                                                  (let-values (((or-part_15)
                                                                                                                                                (path-for-some-system?
                                                                                                                                                 src_0)))
                                                                                                                                    (if or-part_15
                                                                                                                                      or-part_15
                                                                                                                                      (let-values (((or-part_16)
                                                                                                                                                    (string?
                                                                                                                                                     src_0)))
                                                                                                                                        (if or-part_16
                                                                                                                                          or-part_16
                                                                                                                                          (let-values (((or-part_17)
                                                                                                                                                        (bytes?
                                                                                                                                                         src_0)))
                                                                                                                                            (if or-part_17
                                                                                                                                              or-part_17
                                                                                                                                              (symbol?
                                                                                                                                               src_0))))))))))
                                                                                                                            #f)
                                                                                                                        (let-values ()
                                                                                                                          (let-values (((src_1)
                                                                                                                                        (srcloc-source
                                                                                                                                         v_25)))
                                                                                                                            (let-values (((new-src_0)
                                                                                                                                          (if (if (path?
                                                                                                                                                   src_1)
                                                                                                                                                (not
                                                                                                                                                 (path->relative-path-elements_0
                                                                                                                                                  src_1))
                                                                                                                                                #f)
                                                                                                                                            (let-values ()
                                                                                                                                              (truncate-path
                                                                                                                                               src_1))
                                                                                                                                            (let-values ()
                                                                                                                                              src_1))))
                                                                                                                              (begin
                                                                                                                                (write-fasl-integer
                                                                                                                                 fasl-srcloc-type
                                                                                                                                 o_1)
                                                                                                                                (loop_3
                                                                                                                                 new-src_0)
                                                                                                                                (loop_3
                                                                                                                                 (srcloc-line
                                                                                                                                  v_25))
                                                                                                                                (loop_3
                                                                                                                                 (srcloc-column
                                                                                                                                  v_25))
                                                                                                                                (loop_3
                                                                                                                                 (srcloc-position
                                                                                                                                  v_25))
                                                                                                                                (loop_3
                                                                                                                                 (srcloc-span
                                                                                                                                  v_25))))))
                                                                                                                        (if (pair?
                                                                                                                             v_25)
                                                                                                                          (let-values ()
                                                                                                                            (if (pair?
                                                                                                                                 (cdr
                                                                                                                                  v_25))
                                                                                                                              (let-values ()
                                                                                                                                (let-values (((n_1
                                                                                                                                               normal-list?_0)
                                                                                                                                              ((letrec-values (((loop_4)
                                                                                                                                                                (lambda (v_26
                                                                                                                                                                         len_3)
                                                                                                                                                                  (begin
                                                                                                                                                                    'loop
                                                                                                                                                                    (if (null?
                                                                                                                                                                         v_26)
                                                                                                                                                                      (let-values ()
                                                                                                                                                                        (values
                                                                                                                                                                         len_3
                                                                                                                                                                         #t))
                                                                                                                                                                      (if (pair?
                                                                                                                                                                           v_26)
                                                                                                                                                                        (let-values ()
                                                                                                                                                                          (loop_4
                                                                                                                                                                           (cdr
                                                                                                                                                                            v_26)
                                                                                                                                                                           (add1
                                                                                                                                                                            len_3)))
                                                                                                                                                                        (let-values ()
                                                                                                                                                                          (values
                                                                                                                                                                           len_3
                                                                                                                                                                           #f))))))))
                                                                                                                                                 loop_4)
                                                                                                                                               v_25
                                                                                                                                               0)))
                                                                                                                                  (begin
                                                                                                                                    (1/write-byte
                                                                                                                                     (if normal-list?_0
                                                                                                                                       fasl-list-type
                                                                                                                                       fasl-list*-type)
                                                                                                                                     o_1)
                                                                                                                                    (write-fasl-integer
                                                                                                                                     n_1
                                                                                                                                     o_1)
                                                                                                                                    ((letrec-values (((ploop_0)
                                                                                                                                                      (lambda (v_27)
                                                                                                                                                        (begin
                                                                                                                                                          'ploop
                                                                                                                                                          (if (pair?
                                                                                                                                                               v_27)
                                                                                                                                                            (let-values ()
                                                                                                                                                              (begin
                                                                                                                                                                (loop_3
                                                                                                                                                                 (car
                                                                                                                                                                  v_27))
                                                                                                                                                                (ploop_0
                                                                                                                                                                 (cdr
                                                                                                                                                                  v_27))))
                                                                                                                                                            (let-values ()
                                                                                                                                                              (if normal-list?_0
                                                                                                                                                                (void)
                                                                                                                                                                (let-values ()
                                                                                                                                                                  (loop_3
                                                                                                                                                                   v_27)))))))))
                                                                                                                                       ploop_0)
                                                                                                                                     v_25))))
                                                                                                                              (let-values ()
                                                                                                                                (begin
                                                                                                                                  (1/write-byte
                                                                                                                                   fasl-pair-type
                                                                                                                                   o_1)
                                                                                                                                  (loop_3
                                                                                                                                   (car
                                                                                                                                    v_25))
                                                                                                                                  (loop_3
                                                                                                                                   (cdr
                                                                                                                                    v_25))))))
                                                                                                                          (if (vector?
                                                                                                                               v_25)
                                                                                                                            (let-values ()
                                                                                                                              (begin
                                                                                                                                (1/write-byte
                                                                                                                                 (if (treat-immutable?_0
                                                                                                                                      v_25)
                                                                                                                                   fasl-immutable-vector-type
                                                                                                                                   fasl-vector-type)
                                                                                                                                 o_1)
                                                                                                                                (write-fasl-integer
                                                                                                                                 (vector-length
                                                                                                                                  v_25)
                                                                                                                                 o_1)
                                                                                                                                (let-values ()
                                                                                                                                  (let-values (((vec_5
                                                                                                                                                 len_4)
                                                                                                                                                (let-values (((vec_6)
                                                                                                                                                              v_25))
                                                                                                                                                  (begin
                                                                                                                                                    (if (variable-reference-from-unsafe?
                                                                                                                                                         (#%variable-reference))
                                                                                                                                                      (void)
                                                                                                                                                      (let-values ()
                                                                                                                                                        (check-vector
                                                                                                                                                         vec_6)))
                                                                                                                                                    (values
                                                                                                                                                     vec_6
                                                                                                                                                     (unsafe-vector-length
                                                                                                                                                      vec_6))))))
                                                                                                                                    ((letrec-values (((for-loop_8)
                                                                                                                                                      (lambda (pos_2)
                                                                                                                                                        (begin
                                                                                                                                                          'for-loop
                                                                                                                                                          (if (unsafe-fx<
                                                                                                                                                               pos_2
                                                                                                                                                               len_4)
                                                                                                                                                            (let-values (((e_7)
                                                                                                                                                                          (unsafe-vector-ref
                                                                                                                                                                           vec_5
                                                                                                                                                                           pos_2)))
                                                                                                                                                              (let-values ((()
                                                                                                                                                                            (let-values ((()
                                                                                                                                                                                          (let-values ()
                                                                                                                                                                                            (begin
                                                                                                                                                                                              (let-values ()
                                                                                                                                                                                                (loop_3
                                                                                                                                                                                                 e_7))
                                                                                                                                                                                              (values)))))
                                                                                                                                                                              (values))))
                                                                                                                                                                (if (not
                                                                                                                                                                     #f)
                                                                                                                                                                  (for-loop_8
                                                                                                                                                                   (unsafe-fx+
                                                                                                                                                                    1
                                                                                                                                                                    pos_2))
                                                                                                                                                                  (values))))
                                                                                                                                                            (values))))))
                                                                                                                                       for-loop_8)
                                                                                                                                     0)))
                                                                                                                                (void)))
                                                                                                                            (if (flvector?
                                                                                                                                 v_25)
                                                                                                                              (let-values ()
                                                                                                                                (begin
                                                                                                                                  (1/write-byte
                                                                                                                                   fasl-flvector-type
                                                                                                                                   o_1)
                                                                                                                                  (write-fasl-integer
                                                                                                                                   (flvector-length
                                                                                                                                    v_25)
                                                                                                                                   o_1)
                                                                                                                                  (let-values ()
                                                                                                                                    (let-values (((vec_7
                                                                                                                                                   len_5)
                                                                                                                                                  (let-values (((vec_8)
                                                                                                                                                                v_25))
                                                                                                                                                    (begin
                                                                                                                                                      (if (variable-reference-from-unsafe?
                                                                                                                                                           (#%variable-reference))
                                                                                                                                                        (void)
                                                                                                                                                        (let-values ()
                                                                                                                                                          (check-flvector
                                                                                                                                                           vec_8)))
                                                                                                                                                      (values
                                                                                                                                                       vec_8
                                                                                                                                                       (unsafe-flvector-length
                                                                                                                                                        vec_8))))))
                                                                                                                                      ((letrec-values (((for-loop_9)
                                                                                                                                                        (lambda (pos_3)
                                                                                                                                                          (begin
                                                                                                                                                            'for-loop
                                                                                                                                                            (if (unsafe-fx<
                                                                                                                                                                 pos_3
                                                                                                                                                                 len_5)
                                                                                                                                                              (let-values (((e_8)
                                                                                                                                                                            (unsafe-flvector-ref
                                                                                                                                                                             vec_7
                                                                                                                                                                             pos_3)))
                                                                                                                                                                (let-values ((()
                                                                                                                                                                              (let-values ((()
                                                                                                                                                                                            (let-values ()
                                                                                                                                                                                              (begin
                                                                                                                                                                                                (let-values ()
                                                                                                                                                                                                  (write-fasl-flonum
                                                                                                                                                                                                   e_8
                                                                                                                                                                                                   o_1))
                                                                                                                                                                                                (values)))))
                                                                                                                                                                                (values))))
                                                                                                                                                                  (if (not
                                                                                                                                                                       #f)
                                                                                                                                                                    (for-loop_9
                                                                                                                                                                     (unsafe-fx+
                                                                                                                                                                      1
                                                                                                                                                                      pos_3))
                                                                                                                                                                    (values))))
                                                                                                                                                              (values))))))
                                                                                                                                         for-loop_9)
                                                                                                                                       0)))
                                                                                                                                  (void)))
                                                                                                                              (if (fxvector?
                                                                                                                                   v_25)
                                                                                                                                (let-values ()
                                                                                                                                  (begin
                                                                                                                                    (1/write-byte
                                                                                                                                     fasl-fxvector-type
                                                                                                                                     o_1)
                                                                                                                                    (write-fasl-integer
                                                                                                                                     (fxvector-length
                                                                                                                                      v_25)
                                                                                                                                     o_1)
                                                                                                                                    (let-values ()
                                                                                                                                      (let-values (((vec_9
                                                                                                                                                     len_6)
                                                                                                                                                    (let-values (((vec_10)
                                                                                                                                                                  v_25))
                                                                                                                                                      (begin
                                                                                                                                                        (if (variable-reference-from-unsafe?
                                                                                                                                                             (#%variable-reference))
                                                                                                                                                          (void)
                                                                                                                                                          (let-values ()
                                                                                                                                                            (check-fxvector
                                                                                                                                                             vec_10)))
                                                                                                                                                        (values
                                                                                                                                                         vec_10
                                                                                                                                                         (unsafe-fxvector-length
                                                                                                                                                          vec_10))))))
                                                                                                                                        ((letrec-values (((for-loop_10)
                                                                                                                                                          (lambda (pos_4)
                                                                                                                                                            (begin
                                                                                                                                                              'for-loop
                                                                                                                                                              (if (unsafe-fx<
                                                                                                                                                                   pos_4
                                                                                                                                                                   len_6)
                                                                                                                                                                (let-values (((e_9)
                                                                                                                                                                              (unsafe-fxvector-ref
                                                                                                                                                                               vec_9
                                                                                                                                                                               pos_4)))
                                                                                                                                                                  (let-values ((()
                                                                                                                                                                                (let-values ((()
                                                                                                                                                                                              (let-values ()
                                                                                                                                                                                                (begin
                                                                                                                                                                                                  (let-values ()
                                                                                                                                                                                                    (write-fasl-integer
                                                                                                                                                                                                     e_9
                                                                                                                                                                                                     o_1))
                                                                                                                                                                                                  (values)))))
                                                                                                                                                                                  (values))))
                                                                                                                                                                    (if (not
                                                                                                                                                                         #f)
                                                                                                                                                                      (for-loop_10
                                                                                                                                                                       (unsafe-fx+
                                                                                                                                                                        1
                                                                                                                                                                        pos_4))
                                                                                                                                                                      (values))))
                                                                                                                                                                (values))))))
                                                                                                                                           for-loop_10)
                                                                                                                                         0)))
                                                                                                                                    (void)))
                                                                                                                                (if (box?
                                                                                                                                     v_25)
                                                                                                                                  (let-values ()
                                                                                                                                    (begin
                                                                                                                                      (1/write-byte
                                                                                                                                       (if (treat-immutable?_0
                                                                                                                                            v_25)
                                                                                                                                         fasl-immutable-box-type
                                                                                                                                         fasl-box-type)
                                                                                                                                       o_1)
                                                                                                                                      (loop_3
                                                                                                                                       (unbox
                                                                                                                                        v_25))))
                                                                                                                                  (let-values (((c4_0)
                                                                                                                                                (prefab-struct-key
                                                                                                                                                 v_25)))
                                                                                                                                    (if c4_0
                                                                                                                                      ((lambda (k_5)
                                                                                                                                         (let-values ((()
                                                                                                                                                       (begin
                                                                                                                                                         (1/write-byte
                                                                                                                                                          fasl-prefab-type
                                                                                                                                                          o_1)
                                                                                                                                                         (values))))
                                                                                                                                           (let-values ((()
                                                                                                                                                         (begin
                                                                                                                                                           (loop_3
                                                                                                                                                            k_5)
                                                                                                                                                           (values))))
                                                                                                                                             (let-values (((vec_11)
                                                                                                                                                           (struct->vector
                                                                                                                                                            v_25)))
                                                                                                                                               (begin
                                                                                                                                                 (write-fasl-integer
                                                                                                                                                  (sub1
                                                                                                                                                   (vector-length
                                                                                                                                                    vec_11))
                                                                                                                                                  o_1)
                                                                                                                                                 (let-values ()
                                                                                                                                                   (let-values (((v*_1
                                                                                                                                                                  start*_1
                                                                                                                                                                  stop*_2
                                                                                                                                                                  step*_1)
                                                                                                                                                                 (if (variable-reference-from-unsafe?
                                                                                                                                                                      (#%variable-reference))
                                                                                                                                                                   (unsafe-normalise-inputs
                                                                                                                                                                    unsafe-vector-length
                                                                                                                                                                    vec_11
                                                                                                                                                                    1
                                                                                                                                                                    #f
                                                                                                                                                                    1)
                                                                                                                                                                   (normalise-inputs
                                                                                                                                                                    'in-vector
                                                                                                                                                                    "vector"
                                                                                                                                                                    (lambda (x_6)
                                                                                                                                                                      (vector?
                                                                                                                                                                       x_6))
                                                                                                                                                                    (lambda (x_7)
                                                                                                                                                                      (unsafe-vector-length
                                                                                                                                                                       x_7))
                                                                                                                                                                    vec_11
                                                                                                                                                                    1
                                                                                                                                                                    #f
                                                                                                                                                                    1))))
                                                                                                                                                     ((letrec-values (((for-loop_11)
                                                                                                                                                                       (lambda (idx_1)
                                                                                                                                                                         (begin
                                                                                                                                                                           'for-loop
                                                                                                                                                                           (if (unsafe-fx<
                                                                                                                                                                                idx_1
                                                                                                                                                                                stop*_2)
                                                                                                                                                                             (let-values (((e_10)
                                                                                                                                                                                           (unsafe-vector-ref
                                                                                                                                                                                            v*_1
                                                                                                                                                                                            idx_1)))
                                                                                                                                                                               (let-values ((()
                                                                                                                                                                                             (let-values ((()
                                                                                                                                                                                                           (let-values ()
                                                                                                                                                                                                             (begin
                                                                                                                                                                                                               (let-values ()
                                                                                                                                                                                                                 (loop_3
                                                                                                                                                                                                                  e_10))
                                                                                                                                                                                                               (values)))))
                                                                                                                                                                                               (values))))
                                                                                                                                                                                 (if (not
                                                                                                                                                                                      #f)
                                                                                                                                                                                   (for-loop_11
                                                                                                                                                                                    (unsafe-fx+
                                                                                                                                                                                     idx_1
                                                                                                                                                                                     1))
                                                                                                                                                                                   (values))))
                                                                                                                                                                             (values))))))
                                                                                                                                                        for-loop_11)
                                                                                                                                                      start*_1)))
                                                                                                                                                 (void))))))
                                                                                                                                       c4_0)
                                                                                                                                      (if (hash?
                                                                                                                                           v_25)
                                                                                                                                        (let-values ()
                                                                                                                                          (begin
                                                                                                                                            (1/write-byte
                                                                                                                                             (if (treat-immutable?_0
                                                                                                                                                  v_25)
                                                                                                                                               fasl-immutable-hash-type
                                                                                                                                               fasl-hash-type)
                                                                                                                                             o_1)
                                                                                                                                            (1/write-byte
                                                                                                                                             (if (hash-eq?
                                                                                                                                                  v_25)
                                                                                                                                               (let-values ()
                                                                                                                                                 fasl-hash-eq-variant)
                                                                                                                                               (if (hash-eqv?
                                                                                                                                                    v_25)
                                                                                                                                                 (let-values ()
                                                                                                                                                   fasl-hash-eqv-variant)
                                                                                                                                                 (if (hash-equal-always?
                                                                                                                                                      v_25)
                                                                                                                                                   (let-values ()
                                                                                                                                                     fasl-hash-equal-always-variant)
                                                                                                                                                   (let-values ()
                                                                                                                                                     fasl-hash-equal-variant))))
                                                                                                                                             o_1)
                                                                                                                                            (write-fasl-integer
                                                                                                                                             (hash-count
                                                                                                                                              v_25)
                                                                                                                                             o_1)
                                                                                                                                            (hash-for-each
                                                                                                                                             v_25
                                                                                                                                             (lambda (k_6
                                                                                                                                                      v_28)
                                                                                                                                               (begin
                                                                                                                                                 (loop_3
                                                                                                                                                  k_6)
                                                                                                                                                 (loop_3
                                                                                                                                                  v_28)))
                                                                                                                                             #t)))
                                                                                                                                        (if (regexp?
                                                                                                                                             v_25)
                                                                                                                                          (let-values ()
                                                                                                                                            (begin
                                                                                                                                              (1/write-byte
                                                                                                                                               (if (pregexp?
                                                                                                                                                    v_25)
                                                                                                                                                 fasl-pregexp-type
                                                                                                                                                 fasl-regexp-type)
                                                                                                                                               o_1)
                                                                                                                                              (write-fasl-string
                                                                                                                                               (object-name
                                                                                                                                                v_25)
                                                                                                                                               o_1)))
                                                                                                                                          (if (byte-regexp?
                                                                                                                                               v_25)
                                                                                                                                            (let-values ()
                                                                                                                                              (begin
                                                                                                                                                (1/write-byte
                                                                                                                                                 (if (byte-pregexp?
                                                                                                                                                      v_25)
                                                                                                                                                   fasl-byte-pregexp-type
                                                                                                                                                   fasl-byte-regexp-type)
                                                                                                                                                 o_1)
                                                                                                                                                (write-fasl-bytes
                                                                                                                                                 (object-name
                                                                                                                                                  v_25)
                                                                                                                                                 o_1)))
                                                                                                                                            (if (correlated?
                                                                                                                                                 v_25)
                                                                                                                                              (let-values ()
                                                                                                                                                (begin
                                                                                                                                                  (1/write-byte
                                                                                                                                                   fasl-correlated-type
                                                                                                                                                   o_1)
                                                                                                                                                  (loop_3
                                                                                                                                                   (correlated-e
                                                                                                                                                    v_25))
                                                                                                                                                  (loop_3
                                                                                                                                                   (srcloc
                                                                                                                                                    (correlated-source
                                                                                                                                                     v_25)
                                                                                                                                                    (correlated-line
                                                                                                                                                     v_25)
                                                                                                                                                    (correlated-column
                                                                                                                                                     v_25)
                                                                                                                                                    (correlated-position
                                                                                                                                                     v_25)
                                                                                                                                                    (correlated-span
                                                                                                                                                     v_25)))
                                                                                                                                                  (loop_3
                                                                                                                                                   (1/reverse
                                                                                                                                                    (let-values (((fold-var_15)
                                                                                                                                                                  (let-values (((fold-var_16)
                                                                                                                                                                                null))
                                                                                                                                                                    fold-var_16)))
                                                                                                                                                      (let-values (((lst_16)
                                                                                                                                                                    (correlated-property-symbol-keys
                                                                                                                                                                     v_25)))
                                                                                                                                                        (begin
                                                                                                                                                          (if (variable-reference-from-unsafe?
                                                                                                                                                               (#%variable-reference))
                                                                                                                                                            (void)
                                                                                                                                                            (let-values ()
                                                                                                                                                              (check-list
                                                                                                                                                               lst_16)))
                                                                                                                                                          ((letrec-values (((for-loop_12)
                                                                                                                                                                            (lambda (fold-var_17
                                                                                                                                                                                     lst_17)
                                                                                                                                                                              (begin
                                                                                                                                                                                'for-loop
                                                                                                                                                                                (let-values ()
                                                                                                                                                                                  (if (pair?
                                                                                                                                                                                       lst_17)
                                                                                                                                                                                    (let-values (((k_7)
                                                                                                                                                                                                  (unsafe-car
                                                                                                                                                                                                   lst_17))
                                                                                                                                                                                                 ((rest_8)
                                                                                                                                                                                                  (unsafe-cdr
                                                                                                                                                                                                   lst_17)))
                                                                                                                                                                                      (let-values (((fold-var_18)
                                                                                                                                                                                                    (let-values (((fold-var_19)
                                                                                                                                                                                                                  (let-values ()
                                                                                                                                                                                                                    (cons
                                                                                                                                                                                                                     (let-values ()
                                                                                                                                                                                                                       (cons
                                                                                                                                                                                                                        k_7
                                                                                                                                                                                                                        (correlated-property
                                                                                                                                                                                                                         v_25
                                                                                                                                                                                                                         k_7)))
                                                                                                                                                                                                                     fold-var_17))))
                                                                                                                                                                                                      (values
                                                                                                                                                                                                       fold-var_19))))
                                                                                                                                                                                        (if (not
                                                                                                                                                                                             #f)
                                                                                                                                                                                          (for-loop_12
                                                                                                                                                                                           fold-var_18
                                                                                                                                                                                           rest_8)
                                                                                                                                                                                          fold-var_18)))
                                                                                                                                                                                    fold-var_17))))))
                                                                                                                                                             for-loop_12)
                                                                                                                                                           fold-var_15
                                                                                                                                                           lst_16))))))))
                                                                                                                                              (if (eq?
                                                                                                                                                   v_25
                                                                                                                                                   unsafe-undefined)
                                                                                                                                                (let-values ()
                                                                                                                                                  (1/write-byte
                                                                                                                                                   fasl-undefined-type
                                                                                                                                                   o_1))
                                                                                                                                                (let-values (((c3_0)
                                                                                                                                                              (if (struct-type?
                                                                                                                                                                   v_25)
                                                                                                                                                                (prefab-struct-type-key+field-count
                                                                                                                                                                 v_25)
                                                                                                                                                                #f)))
                                                                                                                                                  (if c3_0
                                                                                                                                                    ((lambda (k+c_1)
                                                                                                                                                       (begin
                                                                                                                                                         (1/write-byte
                                                                                                                                                          fasl-prefab-type-type
                                                                                                                                                          o_1)
                                                                                                                                                         (loop_3
                                                                                                                                                          (car
                                                                                                                                                           k+c_1))
                                                                                                                                                         (loop_3
                                                                                                                                                          (cdr
                                                                                                                                                           k+c_1))))
                                                                                                                                                     c3_0)
                                                                                                                                                    (let-values ()
                                                                                                                                                      (if handle-fail_0
                                                                                                                                                        (loop_3
                                                                                                                                                         (handle-fail_0
                                                                                                                                                          v_25))
                                                                                                                                                        (raise-arguments-error
                                                                                                                                                         's-exp->fasl
                                                                                                                                                         "cannot write value"
                                                                                                                                                         "value"
                                                                                                                                                         v_25))))))))))))))))))))))))))))))))))))))))
                                                               loop_3)
                                                             v_21)
                                                            (get-output-bytes o_1 #t)))))
                                            (begin
                                              (write-fasl-integer shared-counter_0 o_0)
                                              (write-fasl-integer (bytes-length bstr_4) o_0)
                                              (1/write-bytes bstr_4 o_0)
                                              (if orig-o_0 (void) (get-output-bytes o_0)))))))))))))))))))))))))
 (define-values
  (fasl->s-exp.1)
  (lambda (datum-intern?16_0 external-lifts17_0 skip-prefix?18_0 orig-i22_0)
    (begin
      'fasl->s-exp
      (let-values (((orig-i_0) orig-i22_0))
        (let-values (((intern?_0) datum-intern?16_0))
          (let-values (((external-lifts_0) (if (eq? external-lifts17_0 unsafe-undefined) '#() external-lifts17_0)))
            (let-values (((skip-prefix?_1) skip-prefix?18_0))
              (let-values ()
                (let-values (((init-i_0)
                              (if (bytes? orig-i_0)
                                (let-values () (mcons orig-i_0 0))
                                (if (input-port? orig-i_0)
                                  (let-values () orig-i_0)
                                  (let-values ()
                                    (raise-argument-error 'fasl->s-exp "(or/c bytes? input-port?)" orig-i_0))))))
                  (let-values ((()
                                (begin
                                  (if skip-prefix?_1
                                    (void)
                                    (let-values ()
                                      (if (bytes=? (read-bytes/exactly* fasl-prefix-length init-i_0) fasl-prefix)
                                        (void)
                                        (let-values () (read-error "unrecognized prefix")))))
                                  (values))))
                    (let-values (((shared-count_0) (read-fasl-integer* init-i_0)))
                      (let-values (((shared_1) (make-vector shared-count_0)))
                        (let-values ((()
                                      (begin
                                        (if (if (vector? external-lifts_0)
                                              (<= (vector-length external-lifts_0) shared-count_0)
                                              #f)
                                          (void)
                                          (let-values ()
                                            (error 'fasl->s-exp "external-lift vector does not match expected size")))
                                        (values))))
                          (let-values ((()
                                        (begin
                                          (let-values ()
                                            (let-values (((vec_12 len_7)
                                                          (let-values (((vec_13) external-lifts_0))
                                                            (begin
                                                              (if (variable-reference-from-unsafe?
                                                                   (#%variable-reference))
                                                                (void)
                                                                (let-values () (check-vector vec_13)))
                                                              (values vec_13 (unsafe-vector-length vec_13)))))
                                                         ((start_3) 0))
                                              (begin
                                                (if (variable-reference-from-unsafe? (#%variable-reference))
                                                  (void)
                                                  (let-values () (check-naturals start_3)))
                                                ((letrec-values (((for-loop_13)
                                                                  (lambda (pos_5 pos_6)
                                                                    (begin
                                                                      'for-loop
                                                                      (if (if (unsafe-fx< pos_5 len_7) #t #f)
                                                                        (let-values (((v_29)
                                                                                      (unsafe-vector-ref vec_12 pos_5))
                                                                                     ((pos_7) pos_6))
                                                                          (let-values ((()
                                                                                        (let-values ((()
                                                                                                      (let-values ()
                                                                                                        (begin
                                                                                                          (let-values ()
                                                                                                            (vector-set!
                                                                                                             shared_1
                                                                                                             pos_7
                                                                                                             (vector-ref
                                                                                                              external-lifts_0
                                                                                                              pos_7)))
                                                                                                          (values)))))
                                                                                          (values))))
                                                                            (if (not #f)
                                                                              (for-loop_13
                                                                               (unsafe-fx+ 1 pos_5)
                                                                               (+ pos_6 1))
                                                                              (values))))
                                                                        (values))))))
                                                   for-loop_13)
                                                 0
                                                 start_3))))
                                          (values))))
                            (let-values ()
                              (let-values (((len_8) (read-fasl-integer* init-i_0)))
                                (let-values (((i_0)
                                              (if (mpair? init-i_0)
                                                init-i_0
                                                (let-values (((bstr_8) (read-bytes/exactly* len_8 init-i_0)))
                                                  (mcons bstr_8 0)))))
                                  (let-values (((intern_0)
                                                (lambda (v_30)
                                                  (begin 'intern (if intern?_0 (datum-intern-literal v_30) v_30)))))
                                    ((letrec-values (((loop_5)
                                                      (lambda ()
                                                        (begin
                                                          'loop
                                                          (let-values (((type_0) (read-byte/no-eof i_0)))
                                                            (let-values (((tmp_0) type_0))
                                                              (let-values (((index_0)
                                                                            (if (fixnum? tmp_0)
                                                                              (if (if (unsafe-fx>= tmp_0 1)
                                                                                    (unsafe-fx< tmp_0 45)
                                                                                    #f)
                                                                                (let-values (((tbl_0)
                                                                                              '#(1
                                                                                                 2
                                                                                                 3
                                                                                                 4
                                                                                                 5
                                                                                                 6
                                                                                                 7
                                                                                                 8
                                                                                                 9
                                                                                                 10
                                                                                                 12
                                                                                                 13
                                                                                                 14
                                                                                                 15
                                                                                                 16
                                                                                                 17
                                                                                                 18
                                                                                                 19
                                                                                                 20
                                                                                                 21
                                                                                                 22
                                                                                                 23
                                                                                                 24
                                                                                                 25
                                                                                                 26
                                                                                                 27
                                                                                                 28
                                                                                                 29
                                                                                                 31
                                                                                                 30
                                                                                                 32
                                                                                                 32
                                                                                                 35
                                                                                                 36
                                                                                                 37
                                                                                                 38
                                                                                                 39
                                                                                                 40
                                                                                                 11
                                                                                                 41
                                                                                                 42
                                                                                                 43
                                                                                                 34
                                                                                                 33)))
                                                                                  (unsafe-vector*-ref
                                                                                   tbl_0
                                                                                   (unsafe-fx- tmp_0 1)))
                                                                                0)
                                                                              0)))
                                                                (if (unsafe-fx< index_0 21)
                                                                  (if (unsafe-fx< index_0 10)
                                                                    (if (unsafe-fx< index_0 4)
                                                                      (if (unsafe-fx< index_0 1)
                                                                        (let-values ()
                                                                          (if (>= type_0 fasl-small-integer-start)
                                                                            (let-values ()
                                                                              (+
                                                                               (- type_0 fasl-small-integer-start)
                                                                               fasl-lowest-small-integer))
                                                                            (let-values ()
                                                                              (read-error
                                                                               "unrecognized fasl tag"
                                                                               "tag"
                                                                               type_0))))
                                                                        (if (unsafe-fx< index_0 2)
                                                                          (let-values ()
                                                                            (let-values (((pos_8)
                                                                                          (read-fasl-integer i_0)))
                                                                              (let-values (((v_31) (loop_5)))
                                                                                (begin
                                                                                  (if (< pos_8 shared-count_0)
                                                                                    (void)
                                                                                    (let-values ()
                                                                                      (read-error "bad graph index")))
                                                                                  (vector-set! shared_1 pos_8 v_31)
                                                                                  v_31))))
                                                                          (if (unsafe-fx< index_0 3)
                                                                            (let-values ()
                                                                              (let-values (((pos_9)
                                                                                            (read-fasl-integer i_0)))
                                                                                (begin
                                                                                  (if (< pos_9 shared-count_0)
                                                                                    (void)
                                                                                    (let-values ()
                                                                                      (read-error "bad graph index")))
                                                                                  (vector-ref shared_1 pos_9))))
                                                                            (let-values () #f))))
                                                                      (if (unsafe-fx< index_0 6)
                                                                        (if (unsafe-fx< index_0 5)
                                                                          (let-values () #t)
                                                                          (let-values () null))
                                                                        (if (unsafe-fx< index_0 7)
                                                                          (let-values () (void))
                                                                          (if (unsafe-fx< index_0 8)
                                                                            (let-values () eof)
                                                                            (if (unsafe-fx< index_0 9)
                                                                              (let-values ()
                                                                                (intern_0 (read-fasl-integer i_0)))
                                                                              (let-values ()
                                                                                (read-fasl-flonum i_0)))))))
                                                                    (if (unsafe-fx< index_0 15)
                                                                      (if (unsafe-fx< index_0 12)
                                                                        (if (unsafe-fx< index_0 11)
                                                                          (let-values ()
                                                                            (real->single-flonum
                                                                             (floating-point-bytes->real
                                                                              (read-bytes/exactly 4 i_0)
                                                                              #f)))
                                                                          (let-values ()
                                                                            (let-values (((bstr_9)
                                                                                          (read-bytes/exactly
                                                                                           (read-fasl-integer i_0)
                                                                                           i_0)))
                                                                              (string->number
                                                                               (bytes->string/utf-8 bstr_9)
                                                                               10
                                                                               'read))))
                                                                        (if (unsafe-fx< index_0 13)
                                                                          (let-values ()
                                                                            (intern_0 (/ (loop_5) (loop_5))))
                                                                          (if (unsafe-fx< index_0 14)
                                                                            (let-values ()
                                                                              (intern_0
                                                                               (make-rectangular (loop_5) (loop_5))))
                                                                            (let-values ()
                                                                              (intern_0
                                                                               (integer->char
                                                                                (read-fasl-integer i_0)))))))
                                                                      (if (unsafe-fx< index_0 17)
                                                                        (if (unsafe-fx< index_0 16)
                                                                          (let-values ()
                                                                            (string->symbol (read-fasl-string i_0)))
                                                                          (let-values ()
                                                                            (string->unreadable-symbol
                                                                             (read-fasl-string i_0))))
                                                                        (if (unsafe-fx< index_0 18)
                                                                          (let-values ()
                                                                            (string->uninterned-symbol
                                                                             (read-fasl-string i_0)))
                                                                          (if (unsafe-fx< index_0 19)
                                                                            (let-values ()
                                                                              (string->keyword (read-fasl-string i_0)))
                                                                            (if (unsafe-fx< index_0 20)
                                                                              (let-values () (read-fasl-string i_0))
                                                                              (let-values ()
                                                                                (intern_0
                                                                                 (string->immutable-string
                                                                                  (read-fasl-string i_0))))))))))
                                                                  (if (unsafe-fx< index_0 32)
                                                                    (if (unsafe-fx< index_0 26)
                                                                      (if (unsafe-fx< index_0 23)
                                                                        (if (unsafe-fx< index_0 22)
                                                                          (let-values () (read-fasl-bytes i_0))
                                                                          (let-values ()
                                                                            (intern_0
                                                                             (bytes->immutable-bytes
                                                                              (read-fasl-bytes i_0)))))
                                                                        (if (unsafe-fx< index_0 24)
                                                                          (let-values ()
                                                                            (bytes->path
                                                                             (read-fasl-bytes i_0)
                                                                             (loop_5)))
                                                                          (if (unsafe-fx< index_0 25)
                                                                            (let-values ()
                                                                              (let-values (((wrt-dir_1)
                                                                                            (let-values (((or-part_18)
                                                                                                          (current-load-relative-directory)))
                                                                                              (if or-part_18
                                                                                                or-part_18
                                                                                                (current-directory)))))
                                                                                (let-values (((rel-elems_1)
                                                                                              (1/reverse
                                                                                               (let-values (((fold-var_20)
                                                                                                             (let-values (((fold-var_21)
                                                                                                                           null))
                                                                                                               fold-var_21)))
                                                                                                 (let-values (((lst_18)
                                                                                                               (loop_5)))
                                                                                                   (begin
                                                                                                     (if (variable-reference-from-unsafe?
                                                                                                          (#%variable-reference))
                                                                                                       (void)
                                                                                                       (let-values ()
                                                                                                         (check-list
                                                                                                          lst_18)))
                                                                                                     ((letrec-values (((for-loop_14)
                                                                                                                       (lambda (fold-var_22
                                                                                                                                lst_19)
                                                                                                                         (begin
                                                                                                                           'for-loop
                                                                                                                           (let-values ()
                                                                                                                             (if (pair?
                                                                                                                                  lst_19)
                                                                                                                               (let-values (((p_7)
                                                                                                                                             (unsafe-car
                                                                                                                                              lst_19))
                                                                                                                                            ((rest_9)
                                                                                                                                             (unsafe-cdr
                                                                                                                                              lst_19)))
                                                                                                                                 (let-values (((fold-var_23)
                                                                                                                                               (let-values (((fold-var_24)
                                                                                                                                                             (let-values ()
                                                                                                                                                               (cons
                                                                                                                                                                (let-values ()
                                                                                                                                                                  (if (bytes?
                                                                                                                                                                       p_7)
                                                                                                                                                                    (bytes->path-element
                                                                                                                                                                     p_7)
                                                                                                                                                                    p_7))
                                                                                                                                                                fold-var_22))))
                                                                                                                                                 (values
                                                                                                                                                  fold-var_24))))
                                                                                                                                   (if (not
                                                                                                                                        #f)
                                                                                                                                     (for-loop_14
                                                                                                                                      fold-var_23
                                                                                                                                      rest_9)
                                                                                                                                     fold-var_23)))
                                                                                                                               fold-var_22))))))
                                                                                                        for-loop_14)
                                                                                                      fold-var_20
                                                                                                      lst_18)))))))
                                                                                  (if wrt-dir_1
                                                                                    (let-values ()
                                                                                      (apply
                                                                                       build-path
                                                                                       wrt-dir_1
                                                                                       rel-elems_1))
                                                                                    (if (null? rel-elems_1)
                                                                                      (let-values () (build-path 'same))
                                                                                      (let-values ()
                                                                                        (apply
                                                                                         build-path
                                                                                         rel-elems_1)))))))
                                                                            (let-values ()
                                                                              (intern_0
                                                                               (pregexp (read-fasl-string i_0)))))))
                                                                      (if (unsafe-fx< index_0 28)
                                                                        (if (unsafe-fx< index_0 27)
                                                                          (let-values ()
                                                                            (intern_0 (regexp (read-fasl-string i_0))))
                                                                          (let-values ()
                                                                            (intern_0
                                                                             (byte-pregexp (read-fasl-bytes i_0)))))
                                                                        (if (unsafe-fx< index_0 29)
                                                                          (let-values ()
                                                                            (intern_0
                                                                             (byte-regexp (read-fasl-bytes i_0))))
                                                                          (if (unsafe-fx< index_0 30)
                                                                            (let-values ()
                                                                              (let-values (((len_9)
                                                                                            (read-fasl-integer i_0)))
                                                                                (1/reverse
                                                                                 (let-values (((fold-var_25)
                                                                                               (let-values (((fold-var_26)
                                                                                                             null))
                                                                                                 fold-var_26)))
                                                                                   (let-values (((start_4) 0)
                                                                                                ((end_0) len_9)
                                                                                                ((inc_0) 1))
                                                                                     (begin
                                                                                       (if (variable-reference-from-unsafe?
                                                                                            (#%variable-reference))
                                                                                         (void)
                                                                                         (let-values ()
                                                                                           (check-range
                                                                                            start_4
                                                                                            end_0
                                                                                            inc_0)))
                                                                                       ((letrec-values (((for-loop_15)
                                                                                                         (lambda (fold-var_27
                                                                                                                  pos_10)
                                                                                                           (begin
                                                                                                             'for-loop
                                                                                                             (let-values ()
                                                                                                               (if (<
                                                                                                                    pos_10
                                                                                                                    end_0)
                                                                                                                 (let-values ()
                                                                                                                   (let-values (((fold-var_28)
                                                                                                                                 (let-values (((fold-var_29)
                                                                                                                                               (let-values ()
                                                                                                                                                 (cons
                                                                                                                                                  (let-values ()
                                                                                                                                                    (loop_5))
                                                                                                                                                  fold-var_27))))
                                                                                                                                   (values
                                                                                                                                    fold-var_29))))
                                                                                                                     (if (not
                                                                                                                          #f)
                                                                                                                       (for-loop_15
                                                                                                                        fold-var_28
                                                                                                                        (+
                                                                                                                         pos_10
                                                                                                                         inc_0))
                                                                                                                       fold-var_28)))
                                                                                                                 fold-var_27))))))
                                                                                          for-loop_15)
                                                                                        fold-var_25
                                                                                        start_4)))))))
                                                                            (if (unsafe-fx< index_0 31)
                                                                              (let-values () (cons (loop_5) (loop_5)))
                                                                              (let-values ()
                                                                                (let-values (((len_10)
                                                                                              (read-fasl-integer i_0)))
                                                                                  ((letrec-values (((ploop_1)
                                                                                                    (lambda (len_11)
                                                                                                      (begin
                                                                                                        'ploop
                                                                                                        (if (zero?
                                                                                                             len_11)
                                                                                                          (loop_5)
                                                                                                          (cons
                                                                                                           (loop_5)
                                                                                                           (ploop_1
                                                                                                            (sub1
                                                                                                             len_11))))))))
                                                                                     ploop_1)
                                                                                   len_10))))))))
                                                                    (if (unsafe-fx< index_0 37)
                                                                      (if (unsafe-fx< index_0 34)
                                                                        (if (unsafe-fx< index_0 33)
                                                                          (let-values ()
                                                                            (let-values (((len_12)
                                                                                          (read-fasl-integer i_0)))
                                                                              (let-values (((vec_14)
                                                                                            (let-values (((len_13)
                                                                                                          len_12))
                                                                                              (begin
                                                                                                (if (exact-nonnegative-integer?
                                                                                                     len_13)
                                                                                                  (void)
                                                                                                  (let-values ()
                                                                                                    (1/raise-argument-error
                                                                                                     'for/vector
                                                                                                     "exact-nonnegative-integer?"
                                                                                                     len_13)))
                                                                                                (let-values (((v_32)
                                                                                                              (make-vector
                                                                                                               len_13
                                                                                                               0)))
                                                                                                  (begin
                                                                                                    (if (zero? len_13)
                                                                                                      (void)
                                                                                                      (let-values ()
                                                                                                        (let-values (((i_1)
                                                                                                                      (let-values (((i_2)
                                                                                                                                    0))
                                                                                                                        i_2)))
                                                                                                          (let-values (((start_5)
                                                                                                                        0)
                                                                                                                       ((end_1)
                                                                                                                        len_12)
                                                                                                                       ((inc_1)
                                                                                                                        1))
                                                                                                            (begin
                                                                                                              (if (variable-reference-from-unsafe?
                                                                                                                   (#%variable-reference))
                                                                                                                (void)
                                                                                                                (let-values ()
                                                                                                                  (check-range
                                                                                                                   start_5
                                                                                                                   end_1
                                                                                                                   inc_1)))
                                                                                                              ((letrec-values (((for-loop_16)
                                                                                                                                (lambda (i_3
                                                                                                                                         pos_11)
                                                                                                                                  (begin
                                                                                                                                    'for-loop
                                                                                                                                    (let-values ()
                                                                                                                                      (if (<
                                                                                                                                           pos_11
                                                                                                                                           end_1)
                                                                                                                                        (let-values (((j_0)
                                                                                                                                                      pos_11))
                                                                                                                                          (let-values (((i_4)
                                                                                                                                                        (let-values (((i_5)
                                                                                                                                                                      (let-values ()
                                                                                                                                                                        (begin
                                                                                                                                                                          (unsafe-vector*-set!
                                                                                                                                                                           v_32
                                                                                                                                                                           i_3
                                                                                                                                                                           (let-values ()
                                                                                                                                                                             (loop_5)))
                                                                                                                                                                          (unsafe-fx+
                                                                                                                                                                           1
                                                                                                                                                                           i_3)))))
                                                                                                                                                          (values
                                                                                                                                                           i_5))))
                                                                                                                                            (if (if (not
                                                                                                                                                     ((lambda x_8
                                                                                                                                                        (unsafe-fx=
                                                                                                                                                         i_4
                                                                                                                                                         len_13))
                                                                                                                                                      j_0))
                                                                                                                                                  (not
                                                                                                                                                   #f)
                                                                                                                                                  #f)
                                                                                                                                              (for-loop_16
                                                                                                                                               i_4
                                                                                                                                               (+
                                                                                                                                                pos_11
                                                                                                                                                inc_1))
                                                                                                                                              i_4)))
                                                                                                                                        i_3))))))
                                                                                                                 for-loop_16)
                                                                                                               i_1
                                                                                                               start_5))))))
                                                                                                    v_32))))))
                                                                                (if (eqv?
                                                                                     type_0
                                                                                     fasl-immutable-vector-type)
                                                                                  (vector->immutable-vector vec_14)
                                                                                  vec_14))))
                                                                          (let-values ()
                                                                            (let-values (((len_14)
                                                                                          (read-fasl-integer i_0)))
                                                                              (let-values (((len_15) len_14))
                                                                                (begin
                                                                                  (if (exact-nonnegative-integer?
                                                                                       len_15)
                                                                                    (void)
                                                                                    (let-values ()
                                                                                      (raise-argument-error
                                                                                       'for/flvector
                                                                                       "exact-nonnegative-integer?"
                                                                                       len_15)))
                                                                                  (let-values (((fill_0) 0.0))
                                                                                    (let-values (((v_33)
                                                                                                  (make-flvector
                                                                                                   len_15
                                                                                                   fill_0)))
                                                                                      (begin
                                                                                        (if (zero? len_15)
                                                                                          (void)
                                                                                          (let-values ()
                                                                                            (let-values (((i_6)
                                                                                                          (let-values (((i_7)
                                                                                                                        0))
                                                                                                            i_7)))
                                                                                              (let-values (((start_6) 0)
                                                                                                           ((end_2)
                                                                                                            len_14)
                                                                                                           ((inc_2) 1))
                                                                                                (begin
                                                                                                  (if (variable-reference-from-unsafe?
                                                                                                       (#%variable-reference))
                                                                                                    (void)
                                                                                                    (let-values ()
                                                                                                      (check-range
                                                                                                       start_6
                                                                                                       end_2
                                                                                                       inc_2)))
                                                                                                  ((letrec-values (((for-loop_17)
                                                                                                                    (lambda (i_8
                                                                                                                             pos_12)
                                                                                                                      (begin
                                                                                                                        'for-loop
                                                                                                                        (let-values ()
                                                                                                                          (if (<
                                                                                                                               pos_12
                                                                                                                               end_2)
                                                                                                                            (let-values (((j_1)
                                                                                                                                          pos_12))
                                                                                                                              (let-values (((i_9)
                                                                                                                                            (let-values (((i_10)
                                                                                                                                                          (let-values ()
                                                                                                                                                            (begin
                                                                                                                                                              (let-values (((elem_0)
                                                                                                                                                                            (let-values ()
                                                                                                                                                                              (read-fasl-flonum
                                                                                                                                                                               i_0))))
                                                                                                                                                                (if (flonum?
                                                                                                                                                                     elem_0)
                                                                                                                                                                  (unsafe-flvector-set!
                                                                                                                                                                   v_33
                                                                                                                                                                   i_8
                                                                                                                                                                   elem_0)
                                                                                                                                                                  (not-an-fX.1
                                                                                                                                                                   'for*/vector
                                                                                                                                                                   elem_0)))
                                                                                                                                                              (unsafe-fx+
                                                                                                                                                               1
                                                                                                                                                               i_8)))))
                                                                                                                                              (values
                                                                                                                                               i_10))))
                                                                                                                                (if (if (not
                                                                                                                                         ((lambda x_9
                                                                                                                                            (unsafe-fx=
                                                                                                                                             i_9
                                                                                                                                             len_15))
                                                                                                                                          j_1))
                                                                                                                                      (not
                                                                                                                                       #f)
                                                                                                                                      #f)
                                                                                                                                  (for-loop_17
                                                                                                                                   i_9
                                                                                                                                   (+
                                                                                                                                    pos_12
                                                                                                                                    inc_2))
                                                                                                                                  i_9)))
                                                                                                                            i_8))))))
                                                                                                     for-loop_17)
                                                                                                   i_6
                                                                                                   start_6))))))
                                                                                        v_33))))))))
                                                                        (if (unsafe-fx< index_0 35)
                                                                          (let-values ()
                                                                            (let-values (((len_16)
                                                                                          (read-fasl-integer i_0)))
                                                                              (let-values (((len_17) len_16))
                                                                                (begin
                                                                                  (if (exact-nonnegative-integer?
                                                                                       len_17)
                                                                                    (void)
                                                                                    (let-values ()
                                                                                      (raise-argument-error
                                                                                       'for/fxvector
                                                                                       "exact-nonnegative-integer?"
                                                                                       len_17)))
                                                                                  (let-values (((fill_1) 0))
                                                                                    (let-values (((v_34)
                                                                                                  (make-fxvector
                                                                                                   len_17
                                                                                                   fill_1)))
                                                                                      (begin
                                                                                        (if (zero? len_17)
                                                                                          (void)
                                                                                          (let-values ()
                                                                                            (let-values (((i_11)
                                                                                                          (let-values (((i_12)
                                                                                                                        0))
                                                                                                            i_12)))
                                                                                              (let-values (((start_7) 0)
                                                                                                           ((end_3)
                                                                                                            len_16)
                                                                                                           ((inc_3) 1))
                                                                                                (begin
                                                                                                  (if (variable-reference-from-unsafe?
                                                                                                       (#%variable-reference))
                                                                                                    (void)
                                                                                                    (let-values ()
                                                                                                      (check-range
                                                                                                       start_7
                                                                                                       end_3
                                                                                                       inc_3)))
                                                                                                  ((letrec-values (((for-loop_18)
                                                                                                                    (lambda (i_13
                                                                                                                             pos_13)
                                                                                                                      (begin
                                                                                                                        'for-loop
                                                                                                                        (let-values ()
                                                                                                                          (if (<
                                                                                                                               pos_13
                                                                                                                               end_3)
                                                                                                                            (let-values (((j_2)
                                                                                                                                          pos_13))
                                                                                                                              (let-values (((i_14)
                                                                                                                                            (let-values (((i_15)
                                                                                                                                                          (let-values ()
                                                                                                                                                            (begin
                                                                                                                                                              (let-values (((elem_1)
                                                                                                                                                                            (let-values ()
                                                                                                                                                                              (read-fasl-integer
                                                                                                                                                                               i_0))))
                                                                                                                                                                (if (fixnum?
                                                                                                                                                                     elem_1)
                                                                                                                                                                  (unsafe-fxvector-set!
                                                                                                                                                                   v_34
                                                                                                                                                                   i_13
                                                                                                                                                                   elem_1)
                                                                                                                                                                  (not-an-fX.1$1
                                                                                                                                                                   'for*/vector
                                                                                                                                                                   elem_1)))
                                                                                                                                                              (unsafe-fx+
                                                                                                                                                               1
                                                                                                                                                               i_13)))))
                                                                                                                                              (values
                                                                                                                                               i_15))))
                                                                                                                                (if (if (not
                                                                                                                                         ((lambda x_10
                                                                                                                                            (unsafe-fx=
                                                                                                                                             i_14
                                                                                                                                             len_17))
                                                                                                                                          j_2))
                                                                                                                                      (not
                                                                                                                                       #f)
                                                                                                                                      #f)
                                                                                                                                  (for-loop_18
                                                                                                                                   i_14
                                                                                                                                   (+
                                                                                                                                    pos_13
                                                                                                                                    inc_3))
                                                                                                                                  i_14)))
                                                                                                                            i_13))))))
                                                                                                     for-loop_18)
                                                                                                   i_11
                                                                                                   start_7))))))
                                                                                        v_34)))))))
                                                                          (if (unsafe-fx< index_0 36)
                                                                            (let-values () (box (loop_5)))
                                                                            (let-values () (box-immutable (loop_5))))))
                                                                      (if (unsafe-fx< index_0 40)
                                                                        (if (unsafe-fx< index_0 38)
                                                                          (let-values ()
                                                                            (let-values (((key_3) (loop_5)))
                                                                              (let-values (((len_18)
                                                                                            (read-fasl-integer i_0)))
                                                                                (apply
                                                                                 make-prefab-struct
                                                                                 key_3
                                                                                 (1/reverse
                                                                                  (let-values (((fold-var_30)
                                                                                                (let-values (((fold-var_31)
                                                                                                              null))
                                                                                                  fold-var_31)))
                                                                                    (let-values (((start_8) 0)
                                                                                                 ((end_4) len_18)
                                                                                                 ((inc_4) 1))
                                                                                      (begin
                                                                                        (if (variable-reference-from-unsafe?
                                                                                             (#%variable-reference))
                                                                                          (void)
                                                                                          (let-values ()
                                                                                            (check-range
                                                                                             start_8
                                                                                             end_4
                                                                                             inc_4)))
                                                                                        ((letrec-values (((for-loop_19)
                                                                                                          (lambda (fold-var_32
                                                                                                                   pos_14)
                                                                                                            (begin
                                                                                                              'for-loop
                                                                                                              (let-values ()
                                                                                                                (if (<
                                                                                                                     pos_14
                                                                                                                     end_4)
                                                                                                                  (let-values ()
                                                                                                                    (let-values (((fold-var_33)
                                                                                                                                  (let-values (((fold-var_34)
                                                                                                                                                (let-values ()
                                                                                                                                                  (cons
                                                                                                                                                   (let-values ()
                                                                                                                                                     (loop_5))
                                                                                                                                                   fold-var_32))))
                                                                                                                                    (values
                                                                                                                                     fold-var_34))))
                                                                                                                      (if (not
                                                                                                                           #f)
                                                                                                                        (for-loop_19
                                                                                                                         fold-var_33
                                                                                                                         (+
                                                                                                                          pos_14
                                                                                                                          inc_4))
                                                                                                                        fold-var_33)))
                                                                                                                  fold-var_32))))))
                                                                                           for-loop_19)
                                                                                         fold-var_30
                                                                                         start_8)))))))))
                                                                          (if (unsafe-fx< index_0 39)
                                                                            (let-values ()
                                                                              (let-values (((ht_3)
                                                                                            (let-values (((tmp_1)
                                                                                                          (read-byte/no-eof
                                                                                                           i_0)))
                                                                                              (if (equal? tmp_1 0)
                                                                                                (let-values ()
                                                                                                  (make-hasheq))
                                                                                                (if (equal? tmp_1 2)
                                                                                                  (let-values ()
                                                                                                    (make-hasheqv))
                                                                                                  (if (equal? tmp_1 3)
                                                                                                    (let-values ()
                                                                                                      (make-hashalw))
                                                                                                    (let-values ()
                                                                                                      (make-hash))))))))
                                                                                (let-values (((len_19)
                                                                                              (read-fasl-integer i_0)))
                                                                                  (begin
                                                                                    (let-values ()
                                                                                      (let-values (((start_9) 0)
                                                                                                   ((end_5) len_19)
                                                                                                   ((inc_5) 1))
                                                                                        (begin
                                                                                          (if (variable-reference-from-unsafe?
                                                                                               (#%variable-reference))
                                                                                            (void)
                                                                                            (let-values ()
                                                                                              (check-range
                                                                                               start_9
                                                                                               end_5
                                                                                               inc_5)))
                                                                                          ((letrec-values (((for-loop_20)
                                                                                                            (lambda (pos_15)
                                                                                                              (begin
                                                                                                                'for-loop
                                                                                                                (if (<
                                                                                                                     pos_15
                                                                                                                     end_5)
                                                                                                                  (let-values ()
                                                                                                                    (let-values ((()
                                                                                                                                  (let-values ((()
                                                                                                                                                (let-values ()
                                                                                                                                                  (begin
                                                                                                                                                    (let-values ()
                                                                                                                                                      (hash-set!
                                                                                                                                                       ht_3
                                                                                                                                                       (loop_5)
                                                                                                                                                       (loop_5)))
                                                                                                                                                    (values)))))
                                                                                                                                    (values))))
                                                                                                                      (if (not
                                                                                                                           #f)
                                                                                                                        (for-loop_20
                                                                                                                         (+
                                                                                                                          pos_15
                                                                                                                          inc_5))
                                                                                                                        (values))))
                                                                                                                  (values))))))
                                                                                             for-loop_20)
                                                                                           start_9))))
                                                                                    (void)
                                                                                    ht_3))))
                                                                            (let-values ()
                                                                              (let-values (((ht_4)
                                                                                            (let-values (((tmp_2)
                                                                                                          (read-byte/no-eof
                                                                                                           i_0)))
                                                                                              (if (equal? tmp_2 0)
                                                                                                (let-values ()
                                                                                                  '#hasheq())
                                                                                                (if (equal? tmp_2 2)
                                                                                                  (let-values ()
                                                                                                    '#hasheqv())
                                                                                                  (if (equal? tmp_2 3)
                                                                                                    (let-values ()
                                                                                                      (hashalw))
                                                                                                    (let-values ()
                                                                                                      '#hash())))))))
                                                                                (let-values (((len_20)
                                                                                              (read-fasl-integer i_0)))
                                                                                  (let-values (((ht_5)
                                                                                                (let-values (((ht_6)
                                                                                                              ht_4))
                                                                                                  ht_6)))
                                                                                    (let-values (((start_10) 0)
                                                                                                 ((end_6) len_20)
                                                                                                 ((inc_6) 1))
                                                                                      (begin
                                                                                        (if (variable-reference-from-unsafe?
                                                                                             (#%variable-reference))
                                                                                          (void)
                                                                                          (let-values ()
                                                                                            (check-range
                                                                                             start_10
                                                                                             end_6
                                                                                             inc_6)))
                                                                                        ((letrec-values (((for-loop_21)
                                                                                                          (lambda (ht_7
                                                                                                                   pos_16)
                                                                                                            (begin
                                                                                                              'for-loop
                                                                                                              (let-values ()
                                                                                                                (if (<
                                                                                                                     pos_16
                                                                                                                     end_6)
                                                                                                                  (let-values ()
                                                                                                                    (let-values (((ht_8)
                                                                                                                                  (let-values (((ht_9)
                                                                                                                                                (let-values ()
                                                                                                                                                  (hash-set
                                                                                                                                                   ht_7
                                                                                                                                                   (loop_5)
                                                                                                                                                   (loop_5)))))
                                                                                                                                    (values
                                                                                                                                     ht_9))))
                                                                                                                      (if (not
                                                                                                                           #f)
                                                                                                                        (for-loop_21
                                                                                                                         ht_8
                                                                                                                         (+
                                                                                                                          pos_16
                                                                                                                          inc_6))
                                                                                                                        ht_8)))
                                                                                                                  ht_7))))))
                                                                                           for-loop_21)
                                                                                         ht_5
                                                                                         start_10)))))))))
                                                                        (if (unsafe-fx< index_0 41)
                                                                          (let-values ()
                                                                            (srcloc
                                                                             (loop_5)
                                                                             (loop_5)
                                                                             (loop_5)
                                                                             (loop_5)
                                                                             (loop_5)))
                                                                          (if (unsafe-fx< index_0 42)
                                                                            (let-values ()
                                                                              (let-values (((e_11) (loop_5)))
                                                                                (let-values (((s_6) (loop_5)))
                                                                                  (let-values (((c_2)
                                                                                                (datum->correlated
                                                                                                 e_11
                                                                                                 (vector
                                                                                                  (srcloc-source s_6)
                                                                                                  (srcloc-line s_6)
                                                                                                  (srcloc-column s_6)
                                                                                                  (srcloc-position s_6)
                                                                                                  (srcloc-span s_6)))))
                                                                                    (let-values (((c_3)
                                                                                                  (let-values (((c_4)
                                                                                                                c_2))
                                                                                                    c_4)))
                                                                                      (let-values (((lst_20) (loop_5)))
                                                                                        (begin
                                                                                          (if (variable-reference-from-unsafe?
                                                                                               (#%variable-reference))
                                                                                            (void)
                                                                                            (let-values ()
                                                                                              (check-list lst_20)))
                                                                                          ((letrec-values (((for-loop_22)
                                                                                                            (lambda (c_5
                                                                                                                     lst_21)
                                                                                                              (begin
                                                                                                                'for-loop
                                                                                                                (let-values ()
                                                                                                                  (if (pair?
                                                                                                                       lst_21)
                                                                                                                    (let-values (((p_8)
                                                                                                                                  (unsafe-car
                                                                                                                                   lst_21))
                                                                                                                                 ((rest_10)
                                                                                                                                  (unsafe-cdr
                                                                                                                                   lst_21)))
                                                                                                                      (let-values (((c_6)
                                                                                                                                    (let-values (((c_7)
                                                                                                                                                  (let-values ()
                                                                                                                                                    (correlated-property
                                                                                                                                                     c_5
                                                                                                                                                     (car
                                                                                                                                                      p_8)
                                                                                                                                                     (cdr
                                                                                                                                                      p_8)))))
                                                                                                                                      (values
                                                                                                                                       c_7))))
                                                                                                                        (if (not
                                                                                                                             #f)
                                                                                                                          (for-loop_22
                                                                                                                           c_6
                                                                                                                           rest_10)
                                                                                                                          c_6)))
                                                                                                                    c_5))))))
                                                                                             for-loop_22)
                                                                                           c_3
                                                                                           lst_20))))))))
                                                                            (if (unsafe-fx< index_0 43)
                                                                              (let-values () unsafe-undefined)
                                                                              (let-values ()
                                                                                (prefab-key->struct-type
                                                                                 (loop_5)
                                                                                 (loop_5)))))))))))))))))
                                       loop_5))))))))))))))))))))
 (define-values
  (write-fasl-integer)
  (lambda (i_16 o_2)
    (begin
      (if (<= -124 i_16 127)
        (let-values () (if (negative? i_16) (1/write-byte (+ i_16 256) o_2) (1/write-byte i_16 o_2)))
        (if (<= -32768 i_16 32767)
          (let-values () (begin (1/write-byte 128 o_2) (1/write-bytes (integer->integer-bytes i_16 2 #t #f) o_2)))
          (if (<= -2147483648 i_16 2147483647)
            (let-values () (begin (1/write-byte 129 o_2) (1/write-bytes (integer->integer-bytes i_16 4 #t #f) o_2)))
            (if (<= -9223372036854775808 i_16 9223372036854775807)
              (let-values () (begin (1/write-byte 130 o_2) (1/write-bytes (integer->integer-bytes i_16 8 #t #f) o_2)))
              (let-values ()
                (let-values ((() (begin (1/write-byte 131 o_2) (values))))
                  (let-values (((s_7) (format "~x" i_16)))
                    (begin (write-fasl-integer (string-length s_7) o_2) (write-string s_7 o_2))))))))))))
 (define-values
  (write-fasl-string)
  (lambda (v_35 o_3)
    (begin
      (let-values (((bstr_10) (string->bytes/utf-8 v_35)))
        (begin (write-fasl-integer (bytes-length bstr_10) o_3) (1/write-bytes bstr_10 o_3))))))
 (define-values
  (write-fasl-bytes)
  (lambda (v_36 o_4) (begin (begin (write-fasl-integer (bytes-length v_36) o_4) (1/write-bytes v_36 o_4)))))
 (define-values
  (write-fasl-flonum)
  (lambda (v_37 o_5)
    (begin (1/write-bytes (if (eqv? v_37 +nan.0) #"\0\0\0\0\0\0\370\177" (real->floating-point-bytes v_37 8 #f)) o_5))))
 (define-values
  (read-error)
  (lambda (s_8 . args_3)
    (begin (apply raise-arguments-error 'fasl-read (string-append "error parsing fasl stream;\n" " " s_8) args_3))))
 (define-values
  (read-byte/no-eof)
  (lambda (i_17)
    (begin
      (let-values (((pos_17) (mcdr i_17)))
        (begin
          (if (fx< pos_17 (bytes-length (mcar i_17))) (void) (let-values () (read-error "truncated stream")))
          (set-mcdr! i_17 (fx+ pos_17 1))
          (bytes-ref (mcar i_17) pos_17))))))
 (define-values
  (read-byte/no-eof*)
  (lambda (i_18)
    (begin
      (if (mpair? i_18)
        (let-values () (read-byte/no-eof i_18))
        (let-values ()
          (let-values (((b_3) (read-byte i_18)))
            (begin (if (eof-object? b_3) (let-values () (read-error "truncated stream")) (void)) b_3)))))))
 (define-values
  (read-bytes/exactly)
  (lambda (n_2 i_19)
    (begin
      (let-values (((pos_18) (mcdr i_19)))
        (begin
          (if (<= (+ pos_18 n_2) (bytes-length (mcar i_19))) (void) (let-values () (read-error "truncated stream")))
          (set-mcdr! i_19 (fx+ pos_18 n_2))
          (subbytes (mcar i_19) pos_18 (fx+ pos_18 n_2)))))))
 (define-values
  (read-bytes/exactly*)
  (lambda (n_3 i_20)
    (begin
      (if (mpair? i_20)
        (let-values () (read-bytes/exactly n_3 i_20))
        (let-values ()
          (let-values (((bstr_11) (read-bytes n_3 i_20)))
            (begin
              (if (if (bytes? bstr_11) (= n_3 (bytes-length bstr_11)) #f)
                (void)
                (let-values () (read-error "truncated stream")))
              bstr_11)))))))
 (define-values
  (read-fasl-integer read-fasl-integer*)
  (let-values ()
    (let-values ()
      (values
       (lambda (i_21)
         (let-values (((b_4) (read-byte/no-eof i_21)))
           (if (fx<= b_4 127)
             (let-values () b_4)
             (if (fx>= b_4 132)
               (let-values () (fx- b_4 256))
               (if (eqv? b_4 128)
                 (let-values ()
                   (let-values (((lo_0) (read-byte/no-eof i_21)))
                     (let-values (((hi_0) (read-byte/no-eof i_21)))
                       (if (fx> hi_0 127) (fxior (fxlshift (fx+ -256 hi_0) 8) lo_0) (fxior (fxlshift hi_0 8) lo_0)))))
                 (if (eqv? b_4 129)
                   (let-values ()
                     (let-values (((a_4) (read-byte/no-eof i_21)))
                       (let-values (((b_5) (read-byte/no-eof i_21)))
                         (let-values (((c_8) (read-byte/no-eof i_21)))
                           (let-values (((d_4) (read-byte/no-eof i_21)))
                             (bitwise-ior
                              a_4
                              (arithmetic-shift
                               (if (fx> d_4 127)
                                 (fxior (fxlshift (fx+ -256 d_4) 16) (fxlshift c_8 8) b_5)
                                 (fxior (fxlshift d_4 16) (fxlshift c_8 8) b_5))
                               8)))))))
                   (if (eqv? b_4 130)
                     (let-values () (integer-bytes->integer (read-bytes/exactly 8 i_21) #t #f))
                     (if (eqv? b_4 131)
                       (let-values ()
                         (let-values (((len_21) (read-fasl-integer i_21)))
                           (let-values (((str_0) (read-fasl-string i_21 len_21)))
                             (begin
                               (if (if (string? str_0) (= len_21 (string-length str_0)) #f)
                                 (void)
                                 (let-values () (read-error "truncated stream at number")))
                               (string->number str_0 16)))))
                       (let-values () (read-error "internal error on integer mode"))))))))))
       (lambda (i_22)
         (let-values (((b_6) (read-byte/no-eof* i_22)))
           (if (fx<= b_6 127)
             (let-values () b_6)
             (if (fx>= b_6 132)
               (let-values () (fx- b_6 256))
               (if (eqv? b_6 128)
                 (let-values ()
                   (let-values (((lo_1) (read-byte/no-eof* i_22)))
                     (let-values (((hi_1) (read-byte/no-eof* i_22)))
                       (if (fx> hi_1 127) (fxior (fxlshift (fx+ -256 hi_1) 8) lo_1) (fxior (fxlshift hi_1 8) lo_1)))))
                 (if (eqv? b_6 129)
                   (let-values ()
                     (let-values (((a_5) (read-byte/no-eof* i_22)))
                       (let-values (((b_7) (read-byte/no-eof* i_22)))
                         (let-values (((c_9) (read-byte/no-eof* i_22)))
                           (let-values (((d_5) (read-byte/no-eof* i_22)))
                             (bitwise-ior
                              a_5
                              (arithmetic-shift
                               (if (fx> d_5 127)
                                 (fxior (fxlshift (fx+ -256 d_5) 16) (fxlshift c_9 8) b_7)
                                 (fxior (fxlshift d_5 16) (fxlshift c_9 8) b_7))
                               8)))))))
                   (if (eqv? b_6 130)
                     (let-values () (integer-bytes->integer (read-bytes/exactly* 8 i_22) #t #f))
                     (if (eqv? b_6 131)
                       (let-values ()
                         (let-values (((len_22) (read-fasl-integer i_22)))
                           (let-values (((str_1) (read-fasl-string i_22 len_22)))
                             (begin
                               (if (if (string? str_1) (= len_22 (string-length str_1)) #f)
                                 (void)
                                 (let-values () (read-error "truncated stream at number")))
                               (string->number str_1 16)))))
                       (let-values () (read-error "internal error on integer mode"))))))))))))))
 (define-values
  (read-fasl-string)
  (let-values (((read-fasl-string_0)
                (lambda (i25_0 len24_0)
                  (begin
                    'read-fasl-string
                    (let-values (((i_23) i25_0))
                      (let-values (((len_23) (if (eq? len24_0 unsafe-undefined) (read-fasl-integer i_23) len24_0)))
                        (let-values ()
                          (let-values (((pos_19) (mcdr i_23)))
                            (let-values (((bstr_12) (mcar i_23)))
                              (if (<= (+ pos_19 len_23) (bytes-length bstr_12))
                                (let-values ()
                                  (let-values ((() (begin (set-mcdr! i_23 (fx+ pos_19 len_23)) (values))))
                                    (let-values (((s_9) (make-string len_23)))
                                      ((letrec-values (((loop_6)
                                                        (lambda (i_24)
                                                          (begin
                                                            'loop
                                                            (if (fx= i_24 len_23)
                                                              (let-values () s_9)
                                                              (let-values ()
                                                                (let-values (((c_10)
                                                                              (bytes-ref bstr_12 (fx+ i_24 pos_19))))
                                                                  (if (fx<= c_10 128)
                                                                    (let-values ()
                                                                      (begin
                                                                        (string-set! s_9 i_24 (integer->char c_10))
                                                                        (loop_6 (fx+ i_24 1))))
                                                                    (let-values ()
                                                                      (bytes->string/utf-8
                                                                       bstr_12
                                                                       #f
                                                                       pos_19
                                                                       (fx+ pos_19 len_23)))))))))))
                                         loop_6)
                                       0))))
                                (let-values ()
                                  (let-values (((bstr_13) (read-bytes/exactly len_23 i_23)))
                                    (bytes->string/utf-8 bstr_13)))))))))))))
    (case-lambda
     ((i_25) (begin (read-fasl-string_0 i_25 unsafe-undefined)))
     ((i_26 len24_1) (read-fasl-string_0 i_26 len24_1)))))
 (define-values
  (read-fasl-bytes)
  (lambda (i_27) (begin (let-values (((len_24) (read-fasl-integer i_27))) (read-bytes/exactly len_24 i_27)))))
 (define-values (read-fasl-flonum) (lambda (i_28) (begin (floating-point-bytes->real (read-bytes/exactly 8 i_28) #f))))
 (define-values
  (fasl->s-exp)
  (let-values (((fasl->s-exp_0)
                (lambda (i2_0 intern?1_0)
                  (begin
                    'fasl->s-exp
                    (let-values (((i_29) i2_0))
                      (let-values (((intern?_1) intern?1_0))
                        (let-values ()
                          (let-values (((i6_0) i_29) ((intern?7_0) intern?_1))
                            (fasl->s-exp.1 intern?7_0 unsafe-undefined #f i6_0)))))))))
    (case-lambda ((i_30) (begin (fasl->s-exp_0 i_30 #t))) ((i_31 intern?1_1) (fasl->s-exp_0 i_31 intern?1_1)))))
 (define-values
  (s-exp->fasl)
  (let-values (((s-exp->fasl_0)
                (lambda (v5_0 o3_0 k4_0)
                  (begin
                    's-exp->fasl
                    (let-values (((v_38) v5_0))
                      (let-values (((o_6) o3_0))
                        (let-values (((k_8) k4_0))
                          (let-values ()
                            (let-values (((v8_0) v_38) ((o9_0) o_6) ((k10_0) k_8))
                              (s-exp->fasl.1 #f #f k10_0 #f v8_0 o9_0))))))))))
    (case-lambda
     ((v_39) (begin (s-exp->fasl_0 v_39 #f #f)))
     ((v_40 o_7 k4_1) (s-exp->fasl_0 v_40 o_7 k4_1))
     ((v_41 o3_1) (s-exp->fasl_0 v_41 o3_1 #f))))))
