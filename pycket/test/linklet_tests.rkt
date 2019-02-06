#lang racket/base

(require '#%linklet rackunit)

(define (empty-target)
  (make-instance #f #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case
    "basic"
  (define l (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
  (check-eq? (instance-variable-value l 'x) 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case ""
  (define l (compile-linklet '(linklet () (x) (define-values (x) 4) (+ x x))))
  (define t (make-instance #f #f #f 'x 1 'y 2))
  (define result (instantiate-linklet l null t))
  (check-eq? result 8)
  (check-eq? (instance-variable-value t 'x) 4))

(test-case
    "even if linklet doesn't export, def goes into target if it doesn't already have it"
  (define l2 (compile-linklet '(linklet () () (define-values (x) 4) (+ x x))))
  (define t2 (empty-target))
  (define result2 (instantiate-linklet l2 null t2))
  (check-eq? result2 8)
  (check-not-false (member 'x (instance-variable-names t2)))
  (check-eq? (instance-variable-value t2 'x) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target_transfer_set_banged
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case
    "target-transfer-set-banged"
  (define l (compile-linklet '(linklet () () (define-values (y) 10) (set! y 50))))
  (define t (empty-target))
  (define result (instantiate-linklet l null t))
  (check-eq? (instance-variable-value t 'y) 50))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defined_var set_banged elsewhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case
    ""
  (define l (compile-linklet '(linklet () ()
                                       (define-values (g) (lambda () (set! t 10)))
                                       (define-values (t) 50)
                                       (g)
                                       t)))
  (define t (empty-target))
  (define result (instantiate-linklet l null t))
  (check-eq? result 10)
  (check-eq? (instance-variable-value t 't) 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target_def_overwrite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case
    "target-def-overwrite"
  (define l (compile-linklet '(linklet () (x) (define-values (x) 4) (+ x x))))
  (define t (make-instance #f #f #f 'x 1 'y 2))
  (define result (instantiate-linklet l null t))
  (check-eq? result 8)
  (check-eq? (instance-variable-value t 'x) 4)
  (check-eq? (instance-variable-value t 'y) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target always overwrite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case
    "if target doesn't have it, then it doesn't matter if linklet exports or not, put the variable in the target"
  (define l (compile-linklet '(linklet () () (define-values (z) 4) z)))
  (define t (empty-target))
  (define result (instantiate-linklet l null t))
  (check-eq? result 4)
  (check-eq? (instance-variable-value t 'z) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target def stays the same
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case
    "if linklet doesn't export, then target's def stay the same"
  (define l (compile-linklet '(linklet () () (define-values (x) 4) (+ x x))))
  (define t (make-instance #f #f #f 'x 1 'y 2))
  (define result (instantiate-linklet l null t))
  (check-eq? result 8)
  (check-eq? (instance-variable-value t 'x) 1)
  (check-eq? (instance-variable-value t 'y) 2))

(test-case
    "use the local var, don't change target's var if you don't export"
  (define l (compile-linklet '(linklet () ((x x15)) (define-values (x) 4) (+ x x))))
  (define t1 (instantiate-linklet (compile-linklet '(linklet () () (define-values (x) 10))) null))
  (define t2 (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 10))) null))
  (define result1 (instantiate-linklet l null t1))
  (define result2 (instantiate-linklet l null t2))
  (check-eq? result1 8)
  (check-eq? result2 8)
  (check-eq? (instance-variable-value t2 'x) 10)
  (check-eq? (instance-variable-value t2 'x) 10))

(test-case
    "imported variables doesn't get into target at all ... let alone overwrite any var inside the target"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
  (define l2 (compile-linklet '(linklet ((x)) () (+ x x))))
  (define t1 (empty-target))
  (define t2 (make-instance #f #f #f 'x 1))
  (define result1 (instantiate-linklet l2 (list l1) t1))
  (define result2 (instantiate-linklet l2 (list l1) t2))
  (check-eq? result1 8)
  (check-eq? result2 8)
  (check-eq? (instance-variable-value t2 'x) 1)
  (check-equal? (instance-variable-names t1) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defs_export_names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case ""
  (define l (instantiate-linklet (compile-linklet '(linklet () ((x x15)) (define-values (x) 4))) null))
  (check-false (member 'x (instance-variable-names l)))
  (check-not-false (member 'x15 (instance-variable-names l))))

(test-case
    "LinkletVars will be referred by the external name (e.g. (+ x15 x15)"
  (define l (compile-linklet '(linklet () ((x x15)) (define-values (x) 4) (+ x x))))
  (define t (empty-target))
  (define result (instantiate-linklet l null t))
  (check-eq? result 8)
  (check-false (member 'x (instance-variable-names t)))
  (check-not-false (member 'x15 (instance-variable-names t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; discarding_defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case ""
  (define l (instantiate-linklet (compile-linklet '(linklet () ((x x15)) (define-values (x) 4) (define-values (x15) 75))) null))
  (check-false (member 'x (instance-variable-names l)))
  (check-eq? (instance-variable-value l 'x15) 4))

(test-case ""
  (define l (instantiate-linklet
             (compile-linklet
              '(linklet () ((x x15) k) (define-values (x) 4) (define-values (x15) 75) (define-values (k) x15))) null))
  (check-false (member 'x (instance-variable-names l)))
  (check-eq? (instance-variable-value l 'x15) 4)
  (check-eq? (instance-variable-value l 'k) 75)
  ;; uninterned symbol W_Symbol("str")
  (check-eq? (instance-variable-value l (list-ref (instance-variable-names l) 2)) 75))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use targets def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case ""
  (define l (compile-linklet '(linklet () (x) (+ x x))))
  (define t (make-instance #f #f #f 'x 10))
  (define result (instantiate-linklet l null t))
  (check-eq? result 20))

(test-case
    "use linklet's definition if both linklet and target have it"
  (define l (compile-linklet '(linklet () () (define-values (x) 4) (+ x x)))) ; doesn't export x
  (define t (make-instance #f #f #f 'x 10))
  (define result (instantiate-linklet l null t))
  (check-eq? result 8)
  (check-eq? (instance-variable-value t 'x) 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic import
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case ""
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
  (define l2 (compile-linklet '(linklet ((x)) () (+ x x))))
  (define result (instantiate-linklet l2 (list l1) (empty-target)))
  (check-eq? result 8))

(test-case ""
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
  (define l2 (compile-linklet '(linklet ((x)) (y) (define-values (y) (+ x x)) (+ y y))))
  (define t (empty-target))
  (define result (instantiate-linklet l2 (list l1) t))
  (check-eq? result 16)
  (check-eq? (instance-variable-value t 'y) 8)
  (check-false (member 'x (instance-variable-names t))))

(test-case
    "target's defs are overwritten only if the linklet has a definition not with an imported variable"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
  (define l2 (compile-linklet '(linklet ((x)) () (+ x x))))
  (define t (make-instance #f #f #f 'x 1000))
  (define result (instantiate-linklet l2 (list l1) t))
  (check-eq? result 8)
  (check-eq? (instance-variable-value t 'x) 1000))

(test-case
    "same thing with the import renaming"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
  (define l2 (compile-linklet '(linklet (((x x2))) () (+ x2 x2))))
  (define t (make-instance #f #f #f 'x 1000 'x2 2000))
  (define result (instantiate-linklet l2 (list l1) t))
  (check-eq? result 8)
  (check-eq? (instance-variable-value t 'x) 1000)
  (check-eq? (instance-variable-value t 'x2) 2000))

(test-case
    "slightly trickier"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
  (define l2 (compile-linklet '(linklet (((x x2))) () (define-values (x) 14) (+ x2 x))))
  (define t (make-instance #f #f #f 'x 1000 'x2 2000))
  (define result (instantiate-linklet l2 (list l1) t))
  (check-eq? result 18)
  (check-eq? (instance-variable-value t 'x) 1000)
  (check-eq? (instance-variable-value t 'x2) 2000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "basic export"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (a) (define-values (a) 4))) null))
  (define l2 (compile-linklet '(linklet ((a)) () (+ a a))))
  (define result (instantiate-linklet l2 (list l1) (empty-target)))
  (check-eq? result 8))

(test-case "export-rename"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () ((a1 a)) (define-values (a1) 4))) null))
  (define l2 (compile-linklet '(linklet ((a)) () (+ a a))))
  (define result (instantiate-linklet l2 (list l1) (empty-target)))
  (check-eq? result 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uninitialize undefined exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "uninitialize undefined exports"
  (define l (compile-linklet '(linklet () (x))))
  (define t (empty-target))
  (instantiate-linklet l null t)
  (check-not-false (member 'x (instance-variable-names t))))

(test-case "don't touch if target has it"
  (define l (compile-linklet '(linklet () (x))))
  (define t (make-instance #f #f #f 'x 10))
  (instantiate-linklet l null t)
  (check-eq? (instance-variable-value t 'x) 10))

(test-case "target exports the same var with another external name"
  (define l (compile-linklet '(linklet () (x2) (+ x2 x2))))
  (define t (instantiate-linklet (compile-linklet '(linklet () ((x x2)) (define-values (x) 10))) null))
  (define result (instantiate-linklet l null t))
  (check-eq? result 20)
  (check-eq? (instance-variable-value t 'x2) 10)
  (check-false (member 'x (instance-variable-names t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; export rename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "export rename"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () ((x1 x)) (define-values (x1) 4))) null))
  (check-eq? (instance-variable-value l1 'x) 4)
  (define l2 (compile-linklet '(linklet ((x)) ((y1 y)) (define-values (y1) x) (+ x y1))))
  (define t (empty-target))
  (define result (instantiate-linklet l2 (list l1) t))
  (check-eq? result 8)
  (check-eq? (instance-variable-value t 'y) 4)
  (check-false (member 'x (instance-variable-names t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import rename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "import rename"
  (define l1 (instantiate-linklet
              (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
  (define l2 (instantiate-linklet
              (compile-linklet '(linklet () (x) (define-values (x) 10))) null))
  (define l3 (compile-linklet '(linklet (((x x1))((x x2))) () (+ x1 x2))))
  (define result (instantiate-linklet l3 (list l1 l2) (empty-target)))
  (check-eq? result 14))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval define values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "eval define-values"
  (define l (compile-linklet '(linklet () ((x x15)) (define-values (x) 4) (+ x x))))
  (define t (instantiate-linklet
             (compile-linklet '(linklet () ((x x16)) (define-values (x) 1000))) null))
  (define result (instantiate-linklet l null t))
  (check-eq? result 8)
  (check-not-false (member 'x15 (instance-variable-names t)))
  (check-not-false (member 'x16 (instance-variable-names t)))
  (check-false (member 'x (instance-variable-names t)))
  (check-eq? (instance-variable-value t 'x15) 4)
  (check-eq? (instance-variable-value t 'x16) 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; closures and variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "closures and variables"
  (define l1 (instantiate-linklet
              (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
  (define l2 (compile-linklet '(linklet ((x)) (g) (define-values (g) (lambda (y) x)))))
  (define t (empty-target))
  (define _ (instantiate-linklet l2 (list l1) t))
  (check-not-false (member 'g (instance-variable-names t)))
  (check-false (member 'x (instance-variable-names t)))

  ; # use the modified target
  (define l3 (compile-linklet '(linklet () (g) (g 5))))
  (define result (instantiate-linklet l3 null t))
  (check-eq? result 4)

  ; # import the closure
  (define l4 (compile-linklet '(linklet ((g)) () (g 3))))
  (define l5 (instantiate-linklet l2 (list l1)))
  (define result2 (instantiate-linklet l4 (list l5) (empty-target)))
  (check-eq? result2 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cannot mutate imported
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "mutating an imported variable is a compilation error"
  (check-exn exn:fail? (lambda () (compile-linklet '(linklet ((x)) () (set! x 5) (+ x x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "set!"
  (define l (compile-linklet '(linklet () () (define-values (x) 3) (set! x 5) (+ x x))))
  (define t (make-instance #f #f #f 'x 6))
  (define result (instantiate-linklet l null t))
  (check-not-false (member 'x (instance-variable-names t)))
  (check-eq? (instance-variable-value t 'x) 6)
  (check-eq? result 10))

(test-case "set!"
  (define l (compile-linklet '(linklet () (x) (set! x 5) (+ x x))))
  (define t (make-instance #f #f #f 'x 3))
  (define result (instantiate-linklet l null t))
  (check-eq? (instance-variable-value t 'x) 5)
  (check-eq? result 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; closure capture and reset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "closure capture and reset1"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) -1))) null))
  (define l2 (instantiate-linklet (compile-linklet '(linklet ((x)) (g) (define-values (g) (lambda (p) x)))) (list l1)))
  (define l3 (compile-linklet '(linklet ((g)) (x) (set! x 5) (g 1000))))
  (define t (make-instance #f #f #f 'x 2000))
  (define result (instantiate-linklet l3 (list l2) t))
  (check-eq? (instance-variable-value t 'x) 5)
  (check-eq? result -1))

(test-case "closure capture and reset2"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) -11))) null))
  (define l2 (instantiate-linklet (compile-linklet '(linklet ((x)) (g) (define-values (y) 131) (define-values (g) (lambda (p) (+ x y))) (set! y 71))) (list l1)))
  (define l3 (compile-linklet '(linklet ((g)) () (g -1))))
  (define result (instantiate-linklet l3 (list l2) (empty-target)))
  (check-eq? result 60))

(test-case "closure capture and reset3"
  (define l2 (compile-linklet '(linklet () (y) (define-values (y) 10) (set! y 50))))
  (define t2 (empty-target))
  (define _2 (instantiate-linklet l2 (list) t2))
  (define l4 (compile-linklet '(linklet () (y) (define-values (z) (+ y y)) (set! y 200) (define-values (y) 90) z)))
  (define result2 (instantiate-linklet l4 null t2))
  (check-eq? result2 100)
  (check-eq? (instance-variable-value t2 'y) 90))

(test-case "closure capture and reset3.5 (order matters)"
  (define l2 (compile-linklet '(linklet () (y) (define-values (y) 10) (set! y 50))))
  (define t2 (empty-target))
  (define _2 (instantiate-linklet l2 (list) t2))
  (define l4 (compile-linklet '(linklet () (y) (define-values (y) 90) (define-values (z) (+ y y)) (set! y 200) z)))
  (define result2 (instantiate-linklet l4 null t2))
  (check-eq? result2 180)
  (check-eq? (instance-variable-value t2 'y) 200))

(test-case "closure capture and reset4"
  (define l2 (compile-linklet '(linklet () (y g) (define-values (y) 10) (define-values (g) (lambda () y)) (set! y 50))))
  (define t2 (empty-target))
  (define _2 (instantiate-linklet l2 (list) t2))
  (define l4 (compile-linklet '(linklet () (y g) (set! y 200) (define-values (y) 90) (g))))
  (define result2 (instantiate-linklet l4 null t2))
  (check-eq? result2 90)
  (check-eq? (instance-variable-value t2 'y) 90))


(test-case "closure capture and reset"
  (define l2 (compile-linklet '(linklet () (y g) (define-values (y) 10) (define-values (g) (lambda () y)) (set! y 50))))
  (define t1 (empty-target))
  (define t2 (empty-target))
  (define t2-2 (empty-target))
  (define t3 (empty-target))
  (define _1 (instantiate-linklet l2 (list) t1))
  (define _2 (instantiate-linklet l2 (list) t2))
  (define _2-2 (instantiate-linklet l2 (list) t2-2))
  (define _3 (instantiate-linklet l2 (list) t3))

  (check-eq? (instance-variable-value t1 'y) 50)
  (check-eq? (instance-variable-value t2 'y) 50)
  (check-eq? (instance-variable-value t3 'y) 50)

  (define l3 (compile-linklet '(linklet () (y g) (set! y 300) (g))))
  (define result1 (instantiate-linklet l3 null t1))
  (check-eq? result1 300)

  (check-eq? (instance-variable-value t1 'y) 300)
  (check-eq? (instance-variable-value t2 'y) 50)
  (check-eq? (instance-variable-value t3 'y) 50)

  (define l4 (compile-linklet '(linklet () (y g) (set! y 200) (define-values (y) 90) (g))))
  (define result2 (instantiate-linklet l4 null t2))
  (check-eq? result2 90)

  (define l4-2 (compile-linklet '(linklet () (y g) (define-values (y) 90) (set! y 200) (g))))
  (define result2-2 (instantiate-linklet l4-2 null t2-2))
  (check-eq? result2-2 200)

  (check-eq? (instance-variable-value t1 'y) 300)
  (check-eq? (instance-variable-value t2 'y) 90)
  (check-eq? (instance-variable-value t3 'y) 50)

  (define l5 (compile-linklet '(linklet () (g) (define-values (y) 90) (+ y (g)))))
  (define result3 (instantiate-linklet l5 null t3))
  (check-eq? result3 140)

  (check-eq? (instance-variable-value t1 'y) 300)
  (check-eq? (instance-variable-value t2 'y) 90)
  (check-eq? (instance-variable-value t3 'y) 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reinstantiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "reinstantiation 1"
  (define l1 (compile-linklet '(linklet () (y) (define-values (x) (+ 10 y)) x)))
  (define t1 (make-instance #f #f #f 'y 30))
  (define t2 (make-instance #f #f #f 'y 40))
  (define result1 (instantiate-linklet l1 null t1))
  (define result2 (instantiate-linklet l1 null t2))
  (check-eq? result1 40)
  (check-eq? result2 50))

(test-case "reinstantiation 2"
  (define l1 (compile-linklet '(linklet () (y) (define-values (x) (+ 10 y)) (set! x y) x)))
  (define t1 (make-instance #f #f #f 'y 30))
  (define t2 (make-instance #f #f #f 'y 40))
  (define result1 (instantiate-linklet l1 null t1))
  (define result2 (instantiate-linklet l1 null t2))
  (check-eq? result1 30)
  (check-eq? result2 40))

(test-case "reinstantiation 3"
  (define l1 (compile-linklet '(linklet () (y) (define-values (x) (+ 10 y)) (set! x y) x)))
  (define t1 (instantiate-linklet (compile-linklet '(linklet () () (define-values (y) 30))) null))
  (define t2 (instantiate-linklet (compile-linklet '(linklet () () (define-values (y) 40))) null))

  (define result1 (instantiate-linklet l1 null t1))
  (define result2 (instantiate-linklet l1 null t2))
  (check-eq? result1 30)
  (check-eq? result2 40))

(test-case "reinstantiation 4"
  (define l1 (compile-linklet '(linklet () (y) (define-values (x) (+ 10 y)) (set! x y) x)))
  (define t1 (instantiate-linklet (compile-linklet '(linklet () () (define-values (y) 30) (set! y 30))) null))
  (define t2 (instantiate-linklet (compile-linklet '(linklet () () (define-values (y) 40) (set! y 40))) null))

  (define result1 (instantiate-linklet l1 null t1))
  (define result2 (instantiate-linklet l1 null t2))
  (check-eq? result1 30)
  (check-eq? result2 40))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "boxed immutable hash table (small-list.rkt)"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (h) (define-values (h) (box (hasheq))))) null))
  (define l2 (compile-linklet '(linklet ((h)) () (set-box! h (hash-set (unbox h) "a" 5)) (hash-ref (unbox h) "a" #f))))
  (define t (empty-target))

  (define result1 (instantiate-linklet l2 (list l1) t))
  (check-eq? result1 5)

  (define result2
    (instantiate-linklet
     (compile-linklet '(linklet ((h)) () (hash-ref (unbox h) "a" #f))) (list l1) t))
  (check-eq? result2 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "hashes"
  (define l1 (instantiate-linklet (compile-linklet '(linklet () (h) (define-values (h) (hasheq "a" 4 "b" 5)))) null))
  (define l2 (compile-linklet '(linklet ((h)) (h2) (define-values (h2) (hash-copy h)) (hash-ref h2 "b"))))
  (define t (empty-target))
  (define result1 (instantiate-linklet l2 (list l1) t))
  (check-eq? result1 5)
  (define l3 (compile-linklet '(linklet ((h2)) () (hash-ref h2 "a"))))
  ; taking the previous target as an imported instance
  (define result2 (instantiate-linklet l3 (list t) (empty-target)))
  (check-eq? result2 4))

(test-case "hash-set! on target"
  (define t (instantiate-linklet (compile-linklet '(linklet () () (define-values (h) (make-hasheq)))) null))
  (define l1 (compile-linklet '(linklet () (h) (hash-set! h "k" 150)(hash-set! h "y" 29))))
  (instantiate-linklet l1 null t)
  (define l2 (compile-linklet '(linklet () (h) (hash-set! h "y" 50) (hash-ref h "k"))))
  (define result1 (instantiate-linklet l2 null t))
  (check-eq? result1 150)
  (define l3 (compile-linklet '(linklet () (h) (+ (hash-ref h "k") (hash-ref h "y")))))
  (define result2 (instantiate-linklet l3 null t))
  (check-eq? result2 200))

(test-case "hash-set! on imported instance"
  (define l1-use-later (instantiate-linklet (compile-linklet '(linklet () (h) (define-values (h) (make-hasheq)))) null))
  (define l2 (compile-linklet '(linklet ((h)) () (hash-set! h "a" 5) (hash-set! h "b" 10) (hash-set! h "c" 20) (hash-ref h "a" #f))))
  (define t (empty-target))
  (define result1 (instantiate-linklet l2 (list l1-use-later) t))
  (check-eq? result1 5)

  (define l3 (compile-linklet '(linklet ((h)) () (hash-set! h "c" 200) (hash-ref h "b" #f))))
  (define result2 (instantiate-linklet l3 (list l1-use-later) t))
  (check-eq? result2 10)
  (define l4 (compile-linklet '(linklet ((h)) () (hash-ref h "c" #f))))
  (define result3 (instantiate-linklet l4 (list l1-use-later) t))
  (check-eq? result3 200)

  ; # slightly more complicated
  (define l10 (compile-linklet '(linklet ((h)) (g) (define-values (g) (hash-copy h)) (hash-ref g "c"))))
  (define t2 (make-instance #f #f #f))
  (define result4 (instantiate-linklet l10 (list l1-use-later) t2))
  (check-eq? result4 200)
  (define l11 (compile-linklet '(linklet () (g) (hash-set! g "a" -1) (hash-ref g "b"))))
  (define result5 (instantiate-linklet l11 null t2))
  (check-eq? result5 10)
  (define l12 (compile-linklet '(linklet () (g) (hash-ref g "a"))))
  (define result6 (instantiate-linklet l12 null t2))
  (check-eq? result6 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lets and Scopes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "lets and scopes"
  (define l (compile-linklet
             '(linklet () ()
                       (letrec-values (((fact) (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))))
                         (fact 5)))))
  (define result (instantiate-linklet l null (empty-target)))
  (check-eq? result 120))

(test-case "lets and scopes"
  (define l1
    (instantiate-linklet (compile-linklet
                          '(linklet () (add2)
                                    (define-values (add) (lambda (x) (lambda (y) (+ x y))))
                                    (define-values (add2) (add 2)))) null))
  (define l2 (compile-linklet '(linklet ((add2)) () (add2 6))))
  (define result (instantiate-linklet l2 (list l1) (empty-target)))
  (check-eq? result 8))

(test-case "lets and scopes"
  (define l (compile-linklet '(linklet () () ((((lambda (x) (lambda (x) (lambda (y) (+ x y)))) 1) 2) 3))))
  (define result (instantiate-linklet l null (empty-target)))
  (check-eq? result 5))

(test-case "lets and scopes"
  (define l (compile-linklet '(linklet () () (let-values (((x) (let-values (((x) 2)) (+ x x)))) (+ x x)))))
  (define result (instantiate-linklet l null (empty-target)))
  (check-eq? result 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Letrec RHS cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "letrec rhs cells"
  (define l1 (compile-linklet
             '(linklet () ()
                       (define-values (k)
                         (lambda (stx_32)
                           (letrec-values (
                                           [(e) 1]
                                           [(x) (+ stx_32 e)]
                                           )
                             x)))
                       (k 5))))
  (define result1 (instantiate-linklet l1 null (empty-target)))
  (check-eq? result1 6))

(test-case ""
  (define l2 (compile-linklet
             '(linklet () ()
                       (define-values (k)
                         (lambda (stx_32)
                           (letrec-values (
                                           [(e) 1]
                                           [(x) (+ stx_32 e)]
                                           [(p) ((lambda (x) x) x)]
                                           )
                             p)))
                       (k 5))))
  (define result2 (instantiate-linklet l2 null (empty-target)))
  (check-eq? result2 6)
)

(test-case "deep"
  (define l2 (compile-linklet
             '(linklet () ()
                       (define-values (k)
                         (lambda (stx_32)
                           (letrec-values (
                                           ((e) 1)
                                           ((x) (+ stx_32 e))
                                           ((p) ((lambda (x)
                                                   (letrec-values
                                                       (
                                                        ((u) e)
                                                        )
                                                     u)) x))
                                           )
                             p)))
                       (k 5))))
  (define result2 (instantiate-linklet l2 null (empty-target)))
  (check-eq? result2 1)
)

(test-case ""
  (define l (compile-linklet
             '(linklet
               () ()
               (let-values ()
                 (letrec-values (((f) (lambda (p) (g (- p 1))))
                                 ((g) (lambda (k) (if (zero? k) 1 (f k)))))
                   (g 100))))))
  (define result (instantiate-linklet l null (empty-target)))
  (check-eq? result 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context.normalize_term might be faulty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "Context.normalize_term might be faulty"
  (define l (compile-linklet
             '(linklet () ()
                       (let-values (((x) 5))
                         (+ x
                            (let-values (((x) 10)) x))))))
  (define result (instantiate-linklet l null (empty-target)))
  (check-eq? result 15))

(test-case "Context.normalize_term might be faulty"
  (define l (compile-linklet
             '(linklet () ()
                       (let-values (((x) 5))
                         (+ x
                            (let-values (((x) 10))
                              (+ x
                                 (let-values (((x) 20) ((y) 21)) (+ x y)))))))))
  (define result (instantiate-linklet l null (empty-target)))
  (check-eq? result 56))
