#lang racket/base

(require '#%linklet)

(require test-engine/racket-tests)

(define (in? ls)
  (lambda (sym)
    (if (member sym ls) #t #f)))

(define (not-in? ls)
  (lambda (sym)
    (if (member sym ls) #f #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l1 (compile-linklet '(linklet () (x) (define-values (x) 4))))
(define l1-inst (instantiate-linklet l1 null))
(check-expect (instance-variable-value l1-inst 'x) 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l21 (compile-linklet '(linklet () (x) (define-values (x) 4) (+ x x))))
(define t2 (make-instance #f #f #f 'x 1 'y 2))
(define result2 (instantiate-linklet l21 null t2))
(check-expect result2 8)
(check-expect (instance-variable-value t2 'x) 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target_def_overwrite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l31 (compile-linklet '(linklet () (x) (define-values (x) 4) (+ x x))))
(define t31 (make-instance #f #f #f 'x 1 'y 2))
(define result31 (instantiate-linklet l31 null t31))
(check-expect result31 8)
(check-expect (instance-variable-value t31 'x) 4)
(check-expect (instance-variable-value t31 'y) 2)

;;;; ; if linklet doesn't export, then target's def stay the same
(define l32 (compile-linklet '(linklet () () (define-values (x) 4) (+ x x))))
(define t32 (make-instance #f #f #f 'x 1 'y 2))
(define result32 (instantiate-linklet l32 null t32))
(check-expect result32 8)
(check-expect (instance-variable-value t32 'x) 1)
(check-expect (instance-variable-value t32 'y) 2)

;;;;; if target doesn't have it, then it doesn't matter if linklet exports or not,
;;;;;  put the variable in the target
(define l33 (compile-linklet '(linklet () () (define-values (z) 4) z)))
(define t33 (make-instance #f #f #f))
(define result33 (instantiate-linklet l33 null t33))
(check-expect result33 4)
(check-expect (instance-variable-value t33 'z) 4)

;;;;; imported variables doesn't get into target at all ... let alone overwrite any var inside the target
(define l34-inst (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
(define l35 (compile-linklet '(linklet ((x)) () (+ x x))))
(define t34 (make-instance #f #f #f 'x 1))
(define t35 (make-instance #f #f #f))
(define result3 (instantiate-linklet l35 (list l34-inst) t34))
(define result4 (instantiate-linklet l35 (list l34-inst) t35))
(check-expect result3 8)
(check-expect (instance-variable-value t34 'x) 1)
(check-expect (instance-variable-names t35) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defs_export_names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l41-inst (instantiate-linklet (compile-linklet '(linklet () ((x x15)) (define-values (x) 4))) null))
(check-satisfied 'x (not-in? (instance-variable-names l41-inst)))
(check-satisfied 'x15 (in? (instance-variable-names l41-inst)))

;;;;;  LinkletVars will be referred by the external name (e.g. (+ x15 x15)
(define l42 (compile-linklet '(linklet () ((x x15)) (define-values (x) 4) (+ x x))))
(define t41 (make-instance #f #f #f))
(define result (instantiate-linklet l42 null t41))
(check-expect result 8)
(check-satisfied 'x (not-in? (instance-variable-names l41-inst)))
(check-satisfied 'x15 (in? (instance-variable-names l41-inst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; discarding_defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l51-inst (instantiate-linklet (compile-linklet '(linklet () ((x x15)) (define-values (x) 4) (define-values (x15) 75))) null))
(check-satisfied 'x (not-in? (instance-variable-names l51-inst)))
(check-expect (instance-variable-value l51-inst 'x15) 4)

(define l52-inst (instantiate-linklet
                  (compile-linklet
                   '(linklet () ((x x15) k) (define-values (x) 4) (define-values (x15) 75) (define-values (k) x15))) null))
(check-satisfied 'x (not-in? (instance-variable-names l52-inst)))
(check-expect (instance-variable-value l52-inst 'x15) 4)
(check-expect (instance-variable-value l52-inst 'k) 75)
(check-expect (instance-variable-value l52-inst (list-ref (instance-variable-names l52-inst) 2)) 75)

;; uninterned symbol W_Symbol("str")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use targets def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l61 (compile-linklet '(linklet () (x) (+ x x))))
(define t61 (make-instance #f #f #f 'x 10))
(define result61 (instantiate-linklet l61 null t61))
(check-expect result61 20)

;;;;; use linklet's definition if both linklet and target have it
(define l62 (compile-linklet '(linklet () () (define-values (x) 4) (+ x x)))) ; doesn't export x
(define t62 (make-instance #f #f #f 'x 10))
(define result62 (instantiate-linklet l62 null t62))
(check-expect result62 8)
(check-expect (instance-variable-value t62 'x) 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic import
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l71-inst (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
(define l72 (compile-linklet '(linklet ((x)) () (+ x x))))
(define targ71 (make-instance #f #f #f))
(define result71 (instantiate-linklet l72 (list l71-inst) targ71))
(check-expect result71 8)

(define l73-inst (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
(define l74 (compile-linklet '(linklet ((x)) (y) (define-values (y) (+ x x)) (+ y y))))
(define targ72 (make-instance #f #f #f))
(define result72 (instantiate-linklet l74 (list l73-inst) targ72))
(check-expect result72 16)
(check-expect (instance-variable-value targ72 'y) 8)
(check-satisfied 'x (not-in? (instance-variable-names targ72)))
    
;; # target's defs are overwritten only if the linklet has a definition
;; # not with an imported variable
(define l75-inst (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
(define l76 (compile-linklet '(linklet ((x)) () (+ x x))))
(define targ73 (make-instance #f #f #f 'x 1000))
(define result73 (instantiate-linklet l76 (list l75-inst) targ73))
(check-expect result73 8)
(check-expect (instance-variable-value targ73 'x) 1000)

;; ## same thing with the import renaming
(define l77-inst (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
(define l78 (compile-linklet '(linklet (((x x2))) () (+ x2 x2))))
(define targ74 (make-instance #f #f #f 'x 1000 'x2 2000))
(define result74 (instantiate-linklet l78 (list l77-inst) targ74))
(check-expect result74 8)
(check-expect (instance-variable-value targ74 'x) 1000)
(check-expect (instance-variable-value targ74 'x2) 2000)

;; ## slightly trickier
(define l79-inst (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
(define l710 (compile-linklet '(linklet (((x x2))) () (define-values (x) 14) (+ x2 x))))
(define targ75 (make-instance #f #f #f 'x 1000 'x2 2000))
(define result75 (instantiate-linklet l710 (list l79-inst) targ75))
(check-expect result75 18)
(check-expect (instance-variable-value targ75 'x) 1000)
(check-expect (instance-variable-value targ75 'x2) 2000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l80-inst (instantiate-linklet (compile-linklet '(linklet () (a) (define-values (a) 4))) null))
(define l81 (compile-linklet '(linklet ((a)) () (+ a a))))
(define targ80 (make-instance #f #f #f))
(define result80 (instantiate-linklet l81 (list l80-inst) targ80))
(check-expect result80 8)
    
;; # export-rename
(define l81-inst (instantiate-linklet (compile-linklet '(linklet () ((a1 a)) (define-values (a1) 4))) null))
(define l82 (compile-linklet '(linklet ((a)) () (+ a a))))
(define targ81 (make-instance #f #f #f))
(define result81 (instantiate-linklet l82 (list l81-inst) targ81))
(check-expect result81 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uninitialize undefined exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l90 (compile-linklet '(linklet () (x))))
(define targ90 (make-instance #f #f #f))
(instantiate-linklet l90 null targ90)
(check-satisfied 'x (in? (instance-variable-names targ90)))

;; # don't touch if target has it
(define targ91 (make-instance #f #f #f 'x 10))
(instantiate-linklet l90 null targ91)
(check-expect (instance-variable-value targ91 'x) 10)

;; # target exports the same var with another external name
(define l91 (compile-linklet '(linklet () (x2) (+ x2 x2))))
(define targ92 (instantiate-linklet (compile-linklet '(linklet () ((x x2)) (define-values (x) 10))) null))
(define result90 (instantiate-linklet l91 null targ92))
(check-expect result90 20)
(check-expect (instance-variable-value targ92 'x2) 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; export rename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l100-inst (instantiate-linklet (compile-linklet '(linklet () ((x1 x)) (define-values (x1) 4))) null))
(check-expect (instance-variable-value l100-inst 'x) 4)
(define l101 (compile-linklet '(linklet ((x)) ((y1 y)) (define-values (y1) x) (+ x y1))))
(define targ100 (make-instance #f #f #f))
(define result100 (instantiate-linklet l101 (list l100-inst) targ100))
(check-expect result100 8)
(check-expect (instance-variable-value targ100 'y) 4)
(check-satisfied 'x (not-in? (instance-variable-names targ100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import rename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l110-inst (instantiate-linklet
                   (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
(define l111-inst (instantiate-linklet
                   (compile-linklet '(linklet () (x) (define-values (x) 10))) null))
(define l112 (compile-linklet '(linklet (((x x1))((x x2))) () (+ x1 x2))))
(define targ110 (make-instance #f #f #f))
(define result110 (instantiate-linklet l112 (list l110-inst l111-inst) targ110))
(check-expect result110 14)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval define values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l120 (compile-linklet '(linklet () ((x x15)) (define-values (x) 4) (+ x x))))
(define targ120 (instantiate-linklet
                 (compile-linklet '(linklet () ((x x16)) (define-values (x) 1000))) null))
(define result120 (instantiate-linklet l120 null targ120))
(check-expect result120 8)
(check-satisfied 'x15 (in? (instance-variable-names targ120)))
(check-satisfied 'x16 (in? (instance-variable-names targ120)))
(check-satisfied 'x (not-in? (instance-variable-names targ120)))
(check-expect (instance-variable-value targ120 'x15) 4)
(check-expect (instance-variable-value targ120 'x16) 1000)

#;(instance-variable-names targ120)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target def stays the same
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; # use the local var, don't change target's var if you don't export
(define l130 (compile-linklet '(linklet () ((x x15)) (define-values (x) 4) (+ x x))))
(define targ130-inst (instantiate-linklet (compile-linklet '(linklet () () (define-values (x) 10))) null))
(define targ131-inst (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 10))) null))
(define result130 (instantiate-linklet l130 null targ130-inst))
(define result131 (instantiate-linklet l130 null targ131-inst))
(check-expect result130 8)
(check-expect result131 8)
(check-expect (instance-variable-value targ130-inst 'x) 10)
(check-expect (instance-variable-value targ131-inst 'x) 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; closures and variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l140-inst (instantiate-linklet
                   (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
(define l141 (compile-linklet '(linklet ((x)) (g) (define-values (g) (lambda (y) x)))))
(define targ140 (make-instance #f #f #f))
(define result140 (instantiate-linklet l141 (list l140-inst) targ140))
#;(check-expect result140 4)
(check-satisfied 'g (in? (instance-variable-names targ140)))
(check-satisfied 'x (not-in? (instance-variable-names targ140)))

#;(instance-variable-names targ140)

#;((instance-variable-value targ140 'g) 2)

; # use the modified target
(define l142 (compile-linklet '(linklet () (g) (g 5))))
(define result141 (instantiate-linklet l142 null targ140))
(check-expect result141 4)

; # import the closure
(define l143 (compile-linklet '(linklet ((g)) () (g 3))))
(define l141-inst (instantiate-linklet l141 (list l140-inst)))
(define targ141 (make-instance #f #f #f))
(define result142 (instantiate-linklet l143 (list l141-inst) targ141))
(check-expect result142 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; # mutating an imported variable is a compilation error
(check-error (compile-linklet '(linklet ((x)) () (set! x 5) (+ x x))))

(define l150 (compile-linklet '(linklet () () (define-values (x) 3) (set! x 5) (+ x x))))
(define targ150 (make-instance #f #f #f))
(define result150 (instantiate-linklet l150 null targ150))
(check-satisfied 'x (in? (instance-variable-names targ150)))
(check-expect (instance-variable-value targ150 'x) 5)
(check-expect result150 10)

(define l151 (compile-linklet '(linklet () (x) (set! x 5) (+ x x))))
(define targ151 (make-instance #f #f #f 'x 3))
(define result151 (instantiate-linklet l151 null targ151))
(check-expect (instance-variable-value targ151 'x) 5)
(check-expect result151 10)


;; (define _l-inst (instantiate-linklet (compile-linklet '(linklet () (x) (define-values (x) 4))) null))
;; (define _l (compile-linklet '(linklet ((x)) () (define-values (x) 3) (+ x x))))
;; (define _targ (make-instance #f #f #f))
;; (instantiate-linklet _l (list _l-inst) _targ)

;; extra

;; (define l1000-inst (instantiate-linklet (compile-linklet '(linklet () (d) (define-values (d) 15))) null))
;; (define l1001 (compile-linklet '(linklet ((d)) (g) (define-values (g) (lambda (x) d)) (g 3))))
;; (define targ1000 (make-instance #f #f #f))
;; (define result1000 (instantiate-linklet l1001 (list l1000-inst) targ1000))
;; (check-expect result1000 15)
;; (check-satisfied 'g (in? (instance-variable-names targ1000)))
;; (instance-variable-names targ1000)




(test)
