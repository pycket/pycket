#lang pycket

(define SIZE 20)

(struct leaf (val))
(struct node leaf (left right))

(define (generate-tree item d)
  (if (= d 0)
    (leaf item)
    (let ([item2 (* item 2)]
          [d2 (- d 1)])
      (node item (generate-tree (- item2 1) d2) (generate-tree item2 d2)))))

(define (in? n b)
  (cond
    [(null? b) #f]
    [else (cond
            [(= n (leaf-val b))
             #t]
            [(leaf? b) #f]
            [(< n (leaf-val b))
             (in? n (node-left b))]
            [(> n (leaf-val b))
             (in? n (node-right b))])]))

; (define (print-tree b)
;   (cond
;     [(null? b) #f]
;     [else (cond
;             [(leaf? b) (display leaf-val b)]
;             [(leaf? node-left b) (print-tree node-left b)]
;             [(leaf? node-right b) (print-tree node-right b)])]))

(generate-tree 1 SIZE)
