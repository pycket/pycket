#lang pycket

(define make-node-topdown cons)
(define (make-empty-node-topdown) #f)
(define (MakeTree iDepth)
  (if (<= iDepth 0)
      (make-empty-node-topdown)
      (make-node-topdown (MakeTree (- iDepth 1))
                         (MakeTree (- iDepth 1)))))

(define (make-empty-node) (make-vector 4 0))
(define (make-node l r)
   (let ((v (make-empty-node)))
     (vector-set! v 0 l)
     (vector-set! v 1 r)
     v))
(define (node.left node) (vector-ref node 0))
(define
       node.right (lambda (node) (vector-ref node 1)))
(define
       node.left-set! (lambda (node x) (vector-set! node 0 x)))
(define
       node.right-set! (lambda (node x) (vector-set! node 1 x)))
  
  ;  Build tree top down, assigning to older objects.
(define (Populate iDepth thisNode)
  (if (<= iDepth 0)
      #f
      (let ((iDepth (- iDepth 1)))
        (node.left-set! thisNode (make-empty-node))
        (node.right-set! thisNode (make-empty-node))
        (Populate iDepth (node.left thisNode))
        (Populate iDepth (node.right thisNode)))))
;(display (MakeTree 3))
(time (Populate 23 (make-empty-node)))
(time (MakeTree 23))
