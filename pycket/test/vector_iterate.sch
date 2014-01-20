(letrec(
    [elements 1000000]
    [loops 20]
    [vec (make-vector elements 2)]
    [do-loop-vec (lambda(n) (if (< n 0) vec 
        (begin
            (vector-set! vec n (* 3 (vector-ref vec n)))
            (do-loop-vec (sub1 n)))))]
    [loop-vec (lambda () (do-loop-vec (sub1 elements)))]
    [repeat-times (lambda (n f) (if (= n 0) (f) (begin (f) (repeat-times (sub1 n) f))))])

    (repeat-times loops loop-vec)

    (cons (vector-length vec) (vector-ref vec 0))
)
