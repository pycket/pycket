(let()
    [define elements 1000000]
    [define loops 20]
    [define vec (make-vector elements 2)]
    [define do-loop-vec (lambda(n) (if (< n 0) vec 
        (begin
            (vector-set! vec n (* 3 (vector-ref vec n)))
            (do-loop-vec (sub1 n)))))]
    [define loop-vec (lambda () (do-loop-vec (sub1 elements)))]
    [define repeat-times (lambda (n f) (if (= n 0) (f) (begin (f) (repeat-times (sub1 n) f))))]

    (time (repeat-times loops loop-vec))

    (display (cons (vector-length vec) (vector-ref vec 0)))
)
