#lang racket/base

(define-values (struct:struct-input-port make-struct-input-port struct-input-port? struct-input-port-ref struct-input-port-set!)
  (make-struct-type 'struct-input-port #f 2 0 0
                    (list (cons prop:input-port 0))
                    #f
                    #f
                    (list 0 1)))
(define a-struct-input-port (make-struct-input-port 0 1))
(define isp1 (make-struct-input-port (open-input-string "a b c d e f g")
                               (open-output-string)))
(read isp1)
(read-char isp1)
(peek-char isp1)
;(read isp1)



(define-values (struct:struct-port make-struct-port struct-port? struct-port-ref struct-port-set!)
  (make-struct-type 'struct-port #f 2 0 0
                    (list (cons prop:input-port 0)
                          (cons prop:output-port 1))
                    #f
                    #f
                    (list 0 1)))
(define a-struct-port (make-struct-port 0 1))
(define asp1 (make-struct-port isp1
                               (open-output-string)))
(write-string "0" asp1)
(write-string "1" asp1)
(write-string "2" asp1)

(read isp1)
(read asp1)
(read asp1)
(read asp1)


(get-output-string (struct-port-ref asp1 1))
;(get-output-string asp1)
(close-input-port isp1)

