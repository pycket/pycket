#lang racket/base
(define-values (struct:struct-port make-struct-port struct-port? struct-port-ref struct-port-set!)
  (make-struct-type 'struct-port #f 2 0 0
                    (list (cons prop:input-port 0)
                          (cons prop:output-port 1))
                    #f
                    #f
                    (list 0 1)))
(define asp1 (make-struct-port (open-input-string "akg cdef")
                               (open-output-string)))
(read asp1)
(read-char asp1)
(peek-char asp1)
(close-input-port asp1)

(write-string "0123" asp1)
(write-char #\c asp1)
(write-byte 1 asp1)
(get-output-string (struct-port-ref asp1 1))
