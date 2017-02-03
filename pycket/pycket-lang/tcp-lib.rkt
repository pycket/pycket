;; Pycket TCP API; library functions (re-exporting primitives)

#lang racket/base

(require pycket/extra-prims/tcp)
(provide
  bytes-vector?
  bytes-vector-size
  chunksize
  chunkbuf
  tcp-vsend
  tcp-vrecv)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vectored send and receive

;; Returns `#t` or `#f`.
(define (bytes-vector? x)
  (and
    (vector? x)
    (for/and ([chunk (in-vector x)])
      (and
        (pair? chunk)
        (exact-positive-integer? (car chunk))
        (bytes? (cdr chunk))
        (<= (car chunk) (bytes-length (cdr chunk)))))))

(define (chunksize chunk) (car chunk))
(define (chunkbuf  chunk) (cdr chunk))

;; Returns the number of bytes stored in bytes-vector `vbs`.
(define (bytes-vector-size vbs)
  (for/sum ([chunk (in-vector vbs)]) (chunksize chunk)))


;; Blocks until bytes-vector `vbs` is sent (ie. flushed out of buffers);
;; returns nothing.
(define (tcp-vsend socket vbs)
  (for ([chunk (in-vector vbs)])
    (define n    (chunksize chunk))
    (define buf  (chunkbuf  chunk))
    (define bstr (if (= n (bytes-length buf)) buf (subbytes buf 0 n)))
    (tcp-sendall socket bstr)))


;; Blocks until having received `amt` bytes on `socket`.
;; Returns a bytes-vector the chunksizes of which as multiples of `align`
;; (except possibly the last chunk) and bounded by `max_chunksize`.
;; The requirements for `align` and `max_chunksize` are the same as for
;; `align` and `size` of function `tcp-recv-chunk`.
(define (tcp-vrecv socket amt [max_chunksize 4096] [align 8])
  ;; loop accumulating chunks once amt < max_chunksize
  (define (small-loop amt chunks)
    (if (zero? amt)
      ;; amt == 0: return accu (reversed and as vector)
      (list->vector (reverse chunks))
      ;; amt > 0
      (let ([aligned_amt (* (quotient amt align) align)])
        (if (zero? aligned_amt)
          ;; 0 < amt < align
          (let ([chunk (tcp-recv-chunk socket amt amt)])
            (if (zero? (chunksize chunk))
              ;; error or unexpected EOF: return empty vector
              (vector)
              ;; else: return chunk + accu (reversed and as vector)
              (list->vector (reverse (cons chunk chunks)))))
          ;; amt >= aligned_amt >= align
          (let ([chunk (tcp-recv-chunk socket aligned_amt align)])
            (if (zero? (chunksize chunk))
              ;; error or unexpected EOF: return empty vector
              (vector)
              ;; else: loop (adding chunk to accu)
              (small-loop (- amt (chunksize chunk)) (cons chunk chunks))))))))
  ;; loop accumulating chunks as long as amt >= max_chunksize
  (define (big-loop amt chunks)
    (if (< amt max_chunksize)
      ;; amt < max_chunksize: jump to small chunk loop
      (small-loop amt chunks)
      ;; amt >= max_chunksize >= align
      (let ([chunk (tcp-recv-chunk socket max_chunksize align)])
        (if (zero? (chunksize chunk))
          ;; error or unexpected EOF: return empty vector
          (vector)
          ;; else: loop (adding chunk to accu)
          (big-loop (- amt (chunksize chunk)) (cons chunk chunks))))))
  ;; start big chunk loop with empty accu
  (big-loop amt '()))
