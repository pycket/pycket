;; Pycket TCP API primitives; dummy implementation on top of Racket's TCP

#lang racket/base

(require (prefix-in lib: racket/tcp) ffi/unsafe)
(provide
  tcp-listener?
  tcp-listen
  tcp-socket?
  tcp-accept
  tcp-connect
  tcp-setsockopt-NODELAY-0!
  tcp-setsockopt-NODELAY-1!
  tcp-send
  tcp-sendall
  tcp-recv
  tcp-close
  tcp-recv-chunk)


;; The following code is from:
;; http://macrologist.blogspot.co.uk/2012/03/avoid-flushing-your-wire-protocols.html

(define IPPROTO_TCP 6)
(define TCP_NODELAY 1)

(define setsockopt_tcp_nodelay
  (get-ffi-obj "setsockopt" #f
    (_fun (socket enabled?) ::
          (socket : _int)
          (_int = IPPROTO_TCP)
          (_int = TCP_NODELAY)
          (enabled-ptr : (_ptr i _int)
                       = (if enabled? 1 0))
          (_int = (compiler-sizeof 'int))
          -> (result : _int)
          -> (if (zero? result)
                 (void)
                 (error 'set-tcp-nodelay! "failed")))))

(define scheme_get_port_socket
  (get-ffi-obj "scheme_get_port_socket" #f
    (_fun (port) ::
          (port : _racket)
          (socket : (_ptr o _intptr))
          -> (result : _int)
          -> (and (positive? result) socket))))

; set-tcp-nodelay! : tcp-port boolean -> void
(define (set-tcp-nodelay! port enabled?)
  (let ([socket (scheme_get_port_socket port)])
    (setsockopt_tcp_nodelay socket enabled?)))


;; Returns `#t` or `#f`.
(define (tcp-listener? x)
  (lib:tcp-listener? x))

;; Returns a listener, or `#f` if binding to `port` fails.
(define (tcp-listen port [backlog 5])
  (define listener-or-#f
    (with-handlers ([exn:fail:network? (lambda (exn) #f)])
      (lib:tcp-listen port backlog)))
  listener-or-#f)


;; Returns `#t` or `#f`.
(define (tcp-socket? x)
  (and (pair? x)
       (lib:tcp-port? (car x)) (input-port?  (car x))
       (lib:tcp-port? (cdr x)) (output-port? (cdr x))))

;; auxiliary functions for sockets (not exported)
(define (inport  socket) (car socket))
(define (outport socket) (cdr socket))

;; Blocks until returning a socket.
(define (tcp-accept listener)
  (define-values (in out) (lib:tcp-accept listener))
  (define socket (cons in out))
  socket)

;; Returns a socket, or `#f` if connecting fails.
(define (tcp-connect host port)
  (define socket-or-#f
    (with-handlers ([exn:fail:network? (lambda (exn) #f)])
      (define-values (in out) (lib:tcp-connect host port))
      (define socket (cons in out))
      socket))
  socket-or-#f)

;; Switch on the Nagle algorithm; returns nothing.
;; We only modify the inport's socket, since they should
;; be the same socket
(define (tcp-setsockopt-NODELAY-0! socket)
  (set-tcp-nodelay! (inport socket) #f))

;; Switch off the Nagle algorithm; returns nothing.
(define (tcp-setsockopt-NODELAY-1! socket)
  (set-tcp-nodelay! (inport socket) #t))

;; Blocks until some bytes from byte string `bstr` can be sent and flushed;
;; returns the number of bytes sent (1 <= length `bstr` unless `bstr` empty).
(define (tcp-send socket bstr)
  (write-bytes-avail bstr (outport socket)))

;; Blocks until byte string `bstr` is sent (ie. flushed out of buffers);
;; returns nothing.
(define (tcp-sendall socket bstr)
  (write-bytes bstr (outport socket))
  (flush-output (outport socket)))

;; Blocks until at least one byte can be read or connection is closed;
;; returns a (mutable) byte string of length <= `max_bytes`.
(define (tcp-recv socket max_bytes)
  (define buf (make-bytes max_bytes))
  (define len (read-bytes-avail! buf (inport socket)))
  (define bstr
    (cond [(eof-object? len)   (make-bytes 0)]
          [(eq? len max_bytes) buf]
          [else                (subbytes buf 0 len)]))
  bstr)


;; Returns nothing.
(define (tcp-close listener-or-socket)
  (cond
    [(tcp-listener? listener-or-socket)
       (lib:tcp-close listener-or-socket)]
    [else
       (close-input-port  (inport  listener-or-socket))
       (close-output-port (outport listener-or-socket))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vectored send and receive

;; Reads `n` bytes from `socket` where `n` a multiple of `align` and <= `size`.
;; On success returns `n` and a mutable bytestring `bstr` containing the bytes,
;; where the length of `bstr` is a multiple of `align` and <= `size`.
;; Returns `n = 0` if end-of-file or an error is encountered.
;; Requirement: `align` must be positive and `size` a pos multiple of `align`.
(define (tcp-recv-chunk socket size [align 8])
  (define buf (make-bytes size))
  (define (loop n0)
    (cond
      [(eof-object? n0)          (cons 0  buf)]  ;; eof: `buf` contents undef'd
      [(zero? (modulo n0 align)) (cons n0 buf)]
      [else
        (define k  (- align (modulo n0 align)))
        (define n1 (read-bytes-avail! buf (inport socket) n0 (+ n0 k)))
        (loop n1)]))
  (define n (read-bytes-avail! buf (inport socket)))
  (if (eq? n size)
    (cons n buf)  ;; then branch: fast path
    (loop n)))    ;; else branch: `n` is eof or < `size`
