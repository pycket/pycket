#lang racket/base

(provide (all-defined-out))

#|
a 	Matches the specified character literal 	q 	q
* 	Matches 0 or more of the previous character 	a* 	"", a, aa, aaa
? 	Matches 0 or 1 of the previous character 	a? 	"", a
. 	Matches any character literal 	. 	a, b, c, d, e ...
^ 	Matches the start of a string 	^c 	c, ca, caa, cbb ...
$ 	Matches the end of a string 	a$ 	ba, baaa, qwerta ...
|#

(require ffi/unsafe/vm)
(require racket/unsafe/ops racket/fixnum)

(define pycket:pe-stop (vm-primitive 'pycket:pe-stop))

(define (match-huh pattern p-pos input-str s-pos huh-pos m)
  (or
   (and (< s-pos (string-length input-str))
        (char=? (string-ref pattern p-pos) (unsafe-string-ref input-str s-pos))
        (match-pat pattern (add1 huh-pos) input-str (add1 s-pos)
                   (cons (unsafe-string-ref input-str s-pos) m)))
   (match-pat pattern (add1 huh-pos) input-str s-pos m)))

(define (match-star pattern p-pos input-str s-pos star-pos m)
  (let ([star-stop
         (and (< s-pos (string-length input-str))
              (char=? (string-ref pattern p-pos) (unsafe-string-ref input-str s-pos))
              (pycket:pe-stop
               match-pat pattern p-pos input-str (add1 s-pos)
               (cons (unsafe-string-ref input-str s-pos) m)))])
    (if star-stop
        star-stop
        (match-pat pattern (add1 star-pos) input-str s-pos m))))

(define (match-pat pattern p-pos input-str s-pos m)
  (let ([pat-len (string-length pattern)]
        [str-len (string-length input-str)])
    (cond
      ; done with the pattern
      [(>= p-pos pat-len) m]
      ; end of string ($)
      [(char=? (string-ref pattern p-pos) #\$)
       (and (>= s-pos str-len) m)]
      ; ?
      [(and (< (+ p-pos 1) pat-len)
            (char=? (string-ref pattern (+ p-pos 1)) #\?))
       (match-huh pattern p-pos input-str s-pos (+ p-pos 1) m)]
      ; *
      [(and (< (+ p-pos 1) pat-len)
            (char=? (string-ref pattern (+ p-pos 1)) #\*))
       (match-star pattern p-pos input-str s-pos (+ p-pos 1) m)]
      ; wildcard (.)
      [(char=? (string-ref pattern p-pos) #\.)
       (and (< s-pos str-len)
            (match-pat pattern (add1 p-pos) input-str (add1 s-pos)
                       (cons (unsafe-string-ref input-str s-pos) m)))]
      ; literal
      [else
       (let ([ss (unsafe-string-ref input-str s-pos)])
         (and (char=? (string-ref pattern p-pos) ss)
              (match-pat pattern (add1 p-pos) input-str (unsafe-fx+ s-pos 1) (cons ss m))))])))

(define (reg-match pattern input-str residual-func)
  (let ([m (reg-match-wrapped pattern input-str residual-func)])
    (and m (list (list->string (reverse m))))))

(define (reg-match-wrapped pattern input-str residual-func)
  (cond
    [(char=? (string-ref pattern 0) #\^) '()]
    #;[(string=? input-str "") (match-pat pattern 0 input-str 0 '())] ; edge case
    [else
     (let ([lim (- (string-length input-str)
                   (string-length pattern))])
       (let loop ([i 0])
         (if (>= i lim)
             #f
             (or (match-pat pattern 0 input-str i '())
                 #;(residual-func input-str i)
                 (loop (unsafe-fx+ i 1))))))]))
