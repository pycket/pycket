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
(define pycket:pe (vm-primitive 'pycket:pe))

(define (match-huh pattern p-pos input-str s-pos huh-pos m)
  (or
   (and (< s-pos (string-length input-str))
        (char=? (string-ref pattern p-pos) (string-ref input-str s-pos))
        (match-pat pattern (add1 huh-pos) input-str (add1 s-pos)
                   (cons (string-ref input-str s-pos) m)))
   (match-pat pattern (add1 huh-pos) input-str s-pos m)))

(define (match-star pattern p-pos input-str s-pos star-pos m)
  (or
   (and (< s-pos (string-length input-str))
        (char=? (string-ref pattern p-pos) (string-ref input-str s-pos))
        (match-pat pattern p-pos input-str (add1 s-pos)
                   (cons (string-ref input-str s-pos) m)))
   (match-pat pattern (add1 star-pos) input-str s-pos m)))

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
                       (cons (string-ref input-str s-pos) m)))]
      ; literal
      [else
       (let ([ss (string-ref input-str s-pos)])
         (and (char=? (string-ref pattern p-pos) ss)
              (match-pat pattern (add1 p-pos) input-str (add1 s-pos) (cons ss m))))])))

(define (reg-match pattern input-str residual-func)
  (let ([m (reg-match-wrapped pattern input-str residual-func)])
    (and m (list (list->string (reverse m))))))

(define (reg-match-wrapped pattern input-str residual-func)
  (cond
    [(char=? (string-ref pattern 0) #\^) '()]
    #;[(string=? input-str "") (match-pat pattern 0 input-str 0 '())] ; edge case
    [else
     (for/or ([i (in-range (string-length input-str))])
       #;(residual-func input-str i)
       #;(let ((k (residual-func input-str i)))
         (printf "trying position -- i : ~a -- k : ~a\n" i k)
         k)

       (match-pat pattern 0 input-str i '())

       #;(lambda (input-str s-pos)
         (if (char=? #\d (string-ref input-str s-pos))
             (if (char=? #\e (string-ref input-str (add1 s-pos)))
                 (if (char=? #\f (string-ref input-str (add1 (add1 s-pos))))
                     (if (char=? #\g (string-ref input-str (add1 (add1 (add1 s-pos)))))
                         (cons (string-ref input-str (add1 (add1 (add1 s-pos))))
                               (cons (string-ref input-str (add1 (add1 s-pos)))
                                     (cons (string-ref input-str (add1 s-pos))
                                           (cons (string-ref input-str s-pos)
                                                 ())))) #f) #f) #f) #f))
       )]))
