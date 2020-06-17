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

(define (match-huh pattern p-pos str s-pos huh-pos m)
  (or
   (and (< s-pos (string-length str))
        (char=? (string-ref pattern p-pos) (string-ref str s-pos))
        (match-pat pattern (add1 huh-pos) str (add1 s-pos)
                   (cons (string-ref str s-pos) m)))
   (match-pat pattern (add1 huh-pos) str s-pos m)))

(define (match-star pattern p-pos str s-pos star-pos m)
  (or
   (and (< s-pos (string-length str))
        (char=? (string-ref pattern p-pos) (string-ref str s-pos))
        (match-pat pattern p-pos str (add1 s-pos)
                   (cons (string-ref str s-pos) m)))
   (match-pat pattern (add1 star-pos) str s-pos m)))

(define (match-pat pattern p-pos str s-pos m)
  (let ([pat-len (string-length pattern)]
        [str-len (string-length str)])  
    (cond
      ; done with the pattern
      [(>= p-pos pat-len) m]
      ; end of string ($)
      [(char=? (string-ref pattern p-pos) #\$)
       (and (>= s-pos str-len) m)]
      ; ?
      [(and (< (+ p-pos 1) pat-len)
            (char=? (string-ref pattern (+ p-pos 1)) #\?))
       (match-huh pattern p-pos str s-pos (+ p-pos 1) m)]
      ; *
      [(and (< (+ p-pos 1) pat-len)
            (char=? (string-ref pattern (+ p-pos 1)) #\*))
       (match-star pattern p-pos str s-pos (+ p-pos 1) m)]
      ; wildcard (.)
      [(char=? (string-ref pattern p-pos) #\.)
       (and (< s-pos str-len)
            (match-pat pattern (add1 p-pos) str (add1 s-pos)
                       (cons (string-ref str s-pos) m)))]
      ; literal
      [else
       (let ([ss (string-ref str s-pos)])
         (and (char=? (string-ref pattern p-pos) ss)
              (match-pat pattern (add1 p-pos) str (add1 s-pos) (cons ss m))))])))

(define (reg-match pattern str)
  (let ([m (reg-match-wrapped pattern str)])
    (and m (list (list->string (reverse m))))))

(define (foror i n pattern str)
  (if (>= i n) #f
      (let ([a (match-pat pattern 0 str i '())
               #;(and (char=? #\d (string-ref str i))
                      (char=? #\e (string-ref str (add1 i)))
                      (char=? #\f (string-ref str (add1 (add1 i))))
                      (char=? #\g (string-ref str (add1 (add1 (add1 i)))))
                      (cons (string-ref str (add1 (add1 (add1 i))))
                            (cons (string-ref str (add1 (add1 i)))
                                  (cons (string-ref str (add1 i))
                                        (cons (string-ref str i) '())))))])
        (if a a
            (foror (add1 i) n pattern str)))))

(define (reg-match-wrapped pattern str)
  (cond
    [(char=? (string-ref pattern 0) #\^) '()]
    #;[(string=? str "") (match-pat pattern 0 str 0 '())] ; edge case
    [else
     (foror 0 (string-length str) pattern str)
     #;(match-pat pattern 3 str 1 (list #\c #\a #\n #\e #\r))
     #;(for/or ([i (in-range (string-length str))])
       (match-pat pattern 0 str i '())
       #;(and (char=? #\d (string-ref str i))
            (char=? #\e (string-ref str (add1 i)))
            (char=? #\f (string-ref str (add1 (add1 i))))
            (char=? #\g (string-ref str (add1 (add1 (add1 i)))))
            (cons (string-ref str (add1 (add1 (add1 i))))
                  (cons (string-ref str (add1 (add1 i)))
                        (cons (string-ref str (add1 i))
                              (cons (string-ref str i) '())))))
       )]))
