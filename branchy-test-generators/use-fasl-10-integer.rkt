#lang racket/base

(require racket/fasl)

(define f #"racket/fasl:\0'\34\n\b\200(\3u\b\200\v\2\b\200@\1\b\200\262\1\b\200\16\2\b\200b\2\b\200 \3\b\200H\2\b\200\225\0")

(printf "---- FASL->S-EXP START---- result : ~a\n" (fasl->s-exp f))
(for ([i (in-range 15)])
  (time (for ([j (in-range 500)])
         (fasl->s-exp f))))
(printf "---- FASL->S-EXP END ----\n")