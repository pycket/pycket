#lang racket/base

(require (prefix-in m: mmap))
(provide mmap)

(define (mmap f len)
  (m:mmap f #:length len))

