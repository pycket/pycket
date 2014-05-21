#lang racket
(provide (except-out (all-from-out racket) #%module-begin))
(require racket/unsafe/ops)
(provide (all-from-out racket/unsafe/ops))
(provide (rename-out [#%plain-module-begin #%module-begin]))
