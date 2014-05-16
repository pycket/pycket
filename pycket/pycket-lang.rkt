#lang racket
(provide (except-out (all-from-out racket) #%module-begin))
(provide (rename-out [#%plain-module-begin #%module-begin]))
