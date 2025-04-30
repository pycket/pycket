#lang racket
(require racket/match
         racket/pretty   ; for pretty-printing Python lists later
         racket/string)


(define RKTIO-SOURCE  "rktio.rktl")         ; <- adjust path if needed
(define PY-OUT-FILE   "_rktio_bootstrap.py")

(define define-fn              (make-parameter '()))
(define define-fn-errno        (make-parameter '()))
(define define-fn-errno+step   (make-parameter '()))

;; Bootstrap needs to know how to map rktio types onto the rffi
;; types we use over there in the rktio.py.
;; (we do use a naming convention where we just capitalize rktio
;; type name to make it rffi type name, but writing them explicitly
;; here for the sake of clarity).
(define type:rktio->rffi
  (hash
    'rktio_ok_t		      "RKTIO_OK_T"
    'rktio_tri_t	      "RKTIO_TRI_T"
    'rktio_bool_t	      "RKTIO_BOOL_T"
    'rktio_char16_t	      "RKTIO_CHAR16_T"
    'rktio_const_string_t     "RKTIO_CONST_STRING_T"
    'rktio_filesize_t	      "RKTIO_FILESIZE_T"
    'rktio_timestamp_t	      "RKTIO_TIMESTAMP_T"
    'int		      "INT"
    'float		      "FLOAT"
    'void		      "VOID"
    'double		      "DOUBLE"
    'intptr_t		      "INTPTR_T"
    'uintptr_t		      "UINTPTR_T"
    'unsigned		      "UNSIGNED"
    'unsigned-8		      "UNSIGNED_8"
    'function-pointer	      "INTPTR_T"
))

;; These are mostly for the return types.
;; It's convenient to get Pycket class/value
;; for the return type of a rktio function
(define type:rffi->pycket
  (hash
    "RKTIO_OK_T"	      "values.W_Fixnum"
    "RKTIO_TRI_T"	      "values.W_Fixnum"
    "RKTIO_BOOL_T"	      "values.W_Fixnum"
    "RKTIO_CHAR16_T"	      "values.W_Fixnum"
    "RKTIO_CONST_STRING_T"    "values_string.W_String"
    "RKTIO_FILESIZE_T"	      "values.W_Fixnum"
    "RKTIO_TIMESTAMP_T"	      "values.W_Fixnum"
    "INT"		      "values.W_Fixnum"
    "FLOAT"		      "values.W_Flonum"
    "VOID"		      "values.w_void" ; value
    "DOUBLE"		      "values.W_Fixnum"
    "INTPTR_T"		      "values.W_Fixnum"
    "UINTPTR_T"		      "values.W_Fixnum"
    "UNSIGNED"		      "values.W_Fixnum"
    "UNSIGNED_8"	      "values.W_Fixnum"
    "INTPTR_T"		      "values.W_Fixnum"
    "CCHARP"		      "values_string.W_String"
    "CCHARPP"		      "values_string.W_String"
))

;; { rktio : rffi }
(define type:struct-ptrs (make-hash))

;; { rffi : pycket }
(define type:w-struct-ptrs (make-hash))

(define constants (make-hash))

;; Bootstrap needs to be able to convert ref/ref*
;; There are some specific items (e.g. (ref rktio_t) -> RKTIO_T_PTR
;; And there are stuff like (ref (ref char))
(define (lower-type rktio-type)
  (match rktio-type
	[`(,(or 'ref '*ref) char) "CCHARP"]
	[`(,(or 'ref '*ref) (,(or 'ref '*ref) char)) "CCHARPP"]
	[`(,(or 'ref '*ref) (,(or 'ref '*ref) ,t)) (format "ARR_PTR(~a)" (lower-type `(ref ,t)))]
	[`(,(or 'ref '*ref) ,(or `(nullable ,struct-pointer) struct-pointer))
	  (or
	      (hash-ref type:rktio->rffi struct-pointer #f)
	      (hash-ref type:struct-ptrs struct-pointer #f)
	      (let* ([rffi_ptr_type
		      (format "~a~a" 
			(string-upcase (symbol->string struct-pointer)) "_PTR")]
		     [w_ptr_type (format "W_~a" rffi_ptr_type)])
		(hash-set! type:struct-ptrs struct-pointer rffi_ptr_type)
		(hash-set! type:w-struct-ptrs rffi_ptr_type w_ptr_type)
		rffi_ptr_type))]
	
	[t (or
	      (hash-ref type:rktio->rffi t #f)
	      (hash-ref type:struct-ptrs t #f)
	      (let* ([rffi_ptr_type
		       (format "~a~a" 
			 (string-upcase (symbol->string t)) "_PTR")]
		     [w_ptr_type (format "W_~a" rffi_ptr_type)])
		(hash-set! type:struct-ptrs t rffi_ptr_type)
		(hash-set! type:w-struct-ptrs rffi_ptr_type w_ptr_type)
		rffi_ptr_type))]
))

;; Add an element to a parameterized list (front-cons style).
(define (acc! p v)  (p (cons v (p))))

;; High-level info to keep for define-function forms
(struct arg (r-type w-type name))
(struct def-fun (rffi-ret-type w-ret-type name rffi-args-list))
(struct def-fun-err (err-v rffi-ret-type w-ret-type name rffi-args-list))

;; Emit the final Python module.
(define (write-python-module)
  (with-output-to-file PY-OUT-FILE
    #:exists 'replace
    (λ ()
      (define (emit fmt . args) (apply printf fmt args))

      (define (compose-arg-type a)
	(format "(~a, ~a, \"~a\")"
	  (arg-r-type a)
	  (arg-w-type a)
	  (arg-name a)))

      (define (fn-to-tuple fn)
	;; (ret-type, name, args)
	;;
	;; ((rffi-type pycket-type),
	;;  name,
	;;  ((rffi-type pycket-type arg-name) ...)
	(format "\n    ((~a, ~a), \"~a\", ~a)"
	  (def-fun-rffi-ret-type fn)
	  (def-fun-w-ret-type fn)
	  (def-fun-name fn)
	  (format "[~a]"
	    (string-join
	      (map compose-arg-type
		(def-fun-rffi-args-list fn))
	      ","))))

      (define (fn/err-to-tuple fn)
	(format "\n    (~a, (~a, ~a), \"~a\", ~a)"
	  (let ([ev (def-fun-err-err-v fn)])
	    (if (not ev) 
		"W_FALSE"
		ev))
	  (def-fun-err-rffi-ret-type fn)
	  (def-fun-err-w-ret-type fn)
	  (def-fun-err-name fn)
	  (format "[~a]"
	    (string-join
	      (map compose-arg-type
		   (def-fun-err-rffi-args-list fn))
	      ","))))

      ;; header
      (emit "\"\"\"\nAuto-generated by bootstrap-rktio-pycket.rkt using rktio.rktl
Modifying by hand is not recommended.


Defines and provides some constants, struct pointer types,
and three list of tuples:

DEFINE_FUNCTION =
  [(rffi-(return)-type, func-name, [(rffi-type arg-name) ...]]

DEFINE_FUNCTION_ERRNO & DEFINE_FUNCTION_ERRNO_STEP =
  [(rffi-err-val, rffi-(return)-type, func-name, [(rffi-type arg-name) ...]

As well as a map of names for struct pointers to rffi and Pycket types.
We use the map to generate pointer types for rffi and Pycket wrappers to
use when passing stuff around.

We use the tuples (in pycket/rktio.py) to auto-generete the
rffi.llexternal calls, as well as the functions exposed to the
Pycket runtime (@expose_with_rffi) to call these.

\"\"\"\n\n

from pycket import values, values_string
from pycket.rktio.types import *
from rpython.rtyper.lltypesystem import rffi
from pycket.foreign import make_w_pointer_class

")
      (printf "\n# Constants \n")
      (hash-for-each
	constants
	(lambda (const-name const-val)
	  (emit "\n~a = ~a"
		const-name const-val)))

      (printf "\n\n\n# Struct pointers\n")
      (hash-for-each
	type:struct-ptrs
	(lambda (rktio-type rffi-type) 
	  (emit "\n~a = rffi.COpaquePtr(\"~a\")\n" rffi-type rktio-type)
	  (emit "W_~a = make_w_pointer_class(\"~a\")\n" rffi-type rktio-type)
	  )
      )

      
      ;; three lists (currently possibly empty)
      (emit "\n\nDEFINE_FUNCTION = [\n~a\n]\n"
            (string-join (map fn-to-tuple (define-fn)) ","))
      (emit "\nDEFINE_FUNCTION_ERRNO = [\n~a\n]\n"
            (string-join (map fn/err-to-tuple (define-fn-errno)) ","))
      (emit "\nDEFINE_FUNCTION_ERRNO_STEP = [\n~a\n]\n"
			(string-join (map fn/err-to-tuple (define-fn-errno+step)) ","))
	  
	  )))

;; ------------------------------------------------------------
;; Main reader/dispatcher
;; ------------------------------------------------------------
;; Read the rktio.rktl and gather high-level info into lists for 
;; each define-function form.
(define (process-rktl port)

  (define (r->w rffi-type)
    (hash-ref
      type:rffi->pycket
      rffi-type
      (hash-ref type:w-struct-ptrs rffi-type 
		;; hack
                (format "W_~a" rffi-type))
    ))

  ;; walk : any-datum → void
  (define (walk expr)
    (match expr
      [`(begin ,forms ...)
       (for-each walk forms)]

      [`(define-constant ,name (<< ,n ,m))
	(hash-set! constants name 
		   (format "~a << ~a"
			   (hash-ref constants n n)
			   (hash-ref constants m m)))]
      [`(define-constant ,name ,value)
	;; check if value referes to another constant
	(hash-set! constants name (hash-ref constants value value))]

      [`(define-function ,flags ,ret-type ,name ,args)
	(let* ([lowered-ret-type (lower-type ret-type)]
	       [w-ret-type (r->w lowered-ret-type)]
	       [lowered-arg-types
		(map (lambda (a)
		       (let* ([arg-r-type (lower-type (car a))]
			      [arg-w-type (r->w arg-r-type)])
			 (arg arg-r-type arg-w-type (cadr a)))) args)])
	  (acc! define-fn (def-fun lowered-ret-type w-ret-type name lowered-arg-types)))
       ]

      [`(define-function/errno ,err-v ,flags ,ret-type ,name ,args)
	(let* ([lowered-ret-type (lower-type ret-type)]
	       [w-ret-type (r->w lowered-ret-type)]
	       [lowered-arg-types
		(map (lambda (a)
		       (let* ([arg-r-type (lower-type (car a))]
			      [arg-w-type (r->w arg-r-type)])
			 (arg arg-r-type arg-w-type (cadr a)))) args)])
	  (acc! define-fn-errno (def-fun-err err-v lowered-ret-type w-ret-type name lowered-arg-types)))
       ]

      [`(define-function/errno+step ,err-v ,flags ,ret-type ,name ,args)
	(let* ([lowered-ret-type (lower-type ret-type)]
	       [w-ret-type (r->w lowered-ret-type)]
	       [lowered-arg-types
		(map (lambda (a)
		       (let* ([arg-r-type (lower-type (car a))]
			      [arg-w-type (r->w arg-r-type)])
			 (arg arg-r-type arg-w-type (cadr a)))) args)])
	  (acc! define-fn-errno+step (def-fun-err err-v lowered-ret-type w-ret-type name lowered-arg-types)))
       ]

      [_ #f]))

  ;; read the single top-level form and start walking
  (walk (read port)))

;; ------------------------------------------------------------
;; Script entry point
;; ------------------------------------------------------------

(module+ main
  (call-with-input-file RKTIO-SOURCE process-rktl)
  (write-python-module)
  (printf "Wrote pycket/~a\n" PY-OUT-FILE))


