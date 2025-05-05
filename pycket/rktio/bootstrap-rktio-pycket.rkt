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
    "VOID"		      "values.W_Fixnum" ; pointer to anything
    "DOUBLE"		      "values.W_Fixnum"
    "INTPTR_T"		      "values.W_Fixnum"
    "UINTPTR_T"		      "values.W_Fixnum"
    "UNSIGNED"		      "values.W_Fixnum"
    "UNSIGNED_8"	      "values.W_Fixnum"
    "INTPTR_T"		      "values.W_Fixnum"
    "CCHARP"		      "values.W_Fixnum" ; just a pointer
    "CCHARPP"		      "values.W_Fixnum" ; just a pointer
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
	[`(,(or 'ref '*ref) (,(or 'ref '*ref) ,t)) "R_PTR" #;(format "ARR_PTR(~a)" (lower-type `(ref ,t)))]
	[`(,(or 'ref '*ref) ,(or `(nullable ,struct-pointer) struct-pointer))
	  (or
	      (hash-ref type:rktio->rffi struct-pointer #f)
	      (and (hash-ref type:struct-ptrs struct-pointer #f) "R_PTR")
	      (let* ([rffi_ptr_type
		      (format "~a~a" 
			(string-upcase (symbol->string struct-pointer)) "_PTR")]
		     [w_ptr_type (format "W_~a" rffi_ptr_type)])
		(hash-set! type:struct-ptrs struct-pointer rffi_ptr_type)
		(hash-set! type:w-struct-ptrs rffi_ptr_type w_ptr_type)
		"R_PTR" #;rffi_ptr_type))]
	
	[t (or
	      (hash-ref type:rktio->rffi t #f)
	      (and (hash-ref type:struct-ptrs t #f) "R_PTR")
	      (let* ([rffi_ptr_type
		       (format "~a~a" 
			 (string-upcase (symbol->string t)) "_PTR")]
		     [w_ptr_type (format "W_~a" rffi_ptr_type)])
		(hash-set! type:struct-ptrs t rffi_ptr_type)
		(hash-set! type:w-struct-ptrs rffi_ptr_type w_ptr_type)
		"R_PTR" #;rffi_ptr_type))]
))

;; Add an element to a parameterized list (front-cons style).
(define (acc! p v)  (p (cons v (p))))

;; High-level info to keep for define-function forms
(struct arg (r-type w-type rktio-name w-name r-name))
(struct def-fun (r-ret-type w-ret-type name args-list))
(struct def-fun-err (err-v r-ret-type w-ret-type name args-list))

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
	  (arg-rktio-name a)))

      (define (fn-to-tuple fn)
	;; (ret-type, name, args)
	;;
	;; ((rffi-type pycket-type),
	;;  name,
	;;  ((rffi-type pycket-type arg-name) ...)
	(format "\n    ((~a, ~a), \"~a\", ~a)"
	  (def-fun-r-ret-type fn)
	  (def-fun-w-ret-type fn)
	  (def-fun-name fn)
	  (format "[~a]"
	    (string-join
	      (map compose-arg-type
		(def-fun-args-list fn))
	      ","))))

      ;; This part emits 2 things:
      ;; 1) llexternal call to register a C function
      ;; 2) RPython function that we @expose to the Pycket runtime
      (define llexternal-template
	"c_~a = rffi.llexternal('~a', [~a], ~a, compilation_info=librktio_a)")

      (define expose-py-fun-template
	;; @expose(name, [W_Args...], simple=True)
	;; def name(w_args ...):
	;;   <convert w_args to r_args>
	;;
	;;   res = c_name(r_args...)
	;;
	;;   return <convert res to w_ret_type>
	"
~a

@expose(\"~a\", [~a], simple=True)
def ~a(~a):
~a
\tres = c_~a(~a)

~a")

      ;; args-str is just a wrapper around string-join
      ;; used to Pythonize argument lists
      (define (args-str lst-of-str)
	(string-join lst-of-str ", "))

      ;; arg-w->r expresses the conversion logic for a single
      ;; w-arg -> r-arg in RPython
      (define (arg-w->r r_name r_type w_name w_type)
	(cond
	  [(equal? r_type "R_PTR")
	   (format "\t~a = rffi.cast(~a, ~a.to_rffi())"
		   r_name r_type w_name)]
	  
	  [(equal? r_type "RKTIO_BOOL_T")
	   (format "\t~a = rffi.cast(rffi.INT, 1 if ~a is values.w_true else 0)"
		   r_name w_name)]
	  
	  [(equal? r_type "RKTIO_CONST_STRING_T")
	   (let*
	     ([_p_str (format "\t_p_str = ~a.as_str_utf8()\n" w_name)]
	      [p_str (format "\tp_str = _p_str if _p_str else \"\"\n")]
	      [r_line (format "\t~a = rffi.str2charp(p_str)" r_name)])
	     (string-append _p_str p_str r_line))]

	  [(or (equal? r_type "RKTIO_CHAR16_T")
	       (equal? r_type "RKTIO_FILESIZE_T"))
	   (format "\t~a = rffi.cast(rffi.INT, ~a.value)"
		   r_name w_name)]

	  [(or (equal? r_type "CCHARP") (equal? r_type "CCHARPP")
	       (equal? r_type "INT") (equal? r_type "UNSIGNED")
	       (equal? r_type "CHAR") (equal? r_type "DOUBLE")
	       (equal? r_type "FLOAT")
	       )
	   (format "\t~a = rffi.cast(rffi.~a, ~a.value)"
		   r_name r_type w_name)]

	  [(or (equal? r_type "INTPTR_T") (equal? r_type "UINTPTR_T"))
	   (format "\t~a = rffi.cast(rffi.SSIZE_T, ~a.value)"
		   r_name w_name)]

	  [(equal? r_type "VOID")
	   (format "\t~a = rffi.cast(rffi.VOIDP, ~a.value)"
		   r_name w_name)]

	  [(equal? r_type "UNSIGNED_8")
	   (format "\t~a = rffi.cast(rffi.UINT, ~a.value)"
		   r_name w_name)]

	  [else (error 'arg-w->r (format "unhandled r_type : ~a" r_type))]

	  ))

      ;; process-args emit a line for each argument that converts
      ;; the w-arg into an rffi arg
      ;; the input list lengths are the same
      ;; returns two compound strings,
      ;; 1) arg list of names of the arguments (to be used in c_func call)
      ;;  "," separated
      ;; 2) lines that converts w-arg to r-arg
      ;;  "\n" separated
      (define (process-args args-list)
	(for/fold 
	  ([w_arg_names null]
	   [w_arg_types null]
	   [r_arg_names null]
	   [r_arg_types null]
	   [r_arg_defns null]
	   #:result (values
		      (string-join (reverse w_arg_names) ", ")
		      (string-join (reverse w_arg_types) ", ")
		      (string-join (reverse r_arg_names) ", ")
		      (string-join (reverse r_arg_types) ", ")
		      (string-join (reverse r_arg_defns) "\n")))
	  ([a args-list])

	  (let* ([rktio_name (arg-rktio-name a)]
		 [w_name (arg-w-name a)]
		 [r_name (arg-r-name a)]
		 [w_type (arg-w-type a)]
		 [r_type (arg-r-type a)]
		 [arg-convert-lines
		   (arg-w->r r_name r_type w_name w_type)])
	    (values
	      (cons w_name w_arg_names)
	      (cons w_type w_arg_types)
	      (cons r_name r_arg_names)
	      (cons r_type r_arg_types)
	      (cons arg-convert-lines r_arg_defns)))))

      ;; return-line produces the "return" line that converts
      ;; the result of the rffi call back to a W_Object
      ;; assumes the result is in a variable called "res"
      (define (return-line w-ret-type r-ret-type)
	(cond
	  [(or (equal? w-ret-type "values.W_Fixnum")
	       (equal? w-ret-type "values.W_Flonum")
	       (equal? w-ret-type "W_R_PTR"))
	   (format "\t# returns ~a\n\treturn ~a(res)" r-ret-type w-ret-type)]
	  [else (error 'return-line (format 
				      "unhandled w-ret-type: ~a"
				      w-ret-type))]))

      (define (fn-to-py-expose fn)
	(let (
	      [name (def-fun-name fn)]
	      [w_ret_line (return-line (def-fun-w-ret-type fn)
				       (def-fun-r-ret-type fn))]
	      [r_ret_type (def-fun-r-ret-type fn)])
	  (let-values ([(w_arg_names w_arg_types r_arg_names r_arg_types r_arg_defns)
			(process-args (def-fun-args-list fn))])
	    (let ([llexternal-lines
		    (format llexternal-template
			    name name
			    r_arg_types
			    r_ret_type)])
	      (format expose-py-fun-template
		      llexternal-lines
		      name w_arg_types
		      name w_arg_names
		      r_arg_defns
		      name r_arg_names
		      w_ret_line)))))

      (define (fn/err-to-tuple fn)
	(format "\n    (~a, (~a, ~a), \"~a\", ~a)"
	  (let ([ev (def-fun-err-err-v fn)])
	    (if (not ev) 
		"W_FALSE"
		ev))
	  (def-fun-err-r-ret-type fn)
	  (def-fun-err-w-ret-type fn)
	  (def-fun-err-name fn)
	  (format "[~a]"
	    (string-join
	      (map compose-arg-type
		   (def-fun-err-args-list fn))
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
import os

from pycket import values, values_string
from pycket.prims.expose import expose
from pycket.rktio.types import *
from pycket.foreign import make_w_pointer_class

from rpython.rtyper.lltypesystem import rffi
from rpython.translator.tool.cbuild import ExternalCompilationInfo

# Load the librktio.a
# TODO: make this absolute (pycket/rktio), instead of \"this file\"
RKTIO_DIR = os.path.dirname(os.path.abspath(__file__))
librktio_a = ExternalCompilationInfo(
    includes=['rktio.h'],
    include_dirs=[RKTIO_DIR],
    libraries=['rktio'],
    library_dirs=[RKTIO_DIR],
)

R_PTR = rffi.COpaquePtr(\"_pointer\")
W_R_PTR = make_w_pointer_class(\"_pointer\")

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
      (map (lambda (fdef) (emit "\n~a\n" (fn-to-py-expose fdef)))
	   (define-fn))

      #;(emit "\n\nDEFINE_FUNCTION = [\n~a\n]\n"
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
			      [arg-w-type (r->w arg-r-type)]
			      [arg-w-name (format "w_~a" (cadr a))]
			      [arg-r-name (format "r_~a" (cadr a))])
			 (arg arg-r-type arg-w-type (cadr a) arg-w-name arg-r-name))) args)])
	  (acc! define-fn (def-fun lowered-ret-type w-ret-type name lowered-arg-types)))
       ]

      [`(define-function/errno ,err-v ,flags ,ret-type ,name ,args)
	(let* ([lowered-ret-type (lower-type ret-type)]
	       [w-ret-type (r->w lowered-ret-type)]
	       [lowered-arg-types
		(map (lambda (a)
		       (let* ([arg-r-type (lower-type (car a))]
			      [arg-w-type (r->w arg-r-type)]
			      [arg-w-name (format "w_~a" (cadr a))]
			      [arg-r-name (format "r_~a" (cadr a))])
			 (arg arg-r-type arg-w-type (cadr a) arg-w-name arg-r-name))) args)])
	  (acc! define-fn-errno (def-fun-err err-v lowered-ret-type w-ret-type name lowered-arg-types)))
       ]

      [`(define-function/errno+step ,err-v ,flags ,ret-type ,name ,args)
	(let* ([lowered-ret-type (lower-type ret-type)]
	       [w-ret-type (r->w lowered-ret-type)]
	       [lowered-arg-types
		(map (lambda (a)
		       (let* ([arg-r-type (lower-type (car a))]
			      [arg-w-type (r->w arg-r-type)]
			      [arg-w-name (format "w_~a" (cadr a))]
			      [arg-r-name (format "r_~a" (cadr a))])
			 (arg arg-r-type arg-w-type (cadr a) arg-w-name arg-r-name))) args)])
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


