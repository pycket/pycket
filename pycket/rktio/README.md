rktio is a layer that brings in the rktio functionality 
from the Racket source.

pycket/rktio/
├── bootstrap-rktio-pycket.rkt
├── bootstrap_structs.py
├── bootstrap_primitives.py
├── librktio.a
├── _rktio_bootstrap.py
├── rktio_config.h
├── rktio.h
├── rktio_platform.h
├── rktio_private.h
├── rktio.py
├── rktio.rktl
├── types.py

Among this quite diverse set of source files, there are several
main groups of functionality:
    - Racket/C stuff (.h, .a .rktl): These come from outside as is (see `make rktio` target at the top).
    - bootstrap-rktio-pycket.rkt: reads the `rktio.rktl` and generate the `_rktio_bootstrap.py` with Python definitions of types, functions, etc.
    - types.py: provides types that both sides agree upon.
    - rktio.py: loads the librktio.a and connects everything together. 

##### Types

Here we have three sets of types that we're working with.
 1. rktio types: the types that reside in rktio.rktl that refer by name to
the actual C types that reside in librktio.a.
 2. rffi (lltype) types that lives in the RPython land.
 3. Pycket (W_Object) types that are used in Pycket runtime.

The `bootstrap-rktio-pycket.rkt` reads the `rktio.rktl` and turns the rktio types into identifiers that represent rffi types (e.g. rktio_ok_t -> RKTIO_OK_T).

These identifiers are bound on the RPython side to the actual rffi types (e.g. RKTIO_OK_T -> rffi.INT)

The `bootstrap-rktio-pycket.rkt` also knows about the mapping of the rffi types onto Pycket types. (e.g. RKTIO_OK_T -> values.W_Fixnum)

###### Struct Pointers

There are a bunch of structures in rktio that are only passed in between the ffi layers without being inspected or used in the intervening layers. The host, therefore, can be completely oblivious to the layout of such structures (i.e. struct can be totally opaque to Pycket--Pycket doesn't need to know the fields and the types of the structure--, it just receives and passes the pointer from and to the rktio functions).

For most of the struct pointers, we use the generic pointer:

R_PTR	= rffi.VOIDP                        # rffi.COpaquePtr('void *')
W_R_PTR = make_w_pointer_class('voidp')     # pycket

Some pointers require special treatment because they're accessed (dereferenced) in some of the primitives in the rktio connector layer. For them, we need to generate code that uses its own pointer types at both rffi and pycket. (e.g. rktio_date_t is a good example, we have RKTIO_DATE_PTR and W_RKTIO_DATE_PTR types that the `rktio_seconds_to_date_star` dereference after calling rktio_seconds_to_date)
