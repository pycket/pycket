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
