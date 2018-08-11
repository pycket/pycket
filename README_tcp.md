Minimal TCP extension for Pycket
================================

Modified files:

 * pycket/interpreter.py            // modded to load TCP API as builtin
 * pycket/prims/general.py          // modded to import TCP module

New files:

 * pycket/pycket-lang/tcp.rkt       // Racket API prims (require to use TCP)
 * pycket/pycket-lang/tcp-lib.rkt   // Library of extensions over TCP prims
 * pycket/prims/tcp.py              // Impl of primitives exposed by API
 * pycket/values_tcp.py             // Impl of classes used by primitives
 * pycket/test/test_tcp.py          // very limited testing
 * README_tcp.md                    // this file
