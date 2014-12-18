[![Build Status](https://travis-ci.org/samth/pycket.png?branch=master)](https://travis-ci.org/samth/pycket)

A rudimentary Racket/Scheme implementation using RPython. It adds a JIT.

In order to do anything with Pycket, you need to check out PyPy:

    $ hg clone https://bitbucket.org/pypy/pypy

The below instructions assume that you do this checkout in this directory.

You also need a reasonably recent version of Racket, at least version
6.1.1. Then you need to set up the `pycket` language in Racket:

    $ raco pkg install -t dir pycket/pycket-lang/

Afterwards you can run the unit-tests as follows:

    $ pypy/pytest.py

To produce an executable, run:

    $ ./pypy/rpython/bin/rpython -Ojit targetpycket.py

This expects that a binary named `pypy` is in your path. Note that
running `make` for `pypy` produces `pypy-c`.

If you don't have a compiled pypy, you can also translate with:

    $ python ./pypy/rpython/bin/rpython -Ojit targetpycket.py

This will take upwards of 5 minutes.
This requires a `PYTHONPATH` that includes both `rpython` (that should
be the `pypy` directory cloned above) and `pycket` (that should be
this directory).


Afterwards you can execute a program:

    $ ./pycket-c program

You can also run pycket under plain python, like this:

    $ ./pycket-slow.sh program

Edit that shell script to make it use pypy, if desired.
