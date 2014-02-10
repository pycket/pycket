[![Build Status](https://travis-ci.org/krono/pycket.png?branch=master)](https://travis-ci.org/krono/pycket)

A rudimentary Racket/Scheme implementation using RPython. It adds a JIT.

In order to do anything with Pycket, you need to check out PyPy:

    $ hg clone https://bitbucket.org/pypy/pypy

The below instructions assume that you do this checkout in this directory.

Afterwards you can run the unit-tests as follows:

    $ pypy/pytest.py

To produce an executable, run:

    $ cd pycket
    $ PYTHONPATH=.. ../pypy/rpython/bin/rpython -Ojit targetpycket.py

This expects that a binary named `pypy` is in your path. Note that
running `make` for `pypy` produces `pypy-c`.

This will take upwards of 5 minutes.

Afterwards you can execute a program:

    $ ./pycket.sh program

See the comment in `pycket.sh` for more details.

This requires a `PYTHONPATH` that includes both `rpython` (that should
be the `pypy` directory cloned above) and `pycket` (that should be
this directory).

You can also run pycket under plain python, like this:

    $ ./pycket-slow.sh program

Edit that shell script to make it use pypy, if desired.
