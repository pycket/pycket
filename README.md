[![Build Status](https://travis-ci.org/samth/pycket.png?branch=master)](https://travis-ci.org/samth/pycket)

A Racket/Scheme implementation using RPython. It adds a JIT.

## Building

In order to do anything with Pycket, you need to check out PyPy:

    $ hg clone https://bitbucket.org/pypy/pypy

The below instructions assume that you do this checkout in this directory.

Additionally, it helps to have the build dependencies of PyPy installed.
On a Debian or Ubuntu system:

    $ sudo apt-get build-dep pypy

You also need a reasonably recent version of Racket, at least version
6.1.1. Then you need to set up the `pycket` language in Racket:

    $ raco pkg install -t dir pycket/pycket-lang/

Afterwards you can run the unit-tests as follows:

    $ pypy/pytest.py pycket

or using `make`

    $ make test

To produce an executable, run:

    $ ./pypy/rpython/bin/rpython -Ojit targetpycket.py

This expects that a binary named `pypy` is in your path. (Note that
a hand-compiled PyPy by running `make` produces `pypy-c`, not `pypy`).

If you don't have a compiled pypy, you can also translate with:

    $ python ./pypy/rpython/bin/rpython -Ojit targetpycket.py

This will take upwards of 10 minutes.
This requires a `PYTHONPATH` that includes both `rpython` (that should
be the `pypy` directory cloned above) and `pycket` (that should be
this directory).

You can also use `make` for any of the above,

 * `make setup` to setup the Racket language and update your `pypy` chekout
 * `make pycket-c` to translate with JIT
 * `make pycket-c-nojit` to translate without JIT (which is may be a lot faster to translate but runs a lot lot slower)


## Running

Afterwards you can execute a program:

    $ ./pycket-c program

You can also run pycket under plain python, like this:

    $ ./pycket-slow.sh program
    
Or even this, when this directory is in your `PYTHONPATH`:

    $ python -mpycket program

You can edit the shell script to make it use pypy, if desired.

## Misc

You can generate a coverage report with `pytest`:

    $ pypy/pytest.py --cov .

or via

    $ make coverage
    
which also generates an HTML report in `pycket/test/coverage_report`.  
You need these Python packages for that to work:
  * `pytest-cov` (provided with the `pypy` checkout)
  * `cov-core` and `coverage`

Also, the `Makefile` reacts to some variables:
 * `PYPYPATH` for when your `pypy` checkout is not in this directory.   
    Defaults to `pypy`.
 * `PYTEST` for when you don’t want to use `pypy`’s version of pytest.  
   Defaults to `$(PYPYPATH)/pytest.py`.
 * `RPYTHON` for when you want to use something other than the default `rpython` script, but  you probablywould not want that.  
   Defaults to `$(PYPYPATH)/rpython/bin/rpython --batch`.

## Benchmarking

Pycket's benchmarks are available at [this repository](https://github.com/krono/pycket-bench), along with instructions for running them.
