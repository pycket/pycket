[![Build Status](https://travis-ci.org/pycket/pycket.png?branch=master)](https://travis-ci.org/pycket/pycket)
[![codecov.io](https://codecov.io/github/pycket/pycket/coverage.svg?branch=master)](https://codecov.io/github/pycket/pycket?branch=master)

Pycket is a Racket/Scheme implementation that is automatically
generated using the [RPython
framework](https://rpython.readthedocs.io/en/latest/). Given an
interpreter written in RPython (in our case a CEK machine interpreter
for Racket), RPython framework produces a fast binary for
it. It can also add a tracing JIT.

There are currently two different modes of evaluation that we refer as
`OLD` and `NEW`. The `NEW` Pycket uses `linklets` and bootstraps the
Racket using the `expander` linklet exported by Racket (version
7+). The `OLD` Pycket, on the other hand, uses Racket's binary to
fully expand the program and generates `json` asts and evaluates them.

Note that both versions require an unmodified Racket installation. The
`OLD` Pycket requires a Racket binary, and while the `NEW` Pycket
doesn't require a Racket binary, it still requires the Racket packages
and libraries to bootstrap.

See the [Makefile targets](#make-targets) section about how to build both versions.

### Quick Links:

 * [#pycket](https://racket.slack.com/messages/pycket) Slack channel under Racket
 * [#pycket](irc://freenode/pycket) channel on IRC freenode
 * Pycket's benchmarks are available at [this repository](https://github.com/krono/pycket-bench),
 along with instructions for running them.

## Building

In order to do anything with Pycket, you need to check out PyPy:

    $ hg clone https://bitbucket.org/pypy/pypy

The below instructions assume that you do this checkout in Pycket's directory.

Additionally, it helps to have the build dependencies of PyPy installed.
On a Debian or Ubuntu system:

    $ sudo apt-get build-dep pypy

To produce an executable, run:

    $ ./pypy/rpython/bin/rpython -Ojit targetpycket.py

This expects that a binary named `pypy` is in your path. (Note that
a hand-compiled PyPy by running `make` produces `pypy-c`, not `pypy`).

If you don't have a PyPy binary, you can also translate with the CPython:

    $ python ./pypy/rpython/bin/rpython -Ojit targetpycket.py

This will take upwards of 10 minutes.

## [Make Targets](#make-targets)

You can also use `make` for any of the above,

 * `make setup` to setup and update the `pypy` checkout and install `pycket-lang` to Racket
 * `make expander` to generate the expander linklet (it assumes an unmodified Racket install and PLTHOME environment variable -- see the Environment Variables section below)
 * `make pycket-c` to translate `OLD Pycket` with JIT
 * `make pycket-c-linklets` to translate `NEW Pycket` with JIT
 * `make pycket-c-nojit` to translate `OLD Pycket` without JIT (which may be a lot faster to translate but runs a lot lot slower)
 * `make pycket-c-linklets-nojit` to translate `NEW Pycket` without JIT (which may be a lot faster to translate but runs a lot lot slower)
 * `make test` to run the unit tests on `OLD Pycket`.
 * `make test-new-with-expander` to run the unit tests on `NEW Pycket` using the Racket's `expander` linklet.
 * `make test-new-no-expander` to run the unit tests on `NEW Pycket` without using the `expander`.

## Testing

Now that Pycket has two different modes with options, we run the unit
tests on each of those settings using the following targets:

 * `make test` : to run the `OLD` Pycket on the tests
 * `make test-new-no-expander` : to run `NEW` Pycket without using the expander linklet
 * `make test-new-with-expander` : to run `NEW` Pycket using the expander linklet

For the `NEW` Pycket, using the expander linklet means that for each
test expression string we use the `read` and `eval` functions in that
linklet to read and evaluate the test. If we're not using the
expander, on the other hand, then we manually create a linklet
containing the expression and instantiate it directly (mostly with an
empty target) to get the result.

## Using Compiled Files

The `NEW` Pycket is able to generate and use its own `.zo` files. For
now both the generation and the use are manual.

To generate a `.zo` file for a `.rkt` source file, use `make
compile-file`:

    $ make compile-file FILE=$(PLTHOME)/racket/collects/racket/private/qq-and-or.rkt

The parameter that enables Racket expander to use compiled code is
`use-compiled-file-paths`, which defaults to `pycket-compiled` in
Pycket. Whenever a module is required, the expander will use the
compiled code if it exists, otherwise it will use the source code of
the module (read, expand, etc.).

    pycket-repl> (#%require racket/private/qq-and-or)

Note that `pycket-compiled` is a folder that `make compile-file` is
going to generate by itself.

Currently we have two `make` targets for working with compiled files:

    $ make compile-racket-modules

will create `.zo` files for the predefined list of Racket modules (see
`compile-file-pycket.rkt`).

    $ make clean-compiled-files

will remove all the `.zo` files under the `pycket-compiled`
directories in Racket libraries.

This is a work in progress. We plan to have a make target that
compiles all the Racket modules automatically by following the module
dependencies (as opposed to using a predefined list of modules with
the respective paths).

## Environment Variables

Running the interpreter on CPython or PyPy (i.e. running the
targetpycket.py) requires a `PYTHONPATH` that includes both `RPython`
(that should be the `pypy` directory cloned above) and `pycket` (that
should be this directory).

Also there are a couple of variables need to be set for the `NEW`
Pycket to interact with the Racket, since it bootstraps Racket by
reading and evaluating Racket's main collection by loading and using
the bootstrap linklets (currently only the `expander.rktl.linklet`)
exported by Racket. So the `NEW` Pycket needs to be able to locate
various different parts of the Racket installation. The `OLD` Pycket
is lucky to use the Racket's own binary.

Naturally, it varies on the way in which the Racket is installed:

### If Racket is installed in a single directory (non-Unix-style) :

Then all the `NEW` Pycket needs is a `PLTHOME` environment variable to
point to the surrounding directory. For example it will assume the
`collects` directory is at:

> $PLTHOME/racket/collects

### If Racket is installed in Unix-style :

Then `NEW` Pycket needs to know the locations of various
directories. In particular, it needs:

 * `PLTEXECFILE` to point to the location of the `racket` binary
 * `PLTCOLLECTS` to point to the `collects` directory for the main
collections
 * `PLTCONFIGDIR` to point to Racket's `etc` directory that contains
`config.rktd`
 * (optional) `PLTADDONDIR` to point to a directory for user-specific
Racket configuration, packages, and extensions. It defaults to
`.racket` in USERHOME.
 * (optional) `PLTUSERHOME` to point to the `home` directory of the
  user. It's optional since Pycket will also look at other environment
  variables to figure out the home directory (e.g. `$HOME`).

You can also use the command line options to provide these paths,
e.g. `-X`, `-G` etc.. Run it with `-h` to see all the commands and
options.

    $ ./pycket-c-linklets -h

Also, the `Makefile` reacts to some variables:
 * `PYPYPATH` for when your `pypy` checkout is not in this directory.
    Defaults to `pypy`.
 * `PYTEST` for when you don’t want to use `pypy`’s version of pytest.
    Defaults to `$(PYPYPATH)/pytest.py`.

 * `RPYTHON` for when you want to use something other than the default
   `rpython` script, but you probably would not want that.
   Defaults to `$(PYPYPATH)/rpython/bin/rpython --batch`.

## Running

Pycket currently defaults to the `OLD` Pycket. To use the `NEW`
version with the linklets, run it with:

    $ ./pycket-c-linklets <arguments>

You can run with the `-h` option to see the different command line
options for each versions:

    $ ./pycket-c -h

    $ ./pycket-c-linklets -h

You can run `pycket-c` like the `racket` binary:

    $ ./pycket-c program.rkt

You can also run pycket under plain python (or `pypy` if its
available), like this:

    $ ./pycket-slow.sh program

## REPL

One of the beautiful perks of bootstrapping Racket is to be able to
run on Pykcet some interesting programs implemented in Racket (as long
as we have enough runtime support for it in Pycket -- i.e. having the
runtime primitives implemented in RPython).

The `NEW` Pycket now features an original Racket REPL that's
implemented in Racket. Try it out!

    $ ./pycket-c-linklets

You can make it more verbose with the `--verbose` flag if you're
curious about what's going on in the background.

    $ ./pycket-c-linklets --verbose

Also increase the verbosity level (defaults to 0).

    $ ./pycket-c-linklets --verbose 1

## Deprecated Stuff Below -- Will be revised

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