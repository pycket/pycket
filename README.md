[![Build Status](https://github.com/pycket/pycket/actions/workflows/build.yml/badge.svg)](https://github.com/pycket/pycket/actions/workflows/build.yml) 
[![Tests with Racket Expander](https://github.com/pycket/pycket/actions/workflows/test-new.yml/badge.svg)](https://github.com/pycket/pycket/actions/workflows/test-new.yml)
[![Tests with Pycket Expander](https://github.com/pycket/pycket/actions/workflows/test-old.yml/badge.svg)](https://github.com/pycket/pycket/actions/workflows/test-old.yml)

[![codecov.io](https://codecov.io/github/pycket/pycket/coverage.svg?branch=master) ](https://codecov.io/github/pycket/pycket?branch=master)
[![license](http://img.shields.io/badge/license-mit-blue.svg)](https://opensource.org/licenses/MIT) <img align="right" width="210" height="60" src="https://github.com/pycket/pycket/blob/master/pycket.png">

[![Slack channel](https://img.shields.io/badge/slack-channel-orange.svg)](https://racket.slack.com/messages/pycket) [![IRC channel](https://img.shields.io/badge/IRC-channel-orange.svg)](irc://freenode/pycket)

Pycket is a Racket/Scheme implementation that is generated using the [RPython framework](https://rpython.readthedocs.io/en/latest/). Given an interpreter written in RPython (in our case a CEK machine interpreter for Racket), RPython framework produces a fast binary for it. It can also add a tracing JIT.

There are currently two different modes that we refer as `OLD` and `NEW`. The `NEW` Pycket uses `linklets` and bootstraps the Racket using the `expander` linklet exported by Racket (version 8+). The `OLD` Pycket, on the other hand, uses Racket's binary to fully expand the program and generates `json` asts and evaluates them.

Note that both versions require an unmodified Racket installation. The `OLD` Pycket requires a Racket binary, and while the `NEW` Pycket doesn't require a Racket binary, it still requires the Racket packages
and libraries to bootstrap.

See the [Makefile targets](#make-targets) section about how to build both versions.

### Quick Links:

 * Pycket's benchmarks are available at [this repository](https://github.com/krono/pycket-bench), along with instructions for running them.
 * [Running Pycket](#running)
 * [Environment Variables](#vars)
 * [Troubleshoot](#trouble)

## Building

Building Pycket means translating the interpreter (written in RPython) into a binary. You can use the [make targets](#make-targets) to translate Pycket. We recommend using the PyPy for translation, since it'll be much faster than CPython. If you don't have a `pypy` binary in your environment, then `make` targets will default to CPython.

You can clone and make the `pypy` with `make make-pypy` target. It will clone the latest pypy repo in Pycket's directory and will start making it. Note that it will take more than 2 hours to build the pypy. It will result in a binary created at `pypy/pypy/goal/pypy`, and you need to add it to your environment for Pycket's translation to use it.

Additionally, it helps to have the build dependencies of PyPy installed.
On a Debian or Ubuntu system:

    $ sudo apt-get build-dep pypy

To produce a Pycket executable, use one of the provided [make targets](#make-targets) to build the binary you need.

## [Make Targets](#make-targets)

### PyPy Stuff:

Assumes the mercurial binary `hg` to be in the environment.

 * `make clone-pypy` : clones the latest pypy into Pycket's directory
 * `make make-pypy` : builds pypy, assumes that pypy directory exists in Pycket's directory

### Building Pycket

 * `make pycket-c` : translates `OLD Pycket` with JIT
 * `make pycket-c-linklets` : translates `NEW Pycket` with JIT
 * `make pycket-c-nojit` : translates `OLD Pycket` without JIT (which may be a lot faster to translate but runs a lot lot slower)
 * `make pycket-c-linklets-nojit` : translates `NEW Pycket` without JIT (which may be a lot faster to translate but runs a lot lot slower)

 * `make setup-old-pycket` : installs the `pycket-lang` to Racket and runs `update-pypy`
 * `make expander` : generates the expander linklet (it assumes an unmodified Racket install and PLTHOME environment variable -- see the Environment Variables section below)

 * `make setup-local-racket` : if you don't have a Racket and don't want to deal with Racket related stuff, then run this to get a latest running Racket. Make sure to run ``export PLTHOME=`pwd`/`` after it's done (see [environment variables](#vars) section).

### [Running Pycket](#running)

Pycket currently defaults to the `OLD` Pycket. To use the `NEW` version with the linklets, run it with:

    $ ./pycket-c-linklets <arguments>

You can run with the `-h` option to see the different command line options for each versions:

    $ ./pycket-c -h

    $ ./pycket-c-linklets -h

You can run `pycket-c` like the `racket` binary:

    $ ./pycket-c program.rkt

You can also run pycket under plain python (or `pypy` if its available), like this:

    $ ./pycket-slow.sh program

### [Environment Variables](#vars)

Running the interpreter on CPython or PyPy (i.e. running the targetpycket.py) requires a `PYTHONPATH` that includes both `RPython` (that should be the `pypy` directory cloned above) and `pycket` (that should be this directory).

Also there are a couple of variables need to be set for the `NEW` Pycket to interact with the Racket, since it bootstraps Racket by reading and evaluating Racket's main collection by loading and using the bootstrap linklets (currently only the `expander.rktl.linklet`) exported by Racket. So the `NEW` Pycket needs to be able to locate various different parts of the Racket installation. The `OLD` Pycket is lucky to use the Racket's own binary.

Naturally, it varies on the way in which the Racket is installed:

#### If Racket is installed in a single directory (non-Unix-style) :

Then all the `NEW` Pycket needs is a `PLTHOME` environment variable to point to the surrounding directory. For example it will assume the `collects` directory is at:

> $PLTHOME/racket/collects

#### If Racket is installed in Unix-style :

Then `NEW` Pycket needs to know the locations of various directories. In particular, it needs:

 * `PLTEXECFILE` to point to the location of the `racket` binary
 * `PLTCOLLECTS` to point to the `collects` directory for the main collections
 * `PLTCONFIGDIR` to point to Racket's `etc` directory that contains `config.rktd`
 * (optional) `PLTADDONDIR` to point to a directory for user-specific Racket configuration, packages, and extensions. It defaults to `.racket` in USERHOME.
 * (optional) `PLTUSERHOME` to point to the `home` directory of the  user. It's optional since Pycket will also look at other environment  variables to figure out the home directory (e.g. `$HOME`).

You can also use the command line options to provide these paths, e.g. `-X`, `-G` etc.. Run it with `-h` to see all the commands and options.

    $ ./pycket-c-linklets -h

Also, the `Makefile` reacts to some variables:

 * `PYPYPATH` for when your `pypy` checkout is not in this directory. Defaults to `pypy`.
 * `PYTEST` for when you don’t want to use `pypy`’s version of pytest. Defaults to `$(PYPYPATH)/pytest.py`.
 * `RPYTHON` for when you want to use something other than the default `rpython` script, but you probably would not want that. Defaults to `$(PYPYPATH)/rpython/bin/rpython --batch`.

### Testing Pycket

Now that Pycket has two different modes with options, we run the unit tests on each of those settings using the following targets:

 * `make test` to run the unit tests on `OLD Pycket`.
 * `make test-new-with-expander` to run the unit tests on `NEW Pycket` using the Racket's `expander` linklet.
 * `make test-new-no-expander` to run the unit tests on `NEW Pycket` without using the `expander`.

For the `NEW` Pycket, using the expander linklet means that for each test expression string we use the `read` and `eval` functions in that linklet to read and evaluate the test. If we're not using the expander, on the other hand, then we manually create a linklet containing the expression and instantiate it directly (mostly with an empty target) to get the result.

## Using Compiled Files

The `NEW` Pycket is able to generate and use its own `.zo` files. For now both the generation and the use are manual.

To generate a `.zo` file for a `.rkt` source file, use `make
compile-file`:

    $ make compile-file FILE=$(PLTHOME)/racket/collects/racket/private/qq-and-or.rkt

The parameter that enables Racket expander to use compiled code is `use-compiled-file-paths`, which defaults to `pycket-compiled` in Pycket. Whenever a module is required, the expander will use the compiled code if it exists, otherwise it will use the source code of the module (read, expand, etc.).

    pycket-repl> (#%require racket/private/qq-and-or)

Note that `pycket-compiled` is a folder that `make compile-file` is going to generate by itself.

Currently we have two `make` targets for working with compiled files:

    $ make compile-racket-modules

will create `.zo` files for the predefined list of Racket modules (see `compile-file-pycket.rkt`).

    $ make clean-compiled-files

will remove all the `.zo` files under the `pycket-compiled` directories in Racket libraries.

This is a work in progress. We plan to have a make target that compiles all the Racket modules automatically by following the module dependencies (as opposed to using a predefined list of modules with the respective paths).

## REPL

One of the beautiful perks of bootstrapping Racket is to be able to run on Pykcet some interesting programs  implemented in Racket (as long as we have enough runtime support for it in Pycket -- i.e. having the runtime primitives implemented in RPython).

The `NEW` Pycket now features an original Racket REPL that's implemented in Racket. Try it out!

    $ ./pycket-c-linklets

You can make it more verbose with the `--verbose` flag if you're curious about what's going on in the background.

    $ ./pycket-c-linklets --verbose

Also increase the verbosity level (defaults to 0).

    $ ./pycket-c-linklets --verbose 1

## [Troubleshoot](#trouble)

#### Getting a version error like : `version mismatch  expected: "7.2.0.11"  found: "7.2.0.7"`
There are a couple of things that might produce this.
 * It might be the case that your `PLTCOLLECTS` environment variable is set and pointing to a different place than you think.
 *  If you're using the `NEW` Pycket (`pycket-c-linklets`), then it might be the case that you have `compiled` zo files in your Racket directory, and they need to be recompiled (you `pulled` a more recent Racket, maybe?).
 *  The `expander.rktl.linklet` might not be up to date with the latest Racket binary. You can use `make expander` target to rebuild the expander linklet, or shoot a message on the slack channel to us to update it.

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

