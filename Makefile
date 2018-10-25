#/usr/bin/make -f

#
# Path of pypy checkout
PYPYPATH ?= pypy

# Invocation of pytest, defaults to pypy's stuff
# but may also be `py.test`
PYTEST ?= $(PYPYPATH)/pytest.py
RPYTHON ?= $(PYPYPATH)/rpython/bin/rpython --batch


TRANSLATE_TARGETS := translate-jit translate-no-callgraph translate-no-two-state \
		translate-no-strategies translate-no-type-size-specialization \
		translate-jit-linklets

PYFILES := $(shell find . -name '*.py' -type f)

.PHONY: all translate-jit-all $(TRANSLATE_TARGETS) translate-no-jit translate-jit-linklets
.PHONY: setup test coverage expander test-expander test-one test-one-expander test-mark test-mark-expander test-random

PYPY_EXECUTABLE := $(shell which pypy)
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)

ifeq ($(PYPY_EXECUTABLE),)
RUNINTERP = python
else
RUNINTERP = $(PYPY_EXECUTABLE)
endif

WITH_JIT = -Ojit --translation-jit_opencoder_model=big


translate-jit-all: $(TRANSLATE_TARGETS)
all: translate-jit-all translate-no-jit


translate-jit: pycket-c
translate-jit-linklets: pycket-c-linklets
translate-no-hidden-classes: pycket-c-no-hidden-classes
translate-no-prune-env: pycket-c-no-prune-env
translate-no-two-state: pycket-c-no-two-state
translate-no-callgraph: pycket-c-no-callgraph
translate-no-strategies: pycket-c-no-strategies
translate-no-type-size-specialization: pycket-c-no-type-size-specialization
translate-no-jit: pycket-c-nojit

pycket-c: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) targetpycket.py
	cp pycket-c pycket-c-$(BRANCH)

pycket-c-linklets: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) targetpycket.py --linklets

pycket-c-no-hidden-classes: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) targetpycket.py --no-hidden-classes

pycket-c-no-prune-env: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) targetpycket.py --no-prune-env

pycket-c-no-two-state: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) targetpycket.py --no-two-state

pycket-c-no-callgraph: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) targetpycket.py --no-callgraph

pycket-c-no-strategies: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) targetpycket.py --no-strategies

pycket-c-no-type-size-specialization: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) targetpycket.py --no-type-size-specialization

pycket-c-nojit: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) targetpycket.py

pycket-c-linklets-nojit: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) targetpycket.py --linklets

debug: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) --lldebug targetpycket.py
	cp pycket-c pycket-c-debug

debug-no-jit: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) --lldebug targetpycket.py
	cp pycket-c pycket-c-debug-no-jit

compile-file:
ifneq (,$(wildcard ./pycket-c-linklets))
	./pycket-c-linklets compile-file-pycket.rkt -- $(FILE)
else
	$(error Pycket binary does not exist)
endif

compile-racket-modules:
ifneq (,$(wildcard ./pycket-c-linklets))
	./pycket-c-linklets compile-file-pycket.rkt -- -b
else
	$(error Pycket binary does not exist)
endif

clean-compiled-files:
ifneq (,$(wildcard ./pycket-c-linklets))
	./pycket-c-linklets -c compile-file-pycket.rkt -- --clean
else
	$(error Pycket binary does not exist)
endif

setup-racket-for-old-pycket:
	raco pkg install -t dir pycket/pycket-lang/ || \
	raco pkg update --link pycket/pycket-lang

clone-pypy:
	hg clone https://bitbucket.org/pypy/pypy

make-pypy:
	$(MAKE) -C pypy
	cp pypy/pypy/goal/pypy-c pypy/pypy/goal/pypy

pull-pypy:
	hg -R $(PYPYPATH) pull

update-pypy: pypy-pypy
	hg -R $(PYPYPATH) update

setup-old-pycket: setup-racket-for-old-pycket update-pypy

expander:
	@echo "WARNING: make expander assumes an unmodified Racket install and PLTHOME environmnent variable"
	$(MAKE) -C linklet-extractor

test:
	$(RUNINTERP) $(PYTEST) pycket --ignore=pycket/test/test_entry_point.py

test-new-no-expander:
	$(RUNINTERP) $(PYTEST) pycket --new --ignore=pycket/test/test_old_entry_point.py

test-new-with-expander:
	$(RUNINTERP) $(PYTEST) pycket --new --use-expander --ignore=pycket/test/test_old_entry_point.py

# test-random: #$(PYFILES)
# 	@echo "Not yet implemented"
# 	# RUNINTERP PYTEST --random pycket --ignore=pycket/test/

coverage: pycket/test/coverage_report .coverage
pycket/test/coverage_report .coverage: $(PYFILES)
	$(PYTEST) pycket --cov pycket \
		--cov-report=term-missing --cov-report=html
