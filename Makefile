#/usr/bin/make -f

#
# Path of pypy checkout
PYPYPATH ?= pypy

# Invocation of pytest, defaults to pypy's stuff
# but may also be `py.test`
PYTEST ?= $(PYPYPATH)/pytest.py
RPYTHON ?= $(PYPYPATH)/rpython/bin/rpython --batch


TRANSLATE_TARGETS := translate-jit translate-no-callgraph translate-no-two-state \
		translate-no-strategies translate-no-type-size-specialization

PYFILES := $(shell find . -name '*.py' -type f)

.PHONY: all translate-jit-all $(TRANSLATE_TARGETS) translate-no-jit
.PHONY: setup test coverage

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

debug: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) --lldebug targetpycket.py
	cp pycket-c pycket-c-debug

setup:
	# raco pkg install -t dir pycket/pycket-lang/ || \
	# 	raco pkg update --link pycket/pycket-lang
	hg -R $(PYPYPATH) pull && \
	hg -R $(PYPYPATH) update

THIS_DIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
## Assumes PLTHOME
EXPANDER_DIR := $(PLTHOME)/racket/src/expander
EXTRACT_DIR := $(EXPANDER_DIR)/extract

expander:
	@echo "WARNING: make expander assumes an unmodified Racket install and PLTHOME environmnent variable"
	mv $(EXTRACT_DIR)/main.rkt $(EXTRACT_DIR)/orig_main.rkt
	cp linklet-extractor/* $(EXTRACT_DIR)/
	$(MAKE) -C $(EXPANDER_DIR) expander-src-generate
	mv $(EXPANDER_DIR)/compiled/expander.rktl.pycket_ast $(THIS_DIR)/expander.rktl.linklet
	rm -f $(EXTRACT_DIR)/main.rkt
	rm -f $(EXTRACT_DIR)/zo-expand.rkt
	rm -f $(EXTRACT_DIR)/linkl-expand.rkt
	mv $(EXTRACT_DIR)/orig_main.rkt $(EXTRACT_DIR)/main.rkt

test:
	$(RUNINTERP) $(PYTEST) pycket --ignore=pycket/test/ #-k test_linklet.py -m linkl #--ignore=pycket/test/

test-with-expander:
	$(RUNINTERP) $(PYTEST) pycket --use-expander --ignore=pycket/test/ #-k test_linklet.py -m linkl #--ignore=pycket/test/

test-random: $(PYFILES)
	$(RUNINTERP) $(PYTEST) --random pycket --ignore=pycket/test/

coverage: pycket/test/coverage_report .coverage
pycket/test/coverage_report .coverage: $(PYFILES)
	$(PYTEST) pycket --cov pycket \
		--cov-report=term-missing --cov-report=html
