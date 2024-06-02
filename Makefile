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

PYFILES := $(shell find .  -maxdepth 1 -name '*.py' -type f) $(shell find pycket  -name '*.py' -type f)

.PHONY: all translate-jit-all $(TRANSLATE_TARGETS) translate-no-jit translate-jit-linklets
.PHONY: test coverage test-expander test-one test-one-expander test-mark test-mark-expander test-random
.PHONY: expander regexp fasl setup-local-racket

PYPY_EXECUTABLE := $(shell which pypy)
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)

ifeq ($(PYPY_EXECUTABLE),)
RUNINTERP = python
else
RUNINTERP = $(PYPY_EXECUTABLE)
endif

WITH_JIT = -Ojit --translation-jit_opencoder_model=big

RACKET_INSTALLER_SCRIPT_NAME := racket-8.13.0.5-x86_64-linux-jammy-cs.sh

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

debug-linklets: $(PYFILES)
	$(RUNINTERP) $(RPYTHON) $(WITH_JIT) --lldebug targetpycket.py --linklets
	cp pycket-c-linklets pycket-c-debug-linklets

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

recompile-racket-modules:
ifneq (,$(wildcard ./pycket-c-linklets))
	./pycket-c-linklets -c compile-file-pycket.rkt -- --recompile
else
	$(error Pycket binary does not exist)
endif

.PHONY: download-racket
download-racket:
	$(info Downloading Racket installer)
	wget http://www.cs.utah.edu/plt/snapshots/current/installers/$(RACKET_INSTALLER_SCRIPT_NAME)

setup-local-racket: download-racket
	$(info Installing Racket)
	chmod 755 $(RACKET_INSTALLER_SCRIPT_NAME)
	./$(RACKET_INSTALLER_SCRIPT_NAME) --in-place --dest racket
	rm -f $(RACKET_INSTALLER_SCRIPT_NAME)
	$(eval export PLTHOME=$(shell pwd)/racket)
	$(eval export PLTCOLLECTS=$(shell pwd)/racket/collects)
	$(info Telling Racket about Pycket)
	./racket/bin/raco pkg install -t dir pycket/pycket-lang/ || \
	./racket/bin/raco pkg update --link pycket/pycket-lang
	$(warning WARNING: PLTHOME needs to be manually set (I can not modify env variables I inherited from my parent process))
	$(warning Copy paste and run the following: export PLTHOME=`pwd`)

# Use the one below for non-local Racket builds
setup-racket-for-old-pycket:
	./racket/bin/raco pkg install -t dir pycket/pycket-lang/ || \
	./racket/bin/raco pkg update --link pycket/pycket-lang

clean-racket:
	rm -rf racket

clone-pypy:
	hg clone https://foss.heptapod.net/pypy/pypy pypy

make-pypy:
	$(MAKE) -C pypy
	cp pypy/pypy/goal/pypy-c pypy/pypy/goal/pypy

pull-pypy:
	hg -R $(PYPYPATH) pull

update-pypy: pull-pypy
	hg -R $(PYPYPATH) update

setup-old-pycket: setup-racket-for-old-pycket update-pypy

bootstrap-linklets: expander fasl
	@echo "ASSUMES: a built pycket-c-linklets binary"
	./pycket-c-linklets --make-linklet-zos

check_plthome:
	@if [ -z "$(PLTHOME)" ]; then \
		echo "WARNING: make expander assumes an unmodified Racket install and PLTHOME environment variable"; \
	fi

check_pycket_c_linklets:
	@if ! command -v $(PYCKET_C_LINKLETS) > /dev/null; then \
		echo "WARNING: also an already built pycket-c-linklets binary (to generate a serialized expander linklet)"; \
	fi

expander: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor expander
	@echo "Done. expander is at: $(CURDIR)/expander.fasl"

expander-json: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor expander-json
	@echo "Done. expander json is at: $(CURDIR)/expander.rkt.linklet"

regexp: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor regexp
	@echo "Done. regexp is at: $(CURDIR)/regexp.fasl"

regexp-json: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor regexp-json
	@echo "Done. regexp json is at: $(CURDIR)/regexp.rktl.linklet"

fasl:
	$(MAKE) -s -C linklet-extractor fasl
	@echo "Done. fasl is at: $(CURDIR)/fasl.fasl"

fasl-json:
	$(MAKE) -s -C linklet-extractor fasl-json
	@echo "Done. fasl json is at: $(CURDIR)/fasl.rktl.linklet"

expander-bytecode: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor expander-bytecode

regexp-bytecode: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor regexp-bytecode

fasl-bytecode:
	$(MAKE) -s -C linklet-extractor fasl-bytecode

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
