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
.PHONY: test coverage test-random test test-old-single test-old-mark test-new-no-expander-single test-new-no-expander-mark test-new test-new-single test-new-mark
.PHONY: expander regexp fasl setup-local-racket bootstrap-linklets

PYPY_EXECUTABLE := $(shell which pypy)
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)

ifeq ($(PYPY_EXECUTABLE),)
RUNINTERP = python
else
RUNINTERP = $(PYPY_EXECUTABLE)
endif

WITH_JIT = -Ojit --translation-jit

RACKET_VERSION := 8.16.0.4
RACKET_INSTALLER_SCRIPT_NAME := racket-$(RACKET_VERSION)-x86_64-linux-jammy-cs.sh

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

build-docker-image:
	@if [ -z "$(username)" ]; then \
		echo "Usage: make build username=<your-username>"; \
		exit 1; \
	fi
	docker build -t $(username)/pycket-benchmark:latest .

push-docker-image:
	@if [ -z "$(username)" ]; then \
		echo "Usage: make push username=<your-username>"; \
		exit 1; \
	fi
	docker push $(username)/pycket-benchmark:latest

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

download-racket:
	$(info Downloading Racket installer)
	wget http://www.cs.utah.edu/plt/snapshots/current/installers/$(RACKET_INSTALLER_SCRIPT_NAME)

setup-local-racket: download-racket
	$(info Installing Racket in $(shell pwd)/racket)
	chmod 755 $(RACKET_INSTALLER_SCRIPT_NAME)
	./$(RACKET_INSTALLER_SCRIPT_NAME) --in-place --dest racket
	rm -f $(RACKET_INSTALLER_SCRIPT_NAME)
	export PLTHOME=$(shell pwd) && \
	export PLTCOLLECTS=$(shell pwd)/racket/collects && \
	echo "Telling Racket about Pycket"
	./racket/bin/raco pkg install -t dir pycket/pycket-lang/ || \
	./racket/bin/raco pkg update --link pycket/pycket-lang
	$(warning WARNING: PLTHOME needs to be manually set (I can not modify env variables I inherited from my parent process))
	$(warning Copy paste and run the following: export PLTHOME=`pwd`)

# Use the one below for non-local Racket builds
setup-racket-for-old-pycket:
	./racket/bin/raco pkg install -t dir pycket/pycket-lang/ || \
	./racket/bin/raco pkg update --link pycket/pycket-lang

PYPY_V=pypy2.7-v7.3.17-linux64
PYPY_PAK=$(PYPY_V).tar.bz2

# Get pypy source, as well as a binary to work with
setup-pypy: clone-pypy
	wget https://downloads.python.org/pypy/$(PYPY_PAK)
	tar xjf $(PYPY_PAK)
	ln -s $(PYPY_V)/bin/pypy pypy-c
	export PATH=$(PATH):$(PYPY_V)/bin

clean-racket:
	rm -rf racket

# Get pypy source for rpython stuff
clone-pypy:
	git clone https://github.com/pypy/pypy.git

make-pypy:
	$(MAKE) -C pypy
	cp pypy/pypy/goal/pypy-c pypy/pypy/goal/pypy

setup-old-pycket: setup-racket-for-old-pycket update-pypy

bootstrap-linklets: expander fasl regexp
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

BOOTSTRAP_LINKLET_DIR := $(CURDIR)/bootstrap-linklets

EXPANDER_PATH_FASL := $(BOOTSTRAP_LINKLET_DIR)/expander.linklet.fasl
EXPANDER_PATH_JSON := $(BOOTSTRAP_LINKLET_DIR)/expander.linklet.json
EXPANDER_PATH_ZO := $(BOOTSTRAP_LINKLET_DIR)/expander.linklet.zo

expander: check_pycket_c_linklets check_plthome
	@echo "Cleaning up current expander code."
	rm -f ${EXPANDER_PATH_FASL} ${EXPANDER_PATH_JSON} ${EXPANDER_PATH_ZO}
	$(MAKE) -s -C linklet-extractor expander
	@echo "Done. expander is at: $(EXPANDER_PATH_FASL)"

expander-json: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor expander-json
	@echo "Done. expander json is at: $(EXPANDER_PATH_JSON)"

IO_PATH_FASL := $(BOOTSTRAP_LINKLET_DIR)/io.linklet.fasl
IO_PATH_JSON := $(BOOTSTRAP_LINKLET_DIR)/io.linklet.json
IO_PATH_ZO := $(BOOTSTRAP_LINKLET_DIR)/io.linklet.zo

io: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor io
	@echo "Done. io is at : $(IO_PATH_FASL)"

io-json: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor io-json
	@echo "Done. io json is at: $(IO_PATH_JSON)"

REGEXP_PATH_FASL := $(BOOTSTRAP_LINKLET_DIR)/regexp.linklet.fasl
REGEXP_PATH_JSON := $(BOOTSTRAP_LINKLET_DIR)/regexp.linklet.json
REGEXP_PATH_ZO := $(BOOTSTRAP_LINKLET_DIR)/regexp.linklet.zo

regexp: check_pycket_c_linklets check_plthome
	@echo "Cleaning up current regexp code."
	rm -f ${REGEXP_PATH_FASL} ${REGEXP_PATH_JSON} ${REGEXP_PATH_ZO}
	$(MAKE) -s -C linklet-extractor regexp
	@echo "Done. regexp is at: $(REGEXP_PATH_FASL)"

regexp-json: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor regexp-json
	@echo "Done. regexp json is at: $(REGEXP_PATH_JSON)"

FASL_PATH_FASL := $(BOOTSTRAP_LINKLET_DIR)/fasl.linklet.fasl
FASL_PATH_JSON := $(BOOTSTRAP_LINKLET_DIR)/fasl.linklet.json
FASL_PATH_ZO := $(BOOTSTRAP_LINKLET_DIR)/fasl.linklet.zo

fasl:
	@echo "Cleaning up current fasl code."
	rm -f ${FASL_PATH_FASL} ${FASL_PATH_JSON} ${FASL_PATH_ZO}
	$(MAKE) -s -C linklet-extractor fasl
	@echo "Done. fasl is at: $(FASL_PATH_FASL)"

fasl-json:
	$(MAKE) -s -C linklet-extractor fasl-json
	@echo "Done. fasl json is at: $(FASL_PATH_JSON)"

expander-bytecode: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor expander-bytecode

regexp-bytecode: check_pycket_c_linklets check_plthome
	$(MAKE) -s -C linklet-extractor regexp-bytecode

fasl-bytecode:
	$(MAKE) -s -C linklet-extractor fasl-bytecode

test-old:
	$(RUNINTERP) $(PYTEST) pycket --ignore=pycket/test/test_entry_point.py

# To run a single test module
# make test-old-single test_basic.py
test-old-single:
	$(RUNINTERP) $(PYTEST) pycket --ignore=pycket/test/test_entry_point.py -k $(filter-out $@,$(MAKECMDGOALS))

# To run only the tests marked with a given mark
# make test-old-mark my-mark
test-old-mark:
	$(RUNINTERP) $(PYTEST) pycket --ignore=pycket/test/test_entry_point.py -m $(filter-out $@,$(MAKECMDGOALS))

test-new-no-expander:
	$(RUNINTERP) $(PYTEST) pycket --new --ignore=pycket/test/test_old_entry_point.py

# To run a single test module
# make test-new-no-expander-single test_basic.py
test-new-no-expander-single:
	$(RUNINTERP) $(PYTEST) pycket --new --ignore=pycket/test/test_old_entry_point.py -k $(filter-out $@,$(MAKECMDGOALS))

# To run only the tests marked with a given mark
# make test-new-no-expander-mark my-mark
test-new-no-expander-mark:
	$(RUNINTERP) $(PYTEST) pycket --new --ignore=pycket/test/test_old_entry_point.py -m $(filter-out $@,$(MAKECMDGOALS))

test-new:
	$(RUNINTERP) $(PYTEST) pycket --new --use-expander --ignore=pycket/test/test_old_entry_point.py

# To run a single test module
# make test-new-single test_basic.py
test-new-single:
	$(RUNINTERP) $(PYTEST) pycket --new --use-expander --ignore=pycket/test/test_old_entry_point.py -k $(filter-out $@,$(MAKECMDGOALS))

# To run only the tests marked with a given mark
# make test-new-mark my-mark
test-new-mark:
	$(RUNINTERP) $(PYTEST) pycket --new --use-expander --ignore=pycket/test/test_old_entry_point.py -m $(filter-out $@,$(MAKECMDGOALS))

# test-random: #$(PYFILES)
# 	@echo "Not yet implemented"
# 	# RUNINTERP PYTEST --random pycket --ignore=pycket/test/

coverage: pycket/test/coverage_report .coverage
pycket/test/coverage_report .coverage: $(PYFILES)
	$(PYTEST) pycket --cov pycket \
		--cov-report=term-missing --cov-report=html

# Prevent make from trying to interpret arguments as targets
%:
	@:
