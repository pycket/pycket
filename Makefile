#/usr/bin/make -f

#
# Path of pypy checkout
PYPYPATH ?= pypy

# Invocation of pytest, defaults to pypy's stuff
# but may also be `py.test`
PYTEST ?= $(PYPYPATH)/pytest.py
RPYTHON ?= $(PYPYPATH)/rpython/bin/rpython --batch


TRANSLATE_TARGETS := translate-jit translate-callgraph translate-no-two-state \
		translate-callgraph-only translate-no-strategies \
		translate-no-type-size-specialization

PYFILES := $(shell find . -name '*.py' -type f)

.PHONY: all translate-jit-all $(TRANSLATE_TARGETS) translate-no-jit
.PHONY: setup test coverage

translate-jit-all: $(TRANSLATE_TARGETS)
all: translate-jit-all translate-no-jit


translate-jit: pycket-c
translate-no-prune-env: pycket-c-no-prune-env
translate-no-two-state: pycket-c-no-two-state
translate-callgraph: pycket-c-callgraph
translate-callgraph-only: pycket-c-callgraph-no-two-state
translate-no-strategies: pycket-c-no-strategies
translate-no-type-size-specialization: pycket-c-no-type-size-specialization
translate-no-jit: pycket-c-nojit

pycket-c: $(PYFILES)
	$(RPYTHON) -Ojit targetpycket.py

pycket-c-no-prune-env: $(PYFILES)
	$(RPYTHON) -Ojit targetpycket.py --no-prune-env

pycket-c-no-two-state: $(PYFILES)
	$(RPYTHON) -Ojit targetpycket.py --no-two-state

pycket-c-callgraph: $(PYFILES)
	$(RPYTHON) -Ojit targetpycket.py --callgraph

pycket-c-callgraph-no-two-state: $(PYFILES)
	$(RPYTHON) -Ojit targetpycket.py --callgraph --no-two-state

pycket-c-no-strategies: $(PYFILES)
	$(RPYTHON) -Ojit targetpycket.py --no-strategies

pycket-c-no-type-size-specialization: $(PYFILES)
	$(RPYTHON) -Ojit targetpycket.py --no-type-size-specialization

pycket-c-nojit: $(PYFILES)
	$(RPYTHON) targetpycket.py


setup:
	raco pkg install -t dir pycket/pycket-lang/ || \
		raco pkg update --link pycket/pycket-lang
	hg -R $(PYPYPATH) pull && \
	hg -R $(PYPYPATH) update

test: $(PYFILES)
	$(PYTEST) pycket


coverage: pycket/test/coverage_report .coverage
pycket/test/coverage_report .coverage: $(PYFILES)
	$(PYTEST) pycket --cov pycket \
		--cov-report=term-missing --cov-report=html
