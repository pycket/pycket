translate-all: translate-jit translate-no-callgraph translate-no-two-state translate-no-loop-opts translate-no-strategies translate-no-type-size-specialization

translate-jit:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py

translate-no-prune-env:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py --no-prune-env

translate-no-two-state:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py --no-two-state

translate-callgraph:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py --callgraph

translate-no-strategies:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py --no-strategies

translate-no-type-size-specialization:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py --no-type-size-specialization

translate-no-jit:
	./pypy/rpython/bin/rpython --batch targetpycket.py

setup:
	raco pkg install -t dir pycket/pycket-lang/ || raco pkg update --link pycket/pycket-lang
	(cd pypy && hg pull && hg update)

test:
	(cd pycket && py.test)

