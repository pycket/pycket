translate-all: translate-jit translate-no-callgraph translate-no-two-state translate-no-loop-opts translate-no-strategies

translate-jit:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py

translate-no-two-state:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py --no-two-state

translate-no-callgraph:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py --no-callgraph

translate-no-loop-opts:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py --no-two-state --no-callgraph

translate-no-strategies:
	./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py --no-strategies

translate-no-jit:
	./pypy/rpython/bin/rpython --batch targetpycket.py

setup:
	raco pkg install -t dir pycket/pycket-lang/ || raco pkg update --link pycket/pycket-lang
	(cd pypy && hg pull && hg update)

test:
	(cd pycket && py.test)

