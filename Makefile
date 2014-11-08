translate-all: translate-jit translate-no-callgraph translate-no-two-state translate-no-loop-opts

translate-jit:
	python ./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py

translate-no-two-state:
	python ./pypy/rpython/bin/rpython --output=pycket-no-two-state -Ojit --batch targetpycket.py --no-two-state

translate-no-callgraph:
	python ./pypy/rpython/bin/rpython --output=pycket-no-callgraph -Ojit --batch targetpycket.py --no-callgraph

translate-no-loop-opts:
	python ./pypy/rpython/bin/rpython --output=pycket-no-loop -Ojit --batch targetpycket.py --no-two-state --no-callgraph

translate-no-jit:
	python ./pypy/rpython/bin/rpython --batch targetpycket.py

setup:
	raco pkg install -t dir pycket/pycket-lang/ || raco pkg update --link pycket/pycket-lang
	(cd pypy && hg pull && hg update)

test:
	(cd pycket && py.test)

