translate-jit:
	python ./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py

translate-all:
	python ./pypy/rpython/bin/rpython --output=pycket-c -Ojit --batch targetpycket.py
	python ./pypy/rpython/bin/rpython --output=pycket-no-two-state -Ojit --batch targetpycket.py --no-two-state
	python ./pypy/rpython/bin/rpython --output=pycket-no-callgraph -Ojit --batch targetpycket.py --no-callgraph
	python ./pypy/rpython/bin/rpython --output=pycket-no-loop -Ojit --batch targetpycket.py --no-two-state --no-callgraph

translate-no-jit:
	python ./pypy/rpython/bin/rpython --batch targetpycket.py

setup:
	raco pkg install -t dir pycket/pycket-lang/ || raco pkg update --link pycket/pycket-lang
	(cd pypy && hg pull && hg update)

test:
	(cd pycket && py.test)

