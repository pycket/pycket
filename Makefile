translate-jit:
	python ./pypy/rpython/bin/rpython -Ojit --batch targetpycket.py

translate-no-jit:
	python ./pypy/rpython/bin/rpython --batch targetpycket.py

setup:
	raco pkg install -t dir pycket/pycket-lang/ || raco pkg update --link pycket/pycket-lang
	(cd pypy && hg pull && hg update)

test:
	py.test

