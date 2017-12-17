all : test run

run: _force
	ghc --make -XScopedTypeVariables run.hs tcl.hs joy.hs

test: _force
	ghc --make -XScopedTypeVariables test.hs tcl.hs joy.hs
	./test
	ci-l *.hs Makefile

_force:
