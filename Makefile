build/main: main.hs
	ghc -o build/gcalc main.hs
	mv main.hi build
	mv main.o build
	
install:
	sudo cp build/gcalc /bin/gcalc

test:
	ghc -o build/tests tests.hs
	mv tests.hi build
	mv tests.o build