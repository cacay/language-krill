binary = sill

all:
	cabal configure
	cabal build
	mkdir -p bin
	cp dist/build/${binary}/${binary} bin/

clean:
	rm -rf bin
	cabal clean
