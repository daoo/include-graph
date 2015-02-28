check:
	cabal build --ghc-options="-Wall -fno-code -fforce-recomp"

build:
	cabal build --ghc-options="-O2 -Wall"

lint:
	hlint .

clean:
	cabal clean

.PHONY: clean
