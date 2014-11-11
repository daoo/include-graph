all:
	cabal build --ghc-options="-Wall"

lint:
	hlint IncludeGraph.hs

clean:
	cabal clean

.PHONY: clean
