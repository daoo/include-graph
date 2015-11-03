build:
	@cabal build --ghc-options="-Wall"

lint:
	@hlint progs src

clean:
	@cabal clean

.PHONY: clean
