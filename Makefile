all:
	cabal install -fdevelopment && ./dist/build/analyze/analyze -e devel
