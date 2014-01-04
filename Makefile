all:
	cabal install -fdevelopment && ./dist/build/analyze/analyze -e devel

test:
	runghc -isrc src/Test/Top.hs
