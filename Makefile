export ZDOTDIR := ./

SHELL := /usr/bin/env zsh

USER = host
SERVER = analyze.positionstudios.com

all:
	cabal install -fdevelopment && ./dist/build/analyze/analyze -e devel

test:
	cabal exec runghc -isrc src/Test.hs

run:
	./dist/build/analyze/analyze

migrate:
	rsync --checksum -ave 'ssh '  migrations/* $(USER)@$(SERVER):migrations
	ssh $(USER)@$(SERVER) "/var/www/moo.sh upgrade"


keter-build:
	cabal install -j
	cp .cabal-sandbox/bin/analyze analyze
	cp .cabal-sandbox/bin/worker worker
	tar czfv analyze.keter analyze worker config production.cfg static snaplets log/_blank

keter-deploy:
	scp analyze.keter $(SERVER):/opt/keter/incoming

deploy: keter-build keter-deploy
