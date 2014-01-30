USER = host
SERVER = analyze.positionstudios.com

all:
	cabal install -fdevelopment && ./dist/build/analyze/analyze -e devel

test:
	runghc -isrc src/Test.hs

run:
	./dist/build/analyze/analyze

deploy: send
	ssh $(USER)@$(SERVER) "/var/www/scripts/deploy.sh"


deploy-static: send-static
	ssh $(USER)@$(SERVER) "/var/www/scripts/reload.sh"

send: send-static
	cabal install
	scp dist/build/analyze/analyze $(USER)@$(SERVER):analyze-new
        scp dist/build/worker/worker $(USER)@$(SERVER):worker

send-static:
	scp angel.conf $(USER)@$(SERVER):
	rsync --checksum -avz -e ssh static/* $(USER)@$(SERVER):static
	rsync --checksum -avz -e ssh snaplets/* $(USER)@$(SERVER):snaplets
	rsync --checksum -avz -e ssh scripts/* $(USER)@$(SERVER):scripts

start:
	ssh $(USER)@$(SERVER) "/var/www/scripts/start.sh"

migrate:
	rsync --checksum -ave 'ssh '  migrations/* $(USER)@$(SERVER):migrations
	ssh $(USER)@$(SERVER) "/var/www/moo.sh upgrade"

rollback:
	ssh $(USER)@$(SERVER) "/var/www/scripts/rollback.sh"
