
HOST=topps.diku.dk
DESTDIR=/var/www/html/topps/janus-playground/

RSYNC_OPTS = \
	--recursive        \
	--copy-links       \
	--perms            \
	--times            \
	--exclude=Makefile \
	--exclude=programs \
	--delete           \
	--verbose

all:
	@echo "Run 'make deploy' to deploy"

deploy:
	rsync $(RSYNC_OPTS) . $(HOST):$(DESTDIR)

deploy-remote: HOST=topps-remote.diku.dk
deploy-remote: deploy
