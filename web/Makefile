
HOST = topps.diku.dk
DESTDIR = /var/www/html/topps/janus-playground/
TUNNEL_PORT = 3022

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

open-tunnel:
	ssh -f -L $(TUNNEL_PORT):topps.diku.dk:22 ask.diku.dk sleep 5

deploy-tunnel: RSYNC_OPTS += --rsh "ssh -p $(TUNNEL_PORT)"
deploy-tunnel: HOST = budde@localhost
deploy-tunnel: open-tunnel deploy
