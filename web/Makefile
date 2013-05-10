
HOST=topps.diku.dk
DESTDIR=/var/www/html/topps/janus-playground/

all:
	@echo "Run 'make deploy' to deploy"

deploy:
	rsync \
		--recursive  \
		--copy-links \
		--perms      \
		--times      \
		--exclude=Makefile \
		--exclude=programs \
		--delete     \
		--verbose    \
		. $(HOST):$(DESTDIR)
