# 
# Written for Fedora 26
#

.PHONY: always

always: docker-fedora

docker-fedora-build:
	docker build -t m2-fedora .;

docker-fedora:
	[ ! -z `docker images -q m2-fedora` ] || docker build -t m2-fedora .;
	docker run -it m2-fedora /usr/bin/M2
