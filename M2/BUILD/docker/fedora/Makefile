# 
# Written for Fedora Linux
#

.PHONY: always

always: build

build:
	cd ../../../../; docker build -t m2-fedora -f M2/BUILD/docker/fedora/Dockerfile .

run:
	[ ! -z `docker images -q m2-fedora` ] || make build; \
	docker run --rm -it m2-fedora

# To use the M2-in-a-container in Emacs add this line to the end of your .emacs-Macaulay2 file:
#   (setq M2-exe "docker run --rm -ti m2-fedora")
# More generally, to run M2 within a container run:
#   docker run --rm -ti -v m2:/M2 m2-fedora
# Then, to access files that M2 uses
#   ln -s /var/lib/docker/volumes/m2/_data m2-folder/
# Of course, you can change "m2" to something else to have multiple mountpoints
