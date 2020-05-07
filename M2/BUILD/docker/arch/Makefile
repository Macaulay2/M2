TAG = m2-arch-build
STORAGE = `pwd`/storage
M2_REPO = `pwd`/../../../..
M2_HOME = /home/macaulay
VOLUME = --volume $(STORAGE):$(M2_HOME) --volume $(M2_REPO):$(M2_HOME)/M2
BUILD_DIR = M2/M2/BUILD/build-docker

## Script for building Macaulay2
define M2_BUILD_SCRIPT
set -xe

## Full build
cmake -GNinja -S M2/M2 -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=Release -DUSING_MPIR=OFF
cmake --build $(BUILD_DIR) --target build-libraries # build-programs
cmake --build $(BUILD_DIR) --target M2-core # install-packages
#cmake --build $(BUILD_DIR) --target M2-emacs

## Setup Emacs
yes | M2 -q --stop --silent -e "setupEmacs(); exit(0);"

## Changes to the docker container are ephemeral,
## but this is how one would do it:
# cmake --install $(BUILD_DIR)
endef
export M2_BUILD_SCRIPT

###############################################################################

always:; @cat README.md

clean:;  rm -rf ../../../../../$(BUILD_DIR)

all: build run

###############################################################################

build:
	docker build --tag $(TAG) .
	docker run $(VOLUME) --interactive --tty --entrypoint="" $(TAG) bash -c "$$M2_BUILD_SCRIPT"

###############################################################################

run: run-graphical

run-graphical:; docker run $(VOLUME) --net=host --env="DISPLAY" $(TAG)

run-terminal:;  docker run $(VOLUME) --interactive --tty $(TAG)

###############################################################################

shell:;   docker run $(VOLUME) --interactive --tty --user root --entrypoint bash $(TAG)

M2:;      docker run $(VOLUME) --interactive --tty --entrypoint="" $(TAG) M2

M2-safe:; docker run $(VOLUME) --interactive --tty --entrypoint="" $(TAG) M2 --int --no-readline --no-randomize --no-threads

###############################################################################

# For anything else, run it in the build directory within the container
%:; docker run $(VOLUME) --interactive --tty --entrypoint="" $(TAG) cmake --build $(M2_HOME)/$(BUILD_DIR) --target $@
