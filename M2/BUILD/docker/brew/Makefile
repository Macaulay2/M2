## Parameters
TAG = m2-homebrew
M2_HOME = /home/linuxbrew
M2_REPO = $(shell git rev-parse --show-toplevel)
STORAGE = $(M2_REPO)/M2/BUILD/docker/storage

VOLUME-A = --volume /home/linuxbrew:$(M2_HOME)
VOLUME-B = --volume $(STORAGE)/.cache:$(M2_HOME)/.cache
VOLUME-C = --volume $(M2_REPO):$(M2_HOME)/M2

###############################################################################

always:; @cat README.md

all: build shell

###############################################################################
# Build targets

build: build-image

build-image:; docker build --tag $(TAG) .

###############################################################################
# Terminal targets

shell: shell-linux

# Run this on Linux
shell-linux:; docker run $(VOLUME-A) $(VOLUME-C) -it --entrypoint bash $(TAG)

# Run this on macOS
shell-macos:; docker run $(VOLUME-B) $(VOLUME-C) -it --entrypoint bash $(TAG)

shell-raw:;   docker run $(VOLUME-C) -it --entrypoint bash $(TAG)
