## Parameters
TAG = m2
M2_HOME = /home/macaulay
M2_REPO = $(shell git rev-parse --show-toplevel)
STORAGE = $(M2_REPO)/M2/BUILD/docker/storage
VOLUME = --volume $(STORAGE):$(M2_HOME) --volume $(M2_REPO):$(M2_HOME)/M2
BUILD_DIR = M2/BUILD/build-docker

###############################################################################

always:; @cat README.md

all: build run

clean:; [[ "$(BUILD_DIR)" =~ "M2/BUILD/build".* ]] && rm -rf $(M2_REPO)/$(BUILD_DIR)
clean-ccache: rm -rf $(STORAGE)/.ccache

###############################################################################
# Build targets

build: build-image

build-image:;	docker build --tag $(TAG) .

###############################################################################
# M2 targets

run: run-emacs
run-M2:;	docker run $(VOLUME) -it --entrypoint M2 $(TAG)
run-M2-safe:;	docker run $(VOLUME) -it --entrypoint M2 $(TAG) -q --int --no-readline --no-randomize --no-threads
run-emacs-tui:;	docker run $(VOLUME) -it --entrypoint emacs $(TAG)
run-emacs:;	docker run $(VOLUME) --entrypoint emacs --net=host --env="DISPLAY" $(TAG)

check: check-1
check-1:;	docker run $(VOLUME) -it --entrypoint M2 $(TAG) -q --no-preload --check 1 -e 'exit 0'
check-2:;	docker run $(VOLUME) -it --entrypoint M2 $(TAG) -q --no-preload --check 2 -e 'exit 0'
check-3:;	docker run $(VOLUME) -it --entrypoint M2 $(TAG) -q --no-preload --check 3 -e 'exit 0'

###############################################################################
# Terminal targets

root:;	docker run $(VOLUME) -it -w $(M2_HOME)/M2/$(BUILD_DIR) --user root --entrypoint bash $(TAG)

shell:;	docker run $(VOLUME) -it -w $(M2_HOME)/M2/$(BUILD_DIR) --entrypoint bash $(TAG)
