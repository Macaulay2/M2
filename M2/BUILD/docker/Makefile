## Parameters
TAG = m2
STORAGE = `pwd`/storage
M2_REPO = `pwd`/../../..
M2_HOME = /home/macaulay
VOLUME = --volume $(STORAGE):$(M2_HOME) --volume $(M2_REPO):$(M2_HOME)/M2
BUILD_DIR = M2/M2/BUILD/build-docker

###############################################################################

always:; @cat README.md

all: build run

clean:;	rm -rf ${M2_REPO}/../$(BUILD_DIR) $(STORAGE)/.ccache

###############################################################################
# Build targets

build: build-image

build-image:;	docker build --tag $(TAG) .

###############################################################################
# M2 targets

run: run-emacs

run-M2:;	docker run $(VOLUME) -it --entrypoint="" $(TAG) M2

run-M2-safe:;	docker run $(VOLUME) -it --entrypoint="" $(TAG) M2 -q --int --no-readline --no-randomize --no-threads

run-emacs-tui:;	docker run $(VOLUME) -it $(TAG)

run-emacs:;	docker run $(VOLUME) --net=host --env="DISPLAY" $(TAG)

###############################################################################
# Terminal targets

root:;	docker run $(VOLUME) -it -w $(M2_HOME)/$(BUILD_DIR) --user root --entrypoint bash $(TAG)

shell:;	docker run $(VOLUME) -it -w $(M2_HOME)/$(BUILD_DIR) --entrypoint bash $(TAG)
