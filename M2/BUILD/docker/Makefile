TAG = m2-cmake-build
STORAGE = `pwd`/storage
M2_REPO = `pwd`/../../..
M2_HOME = /home/macaulay
VOLUME = --volume $(STORAGE):$(M2_HOME) --volume $(M2_REPO):$(M2_HOME)/M2
BUILD_DIR = M2/M2/BUILD/build-docker

define M2_BUILD_SCRIPT
set -xe

cmake -S M2/M2 -B $(BUILD_DIR) \
      -DMP_LIBRARY=MPIR \
      -DBUILD_TESTING=ON \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=$(M2_HOME)/$(BUILD_DIR)/usr-dist

cmake --build $(BUILD_DIR) --target build-libraries build-programs
cmake -S M2/M2 -B $(BUILD_DIR) # just to recheck library paths

cmake --build $(BUILD_DIR) --target all -j4
cmake --install $(BUILD_DIR)

yes | M2 -q --stop --silent -e "setupEmacs(); exit(0);"
endef
export M2_BUILD_SCRIPT

###############################################################################

always:; @cat README.md

clean:;  rm -rf ../build-docker

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

M2-safe:; docker run $(VOLUME) --interactive --tty --entrypoint="" $(TAG) M2 --int --no-readline --no-randomize

###############################################################################

# For anything else, run it in the build directory within the container
%:; docker run $(VOLUME) --interactive --tty --entrypoint="" $(TAG) make -C $(M2_HOME)/$(BUILD_DIR) $@ -j4
