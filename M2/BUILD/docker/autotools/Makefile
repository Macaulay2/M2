# include this Makefile in other docker Makefiles to add a
# 'build-autotools' target for running the entire autotools build

define M2_BUILD_SCRIPT_autotools
set -xe

git config --global --add safe.directory $(M2_HOME)/M2
mkdir -p M2/$(BUILD_DIR)
cd M2/$(BUILD_DIR)
$(M2_HOME)/M2/M2/autogen.sh
$(M2_HOME)/M2/M2/configure $(CONFIG_OPT)
make $(MAKE_OPT)
endef
export M2_BUILD_SCRIPT_autotools

build-autotools: build-image
	docker run $(VOLUME) -it --entrypoint="" $(TAG) \
		bash -c "$$M2_BUILD_SCRIPT_autotools"
