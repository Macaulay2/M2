include ../Makefile
include ../autotools/Makefile

## Parameters
DISTRIBUTION = almalinux
RELEASE = latest
BUILD_ARGS = --build-arg DISTRIBUTION=$(DISTRIBUTION) \
	--build-arg RELEASE=$(RELEASE)
TAG = m2-$(DISTRIBUTION)-$(RELEASE)-build
BUILD_DIR = M2/BUILD/build-docker

CONFIG_OPT = --with-system-gc --enable-download --enable-rpm --prefix=/usr
