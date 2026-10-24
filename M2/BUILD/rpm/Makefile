always:
	@cat README.md

DISTRIBUTION = almalinux
RELEASE = latest
VERSION := $(shell cat ../../VERSION)
TARNAME = Macaulay2-$(VERSION)

ifeq ($(DISTRIBUTION), fedora)
	DOCKERDIR = fedora
else
	DOCKERDIR = rhel
endif

HOST_RPMBUILD = rpmbuild-$(DISTRIBUTION)-$(RELEASE)
CTR_RPMBUILD = /home/macaulay/rpmbuild

rpm: $(HOST_RPMBUILD) .image-built-$(DISTRIBUTION)-$(RELEASE)
	sudo chown -R 1000:0 $(HOST_RPMBUILD)
	docker run \
		--entrypoint bash                             \
		-v $(CURDIR)/$(HOST_RPMBUILD):$(CTR_RPMBUILD) \
		m2-$(DISTRIBUTION)-$(RELEASE)-build           \
		-c 'set -e; \
			rpmbuild -ba $(CTR_RPMBUILD)/SPECS/Macaulay2.spec'

$(HOST_RPMBUILD): Macaulay2.spec
	rm -rf $@
	mkdir -p $@/SPECS
	cp Macaulay2.spec $@/SPECS
	mkdir -p $@/SOURCES
# build tarball (essentially the same as "make dist")
	cd ../../.. && git archive --prefix=$(TARNAME)/ \
		-o $(CURDIR)/M2.tar HEAD
	gzip M2.tar
	mv M2.tar.gz $@/SOURCES/$(TARNAME).tar.gz

.image-built-%:
	$(MAKE) -C ../docker/$(DOCKERDIR) build-image \
		DISTRIBUTION=$(DISTRIBUTION) RELEASE=$(RELEASE)
	touch $@

clean:
	rm -rf .image-built-* rpmbuild-*
