always:
	@cat README.md

DISTRIBUTION = almalinux
RELEASE = latest
VERSION := $(shell cat ../../VERSION)
TARNAME = Macaulay2-$(VERSION)

ifeq ($(DISTRIBUTION), fedora)
	DOCKERDIR = fedora
	SUBMODULES =
else
	DOCKERDIR = rhel
	SUBMODULES = bdwgc fflas_ffpack flint frobby givaro googletest \
		mathic mathicgb memtailor
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
# build "fat" tarball w/ all necessary submodules
	cd ../../.. && git archive --prefix=$(TARNAME)/ \
		-o $(CURDIR)/M2.tar HEAD
	git submodule update --init ../../Macaulay2/editors/emacs
	cd ../../Macaulay2/editors/emacs && \
		git archive \
			--prefix=$(TARNAME)/M2/Macaulay2/editors/emacs/ \
			-o $(CURDIR)/M2-emacs.tar HEAD
	tar --concatenate --file=M2.tar M2-emacs.tar
	for SUBMODULE in $(SUBMODULES); do \
		git submodule update --init ../../submodules/$$SUBMODULE; \
		cd ../../submodules/$$SUBMODULE; \
		git archive \
			--prefix=$(TARNAME)/M2/submodules/$$SUBMODULE/ \
			-o $(CURDIR)/$$SUBMODULE.tar HEAD; \
		cd $(CURDIR); \
		tar --concatenate --file=M2.tar $$SUBMODULE.tar; \
	done
	gzip M2.tar
	mv M2.tar.gz $@/SOURCES/$(TARNAME).tar.gz
	rm *.tar

.image-built-%:
	$(MAKE) -C ../docker/$(DOCKERDIR) build-image \
		DISTRIBUTION=$(DISTRIBUTION) RELEASE=$(RELEASE)
	touch $@

clean:
	rm -rf .image-built-* rpmbuild-*
