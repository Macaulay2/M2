.PHONY : everything world configure all install
include include/configuration
everything: all install check
world : configure everything
configure include/configuration:
	../../../../configure \
		--enable-encap \
		--enable-common-staging-area \
		--prefix=/usr/local

# OPTIONS = RemakeAllDocumentation=true RemakePackages=true

all :
	time $(MAKE) -f GNUmakefile $(OPTIONS)

install :
	rm -rf /usr/local/encap/Macaulay2-$(PACKAGE_VERSION)
	time $(MAKE) install -o all prefix=/usr/local/encap

check:
	$(MAKE) -C Macaulay2/html-check-links check

#		--enable-tarfile \
#		--enable-deb \


# Local Variables:
# compile-command: "make -f Makefile "
# End:
