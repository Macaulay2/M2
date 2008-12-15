.PHONY : everything world configure all install
include include/configuration
everything: all install
world : configure everything
configure include/configuration:
	../../../../configure \
		--enable-encap \
		--enable-common-staging-area \
		--prefix=/usr/local

all :
	time $(MAKE) -f GNUmakefile

install :
	rm -rf /usr/local/encap/Macaulay2-$(PACKAGE_VERSION)
	time $(MAKE) install -o all prefix=/usr/local/encap


#		--enable-tarfile \
#		--enable-deb \


# Local Variables:
# compile-command: "make -f Makefile "
# End:
