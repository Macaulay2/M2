.PHONY : configure install

configure :
	../../../../configure \
		--enable-encap \
		--enable-common-staging-area \
		--prefix=/usr/local

install :
	rm -rf /usr/local/encap/Macaulay2-1.1.1
	$(MAKE) install -o all prefix=/usr/local/encap

#		--enable-tarfile \
#		--enable-deb \


# Local Variables:
# compile-command: "make -f Makefile "
# End:
