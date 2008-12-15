everything:all install
configure :
	../../../../configure \
		--enable-encap \
		--enable-common-staging-area \
		--prefix=/usr/local

all :
	$(MAKE) -s

install :
	rm -rf /usr/local/encap/Macaulay2-1.1.1
	$(MAKE) install -o all prefix=/usr/local/encap POST_INSTALL=": start post install" NORMAL_INSTALL=": start normal install"


#		--enable-tarfile \
#		--enable-deb \


# Local Variables:
# compile-command: "make -f Makefile "
# End:
