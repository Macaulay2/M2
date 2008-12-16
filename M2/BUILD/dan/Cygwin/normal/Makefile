# Cygwin options
CONFIGOPTIONS += --disable-pthreads
CONFIGOPTIONS += --enable-cygwin
CONFIGOPTIONS += --enable-common-staging-area
CONFIGOPTIONS += --build=i486-pc-cygwin

# options unique to Linux

# CONFIGOPTIONS += --enable-deb
# CONFIGOPTIONS += --enable-tarfile

# switchable options, at will

#BUILDOPTIONS += RemakeAllDocumentation=true
#BUILDOPTIONS += RemakePackages=true
#BUILDOPTIONS += -s
#INSTALLOPTIONS += -s

# options same as for Linux

INSTALLOPTIONS += prefix=/usr/local/encap
include ../../Makefile.include
clean-target:;	rm -rf /usr/local/encap/Macaulay2-$(PACKAGE_VERSION)
# Local Variables:
# compile-command: "make -f Makefile "
# End:
