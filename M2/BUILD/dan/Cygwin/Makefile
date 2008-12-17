# Cygwin options
CONFIGOPTIONS += --disable-pthreads
CONFIGOPTIONS += --enable-cygwin
CONFIGOPTIONS += --build=i486-pc-cygwin

#BUILDOPTIONS += RemakeAllDocumentation=true
#BUILDOPTIONS += RemakePackages=true
#BUILDOPTIONS += -s
#INSTALLOPTIONS += -s

INSTALLOPTIONS += prefix=/usr/local/encap
include ../../Makefile.include
clean-target:;	rm -rf /usr/local/encap/Macaulay2-$(PACKAGE_VERSION)
# Local Variables:
# compile-command: "make -f Makefile "
# End:
