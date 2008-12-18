#BUILDOPTIONS += RemakeAllDocumentation=true
#BUILDOPTIONS += RemakePackages=true
#BUILDOPTIONS += -s
#INSTALLOPTIONS += -s

# CONFIGOPTIONS += --enable-tarfile
# CONFIGOPTIONS += --enable-rpm
CONFIGOPTIONS += --build=i486-pc-linux-gnu
INSTALLOPTIONS += prefix=~/local.Linux/encap
include ../Makefile.include
pre-install:
	epkg -r Macaulay2
	rm -rf ~/local.Linux/encap/Macaulay2-$(PACKAGE_VERSION)
post-install:	
	epkg -i Macaulay2
# Local Variables:
# compile-command: "make -f Makefile "
# End:
