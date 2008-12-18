#BUILDOPTIONS += RemakeAllDocumentation=true
#BUILDOPTIONS += RemakePackages=true
#BUILDOPTIONS += -s
#INSTALLOPTIONS += -s

# CONFIGOPTIONS += --enable-tarfile
# CONFIGOPTIONS += --enable-deb
INSTALLOPTIONS += prefix=/usr/local/encap
include ../Makefile.include
pre-install:;	rm -rf /usr/local/encap/Macaulay2-$(PACKAGE_VERSION)
arch-chk:
	grep -q Ubuntu /etc/issue
	[ `uname -m` = x86_64 ]
# Local Variables:
# compile-command: "make -f Makefile "
# End:
