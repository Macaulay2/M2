set(CMAKE_VERBOSE_MAKEFILE OFF)

set(DISTRIBUTION 1) # use this starting number to sequentially number the downstream distributions

set(COMPRESS "gz" CACHE STRING	"compression method for tarball (gz or bz2)")
option(M2TARFILE	"prepare binary and source packages as compressed tar files" OFF)
option(TARLIBS		"include symbolic links to needed shared libraries for tar" OFF)
option(FREEBSD		"prepare a package file for freebsd" OFF)
option(DEB		"prepare a *.deb package (for debian, ubuntu, ...)" OFF)
option(RPM		"prepare a *.rpm package (for red had based systems)" OFF)
option(DMG		"prepare a *.dmg package (for Mac OS)" OFF)

# TODO: check to make sure OPTIMIZE is ON if packaging is ON

#AC_OUTPUT() # distribution files configured

# install/Makefile

# top/Makefile
# top/INSTALL
# top/preremove
# top/postinstall

# tar/Makefile

# dmg/Makefile

# rpm/Makefile
# rpm/Macaulay2-body.spec
# rpm/Macaulay2-common-body.spec

# freebsd/Makefile
# freebsd/post-install
# freebsd/post-deinstall

# deb/Makefile
# deb/macaulay2-common/postrm
# deb/macaulay2-common/postinst
# deb/macaulay2-common/prerm
# deb/macaulay2-common/preinst
# deb/macaulay2/postrm
# deb/macaulay2/postinst
# deb/macaulay2/prerm
# deb/macaulay2/preinst
