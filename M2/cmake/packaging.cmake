################################################################
## Setting variables for installation directories and Macaulay2 Layout

## Only overwrite the default prefix, not one provided via command line
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/usr-dist CACHE PATH "installation prefix" FORCE)
  set(M2_COMMON_INFIX	"/common"	CACHE INTERNAL "infix for architecture independent files")
  set(M2_EXEC_INFIX	"/${MACHINE}"	CACHE INTERNAL "infix for architecture dependent files")
endif()

set(M2_INSTALL_PREFIX	${CMAKE_INSTALL_PREFIX})
set(M2_COMMON_PREFIX	${M2_INSTALL_PREFIX}${M2_COMMON_INFIX})	# staging area for common files as in layout.m2.in
set(M2_EXEC_PREFIX	${M2_INSTALL_PREFIX}${M2_EXEC_INFIX})	# staging area for arch. dep. files as in layout.m2.in
set(M2_HOST_PREFIX	${CMAKE_BINARY_DIR}/usr-host)		# staging area for building libraries needed to compile M2

set(M2_PACKAGE_DIR	${M2_COMMON_PREFIX}/share/Macaulay2)
set(M2_CORE_DIR		${M2_PACKAGE_DIR}/Core)

## Rewrite this so that GNUInstallDirs module can use it to generate architecture independent directories
set(CMAKE_INSTALL_PREFIX ${M2_EXEC_PREFIX})

## This is using https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html#module:GNUInstallDirs
## Which follows https://www.gnu.org/prep/standards/html_node/Directory-Variables.html
include(GNUInstallDirs)

message("## Staging area directories: (set CMAKE_INSTALL_PREFIX to overwrite)
     common:	${M2_COMMON_PREFIX}
     exec:	${M2_EXEC_PREFIX}")

# TODO: install in /opt instead?

################################################################

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
