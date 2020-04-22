###############################################################################
## This script is responsible for generating CPackConfig.cmake in the build
## directory, which in turn is used by CPack to create distribution packages.
## See: https://cmake.org/cmake/help/latest/module/CPack.html
##
## - cpack -G"DEB;RPM;TGZ"

set(PROJECT_DISTRIBUTION 1)

# TODO: check to make sure OPTIMIZE is ON if packaging is ON
# TODO: OSX options: DragNDrop;PackageMaker;OSXX11;Bundle
# TODO: compression options: TGZ;STGZ;TBZ2;TZ;ZIP
# TODO: CPACK_SOURCE_GENERATOR options: TGZ;TZ
set(COMPRESS "gz" CACHE STRING	"compression method for tarball (gz or bz2)")
#option(M2TARFILE	"prepare binary and source packages as compressed tar files" OFF)
#option(TARLIBS		"include symbolic links to needed shared libraries for tar" OFF)
#option(FREEBSD		"prepare a package file for freebsd" OFF)
option(DEB		"prepare a *.deb package (for debian, ubuntu, ...)" OFF)
option(RPM		"prepare a *.rpm package (for red had based systems)" OFF)
#option(DMG		"prepare a *.dmg package (for Mac OS)" OFF)

# TODO: split into ALL, common, etc. using this:
#set(CPACK_INSTALL_CMAKE_PROJECTS
#  ${CMAKE_BINARY_DIR}:Macaulay2:ALL:/usr
#  ${CMAKE_BINARY_DIR}:Macaulay2:packages:/usr
#  ${CMAKE_BINARY_DIR}:Macaulay2:docs:/usr
#  )

set(CPACK_GENERATOR        "TGZ" CACHE STRING "package types to create")
set(CPACK_SOURCE_GENERATOR "TGZ" CACHE STRING "source package types to create")

set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY ON) # Experiment
set(CPACK_STRIP_FILES ON)
set(CPACK_PACKAGE_EXECUTABLES	"M2-binary;Macaulay2")
set(CPACK_PACKAGE_CHECKSUM	"SHA256")
set(CPACK_PACKAGING_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
# vs CPACK_PACKAGE_INSTALL_DIRECTORY
# CPACK_PACKAGE_VENDOR
# CPACK_PROJECT_CONFIG_FILE for this file?
# CPACK_INSTALL_COMMANDS
# CPACK_INSTALL_SCRIPT
# CPACK_INSTALLED_DIRECTORIES
# CPACK_OUTPUT_FILE_PREFIX

set(CPACK_PACKAGE_SUMMARY "Macaulay2 is a software system for algebraic geometry research")

set(CPACK_PACKAGE_NAME	${PROJECT_NAME})
set(CPACK_PACKAGE_URL	${PROJECT_HOMEPAGE_URL})
set(CPACK_PACKAGE_VERSION	${PROJECT_VERSION})
set(CPACK_PACKAGE_RELEASE	${PROJECT_DISTRIBUTION})
set(CPACK_PACKAGE_CONTACT	"mahrud@umn.edu") # FIXME
# ALT: set(CPACK_PACKAGE_DESCRIPTION_FILE ...)
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
  "Macaulay2 is a software system for algebraic geometry research, written by \
  Daniel R. Grayson and Michael E. Stillman.  Based on Groebner bases, it \
  provides algorithms for computing homological invariants of rings and \
  modules.")
set(CPACK_PACKAGE_FILE_NAME
  "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-${CPACK_PACKAGE_RELEASE}.${CMAKE_SYSTEM_PROCESSOR}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME
  "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-${CPACK_PACKAGE_RELEASE}.src")

# TODO: generate changelog from git description
string(TIMESTAMP TODAY "%a %b %d %Y")
file(GENERATE OUTPUT CHANGELOG CONTENT "* ${TODAY} Macaulay2 \n- Packaged ${CPACK_PACKAGE_VERSION}-${CPACK_PACKAGE_RELEASE}\n")

###############################################################################

#CPACK_RPM_PACKAGE_DEBUG
#CPACK_RPM_PACKAGE_ARCHITECTURE noarch for common
#CPACK_RPM_GENERATE_USER_BINARY_SPECFILE_TEMPLATE

set(CPACK_RPM_PACKAGE_NAME	${PROJECT_NAME})
set(CPACK_RPM_PACKAGE_URL	${PROJECT_HOMEPAGE_URL})
set(CPACK_RPM_PACKAGE_VERSION	${PROJECT_VERSION})
set(CPACK_RPM_PACKAGE_RELEASE	${PROJECT_DISTRIBUTION})
set(CPACK_RPM_PACKAGE_SUMMARY	${CPACK_PACKAGE_SUMMARY})

set(CPACK_RPM_PACKAGE_LICENSE	various)
set(CPACK_RPM_PACKAGE_GROUP	Applications/Engineering)
#set(CPACK_RPM_PACKAGE_REQUIRES	"Macaulay2-common = 1.15.0.1")
#CPACK_RPM_PRE_INSTALL_SCRIPT_FILE
#CPACK_RPM_PRE_UNINSTALL_SCRIPT_FILE
#CPACK_RPM_POST_INSTALL_SCRIPT_FILE
#CPACK_RPM_POST_UNINSTALL_SCRIPT_FILE
#CPACK_RPM_NO_<COMPONENT>_INSTALL_PREFIX_RELOCATION
#set(CPACK_RPM_COMPONENT_INSTALL ON) # we need to generate two packages: common and arch-dependent
set(CPACK_RPM_CHANGELOG_FILE	${CMAKE_BINARY_DIR}/CHANGELOG)
#CPACK_RPM_SPEC_INSTALL_POST
#CPACK_RPM_SPEC_MORE_DEFINE

set(CPACK_RPM_EXCLUDE_FROM_AUTO_FILELIST_ADDITION
  /usr/local /usr/local/bin /usr/local/lib64 /usr/local/libexec /usr/local/share)

###############################################################################

#CPACK_DEBIAN_PACKAGE_PRIORITY
#CPACK_DEBIAN_PACKAGE_RECOMMENDS
#CPACK_DEBIAN_PACKAGE_SUGGESTS
#set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA
#  "${CMAKE_CURRENT_SOURCE_DIR}/CMake/debian/postinst;${CMAKE_CURRENT_SOURCE_DIR}/CMake/debian/prerm;" )

set(CPACK_DEBIAN_PACKAGE_NAME	${PROJECT_NAME})
set(CPACK_DEBIAN_PACKAGE_URL	${PROJECT_HOMEPAGE_URL})
set(CPACK_DEBIAN_PACKAGE_VERSION	${PROJECT_VERSION})
set(CPACK_DEBIAN_PACKAGE_RELEASE	${PROJECT_DISTRIBUTION})
#set(CPACK_DEBIAN_PACKAGE_SUMMARY	${CPACK_PACKAGE_SUMMARY})

set(CPACK_DEBIAN_PACKAGE_SECTION	devel)
#set(CPACK_DEBIAN_PACKAGE_DEPENDS	"Macaulay2-common (>= 1.15.0.1)")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION ${CPACK_PACKAGE_DESCRIPTION_SUMMARY})
#CPACK_DEBIAN_PRE_INSTALL_SCRIPT_FILE
#CPACK_DEBIAN_PRE_UNINSTALL_SCRIPT_FILE
#CPACK_DEBIAN_POST_INSTALL_SCRIPT_FILE
#CPACK_DEBIAN_POST_UNINSTALL_SCRIPT_FILE
#CPACK_DEBIAN_NO_<COMPONENT>_INSTALL_PREFIX_RELOCATION
#set(CPACK_DEBIAN_COMPONENT_INSTALL ON) # we need to generate two packages: common and arch-dependent
set(CPACK_DEBIAN_CHANGELOG_FILE	${CMAKE_BINARY_DIR}/CHANGELOG)

###############################################################################
include(CPack)

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
