###############################################################################
## This script is responsible for generating CPackConfig.cmake in the build
## directory, which in turn is used by CPack to create distribution packages.
## See: https://cmake.org/cmake/help/latest/module/CPack.html
##
## - List supported package generators:
##   cpack --help
## - Generate .deb, .rpm, and .tar.gz packages:
##   cpack -G"DEB;RPM;TGZ"

set(CPACK_SOURCE_IGNORE_FILES "BUILD")

set(CPACK_GENERATOR        "TGZ" CACHE STRING "package types to create")
set(CPACK_SOURCE_GENERATOR "TGZ" CACHE STRING "source package types to create")

set(CPACK_PACKAGE_NAME		${PROJECT_NAME})
set(CPACK_PACKAGE_URL		${PROJECT_HOMEPAGE_URL})
set(CPACK_PACKAGE_VERSION	${PROJECT_VERSION}-${COMMIT_COUNT}-${GIT_COMMIT})
set(CPACK_PACKAGE_RELEASE	"1" CACHE STRING "Package distribution version")
set(CPACK_PACKAGE_CONTACT	"Mahrud Sayrafi <mahrud@umn.edu>") # FIXME
set(CPACK_PACKAGE_SUMMARY	"Software system for algebraic geometry research")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${CPACK_PACKAGE_SUMMARY}")
set(CPACK_PACKAGE_DESCRIPTION
  "Macaulay2 is a software system for algebraic geometry research, written by
Daniel R. Grayson and Michael E. Stillman. Based on Groebner bases, it
provides algorithms for computing homological invariants of rings and
modules.")
set(CPACK_PACKAGE_CHECKSUM "SHA256")
set(CPACK_PACKAGE_FILE_NAME
  "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-${CPACK_PACKAGE_RELEASE}.${CMAKE_SYSTEM_PROCESSOR}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME
  "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-${CPACK_PACKAGE_RELEASE}.src")

set(CPACK_STRIP_FILES ON)
set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY ON)
set(CPACK_PACKAGING_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})

# TODO: split into ALL, common, etc. using this:
#set(CPACK_INSTALL_CMAKE_PROJECTS
#  ${CMAKE_BINARY_DIR}:Macaulay2:ALL:/usr
#  ${CMAKE_BINARY_DIR}:Macaulay2:packages:/usr
#  ${CMAKE_BINARY_DIR}:Macaulay2:docs:/usr
#  )

# vs CPACK_PACKAGE_INSTALL_DIRECTORY
# CPACK_PACKAGE_VENDOR
# CPACK_PROJECT_CONFIG_FILE for this file?
# CPACK_INSTALL_COMMANDS
# CPACK_INSTALL_SCRIPT
# CPACK_INSTALLED_DIRECTORIES
# CPACK_OUTPUT_FILE_PREFIX
# CPACK_PACKAGE_EXECUTABLES

# TODO: remove duplicate files from package
# TODO: check to make sure OPTIMIZE is ON if packaging is ON
# TODO: OSX options: DragNDrop;PackageMaker;OSXX11;Bundle
# TODO: compression options: TGZ;STGZ;TBZ2;TZ;ZIP
# TODO: CPACK_SOURCE_GENERATOR options: TGZ;TZ

###############################################################################
## Variables for generating .rpm packages
## TIP: use lintian to check the produced package

set(CPACK_RPM_PACKAGE_NAME	${CPACK_PACKAGE_NAME})
set(CPACK_RPM_PACKAGE_URL	${CPACK_PACKAGE_URL})
set(CPACK_RPM_PACKAGE_VERSION	${CPACK_PACKAGE_VERSION})
set(CPACK_RPM_PACKAGE_RELEASE	${CPACK_PACKAGE_RELEASE})
set(CPACK_RPM_PACKAGE_SUMMARY	${CPACK_PACKAGE_SUMMARY})
set(CPACK_RPM_PACKAGE_DESCRIPTION ${CPACK_PACKAGE_DESCRIPTION})
set(CPACK_RPM_PACKAGE_LICENSE	"GPLv2+")
set(CPACK_RPM_PACKAGE_GROUP	Applications/Engineering)
#set(CPACK_RPM_PACKAGE_REQUIRES	"Macaulay2-common = ${CPACK_PACKAGE_VERSION}")
set(CPACK_RPM_CHANGELOG_FILE	${CMAKE_BINARY_DIR}/CHANGELOG)

#CPACK_RPM_PACKAGE_DEBUG
#CPACK_RPM_PACKAGE_ARCHITECTURE noarch for common
#CPACK_RPM_GENERATE_USER_BINARY_SPECFILE_TEMPLATE

#set(CPACK_RPM_COMPONENT_INSTALL ON) # we need to generate two packages: common and arch-dependent
#CPACK_RPM_NO_<COMPONENT>_INSTALL_PREFIX_RELOCATION
#CPACK_RPM_SPEC_MORE_DEFINE

set(CPACK_RPM_PRE_INSTALL_SCRIPT_FILE    ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/rpm-preinstall.sh)
set(CPACK_RPM_PRE_UNINSTALL_SCRIPT_FILE  ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/rpm-preuninstall.sh)
set(CPACK_RPM_POST_INSTALL_SCRIPT_FILE   ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/rpm-postinstall.sh)
set(CPACK_RPM_POST_UNINSTALL_SCRIPT_FILE ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/rpm-postuninstall.sh)

file(WRITE ${CPACK_RPM_PRE_INSTALL_SCRIPT_FILE} "\
#! /bin/sh -e\n")
file(WRITE ${CPACK_RPM_PRE_UNINSTALL_SCRIPT_FILE} "\
#! /bin/sh -e
cd ${CMAKE_INSTALL_FULL_INFODIR}
for p in ${DISTRIBUTED_PACKAGES}
do install-info --remove --quiet --info-dir=. $p.info || true
done")

file(WRITE ${CPACK_RPM_POST_INSTALL_SCRIPT_FILE} "\
#! /bin/sh -e
cd ${CMAKE_INSTALL_FULL_INFODIR}
for package in ${DISTRIBUTED_PACKAGES}
do install-info --quiet --info-dir=. $p.info || true
done")
file(WRITE ${CPACK_RPM_POST_UNINSTALL_SCRIPT_FILE} "\
#! /bin/sh -e\n")

# TODO: generate changelog from git description
string(TIMESTAMP TODAY "%a %b %d %Y")
file(GENERATE OUTPUT ${CPACK_RPM_CHANGELOG_FILE} CONTENT
  "* ${TODAY} ${CPACK_PACKAGE_CONTACT} - ${CPACK_PACKAGE_VERSION}-${CPACK_PACKAGE_RELEASE}
- Packaged ${CPACK_PACKAGE_VERSION}-${CPACK_PACKAGE_RELEASE}\n")

set(CPACK_RPM_EXCLUDE_FROM_AUTO_FILELIST_ADDITION
  ${CMAKE_INSTALL_PREFIX}
  ${CMAKE_INSTALL_FULL_BINDIR}
  ${CMAKE_INSTALL_FULL_LIBDIR}
  ${CMAKE_INSTALL_FULL_LIBEXECDIR}
  ${CMAKE_INSTALL_FULL_DATAROOTDIR}
  ${CMAKE_INSTALL_FULL_MANDIR}
  ${CMAKE_INSTALL_FULL_MANDIR}/man1
  ${CMAKE_INSTALL_FULL_INFODIR})

###############################################################################
## Variables for generating .deb packages
## TIP: use lintian to check the produced package

## For reproducible packages see:
## https://cmake.org/cmake/help/latest/cpack_gen/deb.html#reproducible-packages

set(CPACK_DEBIAN_PACKAGE_NAME		${CPACK_PACKAGE_NAME})
set(CPACK_DEBIAN_PACKAGE_URL		${CPACK_PACKAGE_URL})
set(CPACK_DEBIAN_PACKAGE_VERSION	${CPACK_PACKAGE_VERSION})
set(CPACK_DEBIAN_PACKAGE_RELEASE	${CPACK_PACKAGE_RELEASE})
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION	${CPACK_PACKAGE_DESCRIPTION})
set(CPACK_DEBIAN_PACKAGE_SECTION	math)
set(CPACK_DEBIAN_PACKAGE_PRIORITY	optional)
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS	ON)
#set(CPACK_DEBIAN_PACKAGE_DEPENDS	"Macaulay2-common (>= ${CPACK_PACKAGE_VERSION})")

# TODO: copyright: https://lists.debian.org/debian-devel-announce/2006/03/msg00023.html
# TODO: changelog: https://www.debian.org/doc/manuals/maint-guide/dreq.en.html#changelog

#CPACK_DEBIAN_PACKAGE_RECOMMENDS
#CPACK_DEBIAN_PACKAGE_SUGGESTS
#set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA
#  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/debian/postinst
#  ${CMAKE_CURRENT_SOURCE_DIR}/CMake/debian/prerm)
#CPACK_DEBIAN_PRE_INSTALL_SCRIPT_FILE
#CPACK_DEBIAN_PRE_UNINSTALL_SCRIPT_FILE
#CPACK_DEBIAN_POST_INSTALL_SCRIPT_FILE
#CPACK_DEBIAN_POST_UNINSTALL_SCRIPT_FILE
#CPACK_DEBIAN_NO_<COMPONENT>_INSTALL_PREFIX_RELOCATION
#set(CPACK_DEBIAN_COMPONENT_INSTALL ON) # we need to generate two packages: common and arch-dependent

# deb/macaulay2-common/postrm
# deb/macaulay2-common/postinst
# deb/macaulay2-common/prerm
# deb/macaulay2-common/preinst
# deb/macaulay2/postrm
# deb/macaulay2/postinst
# deb/macaulay2/prerm
# deb/macaulay2/preinst

###############################################################################
## TODO: Variables for generating .dmg packages

# dmg/Makefile

###############################################################################
# This line calls the internal CPack script that generates CPackConfig.cmake
# and CPackSourceConfig.cmake, which are then read by the cpack utility.

include(CPack)

