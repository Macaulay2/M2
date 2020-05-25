###############################################################################
## The subdirectories here are responsible for various pieces of Macaulay2 and
## this file is the central hub connecting them as well as giving instructions
## for where each piece should be installed on the target system

# TODO: consider using: source_group(TREE . PREFIX "Macaulay2" FILES ../include/M2/config.h ...)
# TODO: should we add [all,check,clean,distclean]-in-$dir?

###############################################################################
## Some miscellaneous instructions

# Generate README and copy the copyright license in usr-dist
configure_file(README.in ${M2_DIST_PREFIX}/${M2_INSTALL_DOCDIR}/README @ONLY)
file(COPY LAYOUT COPYING-GPL-2 COPYING-GPL-3 DESTINATION ${M2_DIST_PREFIX}/${M2_INSTALL_DOCDIR})

# Generate statuscodes.h
# FIXME: where should this go?
file(READ m2/statuscodes STATUSCODES)
string(REGEX
  REPLACE "([0-9]*):([A-Z\_]*):\"?([a-zA-Z ]*)\"?" "  \\2 = \\1,\t\t/* \\3 */"
  STATUSCODES_H ${STATUSCODES})
file(GENERATE OUTPUT ${CMAKE_BINARY_DIR}/include/M2/statuscodes.h CONTENT "${STATUSCODES_H}")

# eg: Style Macaulay2Doc ...
# This will be used by d/CMakeLists.txt and packages/CMakeLists.txt
# TODO: discontinue using this list
file(STRINGS "packages/\=distributed-packages" DISTRIBUTED_PACKAGES REGEX "^[a-zA-Z0-9]+$")
list(JOIN DISTRIBUTED_PACKAGES " " DISTRIBUTED_PACKAGES)

# This tells CMake how to set the RPATH on the installed executable
set(CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/Macaulay2/lib)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# Add extra compiler warning flags for Macaulay2
# NOTE: some warnings are disabled in the subdirectories
# TODO: add a cached option for this?
add_compile_options(
  -Wall -Wextra -Wfatal-errors -Wcast-qual # -Werror
  -Wno-unused-parameter # FIXME: caused by c/chk.c:244:40 and every scc1 generated file
  -Wno-attributes # caused by SIMD unpack functions in fflas-ffpack
  )

###############################################################################
## Call the subdirectories

# TODO: remove util
add_subdirectory(regex) # use POSIX regex
add_subdirectory(c) # rename to scc?
add_subdirectory(d) # rename to interpreter?
add_subdirectory(e) # rename to engine?
add_subdirectory(system) # rename to supervisor?
add_subdirectory(bin) # combine with engine or interpreter?
add_subdirectory(m2) # rename to Core?
add_subdirectory(man) # combine?
add_subdirectory(packages)
add_subdirectory(emacs) # rename to editors and include vim, etc?
#add_subdirectory(book) # separate repository?

if(BUILD_TESTING)
  add_subdirectory(tests)
endif()

if(BUILD_DOCS)
  add_subdirectory(docs)
endif()

###############################################################################
# Add source-based code coverage targets
if(BUILD_COVERAGE)
  include(coverage)
  foreach(_target IN ITEMS scc1 M2-regex M2-interpreter M2-supervisor M2-engine M2-unit-tests M2-binary)
    _ADD_COVERAGE(${_target})
  endforeach()
endif()

###############################################################################
## Install instructions, which are also are very important for CPack

if(NOT VERBOSE)
  set(CMAKE_INSTALL_MESSAGE LAZY)
endif()

# install M2-binary and M2-engine
# install(TARGET) does not support installing targets from subdirectories,
# so these targets are installed in e/CMakeLists.txt & bin/CMakeLists.txt.

# install the M2 wrapper
install(FILES
  ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ
  COMPONENT ALL)

# install shared and dynamic libraries
install(DIRECTORY
  ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2/lib
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/Macaulay2
  USE_SOURCE_PERMISSIONS
  COMPONENT ALL
  OPTIONAL
  FILES_MATCHING PATTERN "*.so*" PATTERN "*.dylib")

# install programs and licenses
install(DIRECTORY
  ${M2_DIST_PREFIX}/${M2_INSTALL_LIBEXECDIR}/Macaulay2/bin
  ${M2_DIST_PREFIX}/${M2_INSTALL_LIBEXECDIR}/Macaulay2/program-licenses
  DESTINATION ${CMAKE_INSTALL_LIBEXECDIR}/Macaulay2
  USE_SOURCE_PERMISSIONS
  COMPONENT ALL
  OPTIONAL)

# install documentation databases
install(DIRECTORY
  ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2
  DESTINATION ${CMAKE_INSTALL_LIBDIR}
  USE_SOURCE_PERMISSIONS
  COMPONENT common
  OPTIONAL
  PATTERN "lib" EXCLUDE)

# install man page, packages, info files, documentation, and emacs package
install(DIRECTORY
  ${M2_DIST_PREFIX}/${M2_INSTALL_MANDIR}
  ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}
  ${M2_DIST_PREFIX}/${M2_INSTALL_INFODIR}
  ${M2_DIST_PREFIX}/${M2_INSTALL_DATAROOTDIR}/doc
  ${M2_DIST_PREFIX}/${M2_INSTALL_DATAROOTDIR}/emacs
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}
  COMPONENT common
  OPTIONAL)

###############################################################################
## Export the CMake targets for use in other projects

install(EXPORT Macaulay2 NAMESPACE Macaulay2::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Macaulay2)
export(TARGETS M2-binary M2-engine M2-interpreter M2-supervisor M2-regex
  NAMESPACE Macaulay2:: FILE Macaulay2Config.cmake)
