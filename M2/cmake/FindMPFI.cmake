# Try to find the MPFI library
# See http://perso.ens-lyon.fr/nathalie.revol/software.html
#
# This module supports requiring a minimum version, e.g. you can do
#   find_package(MPFI 1.5.1)
# to require version 1.5.1 to newer of MPFI.
#
# Once done this will define
#
#  MPFI_FOUND           - system has MPFI lib with correct version
#  MPFI_INCLUDE_DIR     - the MPFI include directory
#  MPFI_LIBRARIES       - the MPFI library
#  MPFI_VERSION         - MPFI version

# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
# Redistribution and use is allowed according to the terms of the BSD license.

# Set MPFI_FIND_VERSION to 1.0.0 if no minimum version is specified
if(NOT MPFI_FIND_VERSION)
  if(NOT MPFI_FIND_VERSION_MAJOR)
    set(MPFI_FIND_VERSION_MAJOR 1)
  endif()
  if(NOT MPFI_FIND_VERSION_MINOR)
    set(MPFI_FIND_VERSION_MINOR 0)
  endif()
  if(NOT MPFI_FIND_VERSION_PATCH)
    set(MPFI_FIND_VERSION_PATCH 0)
  endif()
  set(MPFI_FIND_VERSION
    "${MPFI_FIND_VERSION_MAJOR}.${MPFI_FIND_VERSION_MINOR}.${MPFI_FIND_VERSION_PATCH}")
endif()

macro(_mpfi_check_version)
  # Query MPFI_VERSION
  file(READ "${MPFI_INCLUDE_DIR}/mpfi.h" _mpfi_version_header)

  string(REGEX MATCH "define[ \t]+MPFI_VERSION_MAJOR[ \t]+([0-9]+)"
    _mpfi_major_version_match "${_mpfi_version_header}")
  set(MPFI_MAJOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+MPFI_VERSION_MINOR[ \t]+([0-9]+)"
    _mpfi_minor_version_match "${_mpfi_version_header}")
  set(MPFI_MINOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+MPFI_VERSION_PATCHLEVEL[ \t]+([0-9]+)"
    _mpfi_patchlevel_version_match "${_mpfi_version_header}")
  set(MPFI_PATCHLEVEL_VERSION "${CMAKE_MATCH_1}")

  set(MPFI_VERSION
    ${MPFI_MAJOR_VERSION}.${MPFI_MINOR_VERSION}.${MPFI_PATCHLEVEL_VERSION})

  # Check whether found version exceeds minimum required
  if(${MPFI_VERSION} VERSION_LESS ${MPFI_FIND_VERSION})
    set(MPFI_VERSION_OK FALSE)
    message(STATUS "MPFI version ${MPFI_VERSION} found in ${MPFI_INCLUDE_DIR}, "
                   "but at least version ${MPFI_FIND_VERSION} is required")
  else()
    set(MPFI_VERSION_OK TRUE)
  endif()
endmacro(_mpfi_check_version)

if(NOT MPFI_VERSION_OK)
  set(MPFI_INCLUDE_DIR NOTFOUND)
  set(MPFI_LIBRARIES NOTFOUND)

  # search first if an MPFIConfig.cmake is available in the system,
  # if successful this would set MPFI_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(MPFI ${MPFI_FIND_VERSION} NO_MODULE QUIET)

  if(NOT MPFI_INCLUDE_DIR)
    find_path(MPFI_INCLUDE_DIR NAMES mpfi.h
      HINTS ENV MPFIDIR ENV GMPDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      )
  endif()

  if(MPFI_INCLUDE_DIR)
    _MPFI_check_version()
  endif()

  if(NOT MPFI_LIBRARIES)
    find_library(MPFI_LIBRARIES NAMES mpfi
      HINTS ENV MPFIDIR ENV GMPDIR MPFIDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(MPFI DEFAULT_MSG MPFI_INCLUDE_DIR MPFI_LIBRARIES MPFI_VERSION_OK)

  mark_as_advanced(MPFI_INCLUDE_DIR MPFI_LIBRARIES)

endif()
