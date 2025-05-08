# Try to find the Normaliz executable and library
# See https://www.normaliz.uni-osnabrueck.de/
#
# This module supports requiring a minimum version, e.g. you can do
#   find_package(Normaliz 3.9.1)
# to require version 3.9.1 to newer of NORMALIZ.
#
# Once done this will define
#
#  NORMALIZ_FOUND	- the Normaliz library has correct version
#  NORMALIZ_EXECUTABLE	- the Normaliz executable
#  NORMALIZ_INCLUDE_DIR	- the Normaliz include directory
#  NORMALIZ_LIBRARIES	- the Normaliz library
#  NORMALIZ_VERSION	- the Normaliz version

# Copyright (c) 2021, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

# Set Normaliz_FIND_VERSION to 1.0.0 if no minimum version is specified
if(NOT Normaliz_FIND_VERSION)
  if(NOT Normaliz_FIND_VERSION_MAJOR)
    set(Normaliz_FIND_VERSION_MAJOR 1)
  endif()
  if(NOT Normaliz_FIND_VERSION_MINOR)
    set(Normaliz_FIND_VERSION_MINOR 0)
  endif()
  if(NOT Normaliz_FIND_VERSION_PATCH)
    set(Normaliz_FIND_VERSION_PATCH 0)
  endif()

  set(Normaliz_FIND_VERSION "${Normaliz_FIND_VERSION_MAJOR}.${Normaliz_FIND_VERSION_MINOR}.${Normaliz_FIND_VERSION_PATCH}")
endif()

macro(_NORMALIZ_check_version)
  # Query NORMALIZ_VERSION
  file(READ "${NORMALIZ_INCLUDE_DIR}/libnormaliz/version.h" _NORMALIZ_version_header)

  string(REGEX MATCH "define[ \t]+NMZ_VERSION_MAJOR[ \t]+([0-9]+)"
    _NORMALIZ_major_version_match "${_NORMALIZ_version_header}")
  set(NORMALIZ_MAJOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+NMZ_VERSION_MINOR[ \t]+([0-9]+)"
    _NORMALIZ_minor_version_match "${_NORMALIZ_version_header}")
  set(NORMALIZ_MINOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+NMZ_VERSION_PATCH[ \t]+([0-9]+)"
    _NORMALIZ_patch_version_match "${_NORMALIZ_version_header}")
  set(NORMALIZ_PATCH_VERSION "${CMAKE_MATCH_1}")

  set(NORMALIZ_VERSION
    ${NORMALIZ_MAJOR_VERSION}.${NORMALIZ_MINOR_VERSION}.${NORMALIZ_PATCH_VERSION})

  # Check whether found version exceeds minimum required
  if(${NORMALIZ_VERSION} VERSION_LESS ${Normaliz_FIND_VERSION})
    set(NORMALIZ_VERSION_OK FALSE)
    message(STATUS "Normaliz version ${NORMALIZ_VERSION} found in ${NORMALIZ_INCLUDE_DIR}, "
                   "but at least version ${Normaliz_FIND_VERSION} is required")
  else()
    set(NORMALIZ_VERSION_OK TRUE)
  endif()
endmacro(_NORMALIZ_check_version)

if(NOT NORMALIZ_FOUND)
  set(NORMALIZ_INCLUDE_DIR NOTFOUND)
  set(NORMALIZ_LIBRARIES NOTFOUND)

  # search first if an NormalizConfig.cmake is available in the system,
  # if successful this would set NORMALIZ_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(Normaliz ${Normaliz_FIND_VERSION} NO_MODULE QUIET)

  # find the Normaliz executable
  find_program(NORMALIZ_EXECUTABLE NAMES normaliz)

  if(NOT NORMALIZ_INCLUDE_DIR)
    find_path(NORMALIZ_INCLUDE_DIR NAMES libnormaliz/version.h
      HINTS ENV NORMALIZDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      PATH_SUFFIXES normaliz
      )
  endif()

  if(NORMALIZ_INCLUDE_DIR)
    _NORMALIZ_check_version()
  endif()

  if(NOT NORMALIZ_LIBRARIES)
    find_library(NORMALIZ_LIBRARIES NAMES normaliz
      HINTS ENV NORMALIZDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(Normaliz DEFAULT_MSG NORMALIZ_EXECUTABLE NORMALIZ_INCLUDE_DIR NORMALIZ_LIBRARIES NORMALIZ_VERSION_OK)

  mark_as_advanced(NORMALIZ_EXECUTABLE NORMALIZ_INCLUDE_DIR NORMALIZ_LIBRARIES)

endif()
