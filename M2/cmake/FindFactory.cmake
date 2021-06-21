# Try to find the Singular Factory library
# See https://github.com/Singular/Sources/tree/spielwiese/factory
#
# This module supports requiring a minimum version, e.g. you can do
#   find_package(Factory 4.1.1)
# to require version 4.1.1 to newer of FACTORY.
#
# Once done this will define
#
#  FACTORY_FOUND	- system has the FACTORY library with correct version
#  FACTORY_INCLUDE_DIR	- the FACTORY include directory
#  FACTORY_LIBRARIES	- the FACTORY library
#  FACTORY_VERSION	- FACTORY version

# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

# Set Factory_FIND_VERSION to 4.0.0 if no minimum version is specified
if(NOT Factory_FIND_VERSION)
  if(NOT Factory_FIND_VERSION_MAJOR)
    set(Factory_FIND_VERSION_MAJOR 4)
  endif()
  if(NOT Factory_FIND_VERSION_MINOR)
    set(Factory_FIND_VERSION_MINOR 0)
  endif()
  if(NOT Factory_FIND_VERSION_PATCH)
    set(Factory_FIND_VERSION_PATCH 0)
  endif()

  set(Factory_FIND_VERSION "${Factory_FIND_VERSION_MAJOR}.${Factory_FIND_VERSION_MINOR}.${Factory_FIND_VERSION_PATCH}")
endif()

macro(_Factory_check_version)
  # Query Factory_VERSION
  file(READ "${FACTORYCONF_INCLUDE_DIR}/factory/factoryconf.h" _factory_version_header)

  string(REGEX MATCH "define[ \t]+PACKAGE_VERSION[ \t]+\"([0-9]+)\.([0-9]+)\.([0-9]+)\""
    _factory_version_match "${_factory_version_header}")
  set(FACTORY_MAJOR_VERSION "${CMAKE_MATCH_1}")
  set(FACTORY_MINOR_VERSION "${CMAKE_MATCH_2}")
  set(FACTORY_PATCHLEVEL_VERSION "${CMAKE_MATCH_3}")

  set(FACTORY_VERSION
    ${FACTORY_MAJOR_VERSION}.${FACTORY_MINOR_VERSION}.${FACTORY_PATCHLEVEL_VERSION})

  # Check whether found version exceeds minimum required
  if(${FACTORY_VERSION} VERSION_LESS ${Factory_FIND_VERSION})
    set(FACTORY_VERSION_OK FALSE)
    message(STATUS "Factory version ${FACTORY_VERSION} found in ${FACTORY_INCLUDE_DIR}, "
                   "but at least version ${Factory_FIND_VERSION} is required")
  else()
    set(FACTORY_VERSION_OK TRUE)
  endif()
endmacro(_Factory_check_version)

if(NOT FACTORY_FOUND)
  set(FACTORYCONF_INCLUDE_DIR NOTFOUND)
  set(FACTORY_INCLUDE_DIR NOTFOUND)
  set(FACTORY_LIBRARIES NOTFOUND)
  set(GFTABLESDIR NOTFOUND)

  # search first if an FactoryConfig.cmake is available in the system,
  # if successful this would set FACTORY_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(Factory ${Factory_FIND_VERSION} NO_MODULE QUIET)

  if(NOT FACTORY_INCLUDE_DIR)
    find_path(FACTORYCONF_INCLUDE_DIR NAMES factory/factoryconf.h
      HINTS ENV FACTORYDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      PATH_SUFFIXES singular)
    find_path(FACTORY_INCLUDE_DIR NAMES factory/factory.h
      HINTS ENV FACTORYDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      PATH_SUFFIXES singular)
    find_path(GFTABLESDIR NAMES gftables/961
      HINTS ENV GFTABLESDIR
      PATHS ${CMAKE_SYSTEM_PREFIX_PATH}
      PATH_SUFFIXES share/factory share/singular/factory)
    set(FACTORY_INCLUDE_DIR "${FACTORY_INCLUDE_DIR};${FACTORYCONF_INCLUDE_DIR}")
  endif()

  if(FACTORYCONF_INCLUDE_DIR)
    _Factory_check_version()
  endif()

  if(NOT FACTORY_LIBRARIES)
    find_library(FACTORY_LIBRARIES NAMES factory singular-factory
      HINTS ENV FACTORYDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(Factory DEFAULT_MSG FACTORY_INCLUDE_DIR FACTORY_LIBRARIES FACTORY_VERSION_OK)

  mark_as_advanced(FACTORY_INCLUDE_DIR FACTORY_LIBRARIES)

endif()
