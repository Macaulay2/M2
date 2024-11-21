# Try to find the e-antic libraries
# See https://github.com/flatsurf/e-antic
#
# This file sets up e-antic for CMake. Once done this will define
#  EANTIC_FOUND             - system has EANTIC lib
#  EANTIC_INCLUDE_DIR       - the EANTIC include directory
#  EANTIC_LIBRARIES         - Libraries needed to use EANTIC
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

if(NOT EANTIC_FOUND)
  set(EANTIC_INCLUDE_DIR NOTFOUND)
  set(EANTIC_LIBRARIES NOTFOUND)

  # search first if an EAnticConfig.cmake is available in the system,
  # if successful this would set EANTIC_INCLUDE_DIR and the rest of
  # the script will work as usual
  find_package(EAntic ${EAntic_FIND_VERSION} NO_MODULE QUIET)

  if(NOT EANTIC_INCLUDE_DIR)
    find_path(EANTIC_INCLUDE_DIR NAMES e-antic/e-antic.h
      HINTS ENV EANTICDIR
      PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
      )
  endif()

  if(NOT EANTIC_LIBRARIES)
    find_library(EANTIC_C_LIBRARY NAMES eantic
      HINTS ENV EANTICDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
    find_library(EANTIC_CXX_LIBRARY NAMES eanticxx
      HINTS ENV EANTICDIR
      PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
      )
    set(EANTIC_LIBRARIES ${EANTIC_CXX_LIBRARY} ${EANTIC_C_LIBRARY})
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(EAntic DEFAULT_MSG EANTIC_INCLUDE_DIR EANTIC_LIBRARIES)

  mark_as_advanced(EANTIC_INCLUDE_DIR EANTIC_LIBRARIES)
endif()
