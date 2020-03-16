# Try to find the MPIR libraries
# See http://www.mpir.org/
#
# This file sets up MPIR for CMake. Once done this will define
#  MPIR_FOUND             - system has MPIR lib
#  MPIR_INCLUDE_DIR       - the MPIR include directory
#  MPIR_LIBRARIES         - Libraries needed to use MPIR
#
# Copyright (c) 2006, Laurent Montel, <montel@kde.org>
# Copyright (c) 2018, Thomas Baumgart <tbaumgart@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(MPIR_INCLUDE_DIR NAMES mpir.h)
find_library(MPIR_LIBRARIES NAMES mpir libmpir)

find_path(MPIRXX_INCLUDE_DIR NAMES mpirxx.h)
find_library(MPIRXX_LIBRARIES NAMES mpirxx libmpirxx)

if(NOT MPIR_INCLUDE_DIR STREQUAL MPIRXX_INCLUDE_DIR)
  set(MPIR_INCLUDE_DIR ${MPIR_INCLUDE_DIR};${MPIRXX_INCLUDE_DIR})
endif()
set(MPIR_LIBRARIES ${MPIR_LIBRARIES};${MPIRXX_LIBRARIES})

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(MPIR DEFAULT_MSG MPIR_INCLUDE_DIR MPIR_LIBRARIES)

mark_as_advanced(MPIR_INCLUDE_DIR MPIR_LIBRARIES)
