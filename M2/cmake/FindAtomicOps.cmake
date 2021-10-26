# Try to find the libatomic_ops library
# https://github.com/ivmai/libatomic_ops
#
# This file sets up AtomicOps for CMake. Once done this will define
#  ATOMICOPS_FOUND             - system has the libatomic_ops library
#  ATOMICOPS_INCLUDE_DIR       - the libatomic_ops include directory
#  ATOMICOPS_LIBRARY           - the libatomic_ops library
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(ATOMICOPS_INCLUDE_DIR NAMES atomic_ops.h
  HINTS ${HOMEBREW_PREFIX}/opt/libatomic_ops/include
  PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
  )

find_library(ATOMICOPS_LIBRARY NAMES atomic_ops
  HINTS ${HOMEBREW_PREFIX}/opt/libatomic_ops/lib
  PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
  )

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(AtomicOps DEFAULT_MSG ATOMICOPS_INCLUDE_DIR ATOMICOPS_LIBRARY)

mark_as_advanced(ATOMICOPS_INCLUDE_DIR ATOMICOPS_LIBRARY)
