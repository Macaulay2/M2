# Try to find the libatomic_ops library
# https://github.com/ivmai/libatomic_ops
#
# This file sets up AtomicOps for CMake. Once done this will define
#  ATOMIC_OPS_FOUND             - system has the libatomic_ops library
#  ATOMIC_OPS_INCLUDE_DIR       - the libatomic_ops include directory
#  ATOMIC_OPS_LIBRARY           - the libatomic_ops library
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(ATOMIC_OPS_INCLUDE_DIR NAMES atomic_ops.h
  HINTS /usr/local/opt/libatomic_ops
  PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
  )

find_library(ATOMIC_OPS_LIBRARY NAMES atomic_ops
  HINTS /usr/local/opt/libatomic_ops
  PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
  )

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(AtomicOps DEFAULT_MSG ATOMIC_OPS_INCLUDE_DIR ATOMIC_OPS_LIBRARY)

mark_as_advanced(ATOMIC_OPS_INCLUDE_DIR ATOMIC_OPS_LIBRARY)
