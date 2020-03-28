# Try to find the bdwgc libraries
# See https://github.com/Macaulay2/bdwgc
#
# This file sets up bdwgc for CMake. Once done this will define
#  BDWGC_FOUND             - system has BDWGC lib
#  BDWGC_INCLUDE_DIR       - the BDWGC include directory
#  BDWGC_LIBRARIES         - Libraries needed to use BDWGC
#
# Copyright (c) 2020, Mahrud Sayrafi, <mahrud@umn.edu>
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(BDWGC_INCLUDE_DIR NAMES gc/gc.h)
find_library(BDWGC_LIBRARIES NAMES gc gccpp bdwgc bdwgccpp)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(BDWGC DEFAULT_MSG BDWGC_INCLUDE_DIR BDWGC_LIBRARIES)

mark_as_advanced(BDWGC_INCLUDE_DIR BDWGC_LIBRARIES)
