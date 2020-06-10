# Try to find the MPSOLVE libraries
# See https://numpi.dm.unipi.it/software/mpsolve
#
# This file sets up MPSOLVE for CMake. Once done this will define
#  MPSOLVE_FOUND             - system has MPSOLVE lib
#  MPSOLVE_INCLUDE_DIR       - the MPSOLVE include directory
#  MPSOLVE_LIBRARIES         - the MPSOLVE library
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(MPSOLVE_INCLUDE_DIR NAMES mps/mps.h)
find_library(MPSOLVE_LIBRARIES NAMES mps)

# handle the QUIETLY and REQUIRED arguments and set XXX_FOUND to TRUE if all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(MPSolve DEFAULT_MSG MPSOLVE_LIBRARIES MPSOLVE_INCLUDE_DIR)

mark_as_advanced(MPSOLVE_INCLUDE_DIR MPSOLVE_LIBRARIES)
