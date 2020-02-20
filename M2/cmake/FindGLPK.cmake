# Try to find the GLPK librairies
# See https://www.gnu.org/software/glpk/
#
# This file sets up GLPK for CMake. Once done this will define
#  GLPK_FOUND             - system has GLPK lib
#  GLPK_INCLUDE_DIR       - the GLPK include directory
#  GLPK_LIBRARIES         - Libraries needed to use GLPK
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(GLPK_INCLUDE_DIR NAMES glpk.h )
find_library(GLPK_LIBRARIES NAMES glpk libglpk)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(GLPK DEFAULT_MSG GLPK_INCLUDE_DIR GLPK_LIBRARIES)

mark_as_advanced(GLPK_INCLUDE_DIR GLPK_LIBRARIES)
