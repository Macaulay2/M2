# Try to find the GLPK libraries
# See https://www.gnu.org/software/glpk/
#
# This file sets up GLPK for CMake. Once done this will define
#  GLPK_FOUND             - system has GLPK lib
#  GLPK_ROOT              - the GLPK install prefix
#  GLPK_INCLUDE_DIR       - the GLPK include directory
#  GLPK_LIBRARIES         - Libraries needed to use GLPK
#
# Redistribution and use is allowed according to the terms of the BSD license.

find_path(GLPK_INCLUDE_DIR NAMES glpk.h
  PATHS ${INCLUDE_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/include
  )

find_library(GLPK_LIBRARIES NAMES glpk libglpk
  PATHS ${LIB_INSTALL_DIR} ${CMAKE_INSTALL_PREFIX}/lib
  )

string(REGEX REPLACE "/include(/${CMAKE_LIBRARY_ARCHITECTURE}$)?" "" GLPK_ROOT "${GLPK_INCLUDE_DIR}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(GLPK DEFAULT_MSG GLPK_ROOT GLPK_INCLUDE_DIR GLPK_LIBRARIES)

mark_as_advanced(GLPK_ROOT GLPK_INCLUDE_DIR GLPK_LIBRARIES)
