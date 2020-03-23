set(CMAKE_VERBOSE_MAKEFILE OFF)

# TODO: add necessary header files to this list
add_library(system OBJECT
  supervisor.cpp
  m2file.cpp
  tests.cpp
  )

target_include_directories(system
  PUBLIC
    .
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_BINARY_DIR}/include
    ${CMAKE_SOURCE_DIR}/Macaulay2/c # needs scc-core.h
    ${CMAKE_SOURCE_DIR}/Macaulay2/e # needs newdelete.hpp
    ${CMAKE_BINARY_DIR}/Macaulay2/d # needs pthread-exports.h
    ${LIBXML2_INCLUDE_DIR}
  )

add_dependencies(system interpreter)

# common warning flags
target_compile_options(system PRIVATE
  -Wall
  -Wcast-qual
  -Wextra
  -Wfatal-errors
  -Wno-parentheses
  -Wno-shadow
  -Wno-sign-compare
  -Wno-sign-conversion
  -Wno-uninitialized
  -Wno-unused
  -Wno-unused-parameter
  # temporary flags
  #-Wno-deprecated-copy
  #-Wno-unknown-pragmas
  )
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  # using Clang or AppleClang
  target_compile_options(system PRIVATE
    -Wno-mismatched-tags # needed currently, but if we clean up includes, should not be needed?
    -Wno-unknown-pragmas
    )
endif()
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # using GCC
  target_compile_options(system PRIVATE
    #-pedantic-errors
    #-Wno-extra-semi
    -Wno-error=pragmas
    #-Wno-unused-local-typedefs # one warning, in mathic.h from x-aring.cpp
    )
endif()
# add other compilers (eg: Intel, MSVC) here
