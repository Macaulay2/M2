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
