set(CMAKE_VERBOSE_MAKEFILE OFF)

# TODO: add necessary header files to this list
add_library(M2-supervisor OBJECT
  supervisor.cpp
  m2file.cpp
  tests.cpp # TODO: is this used?
  )

add_dependencies(M2-supervisor M2-interpreter) # supervisor.cpp requires pthread-exports.h

target_include_directories(M2-supervisor
  PUBLIC
    .
    ${CMAKE_SOURCE_DIR}/Macaulay2/c # needs scc-core.h
    ${CMAKE_SOURCE_DIR}/Macaulay2/e # needs newdelete.hpp
    ${CMAKE_BINARY_DIR}/Macaulay2/d # needs pthread-exports.h
    ${LIBXML2_INCLUDE_DIR}
  )

# Compiler warning flags
target_compile_options(M2-supervisor PRIVATE
  -Wcast-qual
  -Wno-unused
  -Wno-unused-parameter
  )
