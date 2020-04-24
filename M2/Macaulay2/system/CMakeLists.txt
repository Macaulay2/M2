###############################################################################
## This directory contains the rules for building M2-supervisor, which is a the
## entity responsible for managing Macaulay2 threads.

# TODO: add necessary header files to this list
add_library(M2-supervisor OBJECT
  supervisor.cpp
  m2file.cpp
  tests.cpp # TODO: is this used?
  )

target_link_libraries(M2-supervisor INTERFACE M2-interpreter Threads::Threads)

# TODO: reduce these dependencies
target_include_directories(M2-supervisor
  PUBLIC
    .
    ${CMAKE_SOURCE_DIR}/Macaulay2/c # needs scc-core.h
    ${CMAKE_SOURCE_DIR}/Macaulay2/e # needs newdelete.hpp
    ${CMAKE_BINARY_DIR}/Macaulay2/d # needs pthread-exports.h
    ${LIBXML2_INCLUDE_DIR}
  )

# Compiler warning flags
target_compile_options(M2-supervisor PRIVATE -Wcast-qual -Wno-unused)
