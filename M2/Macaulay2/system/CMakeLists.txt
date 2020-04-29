###############################################################################
## This directory contains the rules for building M2-supervisor, which is a the
## entity responsible for managing Macaulay2 threads.

# TODO: add necessary header files to this list
add_library(M2-supervisor OBJECT
  supervisor.cpp
  m2file.cpp
  tests.cpp # TODO: is this used?
  )

target_link_libraries(M2-supervisor PUBLIC Threads::Threads)

# TODO: reduce these dependencies
target_include_directories(M2-supervisor PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/Macaulay2/c> # needs scc-core.h
  $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/Macaulay2/e> # needs newdelete.hpp
  $<BUILD_INTERFACE:${LIBXML2_INCLUDE_DIR}>
  $<INSTALL_INTERFACE:include/Macaulay2>
  )

install(TARGETS M2-supervisor EXPORT Macaulay2)

# Compiler warning flags
target_compile_options(M2-supervisor PRIVATE -Wcast-qual -Wno-unused)
