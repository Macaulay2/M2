###############################################################################
## This directory contains the rules for building M2-supervisor, which is a the
## entity responsible for managing Macaulay2 threads.

# TODO: add necessary header files to this list
add_library(M2-supervisor OBJECT
  supervisor.cpp
  m2file.cpp
  tests.cpp # TODO: is this used?
  )

# TODO: reduce these dependencies
target_include_directories(M2-supervisor PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/Macaulay2/c> # needs scc-core.h
  $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/Macaulay2/e> # needs newdelete.hpp
  $<INSTALL_INTERFACE:include/Macaulay2>
  )

# TODO: make this "if" unnecessary
if(BDWGC_FOUND)
  target_link_libraries(M2-supervisor PUBLIC
    Threads::Threads ${BDWGC_LIBRARIES} ${CMAKE_DL_LIBS})
  target_include_directories(M2-supervisor PUBLIC
    $<BUILD_INTERFACE:${BDWGC_INCLUDE_DIR}>)
endif()

target_include_directories(M2-supervisor PRIVATE $<${APPLE}:${ATOMIC_OPS_INCLUDE_DIR}>

# Compiler warning flags
target_compile_options(M2-supervisor PRIVATE -Wcast-qual -Wno-unused)

# Export the target
install(TARGETS M2-supervisor EXPORT Macaulay2)
