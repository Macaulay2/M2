###############################################################################
## This directory contains the rules for building M2-supervisor, which is a the
## entity responsible for managing Macaulay2 threads.

# TODO: m2util.hpp is not used
add_library(M2-supervisor OBJECT
  supervisorinterface.h supervisor.hpp supervisor.cpp # used .. a lot
  m2fileinterface.h     m2file.hpp     m2file.cpp     # used by stdio.d
  pthread-methods.hpp # used by m2file
  gc_std.hpp          # used by m2file and supervisor
  mutex.h             # used by mem.hpp and pthread0.d
  mutexclass.hpp      # used by supervisor.hpp and m2file.hpp and m2util.hpp
  tests.cpp           # TS_test is used in actors5.d
  )

# TODO: reduce these dependencies
target_include_directories(M2-supervisor PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/Macaulay2/c> # needs scc-core.h
  $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/Macaulay2/e> # needs newdelete.hpp
  $<INSTALL_INTERFACE:include/Macaulay2>)

# TODO: make this "if" unnecessary
if(BDWGC_FOUND)
  target_link_libraries(M2-supervisor PUBLIC
    Threads::Threads ${BDWGC_LIBRARIES} ${CMAKE_DL_LIBS})
  target_include_directories(M2-supervisor PUBLIC
    $<BUILD_INTERFACE:${BDWGC_INCLUDE_DIR}>)
endif()

# FIXME: simply this for Mac users
target_include_directories(M2-supervisor PRIVATE $<$<BOOL:${APPLE}>:${ATOMIC_OPS_INCLUDE_DIR}>)

# Compiler warning flags
target_compile_options(M2-supervisor PRIVATE -Wcast-qual -Wno-unused)

# Export the target
install(TARGETS M2-supervisor EXPORT Macaulay2)
