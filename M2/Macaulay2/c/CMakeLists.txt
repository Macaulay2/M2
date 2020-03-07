# Safe C Compiler (SCC)
# In this directory, we compile "scc1", the D to C translator, which we want to run
# on the "build" machine, in the sense of autoconf.

set(CMAKE_VERBOSE_MAKEFILE OFF)

find_package(BISON REQUIRED QUIET)
BISON_TARGET(scc1 ${CMAKE_CURRENT_SOURCE_DIR}/grammar.y ${CMAKE_CURRENT_BINARY_DIR}/grammar.c
  DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/grammar.h
  COMPILE_FLAGS -yvdlt)

# The header files are only listed so that they show up in IDEs
add_executable(scc1
  scc1.c
  scc1.h
  readfile.c
  readfile.h
  error.c
  error.h
  dictionary.c
  dictionary.h
  list.c
  list.h
  cprint.c
  cprint.h
  type.c
  type.h
  chk.c
  chk.h
  compat.c
  compat.h
  debugging.c
  debugging.h
  grammar.c
  scc.h
  grammar.h
  keywords.h
  )

target_include_directories(scc1
  PUBLIC
    .
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_BINARY_DIR}/include
    /usr/local/include
  )
# TODO: replace gc with a build-bdwc dependency?
target_link_libraries(scc1 gc ${LIBGDBM}) #PkgConfig::GC
target_link_directories(scc1 PRIVATE ${CMAKE_BUILD_DIR}/usr-host/lib)

# Why do we do this?
# CC scc-core.c
